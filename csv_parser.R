#' parse_pbreport
#' 
#' Deterministic, manifest-driven parser for a "PB report" CSV exported as plain text.
#' The parser uses a YAML manifest to anchor every title and fixed row label
#' (no heuristics). It reads the report line-by-line, respecting the declared
#' hierarchy (sections → subsections → subsubsections / groups / entities / quarters),
#' with strict forward-only cursoring.
#'
#' @param csv_text Character scalar: the full CSV content (as pasted).
#' @param manifest_yaml Character scalar: YAML manifest text describing the exact
#'   structure of the report. Expected top-level keys under `manifest`:
#'   - description: free text.
#'   - q_header: character vector of nominal Q columns present (e.g., ["Q0","Q1"]).
#'     (If the CSV contains more Q columns than listed, detected CSV Qs are used.)
#'   - metadata$order: ordered vector of metadata keys that appear BEFORE the Q header.
#'   - sections: list of section nodes. Each node can contain (in the declared order):
#'       * rows: character vector of row labels (exact match).
#'       * subsections: list of subsections {subsection: "<title>", ...}.
#'       * subsubsections: list under a subsection, each {title: "<title>", rows: [...] }.
#'       * head_rows: character vector of rows immediately after the node title.
#'       * groups: list of groups {title: "<title>", rows: [...] }.
#'       * tail_rows: character vector of rows immediately after groups.
#'       * entities: list of entities {name: "<entity name>", rows: [...] }.
#'       * quarters: list of quarter blocks {quarter: "<Quarter T+k>", rows: [...] }.
#'
#' Parsing rules implemented:
#' - Empty lines are ignored.
#' - Titles are anchored by lines whose first CSV field equals the manifest title (quotes allowed).
#' - Q header is detected from the first line matching exactly ",Q\\d+(,Q\\d+)*" (any length).
#' - Metadata lines (before the Q header) are read in the exact order given by manifest.metadata$order,
#'   expecting "Key:,Value" or 'Key:,"Value"'.
#' - For any rows-block, the parser reads exactly length(rows) subsequent data lines,
#'   validates that each leftmost label equals the manifest label at that position (after trimming / unquoting).
#' - Entities: immediately after the section title, the parser expects a single line
#'   whose first CSV field equals the entity name (quoted or unquoted), then its rows block.
#' - Groups: expects the exact group title line, then its fixed rows block.
#' - Quarters: emits one table **per quarter**. The quarter label is part of the table key
#'   and available as `__quarter` in the long output.
#'
#' Typing & normalization:
#' - All Q columns are cleaned to numeric via a common cleaner (remove %, commas, spaces and any
#'   non-numeric characters except . and -). Non-numeric become NA.
#'
#' Errors & validation:
#' - If `strict=TRUE` (default), the parser stops on:
#'   * missing section/subsection/subsubsection/group/entity/quarter title,
#'   * mismatched number of data lines vs. manifest `rows`,
#'   * any row label mismatch,
#'   * missing Q header or mismatch in discovered columns.
#' - If `strict=FALSE`, malformed blocks are skipped with a warning; a summary of skips is attached
#'   as attribute `skipped` on the returned list.
#'
#' Output:
#' - A named list with:
#'   * $meta_tbl: tibble of metadata (Key, Value), in declared order
#'   * $tables: a named list of tibbles, each with columns: Item, Q0..Qk
#'   * $tables_index: tibble with one row per emitted table: key, section, subsection, group,
#'       quarter, entity, start_line, end_line, n_rows
#'   * $all_long (if return_long=TRUE): tibble stacking all tables with columns
#'       __table_key, __section, __subsection, __group, __quarter, __entity, Item, Quarter, Value
#'
#' Dependencies: base R + yaml + tidyverse (and openxlsx optionally for Excel export).
#' No file I/O; works in RStudio.
#'
#'
#'
#'
# --- Shared helpers (global) ---
`%||%` <- function(x, y) if (is.null(x)) y else x

trim_ws <- function(x) gsub("^\\s+|\\s+$", "", x)

unquote <- function(x) {
  x <- trim_ws(x)
  if (grepl('^".*"$', x)) substr(x, 2, nchar(x) - 1) else x
}

is_empty_line <- function(line) nchar(trim_ws(line)) == 0

left_field <- function(line) {
  if (grepl('^\\s*"', line)) {
    m <- regexpr('^\\s*"([^"]*)"', line, perl = TRUE)
    if (m[1] == -1) return(trim_ws(line))
    val <- regmatches(line, m)
    return(unquote(val))
  } else {
    parts <- strsplit(line, ",", fixed = TRUE)[[1]]
    return(trim_ws(parts[1]))
  }
}


#' @return Named list as described above.
#' @export
parse_pbreport <- function(csv_text, manifest_yaml, strict = TRUE, return_long = TRUE) {
  stopifnot(is.character(csv_text), length(csv_text) == 1L)
  stopifnot(is.character(manifest_yaml), length(manifest_yaml) == 1L)
  
  # --- Libraries
  suppressPackageStartupMessages({
    requireNamespace("yaml", quietly = TRUE)
    requireNamespace("tidyverse", quietly = TRUE)
  })
  
  
  # --- Split lines and pre-clean
  raw_lines <- strsplit(csv_text, "\r?\n", perl = TRUE)[[1]]
  # remove trailing carriage returns and keep positions for index reporting
  lines <- raw_lines
  # We'll index lines by their 1-based position in 'lines'
  
  # --- Read manifest
  manifest_all <- yaml::yaml.load(manifest_yaml)
  manifest <- manifest_all$manifest
  if (is.null(manifest)) stop("Manifest YAML must have top-level key 'manifest'.")
  
  # --- Find Q header from CSV (dynamic, takes precedence when longer than manifest listed)
  # --- Find Q header from CSV (ONLY from the report)
  q_cols_detected <- read_q_header(lines = lines)
  if (length(q_cols_detected) == 0) stop("Q header not found in CSV (looking for a line like ',Q0,Q1,...').")
  
  q_cols <- q_cols_detected
  attr(q_cols, "line") <- attr(q_cols_detected, "line")
  
  # --- Parse metadata (must occur before q-header)
  meta_tbl <- parse_metadata(csv_lines = lines, manifest = manifest, q_header_line = attr(q_cols_detected, "line"))
  
  # --- Cursor state & accumulators
  cur <- list(pos = attr(q_cols_detected, "line"))  # start AFTER metadata, positioned at Q header line
  tables <- list()
  tables_index <- list()
  skipped <- list()
  
  # Fast forward cursor to first non-empty after Q header line
  advance_to_next_nonempty <- function(cur) {
    i <- cur$pos + 1L
    while (i <= length(lines) && is_empty_line(lines[i])) i <- i + 1L
    cur$pos <- i - 1L
    cur
  }
  cur <- advance_to_next_nonempty(cur)
  
  # --- Builders ----
  
  make_table_key <- function(section = NULL, subsection = NULL, subsub = NULL, group = NULL, quarter = NULL, entity = NULL) {
    parts <- c(section, subsection, subsub, group, quarter, entity)
    parts <- parts[!is.null(parts) & nzchar(parts)]
    paste(parts, collapse = " / ")
  }
  
  coerce_numeric <- function(df, q_cols) {
    clean_num <- function(x) {
      x <- as.character(x)
      x <- gsub("%", "", x, fixed = TRUE)
      x <- gsub(",", "", x, fixed = TRUE)
      x <- gsub("\\s+", "", x, perl = TRUE)
      # Remove anything not digit, dot, or minus (keep scientific if present)
      x <- gsub("[^0-9eE+\\.-]", "", x, perl = TRUE)
      suppressWarnings(as.numeric(x))
    }
    df[q_cols] <- lapply(df[q_cols], clean_num)
    df
  }
  
  cursor_find_line <- function(csv_lines, pattern_exact, from) {
    # Find the next line whose FIRST FIELD equals pattern_exact (ignoring quotes/space)
    target <- trim_ws(pattern_exact)
    i <- from + 1L
    while (i <= length(csv_lines)) {
      if (!is_empty_line(csv_lines[i])) {
        lf <- left_field(csv_lines[i])
        if (identical(lf, target)) return(i)
      }
      i <- i + 1L
    }
    return(NA_integer_)
  }
  
  read_block_rows <- function(csv_lines, start_after, n_rows, q_cols, strict, expected_labels, context_key) {
    start_line <- start_after + 1L
    # Skip empty lines between title and data if any
    i <- start_line
    while (i <= length(csv_lines) && is_empty_line(csv_lines[i])) i <- i + 1L
    start_line <- i
    
    read_one <- function(line) {
      # split into fields; tolerate quoted item + numeric fields
      # We'll parse by base CSV read on a single line using textConnection to be safe with commas
      suppressWarnings({
        df <- tryCatch(
          utils::read.csv(text = line, header = FALSE, stringsAsFactors = FALSE, strip.white = FALSE),
          error = function(e) NULL
        )
      })
      if (is.null(df) || ncol(df) == 0) {
        # fallback: manual split (best effort)
        parts <- strsplit(line, ",", fixed = TRUE)[[1]]
        parts <- trimws(parts)
        return(parts)
      }
      as.character(df[1, ])
    }
    
    rows <- vector("list", n_rows)
    end_line <- start_line - 1L
    for (k in seq_len(n_rows)) {
      idx <- start_line + (k - 1L)
      if (idx > length(csv_lines)) {
        msg <- sprintf("Ran out of lines while reading rows for '%s'. Expected %d rows, found %d.", context_key, n_rows, k - 1L)
        if (strict) stop(msg) else warning(msg)
        end_line <- idx - 1L
        rows <- rows[seq_len(k - 1L)]
        break
      }
      while (idx <= length(csv_lines) && is_empty_line(csv_lines[idx])) idx <- idx + 1L
      # Guard again after skipping empties
      if (idx > length(csv_lines)) {
        msg <- sprintf("Ran out of lines while reading rows for '%s' (after skipping empties).", context_key)
        if (strict) stop(msg) else warning(msg)
        end_line <- idx - 1L
        rows <- rows[seq_len(k - 1L)]
        break
      }
      parts <- read_one(csv_lines[idx])
      # parts[1] is Item (maybe quoted)
      item_lab <- unquote(parts[1])
      exp_lab <- expected_labels[k]
      if (!identical(item_lab, exp_lab)) {
        msg <- sprintf("Row label mismatch in '%s' at line %d: saw '%s' but expected '%s'.", context_key, idx, item_lab, exp_lab)
        if (strict) stop(msg) else warning(msg)
      }
      # Map values to q_cols length
      vals <- parts[-1]
      # Ensure we have as many as q_cols (pad or truncate)
      if (length(vals) < length(q_cols)) vals <- c(vals, rep(NA_character_, length(q_cols) - length(vals)))
      if (length(vals) > length(q_cols)) vals <- vals[seq_len(length(q_cols))]
      rows[[k]] <- c(item_lab, vals)
      end_line <- idx
    }
    
    if (length(rows) == 0) {
      df <- tibble::tibble(Item = character(), !!!setNames(rep(list(numeric()), length(q_cols)), q_cols))
      return(list(df = df, start_line = start_line, end_line = end_line))
    }
    
    mat <- do.call(rbind, rows)
    df <- tibble::as_tibble(as.data.frame(mat, stringsAsFactors = FALSE))
    names(df) <- c("Item", q_cols)
    df <- coerce_numeric(df, q_cols)
    list(df = df, start_line = start_line, end_line = end_line)
  }
  
  # --- FIX: append instead of overwrite when key already exists
  emit_table <- function(tbl, key, ctx, start_line, end_line) {
    if (!is.null(tables[[key]])) {
      tables[[key]] <<- dplyr::bind_rows(tables[[key]], tbl)
    } else {
      tables[[key]] <<- tbl
    }
    tables_index[[length(tables_index) + 1L]] <<- tibble::tibble(
      key = key,
      section = ctx$section %||% NA_character_,
      subsection = ctx$subsection %||% NA_character_,
      subsubsection = ctx$subsubsection %||% NA_character_,
      group = ctx$group %||% NA_character_,
      quarter = ctx$quarter %||% NA_character_,
      entity = ctx$entity %||% NA_character_,
      start_line = start_line,
      end_line = end_line,
      n_rows = nrow(tbl)
    )
  }
  
  build_rows_block <- function(cur, title_for_context, rows_vec, ctx) {
    # Immediately read rows_vec after current cursor
    res <- read_block_rows(csv_lines = lines, start_after = cur$pos, n_rows = length(rows_vec),
                           q_cols = q_cols, strict = strict, expected_labels = rows_vec,
                           context_key = title_for_context)
    key <- make_table_key(section = ctx$section, subsection = ctx$subsection, subsub = ctx$subsubsection,
                          group = ctx$group, quarter = ctx$quarter, entity = ctx$entity)
    emit_table(res$df, key, ctx, res$start_line, res$end_line)
    cur$pos <- res$end_line
    cur
  }
  
  build_group <- function(cur, group_node, ctx) {
    title <- group_node$title
    pos <- cursor_find_line(lines, title, cur$pos)
    if (is.na(pos)) {
      msg <- sprintf("Group title '%s' not found under '%s'.", title, ctx$subsection %||% ctx$section)
      if (strict) stop(msg) else { warning(msg); return(cur) }
    }
    
    # Decide if the matched line is a pure title (1 field) or an inline data row (>=2 fields)
    is_pure_title <- FALSE
    parsed <- tryCatch(utils::read.csv(text = lines[pos], header = FALSE, stringsAsFactors = FALSE),
                       error = function(e) NULL)
    if (!is.null(parsed)) {
      is_pure_title <- (ncol(parsed) == 1L)
    } else {
      # fallback heuristic: no comma → pure title
      is_pure_title <- !grepl(",", lines[pos], fixed = TRUE)
    }
    
    # Context
    ctx2 <- ctx
    ctx2$group <- title
    
    if (is_pure_title) {
      # Standard case: title line, then rows after it
      cur$pos <- pos
      cur <- build_rows_block(cur,
                              paste0("Group '", title, "'"),
                              group_node$rows %||% character(),
                              ctx2)
    } else {
      # Inline group: the matched line is already the first data row.
      # Force rows to be read starting at this very line by setting pos-1.
      cur$pos <- pos - 1L
      cur <- build_rows_block(cur,
                              paste0("Group '", title, "' (inline)"),
                              group_node$rows %||% character(),
                              ctx2)
    }
    cur
  }
  build_entity <- function(cur, entity_node, ctx) {
    # Expect immediate entity name line (first field equals entity name)
    expected <- entity_node$name
    pos <- cursor_find_line(lines, expected, cur$pos)
    if (is.na(pos)) {
      msg <- sprintf("Entity '%s' not found under section '%s'.", expected, ctx$section)
      if (strict) stop(msg) else { warning(msg); return(cur) }
    }
    cur$pos <- pos
    ctx2 <- ctx
    ctx2$entity <- expected
    # Now read rows for this entity
    cur <- build_rows_block(cur, paste0("Entity '", expected, "'"), entity_node$rows %||% character(), ctx2)
    cur
  }
  
  build_quarters <- function(cur, quarters_list, ctx) {
    # For each quarter block, find the quarter title then read its rows.
    for (qnode in quarters_list) {
      qtitle <- qnode$quarter
      pos <- cursor_find_line(lines, qtitle, cur$pos)
      if (is.na(pos)) {
        msg <- sprintf("Quarter title '%s' not found under '%s'.", qtitle, ctx$section)
        if (strict) stop(msg) else { warning(msg); next }
      }
      cur$pos <- pos
      ctx2 <- ctx
      ctx2$quarter <- qtitle
      cur <- build_rows_block(cur, paste0("Quarter '", qtitle, "'"), qnode$rows %||% character(), ctx2)
    }
    cur
  }
  
  build_subsubsections <- function(cur, subsub_list, ctx) {
    for (node in subsub_list) {
      title <- node$title
      pos <- cursor_find_line(lines, title, cur$pos)
      if (is.na(pos)) {
        msg <- sprintf("Subsubsection title '%s' not found under '%s / %s'.", title, ctx$section, ctx$subsection %||% "")
        if (strict) stop(msg) else { warning(msg); next }
      }
      cur$pos <- pos
      ctx2 <- ctx
      ctx2$subsubsection <- title
      # rows
      if (!is.null(node$rows)) {
        cur <- build_rows_block(cur, paste0("Subsubsection '", title, "' rows"), node$rows, ctx2)
      }
      # reset subsub label after emission
      ctx2$subsubsection <- NULL
    }
    cur
  }
  
  build_subsection <- function(cur, subsection_node, ctx) {
    title <- subsection_node$subsection
    pos <- cursor_find_line(lines, title, cur$pos)
    if (is.na(pos)) {
      msg <- sprintf("Subsection '%s' not found under section '%s'.", title, ctx$section)
      if (strict) stop(msg) else { warning(msg); return(cur) }
    }
    cur$pos <- pos
    ctx2 <- ctx
    ctx2$subsection <- title
    
    # In-declared-order consumption of possible blocks:
    # head_rows → rows → subsubsections → groups → tail_rows → entities → quarters
    if (!is.null(subsection_node$head_rows)) {
      cur <- build_rows_block(cur, paste0("Head rows of '", title, "'"), subsection_node$head_rows, ctx2)
    }
    if (!is.null(subsection_node$rows)) {
      cur <- build_rows_block(cur, paste0("Rows of '", title, "'"), subsection_node$rows, ctx2)
    }
    if (!is.null(subsection_node$subsubsections)) {
      cur <- build_subsubsections(cur, subsection_node$subsubsections, ctx2)
    }
    if (!is.null(subsection_node$groups)) {
      for (g in subsection_node$groups) cur <- build_group(cur, g, ctx2)
    }
    if (!is.null(subsection_node$tail_rows)) {
      cur <- build_rows_block(cur, paste0("Tail rows of '", title, "'"), subsection_node$tail_rows, ctx2)
    }
    if (!is.null(subsection_node$entities)) {
      for (e in subsection_node$entities) cur <- build_entity(cur, e, ctx2)
    }
    if (!is.null(subsection_node$quarters)) {
      cur <- build_quarters(cur, subsection_node$quarters, ctx2)
    }
    cur
  }
  
  build_section <- function(cur, section_node) {
    title <- section_node$section
    pos <- cursor_find_line(lines, title, cur$pos)
    if (is.na(pos)) {
      msg <- sprintf("Section '%s' not found.", title)
      if (strict) stop(msg) else { warning(msg); return(cur) }
    }
    cur$pos <- pos
    ctx <- list(section = title, subsection = NULL, subsubsection = NULL, group = NULL, quarter = NULL, entity = NULL)
    
    # Consume in declared order at this node level as well
    if (!is.null(section_node$head_rows)) {
      cur <- build_rows_block(cur, paste0("Head rows of '", title, "'"), section_node$head_rows, ctx)
    }
    if (!is.null(section_node$rows)) {
      cur <- build_rows_block(cur, paste0("Rows of '", title, "'"), section_node$rows, ctx)
    }
    if (!is.null(section_node$subsections)) {
      for (ss in section_node$subsections) cur <- build_subsection(cur, ss, ctx)
    }
    if (!is.null(section_node$subsubsections)) {
      # Typically subsubsections are nested under subsections, but support if declared here.
      cur <- build_subsubsections(cur, section_node$subsubsections, ctx)
    }
    if (!is.null(section_node$groups)) {
      for (g in section_node$groups) cur <- build_group(cur, g, ctx)
    }
    if (!is.null(section_node$tail_rows)) {
      cur <- build_rows_block(cur, paste0("Tail rows of '", title, "'"), section_node$tail_rows, ctx)
    }
    if (!is.null(section_node$entities)) {
      for (e in section_node$entities) cur <- build_entity(cur, e, ctx)
    }
    if (!is.null(section_node$quarters)) {
      cur <- build_quarters(cur, section_node$quarters, ctx)
    }
    cur
  }
  
  # --- Walk all sections in manifest order
  for (sec in manifest$sections %||% list()) {
    # Keep a copy of cur in case of non-strict skipping
    cur_before <- cur
    tryCatch({
      cur <<- build_section(cur, sec)
    }, error = function(e) {
      if (strict) stop(e$message) else {
        warning(e$message)
        skipped[[length(skipped) + 1L]] <<- list(scope = sec$section %||% NA_character_, message = e$message)
        cur <<- cur_before
      }
    })
  }
  
  # --- Build index tibble
  tables_index_tbl <- dplyr::bind_rows(tables_index %||% list())
  
  # --- Return result
  out <- list(
    meta_tbl = meta_tbl,
    tables = tables,
    tables_index = tables_index_tbl
  )
  
  if (isTRUE(return_long)) {
    # Stack all tables into long format with context columns parsed from their keys
    if (length(tables)) {
      long_list <- vector("list", length(tables))
      nm <- names(tables)
      for (i in seq_along(tables)) {
        df <- tables[[i]]
        key <- nm[i]
        # Find index row for this key to fetch context
        idxrow <- tables_index_tbl[tables_index_tbl$key == key, ]
        ctx_cols <- dplyr::tibble(
          `__table_key` = key,
          `__section`   = idxrow$section %||% NA_character_,
          `__subsection`= idxrow$subsection %||% NA_character_,
          `__group`     = idxrow$group %||% NA_character_,
          `__quarter`   = idxrow$quarter %||% NA_character_,
          `__entity`    = idxrow$entity %||% NA_character_
        )
        df_long <- df %>%
          tidyr::pivot_longer(cols = dplyr::all_of(q_cols), names_to = "Quarter", values_to = "Value")
        df_long <- dplyr::bind_cols(ctx_cols[rep(1, nrow(df_long)), ], df_long)
        long_list[[i]] <- df_long
      }
      out$all_long <- dplyr::bind_rows(long_list)
    } else {
      out$all_long <- tibble::tibble(
        `__table_key` = character(), `__section` = character(), `__subsection` = character(),
        `__group` = character(), `__quarter` = character(), `__entity` = character(),
        Item = character(), Quarter = character(), Value = numeric()
      )
    }
  }
  
  if (!strict && length(skipped)) attr(out, "skipped") <- skipped
  out
}

# --- Helper: detect Q header line and return discovered Q columns (Q0..Qk)
#' @keywords internal
read_q_header <- function(lines) {
  # Look for the first line that exactly matches ",Q0,Q1,..." (allow arbitrary length)
  pat <- "^\\s*,\\s*Q\\d+(\\s*,\\s*Q\\d+)*\\s*$"
  for (i in seq_along(lines)) {
    ln <- lines[i]
    if (grepl(pat, ln, perl = TRUE)) {
      # parse Qs
      parts <- strsplit(ln, ",", fixed = TRUE)[[1]]
      parts <- trimws(parts)
      qcols <- parts[nzchar(parts)]
      attr(qcols, "line") <- i
      return(qcols)
    }
  }
  character()
}

# --- Helper: parse top metadata before Q header, in declared order
#' @keywords internal
parse_metadata <- function(csv_lines, manifest, q_header_line) {
  meta_order <- manifest$metadata$order %||% character()
  if (!length(meta_order)) {
    return(tibble::tibble(Key = character(), Value = character()))
  }
  # Restrict search up to q_header_line - 1
  search_lines <- csv_lines[seq_len(q_header_line - 1L)]
  
  # Build map of first-field to full line index (latest occurrence wins, but we need ordered lookup)
  key_to_line <- list()
  for (i in seq_along(search_lines)) {
    line <- search_lines[i]
    if (is_empty_line(line)) next
    # Expect "Key:," as first token; extract first token before comma
    first <- left_field(line)
    # Many metadata lines include a colon in the printed label (e.g., "Game:")
    # manifest uses "Game" → normalize by removing trailing colon for matching
    first_norm <- sub(":$", "", first)
    key_to_line[[first_norm]] <- i
  }
  
  get_value_from_line <- function(line) {
    # return the remainder after first comma as a single field (quoted or not)
    df <- tryCatch(
      utils::read.csv(text = line, header = FALSE, stringsAsFactors = FALSE, strip.white = FALSE),
      error = function(e) NULL
    )
    if (is.null(df) || ncol(df) < 2) {
      # fallback: split
      parts <- strsplit(line, ",", fixed = TRUE)[[1]]
      val <- if (length(parts) >= 2) parts[2] else ""
      return(unquote(val))
    } else {
      return(as.character(df[1, 2]))
    }
  }
  
  vals <- character(length(meta_order))
  for (k in seq_along(meta_order)) {
    key <- meta_order[k]
    li <- key_to_line[[key]]
    if (is.null(li)) stop(sprintf("Metadata key '%s' not found before Q header.", key))
    line <- search_lines[[li]]
    vals[k] <- get_value_from_line(line)
  }
  tibble::tibble(Key = meta_order, Value = vals)
}

# --- Optional: Excel export (if openxlsx available)
#' Write parsed tables to an Excel workbook (one sheet per table) + metadata and index.
#' @param tables Named list of tibbles from parse_pbreport()$tables
#' @param meta_tbl Tibble from parse_pbreport()$meta_tbl
#' @param tables_index Tibble from parse_pbreport()$tables_index
#' @param path Output path (default "PBreport_tables.xlsx")
#' @export
write_tables_excel <- function(tables, meta_tbl, tables_index, path = "PBreport_tables.xlsx") {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Package 'openxlsx' is not installed. Please install it to use Excel export.")
  }
  wb <- openxlsx::createWorkbook()
  # Metadata
  openxlsx::addWorksheet(wb, "metadata")
  openxlsx::writeData(wb, "metadata", meta_tbl)
  # Index
  openxlsx::addWorksheet(wb, "tables_index")
  openxlsx::writeData(wb, "tables_index", tables_index)
  # Tables (one sheet per table)
  if (length(tables)) {
    for (nm in names(tables)) {
      sheet <- nm
      # Excel sheet name max 31 chars; sanitize
      sheet <- gsub("[\\:\\/\\?\\*\\[\\]]", "_", sheet)
      if (nchar(sheet) > 31) sheet <- paste0(substr(sheet, 1, 27), "...", substr(digest::digest(nm), 1, 3))
      # Ensure uniqueness
      if (sheet %in% openxlsx::sheets(wb)) {
        sheet <- paste0(substr(sheet, 1, 27), "_", sample(1000:9999, 1))
      }
      openxlsx::addWorksheet(wb, sheet)
      openxlsx::writeData(wb, sheet, tables[[nm]])
    }
  }
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  invisible(path)
}
