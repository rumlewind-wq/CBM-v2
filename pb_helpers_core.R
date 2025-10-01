# pb_helpers_core.R
# Common utilities for pb_* calculators built on top of parse_pbreport() output.
# Every helper is pure and does not mutate its inputs.

pb_helper_require_packages <- function(packages, context = "Denne funktion") {
  pkgs <- unique(as.character(packages))
  pkgs <- pkgs[nzchar(pkgs)]
  if (!length(pkgs)) return(invisible(TRUE))
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) {
    stop(
      sprintf(
        "%s kræver følgende R-pakker: %s. Installer dem med install.packages().",
        context,
        paste(missing, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

pb_helper_chr <- function(x) {
  if (is.null(x)) return(character())
  if (is.character(x)) return(x)
  if (is.factor(x)) return(as.character(x))
  if (is.logical(x) || is.numeric(x) || inherits(x, "Date")) return(as.character(x))
  vapply(x, function(elem) paste(elem, collapse = " "), character(1))
}

pb_helper_norm <- function(x) {
  y <- pb_helper_chr(x)
  is_missing <- is.na(x)
  y[is_missing] <- NA_character_
  y <- tolower(y)
  y <- gsub("[^a-z0-9]+", " ", y, perl = TRUE)
  y <- gsub("\\s+", " ", y, perl = TRUE)
  y <- trimws(y)
  y[y == ""] <- ""
  y[is_missing] <- NA_character_
  y
}

pb_helper_prepare_all_long <- function(res, entity = NULL,
                                       required_cols = c("Quarter", "Item", "Value", "__section", "__subsection", "__group"),
                                       context = "Datagrundlag") {
  pb_helper_require_packages("tibble", context)
  if (!is.list(res) || is.null(res$all_long)) {
    stop(sprintf("%s forventer et parser-resultat (liste) med elementet res$all_long.", context), call. = FALSE)
  }
  al <- tibble::as_tibble(res$all_long)
  missing_cols <- setdiff(required_cols, names(al))
  if (length(missing_cols)) {
    stop(
      sprintf(
        "%s mangler følgende kolonner i res$all_long: %s. Opdater parseren til at inkludere dem.",
        context,
        paste(missing_cols, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  char_cols <- intersect(
    c("Quarter", "Item", "__section", "__subsection", "__subsubsection", "__group", "__entity", "__table_key", "__quarter"),
    names(al)
  )
  for (nm in char_cols) {
    al[[nm]] <- pb_helper_chr(al[[nm]])
  }
  if ("Value" %in% names(al)) {
    suppressWarnings(al$Value <- as.numeric(al$Value))
  }
  if (!is.null(entity)) {
    if ("__entity" %in% names(al)) {
      ent <- pb_helper_chr(al$`__entity`)
      if (all(!nzchar(ent))) {
        stop(
          sprintf(
            "%s: __entity-kolonnen er tom, så filtrering på entity='%s' kan ikke udføres.",
            context,
            entity
          ),
          call. = FALSE
        )
      }
      keep <- ent == entity
      al <- al[keep, , drop = FALSE]
      if (!nrow(al)) {
        stop(sprintf("%s: Ingen rækker matcher entity='%s'.", context, entity), call. = FALSE)
      }
    } else {
      stop(
        sprintf("%s: __entity-kolonnen findes ikke i res$all_long, så entity-filtrering er ikke mulig.", context),
        call. = FALSE
      )
    }
  }
  rownames(al) <- NULL
  al
}

pb_helper_quarter_levels <- function(al, quarter_col = "Quarter") {
  if (!quarter_col %in% names(al)) return(character())
  q_vals <- pb_helper_chr(al[[quarter_col]])
  q_vals <- q_vals[nzchar(q_vals)]
  q_vals <- unique(q_vals)
  if (!length(q_vals)) return(q_vals)
  q_num <- suppressWarnings(as.numeric(sub("^.*?(\\d+)$", "\\1", q_vals, perl = TRUE)))
  ord <- order(ifelse(is.na(q_num), Inf, q_num), q_vals)
  q_vals[ord]
}

pb_helper_select_quarters <- function(quarters_all, q = "latest", context = "Kvartalsfilter") {
  quarters_all <- pb_helper_chr(quarters_all)
  quarters_all <- quarters_all[nzchar(quarters_all)]
  if (!length(quarters_all)) {
    stop(sprintf("%s: Ingen kvartaler fundet i datagrundlaget.", context), call. = FALSE)
  }
  if (is.null(q) || (is.character(q) && length(q) == 1 && tolower(q) == "latest")) {
    return(tail(quarters_all, 1))
  }
  if (is.character(q) && length(q) == 1 && tolower(q) == "all") {
    return(quarters_all)
  }
  if (is.numeric(q) && length(q) == 1 && is.finite(q) && q >= 1) {
    idx <- as.integer(q)
    if (idx > length(quarters_all)) {
      stop(
        sprintf(
          "%s: Indekset %d overstiger antallet af kendte kvartaler (%d).",
          context,
          idx,
          length(quarters_all)
        ),
        call. = FALSE
      )
    }
    return(quarters_all[idx])
  }
  if (is.character(q)) {
    q_norm <- pb_helper_norm(q)
    quarters_norm <- pb_helper_norm(quarters_all)
    matches <- quarters_all[quarters_norm %in% q_norm]
    if (!length(matches)) {
      stop(
        sprintf(
          "%s: Ingen af de angivne kvartaler matcher kendte labels (%s).",
          context,
          paste(q, collapse = ", ")
        ),
        call. = FALSE
      )
    }
    return(unique(matches))
  }
  stop(
    sprintf(
      "%s: Argumentet q skal være 'latest', 'all', et indeks eller en vektor af kendte kvartaler.",
      context
    ),
    call. = FALSE
  )
}

pb_helper_filter_quarters <- function(al, q = "latest", quarter_col = "Quarter", context = "Kvartalsfilter") {
  quarters_all <- pb_helper_quarter_levels(al, quarter_col)
  quarters_sel <- pb_helper_select_quarters(quarters_all, q, context)
  keep <- al[[quarter_col]] %in% quarters_sel
  list(
    data = al[keep, , drop = FALSE],
    quarters = quarters_sel,
    quarters_all = quarters_all
  )
}

pb_helper_empty_result <- function() {
  pb_helper_require_packages("tibble", "Resultatopbygning")
  tibble::tibble(
    Quarter = character(),
    Side = character(),
    Product = character(),
    Maturity = character(),
    Component = character(),
    Value = double()
  )
}

pb_helper_make_result <- function(df, trace = NULL, context = "Resultatopbygning") {
  pb_helper_require_packages("tibble", context)
  required <- c("Quarter", "Side", "Product", "Maturity", "Component", "Value")
  missing <- setdiff(required, names(df))
  if (length(missing)) {
    stop(
      sprintf(
        "%s: Tabellen mangler kolonnerne %s.",
        context,
        paste(missing, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  out <- tibble::as_tibble(df[, required])
  rownames(out) <- NULL
  if (!is.null(trace)) {
    attr(out, "trace") <- trace
  }
  out
}

pb_helper_manifest_load <- function(res = NULL, manifest_yaml = NULL, ensure_sections = TRUE,
                                    context = "Manifestopslag") {
  if (is.null(manifest_yaml) && !is.null(res)) {
    manifest_yaml <- attr(res, "manifest_yaml")
    if (is.null(manifest_yaml) && !is.null(res$manifest_yaml)) {
      manifest_yaml <- res$manifest_yaml
    }
  }
  if (is.null(manifest_yaml)) {
    default_path <- file.path(getwd(), "manifest.yml")
    if (file.exists(default_path)) {
      manifest_yaml <- paste(readLines(default_path, warn = FALSE), collapse = "\n")
    }
  }
  if (is.null(manifest_yaml)) {
    stop(sprintf("%s: manifest_yaml mangler; angiv tekststrengen eller sørg for at res indeholder den.", context), call. = FALSE)
  }
  pb_helper_require_packages("yaml", context)
  manifest_all <- yaml::yaml.load(manifest_yaml)
  manifest <- manifest_all$manifest
  if (is.null(manifest)) manifest <- manifest_all
  if (ensure_sections && is.null(manifest$sections)) {
    stop(sprintf("%s: Manifestet indeholder ikke nøglen 'sections'.", context), call. = FALSE)
  }
  list(manifest = manifest, yaml = manifest_yaml)
}

pb_helper_manifest_path <- function(section = NULL, subsection = NULL, subsubsection = NULL,
                                    group = NULL, entity = NULL, quarter = NULL) {
  parts <- c(section, subsection, subsubsection, group, entity, quarter)
  parts <- parts[nzchar(pb_helper_chr(parts))]
  paste(parts, collapse = " → ")
}

pb_helper_manifest_find_node <- function(nodes, title, field, context) {
  if (is.null(title)) return(NULL)
  if (is.null(nodes)) {
    stop(sprintf("%s: Kan ikke finde '%s', fordi %s-listen mangler i manifestet.", context, title, field), call. = FALSE)
  }
  if (!length(nodes)) {
    stop(sprintf("%s: Manifestet indeholder ingen poster for '%s'.", context, field), call. = FALSE)
  }
  titles <- vapply(
    nodes,
    function(node) {
      values <- pb_helper_chr(node[[field]])
      if (length(values)) values[[1]] else ""
    },
    character(1)
  )
  norm_titles <- pb_helper_norm(titles)
  idx <- which(norm_titles == pb_helper_norm(title))
  if (!length(idx)) {
    stop(sprintf("%s: '%s' findes ikke i manifestet.", context, title), call. = FALSE)
  }
  nodes[[idx[1]]]
}

pb_helper_manifest_locate <- function(manifest, section, subsection = NULL, subsubsection = NULL,
                                      group = NULL, entity = NULL, quarter = NULL,
                                      context = "Manifestopslag") {
  sec_node <- pb_helper_manifest_find_node(manifest$sections, section, "section", context)
  sub_node <- NULL
  if (!is.null(subsection)) {
    sub_node <- pb_helper_manifest_find_node(sec_node$subsections, subsection, "subsection", context)
  }
  subsub_node <- NULL
  if (!is.null(subsubsection)) {
    if (is.null(sub_node$subsubsections)) {
      stop(sprintf("%s: Under-undersektioner er ikke deklareret under '%s'.", context, subsection), call. = FALSE)
    }
    subsub_node <- pb_helper_manifest_find_node(sub_node$subsubsections, subsubsection, "title", context)
  }
  group_node <- NULL
  if (!is.null(group)) {
    nodes <- if (!is.null(sub_node$groups)) sub_node$groups else sec_node$groups
    group_node <- pb_helper_manifest_find_node(nodes, group, "title", context)
  }
  entity_node <- NULL
  if (!is.null(entity)) {
    entity_node <- pb_helper_manifest_find_node(sec_node$entities, entity, "name", context)
  }
  quarter_node <- NULL
  if (!is.null(quarter)) {
    nodes <- if (!is.null(sub_node$quarters)) sub_node$quarters else sec_node$quarters
    quarter_node <- pb_helper_manifest_find_node(nodes, quarter, "quarter", context)
  }
  list(
    section = sec_node,
    subsection = sub_node,
    subsubsection = subsub_node,
    group = group_node,
    entity = entity_node,
    quarter = quarter_node
  )
}

pb_helper_manifest_rows <- function(manifest, section, subsection = NULL, subsubsection = NULL,
                                    group = NULL, entity = NULL, quarter = NULL,
                                    slot = "rows", allow_missing = TRUE,
                                    context = "Manifestopslag") {
  nodes <- pb_helper_manifest_locate(manifest, section, subsection, subsubsection, group, entity, quarter, context)
  candidate <- nodes[[slot]]
  if (is.null(candidate)) {
    holder <- nodes$group %||% nodes$subsubsection %||% nodes$subsection %||% nodes$entity %||% nodes$quarter %||% nodes$section
    if (!is.null(holder)) {
      candidate <- holder[[slot]]
    }
  }
  if (is.null(candidate)) {
    if (allow_missing) return(character())
    stop(
      sprintf(
        "%s: Ingen '%s' fundet for stien %s.",
        context,
        slot,
        pb_helper_manifest_path(section, subsection, subsubsection, group, entity, quarter)
      ),
      call. = FALSE
    )
  }
  pb_helper_chr(candidate)
}

pb_helper_match_values <- function(values, patterns) {
  if (is.null(patterns) || !length(patterns)) {
    return(rep(TRUE, length(values)))
  }
  vals_norm <- pb_helper_norm(values)
  pats_norm <- unique(pb_helper_norm(patterns))
  pats_norm <- pats_norm[!is.na(pats_norm)]
  if (!length(pats_norm)) {
    return(rep(TRUE, length(values)))
  }
  vapply(vals_norm, function(v) !is.na(v) && v %in% pats_norm, logical(1))
}

pb_helper_filter_all_long <- function(al,
                                      section = NULL,
                                      subsection = NULL,
                                      subsubsection = NULL,
                                      group = NULL,
                                      entity = NULL,
                                      items = NULL) {
  stopifnot(is.data.frame(al))
  n <- nrow(al)
  if (!n) {
    return(al)
  }
  keep <- rep(TRUE, n)
  if ("__section" %in% names(al)) {
    keep <- keep & pb_helper_match_values(al$`__section`, section)
  }
  if ("__subsection" %in% names(al)) {
    keep <- keep & pb_helper_match_values(al$`__subsection`, subsection)
  }
  if ("__subsubsection" %in% names(al)) {
    keep <- keep & pb_helper_match_values(al$`__subsubsection`, subsubsection)
  }
  if ("__group" %in% names(al)) {
    keep <- keep & pb_helper_match_values(al$`__group`, group)
  }
  if (!is.null(entity) && "__entity" %in% names(al)) {
    keep <- keep & pb_helper_match_values(al$`__entity`, entity)
  }
  if (!is.null(items)) {
    keep <- keep & pb_helper_match_values(al$Item, items)
  }
  al[keep, , drop = FALSE]
}

pb_helper_quarter_order <- function(labels) {
  labs <- pb_helper_chr(labels)
  labs <- labs[nzchar(labs)]
  if (!length(labs)) {
    return(character())
  }
  nums <- suppressWarnings(as.numeric(sub("^.*?(\\d+)$", "\\1", labs, perl = TRUE)))
  labs[order(ifelse(is.na(nums), Inf, nums), labs)]
}

pb_get_table <- function(res,
                         section = NULL,
                         subsection = NULL,
                         subsubsection = NULL,
                         group = NULL,
                         entity = NULL,
                         items = NULL,
                         quarter = NULL,
                         context = "pb_get_table") {
  pb_helper_require_packages(c("dplyr", "tidyr", "tibble"), context)
  al <- pb_helper_prepare_all_long(res, entity = entity, context = context)
  filtered <- pb_helper_filter_all_long(al,
                                        section = section,
                                        subsection = subsection,
                                        subsubsection = subsubsection,
                                        group = group,
                                        entity = NULL,
                                        items = items)
  if (!nrow(filtered)) {
    empty_cols <- unique(c("Item", intersect(c("__section", "__subsection", "__subsubsection", "__group", "__entity"), names(al))))
    if (!length(empty_cols)) {
      return(tibble::tibble(Item = character()))
    }
    out <- vector("list", length(empty_cols))
    names(out) <- empty_cols
    out[] <- replicate(length(out), character(), simplify = FALSE)
    return(tibble::as_tibble(out))
  }
  
  meta_cols <- intersect(c("__section", "__subsection", "__subsubsection", "__group", "__entity"), names(filtered))
  aggregated <- filtered |>
    dplyr::mutate(
      Item = pb_helper_chr(Item),
      Quarter = pb_helper_chr(Quarter),
      Value = suppressWarnings(as.numeric(Value))
    ) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(meta_cols, "Item", "Quarter")))) |>
    dplyr::summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop")
  
  wide <- aggregated |>
    tidyr::pivot_wider(names_from = "Quarter", values_from = "Value")
  
  quarter_cols <- setdiff(names(wide), c(meta_cols, "Item"))
  quarter_cols <- pb_helper_quarter_order(quarter_cols)
  ordered_cols <- c(meta_cols, "Item", setdiff(quarter_cols, c(meta_cols, "Item")))
  ordered_cols <- ordered_cols[!duplicated(ordered_cols)]
  ordered_cols <- ordered_cols[ordered_cols %in% names(wide)]
  wide <- wide[, ordered_cols, drop = FALSE]
  
  if (!is.null(quarter)) {
    qs <- pb_helper_chr(quarter)
    missing <- setdiff(qs, names(wide))
    if (length(missing)) {
      stop(sprintf("%s: Kvartalet/kvartalerne %s findes ikke i tabellen.", context, paste(missing, collapse = ", ")), call. = FALSE)
    }
    keep_cols <- unique(c(meta_cols, "Item", qs))
    wide <- wide[, keep_cols, drop = FALSE]
  }
  
  wide
}

pb_find_items <- function(res,
                          section = NULL,
                          subsection = NULL,
                          subsubsection = NULL,
                          group = NULL,
                          entity = NULL,
                          pattern = NULL,
                          fixed = TRUE,
                          context = "pb_find_items") {
  pb_helper_require_packages("tibble", context)
  al <- pb_helper_prepare_all_long(res, entity = entity, context = context)
  filtered <- pb_helper_filter_all_long(al,
                                        section = section,
                                        subsection = subsection,
                                        subsubsection = subsubsection,
                                        group = group,
                                        entity = NULL)
  items <- unique(pb_helper_chr(filtered$Item))
  if (!length(items)) {
    return(character())
  }
  if (!is.null(pattern)) {
    if (isTRUE(fixed)) {
      items <- items[grepl(pattern, items, fixed = TRUE)]
    } else {
      items <- items[grepl(pattern, items)]
    }
  }
  items
}

pb_quarters <- function(res, entity = NULL, context = "pb_quarters") {
  al <- pb_helper_prepare_all_long(res, entity = entity, context = context)
  qs <- unique(pb_helper_chr(al$Quarter))
  qs <- qs[!is.na(qs) & nzchar(qs)]
  pb_helper_quarter_order(qs)
}

pb_list_structure <- function(res, entity = NULL, context = "pb_list_structure") {
  pb_helper_require_packages(c("dplyr", "tibble"), context)
  al <- pb_helper_prepare_all_long(res, entity = entity, context = context)
  cols <- intersect(c("__section", "__subsection", "__subsubsection", "__group", "__entity"), names(al))
  if (!length(cols)) {
    return(tibble::tibble())
  }
  out <- al |>
    dplyr::select(dplyr::all_of(cols)) |>
    dplyr::distinct() |>
    dplyr::arrange(dplyr::across(dplyr::all_of(cols)))
  names(out) <- sub("^__", "", names(out))
  out
}

pb_list_sections <- function(res, entity = NULL, context = "pb_list_sections") {
  unique(pb_helper_chr(pb_helper_prepare_all_long(res, entity = entity, context = context)$`__section`))
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
