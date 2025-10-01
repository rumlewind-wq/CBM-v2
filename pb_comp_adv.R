# pb_comp_adv.R
# Advertising cost component rates computed using helper utilities.

pb_comp_adv <- function(res, q = NULL, entity = NULL, manifest_yaml = NULL) {
  context <- "pb_comp_adv"
  pb_helper_require_packages(c("dplyr", "purrr", "tibble", "tidyr", "stringr"), context)
  
  al <- pb_helper_prepare_all_long(res, entity = entity, context = context)
  quarters_all <- pb_helper_quarter_levels(al)
  quarters <- pb_helper_select_quarters(quarters_all, q, context = context)
  idx_sel <- match(quarters, quarters_all)
  manifest_info <- pb_helper_manifest_load(res, manifest_yaml, context = context)
  manifest <- manifest_info$manifest
  
  pick_label <- function(x) {
    vals <- pb_helper_chr(x)
    vals <- vals[nzchar(vals)]
    if (!length(vals)) {
      return(NULL)
    }
    vals[[1]]
  }
  
  match_label <- function(candidates, pattern, path_context) {
    cand_chr <- pb_helper_chr(candidates)
    cand_chr <- cand_chr[nzchar(cand_chr)]
    if (!length(cand_chr)) {
      return(NULL)
    }
    pats <- pb_helper_chr(pattern)
    pats <- pats[nzchar(pats)]
    if (!length(pats)) {
      return(cand_chr[[1]])
    }
    cand_norm <- pb_helper_norm(cand_chr)
    pats_norm <- unique(pb_helper_norm(pats))
    pats_norm <- pats_norm[!is.na(pats_norm) & nzchar(pats_norm)]
    if (!length(pats_norm)) {
      return(cand_chr[[1]])
    }
    match_idx <- which(cand_norm %in% pats_norm)
    if (!length(match_idx)) {
      match_idx <- which(vapply(cand_norm, function(val) {
        if (is.na(val) || !nzchar(val)) {
          return(FALSE)
        }
        any(vapply(pats_norm, function(pn) stringr::str_detect(val, stringr::fixed(pn)), logical(1)))
      }, logical(1)))
    }
    if (!length(match_idx)) {
      stop(
        sprintf(
          "%s: Kunne ikke finde række der matcher '%s'.",
          path_context,
          paste(unique(pats), collapse = ", ")
        ),
        call. = FALSE
      )
    }
    cand_chr[[match_idx[[1]]]]
  }
  
  resolve_spec <- function(spec) {
    path_ctx <- sprintf(
      "%s: Sti %s",
      context,
      pb_helper_manifest_path(spec$section, spec$subsection, NULL, spec$group)
    )
    nodes <- pb_helper_manifest_locate(
      manifest,
      section = spec$section,
      subsection = spec$subsection,
      group = spec$group,
      context = path_ctx
    )
    resolved <- list(
      section = pick_label(nodes$section$section),
      subsection = if (!is.null(nodes$subsection)) pick_label(nodes$subsection$subsection) else NULL,
      group = if (!is.null(nodes$group)) pick_label(nodes$group$title) else NULL,
      item = NULL
    )
    if (!is.null(spec$item)) {
      rows <- pb_helper_manifest_rows(
        manifest,
        section = spec$section,
        subsection = spec$subsection,
        group = spec$group,
        allow_missing = FALSE,
        context = path_ctx
      )
      resolved$item <- match_label(rows, spec$item, path_ctx)
    }
    resolved
  }
  
  path_string <- function(resolved) {
    parts <- pb_helper_chr(c(resolved$section, resolved$subsection, resolved$group, resolved$item))
    parts <- parts[nzchar(parts)]
    paste(parts, collapse = " / ")
  }
  
  append_note <- function(existing, addition) {
    existing_chr <- pb_helper_chr(existing)
    addition_chr <- pb_helper_chr(addition)
    if (!length(existing_chr)) {
      existing_chr <- rep("", length(addition_chr))
    }
    if (!length(addition_chr)) {
      return(existing_chr)
    }
    addition_chr[is.na(addition_chr)] <- ""
    out <- existing_chr
    add_idx <- nzchar(addition_chr)
    both <- add_idx & nzchar(out)
    out[both] <- paste0(out[both], "; ", addition_chr[both])
    only_add <- add_idx & !nzchar(out)
    out[only_add] <- addition_chr[only_add]
    out
  }
  
  positive_delta <- function(values) {
    vals <- as.numeric(values)
    if (!length(vals)) {
      return(vals)
    }
    delta <- c(NA_real_, diff(vals))
    delta[delta < 0] <- 0
    delta
  }
  
  get_series <- function(spec, allow_missing = FALSE) {
    resolved <- resolve_spec(spec)
    filtered <- pb_helper_filter_all_long(
      al,
      section = resolved$section,
      subsection = resolved$subsection,
      group = resolved$group,
      items = resolved$item
    )
    if (!nrow(filtered)) {
      if (!allow_missing) {
        stop(
          sprintf("%s: Ingen data fundet for %s.", context, path_string(resolved)),
          call. = FALSE
        )
      }
      series <- tibble::tibble(Quarter = quarters_all, value = NA_real_)
    } else {
      aggregated <- filtered |>
        dplyr::group_by(Quarter) |>
        dplyr::summarise(value = sum(Value, na.rm = TRUE), .groups = "drop")
      series <- tibble::tibble(Quarter = quarters_all) |>
        dplyr::left_join(aggregated, by = "Quarter") |>
        dplyr::mutate(value = as.numeric(value))
    }
    list(series = series, resolved = resolved, path = path_string(resolved))
  }
  
  adv_configs <- list(
    list(
      key = "consumer_loans",
      side = "asset",
      maturity = "4-quarter",
      annual_factor = 1,
      numerator_spec = list(section = "INCOME AND EXPENSE REPORT", subsection = "CURRENT EXPENSES", group = "Advertising", item = "Consumer Loans"),
      denominator_spec = list(section = "BANK BALANCE SHEET", subsection = "ASSETS", group = "Consumer Loans, maturing start of:", item = "T+4"),
      denominator_allow_missing = TRUE,
      fallback_spec = list(section = "SUMMARY BALANCE SHEET", subsection = "ASSETS", item = "Consumer Loans")
    ),
    list(
      key = "mortgages",
      side = "asset",
      maturity = "8-quarter",
      annual_factor = 0.5,
      numerator_spec = list(section = "INCOME AND EXPENSE REPORT", subsection = "CURRENT EXPENSES", group = "Advertising", item = "Mortgages"),
      denominator_spec = list(section = "BANK BALANCE SHEET", subsection = "ASSETS", group = "Mortgage Loans, maturing start of:", item = "T+8"),
      denominator_allow_missing = TRUE,
      fallback_spec = list(section = "SUMMARY BALANCE SHEET", subsection = "ASSETS", item = "Mortgages")
    ),
    list(
      key = "retail_dd",
      side = "funding",
      maturity = "No-Maturity",
      annual_factor = 4,
      numerator_spec = list(section = "INCOME AND EXPENSE REPORT", subsection = "CURRENT EXPENSES", group = "Advertising", item = "Retail Demand Deposits"),
      denominator_spec = list(section = "SUMMARY BALANCE SHEET", subsection = "LIABILITIES", item = "Retail Demand Deposits"),
      denominator_allow_missing = FALSE,
      fallback_spec = NULL
    ),
    list(
      key = "corporate_dd",
      side = "funding",
      maturity = "No-Maturity",
      annual_factor = 4,
      numerator_spec = list(section = "INCOME AND EXPENSE REPORT", subsection = "CURRENT EXPENSES", group = "Advertising", item = "Corporate Demand Deposits"),
      denominator_spec = list(section = "SUMMARY BALANCE SHEET", subsection = "LIABILITIES", item = "Corporate Demand Deposits"),
      denominator_allow_missing = FALSE,
      fallback_spec = NULL
    ),
    list(
      key = "savings_deposits",
      side = "funding",
      maturity = "No-Maturity",
      annual_factor = 4,
      numerator_spec = list(section = "INCOME AND EXPENSE REPORT", subsection = "CURRENT EXPENSES", group = "Advertising", item = "Savings Deposits"),
      denominator_spec = list(section = "SUMMARY BALANCE SHEET", subsection = "LIABILITIES", item = "Savings Deposits"),
      denominator_allow_missing = FALSE,
      fallback_spec = NULL
    ),
    list(
      key = "cds",
      side = "funding",
      maturity = "2-quarter",
      annual_factor = 2,
      numerator_spec = list(section = "INCOME AND EXPENSE REPORT", subsection = "CURRENT EXPENSES", group = "Advertising", item = "Savings Certificates (CDs)"),
      denominator_spec = list(section = "BANK BALANCE SHEET", subsection = "LIABILITIES", group = "Savings Certificates (CDs), maturing start of:", item = "T+2"),
      denominator_allow_missing = FALSE,
      fallback_spec = NULL
    ),
    list(
      key = "lttd",
      side = "funding",
      maturity = "8-quarter",
      annual_factor = 0.5,
      numerator_spec = list(section = "INCOME AND EXPENSE REPORT", subsection = "CURRENT EXPENSES", group = "Advertising", item = "Long-term Time Deposits"),
      denominator_spec = list(section = "BANK BALANCE SHEET", subsection = "LIABILITIES", group = "Long-term Time Deposits, maturing start of:", item = "T+8"),
      denominator_allow_missing = FALSE,
      fallback_spec = NULL
    )
  )
  
  trace_rows <- list()
  
  adv_details <- purrr::map_dfr(adv_configs, function(cfg) {
    numerator <- get_series(cfg$numerator_spec, allow_missing = FALSE)
    denom <- get_series(cfg$denominator_spec, allow_missing = cfg$denominator_allow_missing)
    
    numerator_values <- numerator$series$value
    denom_values <- denom$series$value
    
    notes_full <- rep("", length(quarters_all))
    num_na_idx <- which(is.na(numerator_values))
    if (length(num_na_idx)) {
      tmp <- rep("", length(quarters_all))
      tmp[num_na_idx] <- "Advertising expense mangler; sat til 0"
      notes_full <- append_note(notes_full, tmp)
      numerator_values[num_na_idx] <- 0
    }
    
    denom_used <- denom_values
    if (!is.null(cfg$fallback_spec)) {
      fallback <- get_series(cfg$fallback_spec, allow_missing = FALSE)
      fallback_values <- fallback$series$value
      delta_values <- positive_delta(fallback_values)
      fallback_idx <- which(is.na(denom_used) | denom_used <= 0)
      if (length(fallback_idx)) {
        replacement <- delta_values[fallback_idx]
        valid <- !is.na(replacement) & replacement > 0
        if (any(valid)) {
          denom_used[fallback_idx[valid]] <- replacement[valid]
          tmp <- rep("", length(quarters_all))
          tmp[fallback_idx[valid]] <- "Denominator fallback til positiv balance-Δ"
          notes_full <- append_note(notes_full, tmp)
        }
        invalid_idx <- fallback_idx[!valid]
        if (length(invalid_idx)) {
          tmp <- rep("", length(quarters_all))
          tmp[invalid_idx] <- "Denominator mangler eller ≤0; Value sat til 0"
          notes_full <- append_note(notes_full, tmp)
        }
      }
      fallback_path <- fallback$path
    } else {
      fallback_path <- NA_character_
    }
    
    zero_idx <- which(is.na(denom_used) | denom_used <= 0)
    if (length(zero_idx)) {
      tmp <- rep("", length(quarters_all))
      tmp[zero_idx] <- "Denominator mangler eller ≤0; Value sat til 0"
      notes_full <- append_note(notes_full, tmp)
      denom_used[zero_idx] <- NA_real_
    }
    
    denom_sel <- denom_used[idx_sel]
    num_sel <- numerator_values[idx_sel]
    q_rate <- ifelse(!is.na(denom_sel) & denom_sel > 0, num_sel / denom_sel, 0)
    value <- q_rate * cfg$annual_factor
    note_sel <- notes_full[idx_sel]
    product_label <- cfg$product_label %||% numerator$resolved$item %||% denom$resolved$item %||% cfg$key
    
    details <- tibble::tibble(
      Quarter = quarters,
      Side = cfg$side,
      Product = product_label,
      Maturity = cfg$maturity,
      Component = "adv",
      Value = value,
      ad_expense = num_sel,
      denominator = denom_sel,
      q_rate = q_rate,
      annual_factor = cfg$annual_factor,
      numerator_path = numerator$path,
      denominator_path = denom$path,
      fallback_path = fallback_path,
      notes = note_sel
    )
    
    trace_rows[[length(trace_rows) + 1]] <<- details |>
      dplyr::transmute(
        Quarter, Side, Product, Maturity, Component,
        step = "adv",
        formula = "Value = (AdExpense/Denominator) × AF",
        inputs = paste0("{AdExp=", signif(ad_expense, 6), ", Denominator=", signif(denominator, 6), "}"),
        parameters = paste0("{AF=", annual_factor, "}"),
        source_table = numerator_path,
        source_cols = dplyr::if_else(
          is.na(fallback_path) | !nzchar(fallback_path),
          paste0("Denominator: ", denominator_path),
          paste0("Denominator: ", denominator_path, "; Fallback: ", fallback_path)
        ),
        notes = notes
      )
    
    details
  })
  
  trace_tbl <- dplyr::bind_rows(trace_rows)
  
  grid_entries <- adv_details |>
    dplyr::distinct(Side, Product, Maturity)
  
  cd_label <- adv_details |>
    dplyr::filter(stringr::str_detect(Product, "Savings Certificates")) |>
    dplyr::distinct(Product) |>
    dplyr::pull(Product)
  if (length(cd_label)) {
    grid_entries <- dplyr::bind_rows(
      grid_entries,
      tibble::tibble(Side = "funding", Product = cd_label[[1]], Maturity = "1-quarter")
    )
  }
  
  lttd_label <- adv_details |>
    dplyr::filter(stringr::str_detect(Product, "Long-term Time Deposits")) |>
    dplyr::distinct(Product) |>
    dplyr::pull(Product)
  if (length(lttd_label)) {
    grid_entries <- dplyr::bind_rows(
      grid_entries,
      tibble::tibble(
        Side = "funding",
        Product = lttd_label[[1]],
        Maturity = paste0(1:7, "-quarter")
      )
    )
  }
  
  zero_specs <- list(
    list(side = "funding", maturities = "No-Maturity", spec = list(section = "SUMMARY BALANCE SHEET", subsection = "LIABILITIES", item = "Interbank Borrowing")),
    list(side = "funding", maturities = paste0(1:4, "-quarter"), spec = list(section = "SUMMARY BALANCE SHEET", subsection = "LIABILITIES", item = "Wholesale Deposits")),
    list(side = "funding", maturities = "No-Maturity", spec = list(section = "SUMMARY BALANCE SHEET", subsection = "LIABILITIES", item = "Discount Window Advances")),
    list(side = "asset", maturities = "1-quarter", spec = list(section = "SUMMARY BALANCE SHEET", subsection = "ASSETS", item = "Interbank Lending (at CIBOR)")),
    list(side = "asset", maturities = "1-quarter", spec = list(section = "SUMMARY BALANCE SHEET", subsection = "ASSETS", item = "Business Loans - Fixed-Rate")),
    list(side = "asset", maturities = "2-quarter", spec = list(section = "SUMMARY BALANCE SHEET", subsection = "ASSETS", item = "Business Loans - Floating-Rate")),
    list(side = "asset", maturities = "8-quarter", spec = list(section = "SUMMARY BALANCE SHEET", subsection = "ASSETS", item = "Government Bonds"))
  )
  
  zero_entries <- purrr::map_dfr(zero_specs, function(z) {
    info <- resolve_spec(z$spec)
    tibble::tibble(
      Side = z$side,
      Product = info$item %||% info$group %||% info$section,
      Maturity = z$maturities
    )
  })
  
  grid_entries <- dplyr::bind_rows(grid_entries, zero_entries) |>
    dplyr::distinct()
  
  grid <- tidyr::expand_grid(Quarter = quarters, grid_entries) |>
    dplyr::mutate(Component = "adv")
  
  result_tbl <- grid |>
    dplyr::left_join(
      adv_details |>
        dplyr::select(Quarter, Side, Product, Maturity, Value),
      by = c("Quarter", "Side", "Product", "Maturity")
    ) |>
    dplyr::mutate(Value = dplyr::coalesce(Value, 0)) |>
    dplyr::select(Quarter, Side, Product, Maturity, Component, Value) |>
    dplyr::arrange(Quarter, Side, Product, Maturity)
  
  pb_helper_make_result(result_tbl, trace = trace_tbl, context = context)
}