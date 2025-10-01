# pb_flow_generic_core.R
# Generisk T+N-flowberegning baseret på pb_helpers_core.R-konventioner.

pb_flow_generic_core <- function(res,
                                 section,
                                 subsection,
                                 group_title,
                                 entity = NULL,
                                 q_from = NULL,
                                 q_to = NULL,
                                 item_regex = "^T\\+\\d+$",
                                 drop_rates = TRUE,
                                 mature_label = "T+1",
                                 new_label_max = "auto",
                                 side = NULL,
                                 product = NULL,
                                 context = "pb_flow_generic_core") {
  pb_helper_require_packages(c("dplyr", "tidyr", "tibble"), context)
  
  quarters_all <- pb_quarters(res, entity = entity, context = paste0(context, ": quarters"))
  if (length(quarters_all) < 2L) {
    stop(sprintf("%s: Datagrundlaget indeholder færre end to kvartaler.", context), call. = FALSE)
  }
  q0 <- if (is.null(q_from)) {
    quarters_all[[length(quarters_all) - 1L]]
  } else {
    q_candidate <- pb_helper_chr(q_from)[1]
    if (!q_candidate %in% quarters_all) {
      stop(sprintf("%s: Kvartalet '%s' findes ikke i datagrundlaget.", context, q_candidate), call. = FALSE)
    }
    q_candidate
  }
  q1 <- if (is.null(q_to)) {
    quarters_all[[length(quarters_all)]]
  } else {
    q_candidate <- pb_helper_chr(q_to)[1]
    if (!q_candidate %in% quarters_all) {
      stop(sprintf("%s: Kvartalet '%s' findes ikke i datagrundlaget.", context, q_candidate), call. = FALSE)
    }
    q_candidate
  }
  if (identical(q0, q1)) {
    stop(sprintf("%s: q_from (%s) og q_to (%s) skal være forskellige kvartaler.", context, q0, q1), call. = FALSE)
  }
  
  table_wide <- pb_get_table(
    res,
    section = section,
    subsection = subsection,
    group = group_title,
    entity = entity,
    context = paste0(context, ": pb_get_table")
  )
  if (!nrow(table_wide)) {
    stop(
      sprintf(
        "%s: Ingen rækker fundet for %s.",
        context,
        pb_helper_manifest_path(section, subsection, group = group_title, entity = entity)
      ),
      call. = FALSE
    )
  }
  if (!"Item" %in% names(table_wide)) {
    stop(sprintf("%s: Tabellens Item-kolonne mangler.", context), call. = FALSE)
  }
  
  items <- pb_helper_chr(table_wide$Item)
  keep <- rep(TRUE, length(items))
  if (isTRUE(drop_rates)) {
    keep <- keep & !grepl("\\(rate\\)\\s*$", items)
  }
  if (!is.null(item_regex) && length(item_regex) == 1L && nzchar(item_regex)) {
    keep <- keep & grepl(item_regex, items)
  }
  table_filtered <- tibble::as_tibble(table_wide[keep, , drop = FALSE])
  table_filtered$Item <- pb_helper_chr(table_filtered$Item)
  if (!nrow(table_filtered)) {
    stop(
      sprintf(
        "%s: Ingen rækker matcher filteret i gruppen '%s'.",
        context,
        group_title
      ),
      call. = FALSE
    )
  }
  
  missing_quarters <- setdiff(c(q0, q1), names(table_filtered))
  if (length(missing_quarters)) {
    stop(
      sprintf(
        "%s: Kvartal/kvartaler mangler i tabellen: %s.",
        context,
        paste(missing_quarters, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  
  base <- tibble::tibble(
    Maturity = table_filtered$Item,
    Q0 = suppressWarnings(as.numeric(table_filtered[[q0]])),
    Q1 = suppressWarnings(as.numeric(table_filtered[[q1]]))
  )
  tn <- suppressWarnings(as.integer(sub("^T\\+", "", pb_helper_chr(base$Maturity))))
  order_idx <- order(ifelse(is.na(tn), Inf, tn), base$Maturity)
  base <- base[order_idx, , drop = FALSE]
  base <- dplyr::mutate(base, Delta = Q1 - Q0)
  if (!nrow(base)) {
    stop(sprintf("%s: Ingen T+N-rækker tilgængelige efter sortering.", context), call. = FALSE)
  }
  
  tn_all <- suppressWarnings(as.integer(sub("^T\\+", "", pb_helper_chr(table_filtered$Item))))
  tn_valid <- tn_all[is.finite(tn_all)]
  mature_row <- if (any(base$Maturity == mature_label)) {
    mature_label
  } else if (length(tn_valid)) {
    paste0("T+", min(tn_valid))
  } else {
    mature_label
  }
  new_row <- if (!identical(new_label_max, "auto")) {
    pb_helper_chr(new_label_max)[1]
  } else if (length(tn_valid)) {
    paste0("T+", max(tn_valid))
  } else {
    mature_label
  }
  
  maturing_val <- if (any(base$Maturity == mature_row)) {
    sum(base$Q0[base$Maturity == mature_row], na.rm = TRUE)
  } else {
    NA_real_
  }
  new_val <- if (any(base$Maturity == new_row)) {
    sum(base$Q1[base$Maturity == new_row], na.rm = TRUE)
  } else {
    NA_real_
  }
  net_change <- sum(base$Delta, na.rm = TRUE)
  
  balances <- base |>
    dplyr::select(Maturity, Q0, Q1) |>
    tidyr::pivot_longer(
      cols = c("Q0", "Q1"),
      names_to = "Quarter_key",
      values_to = "Value"
    ) |>
    dplyr::mutate(
      Quarter = dplyr::case_when(
        Quarter_key == "Q0" ~ q0,
        Quarter_key == "Q1" ~ q1,
        TRUE ~ Quarter_key
      ),
      Component = "Balance"
    ) |>
    dplyr::select(Quarter, Maturity, Component, Value)
  
  deltas <- base |>
    dplyr::transmute(
      Quarter = q1,
      Maturity = Maturity,
      Component = "Delta",
      Value = Delta
    )
  
  totals_long <- tibble::tibble(
    Quarter = c(q0, q1, q1),
    Maturity = c(mature_row, new_row, "All"),
    Component = c("Maturing", "New", "Net"),
    Value = c(maturing_val, new_val, net_change)
  )
  
  side_value_chr <- pb_helper_chr(if (!is.null(side)) side else subsection)
  product_value_chr <- pb_helper_chr(if (!is.null(product)) product else group_title)
  side_value <- if (length(side_value_chr)) side_value_chr[[1]] else NA_character_
  product_value <- if (length(product_value_chr)) product_value_chr[[1]] else NA_character_
  
  result_raw <- dplyr::bind_rows(balances, deltas, totals_long) |>
    dplyr::mutate(
      Side = side_value,
      Product = product_value,
      .before = "Maturity"
    ) |>
    dplyr::select(Quarter, Side, Product, Maturity, Component, Value) |>
    dplyr::mutate(Value = as.numeric(Value))
  
  trace <- list(
    quarters = list(from = q0, to = q1),
    table = tibble::as_tibble(base),
    totals = tibble::tibble(
      Group = group_title,
      Maturing_row = mature_row,
      New_row = new_row,
      Maturing = maturing_val,
      New = new_val,
      Net = net_change,
      Quarter_from = q0,
      Quarter_to = q1
    )
  )
  
  pb_helper_make_result(result_raw, trace = trace, context = context)
}

pb_lttd_flow_latest <- function(res,
                                section = "BANK BALANCE SHEET",
                                entity = NULL,
                                ...) {
  pb_flow_generic_core(
    res,
    section = section,
    subsection = "LIABILITIES",
    group_title = "Long-term Time Deposits, maturing start of:",
    entity = entity,
    ...
  )
}

pb_lttd_flow_custom <- function(res,
                                q_from,
                                q_to,
                                section = "BANK BALANCE SHEET",
                                entity = NULL,
                                ...) {
  pb_lttd_flow_latest(
    res,
    section = section,
    entity = entity,
    q_from = q_from,
    q_to = q_to,
    ...
  )
}

pb_wholesale_flow_latest <- function(res,
                                     section = "BANK BALANCE SHEET",
                                     entity = NULL,
                                     ...) {
  pb_flow_generic_core(
    res,
    section = section,
    subsection = "LIABILITIES",
    group_title = "Wholesale Deposits, maturing start of:",
    entity = entity,
    ...
  )
}

pb_wholesale_flow_custom <- function(res,
                                     q_from,
                                     q_to,
                                     section = "BANK BALANCE SHEET",
                                     entity = NULL,
                                     ...) {
  pb_wholesale_flow_latest(
    res,
    section = section,
    entity = entity,
    q_from = q_from,
    q_to = q_to,
    ...
  )
}

pb_cds_flow_latest <- function(res,
                               section = "BANK BALANCE SHEET",
                               entity = NULL,
                               ...) {
  pb_flow_generic_core(
    res,
    section = section,
    subsection = "LIABILITIES",
    group_title = "Savings Certificates (CDs), maturing start of:",
    entity = entity,
    ...
  )
}

pb_cds_flow_custom <- function(res,
                               q_from,
                               q_to,
                               section = "BANK BALANCE SHEET",
                               entity = NULL,
                               ...) {
  pb_cds_flow_latest(
    res,
    section = section,
    entity = entity,
    q_from = q_from,
    q_to = q_to,
    ...
  )
}

pb_blfloating_flow_latest <- function(res,
                                      section = "BANK BALANCE SHEET",
                                      entity = NULL,
                                      ...) {
  pb_flow_generic_core(
    res,
    section = section,
    subsection = "ASSETS",
    group_title = "Business Loans - Floating-Rate, maturing start of:",
    entity = entity,
    ...
  )
}

pb_blfloating_flow_custom <- function(res,
                                      q_from,
                                      q_to,
                                      section = "BANK BALANCE SHEET",
                                      entity = NULL,
                                      ...) {
  pb_blfloating_flow_latest(
    res,
    section = section,
    entity = entity,
    q_from = q_from,
    q_to = q_to,
    ...
  )
}

pb_consumer_flow_latest <- function(res,
                                    section = "BANK BALANCE SHEET",
                                    entity = NULL,
                                    ...) {
  pb_flow_generic_core(
    res,
    section = section,
    subsection = "ASSETS",
    group_title = "Consumer Loans, maturing start of:",
    entity = entity,
    ...
  )
}

pb_consumer_flow_custom <- function(res,
                                    q_from,
                                    q_to,
                                    section = "BANK BALANCE SHEET",
                                    entity = NULL,
                                    ...) {
  pb_consumer_flow_latest(
    res,
    section = section,
    entity = entity,
    q_from = q_from,
    q_to = q_to,
    ...
  )
}

pb_mortgage_flow_latest <- function(res,
                                    section = "BANK BALANCE SHEET",
                                    entity = NULL,
                                    ...) {
  pb_flow_generic_core(
    res,
    section = section,
    subsection = "ASSETS",
    group_title = "Mortgage Loans, maturing start of:",
    entity = entity,
    ...
  )
}

pb_mortgage_flow_custom <- function(res,
                                    q_from,
                                    q_to,
                                    section = "BANK BALANCE SHEET",
                                    entity = NULL,
                                    ...) {
  pb_mortgage_flow_latest(
    res,
    section = section,
    entity = entity,
    q_from = q_from,
    q_to = q_to,
    ...
  )
}

pb_govbonds_flow_latest <- function(res,
                                    section = "BANK BALANCE SHEET",
                                    entity = NULL,
                                    ...) {
  pb_flow_generic_core(
    res,
    section = section,
    subsection = "ASSETS",
    group_title = "Government Bonds, maturing start of:",
    entity = entity,
    ...
  )
}

pb_govbonds_flow_custom <- function(res,
                                    q_from,
                                    q_to,
                                    section = "BANK BALANCE SHEET",
                                    entity = NULL,
                                    ...) {
  pb_govbonds_flow_latest(
    res,
    section = section,
    entity = entity,
    q_from = q_from,
    q_to = q_to,
    ...
  )
}

pb_lttd_flow_market_latest <- function(res, entity = NULL, ...) {
  pb_lttd_flow_latest(
    res,
    section = "MARKET AVERAGE BALANCE SHEET",
    entity = entity,
    ...
  )
}

pb_wholesale_flow_market_latest <- function(res, entity = NULL, ...) {
  pb_wholesale_flow_latest(
    res,
    section = "MARKET AVERAGE BALANCE SHEET",
    entity = entity,
    ...
  )
}

pb_cds_flow_market_latest <- function(res, entity = NULL, ...) {
  pb_cds_flow_latest(
    res,
    section = "MARKET AVERAGE BALANCE SHEET",
    entity = entity,
    ...
  )
}

pb_blfloating_flow_market_latest <- function(res, entity = NULL, ...) {
  pb_blfloating_flow_latest(
    res,
    section = "MARKET AVERAGE BALANCE SHEET",
    entity = entity,
    ...
  )
}

pb_consumer_flow_market_latest <- function(res, entity = NULL, ...) {
  pb_consumer_flow_latest(
    res,
    section = "MARKET AVERAGE BALANCE SHEET",
    entity = entity,
    ...
  )
}

pb_mortgage_flow_market_latest <- function(res, entity = NULL, ...) {
  pb_mortgage_flow_latest(
    res,
    section = "MARKET AVERAGE BALANCE SHEET",
    entity = entity,
    ...
  )
}

pb_govbonds_flow_market_latest <- function(res, entity = NULL, ...) {
  pb_govbonds_flow_latest(
    res,
    section = "MARKET AVERAGE BALANCE SHEET",
    entity = entity,
    ...
  )
}