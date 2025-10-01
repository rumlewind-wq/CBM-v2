# pb_fee_income_core.R
#' Compute quarterly fee income percentages for funding products using core helpers.
#'
#' @param res Parsed report list containing an `all_long` table.
#' @param manifest_yaml Optional YAML manifest text.
#' @param include_total Ignored (kept for backwards compatibility).
#' @param entity Optional entity filter when the `__entity` column exists.
#'
#' @return Tibble with columns Quarter, Side, Product, Maturity, Component, Value.
#'         Attaches trace information as an attribute.
pb_fee_income <- function(res, manifest_yaml = NULL, include_total = NULL, entity = NULL) {
  context <- "pb_fee_income"
  pb_helper_require_packages("tibble", context)
  
  al <- pb_helper_prepare_all_long(
    res,
    entity = NULL,
    required_cols = c("Quarter", "Item", "Value", "__section", "__subsection"),
    context = sprintf("%s: Datagrundlag", context)
  )
  
  if (!is.null(entity) && "__entity" %in% names(al)) {
    keep_entity <- pb_helper_match_values(al$`__entity`, entity)
    al <- al[keep_entity, , drop = FALSE]
  }
  
  if (!nrow(al)) {
    out <- pb_helper_empty_result()
    attr(out, "trace") <- list(
      filters = list(),
      matched_fee_rows = 0L,
      notes = "Ingen data efter entity-filter."
    )
    return(out)
  }
  
  manifest_info <- tryCatch(
    pb_helper_manifest_load(
      res = res,
      manifest_yaml = manifest_yaml,
      context = sprintf("%s: Manifestopslag", context)
    ),
    error = function(...) NULL
  )
  manifest_rows_safe <- function(section, subsection = NULL, group = NULL) {
    if (is.null(manifest_info)) {
      return(character())
    }
    pb_helper_manifest_rows(
      manifest_info$manifest,
      section = section,
      subsection = subsection,
      group = group,
      allow_missing = TRUE,
      context = sprintf("%s: Manifestopslag", context)
    )
  }
  
  deposit_products <- c("Retail Demand Deposits", "Corporate Demand Deposits")
  
  make_alias_map <- function(base_labels, manifest_labels) {
    aliases <- stats::setNames(base_labels, pb_helper_norm(base_labels))
    labels <- pb_helper_chr(manifest_labels)
    labels <- labels[nzchar(labels)]
    if (!length(labels)) {
      return(aliases)
    }
    labels_norm <- pb_helper_norm(labels)
    for (i in seq_along(labels)) {
      label_norm <- labels_norm[[i]]
      if (is.na(label_norm) || !nzchar(label_norm)) next
      target <- NA_character_
      if (grepl("retail", label_norm, fixed = TRUE)) {
        target <- "Retail Demand Deposits"
      } else if (grepl("corporate", label_norm, fixed = TRUE)) {
        target <- "Corporate Demand Deposits"
      }
      if (!is.na(target)) {
        aliases[[label_norm]] <- target
      }
    }
    aliases
  }
  
  lookup_product <- function(items, alias_map) {
    items_norm <- pb_helper_norm(items)
    alias_values <- alias_map[items_norm]
    unname(ifelse(is.na(alias_values), NA_character_, alias_values))
  }
  
  fee_alias_map <- make_alias_map(
    deposit_products,
    manifest_rows_safe(
      section = "INCOME AND EXPENSE REPORT",
      subsection = "CURRENT REVENUE",
      group = "Fee Income"
    )
  )
  
  balance_alias_map <- make_alias_map(
    deposit_products,
    manifest_rows_safe(
      section = "SUMMARY BALANCE SHEET",
      subsection = "LIABILITIES"
    )
  )
  
  fee_rows <- pb_helper_filter_all_long(
    al,
    section = "INCOME AND EXPENSE REPORT",
    subsection = "CURRENT REVENUE",
    group = "Fee Income"
  )
  fee_rows <- fee_rows[is.finite(fee_rows$Value), c("Quarter", "Item", "Value"), drop = FALSE]
  fee_rows$Product <- lookup_product(fee_rows$Item, fee_alias_map)
  fee_rows <- fee_rows[!is.na(fee_rows$Product), , drop = FALSE]
  matched_fee_rows <- nrow(fee_rows)
  
  balance_rows <- pb_helper_filter_all_long(
    al,
    section = "SUMMARY BALANCE SHEET",
    subsection = "LIABILITIES"
  )
  balance_rows <- balance_rows[is.finite(balance_rows$Value), c("Quarter", "Item", "Value"), drop = FALSE]
  balance_rows$Product <- lookup_product(balance_rows$Item, balance_alias_map)
  balance_rows <- balance_rows[!is.na(balance_rows$Product), , drop = FALSE]
  
  quarters <- pb_helper_quarter_order(unique(c(fee_rows$Quarter, balance_rows$Quarter)))
  if (!length(quarters)) {
    out <- pb_helper_empty_result()
    attr(out, "trace") <- list(
      filters = list(
        fee = list(section = "INCOME AND EXPENSE REPORT", subsection = "CURRENT REVENUE", group = "Fee Income"),
        balance = list(section = "SUMMARY BALANCE SHEET", subsection = "LIABILITIES")
      ),
      matched_fee_rows = 0L,
      notes = "Ingen fee- eller balance-rækker fundet."
    )
    return(out)
  }
  
  qp <- expand.grid(Quarter = quarters, Product = deposit_products, stringsAsFactors = FALSE)
  qp <- qp[order(match(qp$Product, deposit_products), match(qp$Quarter, quarters)), , drop = FALSE]
  
  if (nrow(fee_rows)) {
    fee_rows <- stats::aggregate(Value ~ Quarter + Product, fee_rows, sum, na.rm = TRUE)
  }
  if (nrow(balance_rows)) {
    balance_rows <- stats::aggregate(Value ~ Quarter + Product, balance_rows, sum, na.rm = TRUE)
  }
  
  fee_full <- merge(qp, fee_rows, by = c("Quarter", "Product"), all.x = TRUE, sort = FALSE)
  fee_full$Value[is.na(fee_full$Value)] <- 0
  
  balance_full <- merge(qp, balance_rows, by = c("Quarter", "Product"), all.x = TRUE, sort = FALSE)
  balance_full$Value <- as.numeric(balance_full$Value)
  balance_full <- balance_full[order(match(balance_full$Product, deposit_products), match(balance_full$Quarter, quarters)), , drop = FALSE]
  
  balance_end <- balance_full$Value
  fee_values <- fee_full$Value
  fee_pct <- ifelse(is.na(balance_end) | balance_end <= 0, 0, (fee_values / balance_end) * 4)
  
  missing_balance_idx <- which(is.na(balance_end) & fee_values != 0)
  notes <- character()
  if (length(missing_balance_idx)) {
    notes <- sprintf(
      "Manglende balance for %s i %s",
      balance_full$Product[missing_balance_idx],
      balance_full$Quarter[missing_balance_idx]
    )
  }
  
  fee_pct_df <- tibble::tibble(
    Quarter = qp$Quarter,
    Product = qp$Product,
    Value = fee_pct
  )
  
  grid_map <- list(
    "Interbank Borrowing" = "No-Maturity",
    "Retail Demand Deposits" = "No-Maturity",
    "Corporate Demand Deposits" = "No-Maturity",
    "Wholesale Deposits" = paste0(1:4, "-quarter"),
    "Savings Deposits" = "No-Maturity",
    "Savings Certificates (CDs)" = paste0(1:2, "-quarter"),
    "Long-term Time Deposits" = paste0(1:8, "-quarter"),
    "Discount Window Advances" = "No-Maturity"
  )
  
  grid <- tibble::tibble(
    Product = rep(names(grid_map), times = vapply(grid_map, length, integer(1))),
    Maturity = unlist(grid_map, use.names = FALSE)
  )
  
  template <- tibble::tibble(
    Quarter = rep(quarters, each = nrow(grid)),
    Side = "funding",
    Product = rep(grid$Product, times = length(quarters)),
    Maturity = rep(grid$Maturity, times = length(quarters)),
    Component = "fee",
    Value = 0
  )
  
  if (nrow(fee_pct_df)) {
    key <- paste(fee_pct_df$Quarter, fee_pct_df$Product, sep = "\r")
    template_key <- paste(template$Quarter, template$Product, sep = "\r")
    match_idx <- match(key, template_key)
    keep <- !is.na(match_idx)
    if (any(keep)) {
      template$Value[match_idx[keep]] <- fee_pct_df$Value[keep]
    }
  }
  
  product_order <- names(grid_map)
  template <- template[order(match(template$Quarter, quarters), match(template$Product, product_order)), , drop = FALSE]
  
  trace <- list(
    filters = list(
      fee = list(section = "INCOME AND EXPENSE REPORT", subsection = "CURRENT REVENUE", group = "Fee Income"),
      balance = list(section = "SUMMARY BALANCE SHEET", subsection = "LIABILITIES")
    ),
    matched_fee_rows = matched_fee_rows,
    notes = if (length(notes)) notes else NULL
  )
  
  pb_helper_make_result(template, trace = trace, context = sprintf("%s: Resultatopbygning", context))
}