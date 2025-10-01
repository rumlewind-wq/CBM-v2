# pb_fee_income.R
#' Compute quarterly fee income percentages for funding products in ProBanker reports.
#'
#' The function derives fee income rates for retail and corporate demand deposits and
#' supplies a full funding maturity grid (including zero-rate products).
#'
#' @param res Parsed report list containing an `all_long` table.
#' @param manifest_yaml Optional YAML text used to enrich label aliases.
#' @param include_total Ignored (kept for backwards compatibility).
#' @param entity Optional entity filter when the `__entity` column exists.
#'
#' @return Tibble with columns Quarter, Side, Product, Maturity, Component, Value.
#'         Attaches trace information as an attribute.
if (!exists("%||%", mode = "function")) `%||%` <- function(x, y) if (is.null(x)) y else x

pb_fee_income <- function(res, manifest_yaml = NULL, include_total = NULL, entity = NULL) {
  if (!requireNamespace("tibble", quietly = TRUE)) {
    stop("Package 'tibble' skal være installeret for at køre pb_fee_income().")
  }
  
  stopifnot(is.list(res), !is.null(res$all_long))
  al <- res$all_long
  
  required_cols <- c("Quarter", "Item", "Value", "__section", "__subsection")
  missing_cols <- setdiff(required_cols, names(al))
  if (length(missing_cols)) {
    stop(sprintf("res$all_long mangler kolonnerne: %s", paste(missing_cols, collapse = ", ")))
  }
  
  chr <- function(x) {
    if (is.factor(x)) as.character(x) else as.character(x)
  }
  norm <- function(x) trimws(tolower(chr(x)))
  
  al$Quarter        <- chr(al$Quarter)
  al$Item           <- chr(al$Item)
  al$Value          <- suppressWarnings(as.numeric(al$Value))
  al$`__section`    <- chr(al$`__section`)
  al$`__subsection` <- chr(al$`__subsection`)
  if ("__group" %in% names(al)) al$`__group` <- chr(al$`__group`)
  if ("__entity" %in% names(al)) al$`__entity` <- chr(al$`__entity`)
  
  if (!is.null(entity) && "__entity" %in% names(al)) {
    entity_mask <- !is.na(al$`__entity`) & trimws(al$`__entity`) != ""
    if (any(entity_mask)) {
      al <- al[!entity_mask | trimws(al$`__entity`) == entity, , drop = FALSE]
    }
  }
  
  if (!nrow(al)) {
    out <- tibble::tibble(Quarter = character(), Side = character(), Product = character(),
                          Maturity = character(), Component = character(), Value = double())
    attr(out, "trace") <- list(filters = list(), matched_fee_rows = 0L,
                               notes = "Ingen data efter entity-filter.")
    return(out)
  }
  
  manifest_rows <- function(manifest_yaml, section, subsection = NULL, group = NULL) {
    if (is.null(manifest_yaml)) return(character())
    if (!requireNamespace("yaml", quietly = TRUE)) return(character())
    tryCatch({
      y <- yaml::yaml.load(manifest_yaml)
      man <- y$manifest
      if (is.null(man$sections)) return(character())
      target_section <- norm(section)
      target_subsection <- if (!is.null(subsection)) norm(subsection) else NULL
      target_group <- if (!is.null(group)) norm(group) else NULL
      out <- character()
      for (sec in man$sections) {
        if (!is.list(sec) || norm(sec$section) != target_section) next
        subs <- sec$subsections %||% list()
        for (sub in subs) {
          if (!is.list(sub)) next
          if (!is.null(target_subsection) && norm(sub$subsection) != target_subsection) next
          if (is.null(target_group)) {
            out <- c(out, chr(sub$rows %||% character()))
          } else {
            grps <- sub$groups %||% list()
            for (grp in grps) {
              if (!is.list(grp)) next
              if (norm(grp$title) == target_group) {
                out <- c(out, chr(grp$rows %||% character()))
              }
            }
          }
        }
      }
      out
    }, error = function(e) character())
  }
  
  base_aliases <- list(
    fee = c("Retail Demand Deposits", "Corporate Demand Deposits"),
    balance = c("Retail Demand Deposits", "Corporate Demand Deposits")
  )
  
  add_manifest_aliases <- function(alias_vec, labels) {
    if (!length(labels)) return(alias_vec)
    for (label in labels) {
      lower <- norm(label)
      if (grepl("retail", lower, fixed = TRUE)) {
        alias_vec[[label]] <- "Retail Demand Deposits"
      } else if (grepl("corporate", lower, fixed = TRUE)) {
        alias_vec[[label]] <- "Corporate Demand Deposits"
      }
    }
    alias_vec
  }
  
  fee_aliases <- as.list(stats::setNames(base_aliases$fee, base_aliases$fee))
  bal_aliases <- as.list(stats::setNames(base_aliases$balance, base_aliases$balance))
  
  fee_aliases <- add_manifest_aliases(fee_aliases,
                                      manifest_rows(manifest_yaml, "INCOME AND EXPENSE REPORT",
                                                    "CURRENT REVENUE", "Fee Income"))
  bal_aliases <- add_manifest_aliases(bal_aliases,
                                      manifest_rows(manifest_yaml, "SUMMARY BALANCE SHEET",
                                                    "LIABILITIES", NULL))
  
  match_alias <- function(x, alias_list) {
    keys <- names(alias_list)
    norm_keys <- vapply(keys, norm, character(1))
    idx <- match(norm(x), norm_keys)
    ifelse(is.na(idx), NA_character_, unname(unlist(alias_list))[idx])
  }
  
  fee_mask <- norm(al$`__section`) == "income and expense report" &
    norm(al$`__subsection`) == "current revenue" &
    (if ("__group" %in% names(al)) norm(al$`__group`) == "fee income" else TRUE) &
    is.finite(al$Value)
  fee_rows <- al[fee_mask, c("Quarter", "Item", "Value"), drop = FALSE]
  fee_rows$Product <- match_alias(fee_rows$Item, fee_aliases)
  fee_rows <- fee_rows[!is.na(fee_rows$Product), , drop = FALSE]
  
  bal_mask <- norm(al$`__section`) == "summary balance sheet" &
    norm(al$`__subsection`) == "liabilities" &
    is.finite(al$Value)
  bal_rows <- al[bal_mask, c("Quarter", "Item", "Value"), drop = FALSE]
  bal_rows$Product <- match_alias(bal_rows$Item, bal_aliases)
  bal_rows <- bal_rows[!is.na(bal_rows$Product), , drop = FALSE]
  
  quarters <- sort(unique(c(fee_rows$Quarter, bal_rows$Quarter)))
  if (!length(quarters)) {
    out <- tibble::tibble(Quarter = character(), Side = character(), Product = character(),
                          Maturity = character(), Component = character(), Value = double())
    attr(out, "trace") <- list(filters = list(
      fee = list(section = "INCOME AND EXPENSE REPORT",
                 subsection = "CURRENT REVENUE",
                 group = "Fee Income"),
      balance = list(section = "SUMMARY BALANCE SHEET",
                     subsection = "LIABILITIES")),
      matched_fee_rows = 0L,
      notes = "Ingen fee- eller balance-rækker fundet.")
    return(out)
  }
  
  deposit_products <- c("Retail Demand Deposits", "Corporate Demand Deposits")
  qp <- expand.grid(Quarter = quarters, Product = deposit_products,
                    stringsAsFactors = FALSE)
  qp <- qp[order(match(qp$Product, deposit_products), match(qp$Quarter, quarters)), , drop = FALSE]
  
  fee_agg <- if (nrow(fee_rows)) stats::aggregate(Value ~ Quarter + Product, fee_rows, sum, na.rm = TRUE)
  else fee_rows
  bal_agg <- if (nrow(bal_rows)) stats::aggregate(Value ~ Quarter + Product, bal_rows, sum, na.rm = TRUE)
  else bal_rows
  
  fee_full <- merge(qp, fee_agg, by = c("Quarter", "Product"), all.x = TRUE, sort = FALSE)
  fee_full$Value[is.na(fee_full$Value)] <- 0
  
  bal_full <- merge(qp, bal_agg, by = c("Quarter", "Product"), all.x = TRUE, sort = FALSE)
  bal_full$Value <- as.numeric(bal_full$Value)
  bal_full <- bal_full[order(match(bal_full$Product, deposit_products),
                             match(bal_full$Quarter, quarters)), , drop = FALSE]
  
  balance_end <- bal_full$Value
  fee_values <- fee_full$Value
  fee_pct <- ifelse(is.na(balance_end) | balance_end <= 0, 0, (fee_values / balance_end) * 4)
  
  missing_balance_idx <- which(is.na(balance_end) & fee_values != 0)
  notes <- character()
  if (length(missing_balance_idx)) {
    notes <- c(notes, sprintf("Manglende balance for %s i %s", bal_full$Product[missing_balance_idx],
                              bal_full$Quarter[missing_balance_idx]))
  }
  
  fee_pct_df <- tibble::tibble(Quarter = qp$Quarter,
                               Product = qp$Product,
                               Value = fee_pct)
  
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
  
  grid <- do.call(rbind, lapply(names(grid_map), function(prod) {
    mats <- grid_map[[prod]]
    data.frame(Product = rep(prod, length(mats)), Maturity = mats,
               stringsAsFactors = FALSE)
  }))
  rownames(grid) <- NULL
  
  n_quarters <- length(quarters)
  template <- tibble::tibble(
    Quarter = rep(quarters, each = nrow(grid)),
    Side = rep("funding", n_quarters * nrow(grid)),
    Product = rep(grid$Product, times = n_quarters),
    Maturity = rep(grid$Maturity, times = n_quarters),
    Component = rep("fee", n_quarters * nrow(grid)),
    Value = rep(0, n_quarters * nrow(grid))
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
  
  template <- template[order(match(template$Quarter, quarters),
                             match(template$Product, names(grid_map))), , drop = FALSE]
  
  attr(template, "trace") <- list(
    filters = list(
      fee = list(section = "INCOME AND EXPENSE REPORT", subsection = "CURRENT REVENUE",
                 group = "Fee Income"),
      balance = list(section = "SUMMARY BALANCE SHEET", subsection = "LIABILITIES")
    ),
    matched_fee_rows = nrow(fee_rows),
    notes = if (length(notes)) notes else NULL
  )
  
  template
}