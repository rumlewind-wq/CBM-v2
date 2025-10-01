# pb_liabilities_amounts_and_rates_core.R
# Refactored liabilities amounts & rates calculations built on pb_helpers_core.R utilities.

pb_liabilities_format_amount <- function(x) {
  format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
}

pb_liabilities_resolve <- function(res, context) {
  if (!is.null(res)) {
    return(res)
  }
  if (exists("res", envir = .GlobalEnv, inherits = TRUE)) {
    return(get("res", envir = .GlobalEnv, inherits = TRUE))
  }
  stop(sprintf("%s: Argument 'res' mangler, og global 'res' blev ikke fundet.", context), call. = FALSE)
}

pb_liabilities_quarters <- function(res, context) {
  quarters <- pb_quarters(res, context = context)
  if (!length(quarters)) {
    stop(sprintf("%s: Ingen kvartaler fundet i datagrundlaget.", context), call. = FALSE)
  }
  quarter_nums <- suppressWarnings(as.integer(gsub("^.*?(\\\d+)$", "\\\1", quarters)))
  max_num <- max(quarter_nums, na.rm = TRUE)
  list(
    current = paste0("Q", max_num),
    previous = paste0("Q", max_num - 1L)
  )
}

pb_liabilities_group_table <- function(res, group_title, context) {
  pb_get_table(
    res,
    section = "BANK BALANCE SHEET",
    subsection = "LIABILITIES",
    group = group_title,
    context = context
  )
}

pb_liabilities_amounts <- function(res = NULL) {
  context <- "pb_liabilities_amounts()"
  res <- pb_liabilities_resolve(res, context)
  pb_helper_require_packages(c("dplyr", "tibble"), context)
  
  qs <- pb_liabilities_quarters(res, context)
  q_prev <- qs$previous
  q_cur <- qs$current
  
  ensure_quarters <- function(df) {
    if (!nrow(df)) {
      return(df)
    }
    if (!q_prev %in% names(df)) {
      df[[q_prev]] <- NA_real_
    }
    if (!q_cur %in% names(df)) {
      df[[q_cur]] <- NA_real_
    }
    df
  }
  
  build_block <- function(group_title, block_label, item_filter) {
    tbl <- pb_liabilities_group_table(res, group_title, context)
    if (!nrow(tbl)) {
      return(tibble::tibble(Block = character(), Item = character(), Q_prev = numeric(), Q_cur = numeric()))
    }
    tbl <- ensure_quarters(tbl)
    items <- pb_helper_chr(tbl$Item)
    keep <- item_filter(items)
    tbl <- tbl[keep, , drop = FALSE]
    if (!nrow(tbl)) {
      return(tibble::tibble(Block = character(), Item = character(), Q_prev = numeric(), Q_cur = numeric()))
    }
    tibble::tibble(
      Block = block_label,
      Item = items[keep],
      Q_prev = suppressWarnings(as.numeric(tbl[[q_prev]])),
      Q_cur = suppressWarnings(as.numeric(tbl[[q_cur]]))
    )
  }
  
  dplyr::bind_rows(
    build_block("Demand Deposits", "Demand Deposits", function(x) x %in% c("Retail", "Corporate")),
    build_block("Wholesale Deposits, maturing start of:", "Wholesale Deposits (maturing start of:)", function(x) grepl("^T\\\+\\\d+$", x)),
    build_block("Savings Deposits", "Savings Deposits", function(x) x == "Savings Deposits"),
    build_block("Savings Certificates (CDs), maturing start of:", "Savings Certificates (maturing start of:)", function(x) grepl("^T\\\+\\\d+$", x)),
    build_block("Long-term Time Deposits, maturing start of:", "Long-term Time Deposits (maturing start of:)", function(x) grepl("^T\\\+\\\d+$", x))
  ) |>
    dplyr::mutate(
      Delta = Q_cur - Q_prev,
      Pct_Change = dplyr::if_else(is.na(Q_prev) | Q_prev == 0, NA_real_, (Q_cur - Q_prev) / Q_prev * 100)
    ) |>
    dplyr::mutate(
      Q_prev = pb_liabilities_format_amount(Q_prev),
      Q_cur = pb_liabilities_format_amount(Q_cur),
      Delta = pb_liabilities_format_amount(Delta),
      Pct_Change = ifelse(is.na(Pct_Change), "", paste0(format(round(Pct_Change, 2), nsmall = 2), "%"))
    ) |>
    dplyr::select(Block, Item, Q_prev, Q_cur, Delta, Pct_Change)
}

pb_liabilities_rates <- function(res = NULL) {
  context <- "pb_liabilities_rates()"
  res <- pb_liabilities_resolve(res, context)
  pb_helper_require_packages(c("dplyr", "tibble"), context)
  
  qs <- pb_liabilities_quarters(res, context)
  q_prev <- qs$previous
  q_cur <- qs$current
  
  ensure_quarters <- function(df) {
    if (!nrow(df)) {
      return(df)
    }
    if (!q_prev %in% names(df)) {
      df[[q_prev]] <- NA_real_
    }
    if (!q_cur %in% names(df)) {
      df[[q_cur]] <- NA_real_
    }
    df
  }
  
  build_block <- function(group_title, block_label) {
    tbl <- pb_liabilities_group_table(res, group_title, context)
    if (!nrow(tbl)) {
      return(tibble::tibble(Block = character(), Item = character(), Q_prev = numeric(), Q_cur = numeric()))
    }
    tbl <- ensure_quarters(tbl)
    items <- pb_helper_chr(tbl$Item)
    keep <- grepl("\\\(rate\\\)$", items)
    tbl <- tbl[keep, , drop = FALSE]
    if (!nrow(tbl)) {
      return(tibble::tibble(Block = character(), Item = character(), Q_prev = numeric(), Q_cur = numeric()))
    }
    tibble::tibble(
      Block = block_label,
      Item = items[keep],
      Q_prev = suppressWarnings(as.numeric(tbl[[q_prev]])),
      Q_cur = suppressWarnings(as.numeric(tbl[[q_cur]]))
    )
  }
  
  dplyr::bind_rows(
    build_block("Demand Deposits", "Demand Deposits (rates)"),
    build_block("Wholesale Deposits, maturing start of:", "Wholesale Deposits (rates)"),
    build_block("Savings Deposits", "Savings Deposits (rates)"),
    build_block("Savings Certificates (CDs), maturing start of:", "Savings Certificates (rates)"),
    build_block("Long-term Time Deposits, maturing start of:", "Long-term Time Deposits (rates)")
  ) |>
    dplyr::mutate(
      Delta = Q_cur - Q_prev,
      Pct_Change = dplyr::if_else(is.na(Q_prev) | Q_prev == 0, NA_real_, (Q_cur - Q_prev) / Q_prev * 100)
    ) |>
    dplyr::mutate(
      Q_prev = ifelse(is.na(Q_prev), "", paste0(format(round(Q_prev, 2), nsmall = 2), "%")),
      Q_cur = ifelse(is.na(Q_cur), "", paste0(format(round(Q_cur, 2), nsmall = 2), "%")),
      Delta = ifelse(is.na(Delta), "", paste0(format(round(Delta, 2), nsmall = 2), " pp")),
      Pct_Change = ifelse(is.na(Pct_Change), "", paste0(format(round(Pct_Change, 2), nsmall = 2), "%"))
    ) |>
    dplyr::select(Block, Item, Q_prev, Q_cur, Delta, Pct_Change)
}

pb_liabilities_all <- function(res = NULL) {
  res <- pb_liabilities_resolve(res, "pb_liabilities_all()")
  list(
    amounts = pb_liabilities_amounts(res),
    rates = pb_liabilities_rates(res)
  )
}

pb_liabilities_amounts_print <- function(res = NULL) {
  res <- pb_liabilities_resolve(res, "pb_liabilities_amounts_print()")
  out <- pb_liabilities_amounts(res)
  print(out, n = nrow(out))
  invisible(out)
}

pb_liabilities_rates_print <- function(res = NULL) {
  res <- pb_liabilities_resolve(res, "pb_liabilities_rates_print()")
  out <- pb_liabilities_rates(res)
  print(out, n = nrow(out))
  invisible(out)
}

pb_liabilities_all_print <- function(res = NULL) {
  res <- pb_liabilities_resolve(res, "pb_liabilities_all_print()")
  both <- pb_liabilities_all(res)
  cat("=== AMOUNTS ===\n")
  print(both$amounts, n = nrow(both$amounts))
  cat("\n=== RATES ===\n")
  print(both$rates, n = nrow(both$rates))
  invisible(both)
}

pb_liab_amounts <- function(res = NULL) pb_liabilities_amounts_print(res)
pb_liab_rates <- function(res = NULL) pb_liabilities_rates_print(res)
pb_liab_both <- function(res = NULL) pb_liabilities_all_print(res)