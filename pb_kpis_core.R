#' Beregn ProBanker KPI'er for market aggregate (core helper version)
#'
#' @param res Parser-resultat med komponenten `all_long`
#' @param q Kvartal ("latest" vælger seneste tilgængelige)
#' @param unit Outputenhed ("decimal" eller "percent")
#'
#' @return Tibble med kolonnerne Quarter, FundingLoanRatio, LoanDepositRatio,
#'   NetInterestMargin, ROE og ROA
pb_kpis <- function(res, q = "latest", unit = c("decimal", "percent")) {
  context <- "pb_kpis"
  unit <- match.arg(unit)
  
  pb_helper_require_packages(c("dplyr", "tidyr", "tibble", "stringr"), context)
  
  if (!exists("pb_quarters", mode = "function")) {
    stop("pb_quarters() mangler. Source PB-helpers først.", call. = FALSE)
  }
  
  data <- pb_helper_prepare_all_long(
    res,
    context = context,
    required_cols = c("Quarter", "Item", "Value", "__section")
  )
  
  has_subsection <- "__subsection" %in% names(data)
  has_group <- "__group" %in% names(data)
  
  section_balance <- pb_helper_norm("Market Average Balance Sheet")
  section_income <- pb_helper_norm("Income and Expense Report")
  
  data_norm <- data %>%
    dplyr::mutate(
      Quarter = pb_helper_chr(.data$Quarter),
      Item_raw = pb_helper_chr(.data$Item),
      section_norm = pb_helper_norm(.data$`__section`),
      subsection_norm = if (has_subsection) pb_helper_norm(.data$`__subsection`) else NA_character_,
      group_norm = if (has_group) pb_helper_norm(.data$`__group`) else NA_character_,
      item_norm = pb_helper_norm(.data$Item),
      value_num = suppressWarnings(as.numeric(.data$Value))
    ) %>%
    dplyr::filter(!stringr::str_detect(.data$Item_raw, "(?i)\\(rate\\)"))
  
  balance_data <- dplyr::filter(data_norm, .data$section_norm == section_balance)
  income_data <- dplyr::filter(data_norm, .data$section_norm == section_income)
  
  if (!nrow(balance_data)) {
    stop("Balancerapporten 'Market Average Balance Sheet' blev ikke fundet i data.", call. = FALSE)
  }
  if (!nrow(income_data)) {
    stop("Resultatopgørelsen 'Income and Expense Report' blev ikke fundet i data.", call. = FALSE)
  }
  
  quarters_all <- pb_quarters(res)
  if (!length(quarters_all)) {
    stop("Ingen kvartaler fundet i parser-outputtet.", call. = FALSE)
  }
  
  if (length(q) > 1L) {
    q <- q[[1L]]
  }
  
  q_candidates <- pb_helper_select_quarters(quarters_all, q, context = sprintf("%s: kvartalsvalg", context))
  if (!length(q_candidates)) {
    stop("Kvartalet findes ikke i data.", call. = FALSE)
  }
  if (length(q_candidates) > 1L) {
    stop("pb_kpis understøtter kun ét kvartal ad gangen.", call. = FALSE)
  }
  q_sel <- q_candidates[[1L]]
  
  idx <- match(q_sel, quarters_all)
  q_prev <- if (!is.na(idx) && idx > 1) quarters_all[[idx - 1]] else NA_character_
  
  quarters_needed <- unique(c(q_sel, q_prev))
  quarters_needed <- quarters_needed[!is.na(quarters_needed)]
  
  balance_use <- dplyr::filter(balance_data, .data$Quarter %in% quarters_needed)
  income_use <- dplyr::filter(income_data, .data$Quarter %in% quarters_needed)
  
  build_matcher <- function(exact = character(), prefix = character(), regex = character()) {
    exact_norm <- pb_helper_norm(exact)
    prefix_norm <- pb_helper_norm(prefix)
    regex_vals <- regex
    function(norm_values, raw_values = NULL) {
      matched <- rep(FALSE, length(norm_values))
      if (length(exact_norm)) {
        matched <- matched | (!is.na(norm_values) & norm_values %in% exact_norm)
      }
      if (length(prefix_norm)) {
        for (p in prefix_norm) {
          matched <- matched | (!is.na(norm_values) & stringr::str_starts(norm_values, p))
        }
      }
      if (length(regex_vals)) {
        source_vals <- if (is.null(raw_values)) norm_values else raw_values
        for (r in regex_vals) {
          matched <- matched | (!is.na(source_vals) & stringr::str_detect(source_vals, r))
        }
      }
      matched
    }
  }
  
  pull_value <- function(df, quarter, matcher, label, default = NA_real_, required = TRUE) {
    match_idx <- df$Quarter == quarter & matcher(df$item_norm, df$Item_raw)
    rows <- df[match_idx, , drop = FALSE]
    if (!nrow(rows)) {
      if (required) {
        stop(
          sprintf("Kan ikke finde posten '%s' for kvartalet %s.", label, quarter),
          call. = FALSE
        )
      }
      return(default)
    }
    sum(rows$value_num, na.rm = TRUE)
  }
  
  matcher_loans_fixed <- build_matcher(exact = "Business Loans - Fixed-Rate")
  matcher_loans_float <- build_matcher(
    exact = "Business Loans - Floating-Rate",
    prefix = "Business Loans - Floating-Rate, maturing start of:"
  )
  matcher_consumer <- build_matcher(
    exact = "Consumer Loans",
    prefix = "Consumer Loans, maturing start of:"
  )
  matcher_mortgage <- build_matcher(
    exact = c("Mortgages", "Mortgage Loans"),
    prefix = "Mortgage Loans, maturing start of:"
  )
  matcher_lla <- build_matcher(exact = "Loan Loss Allowance")
  
  matcher_retail_dd <- build_matcher(exact = "Retail Demand Deposits")
  matcher_corp_dd <- build_matcher(exact = "Corporate Demand Deposits")
  matcher_savings <- build_matcher(exact = "Savings Deposits")
  matcher_cds <- build_matcher(
    exact = "Savings Certificates (CDs)",
    prefix = "Savings Certificates (CDs), maturing start of:"
  )
  matcher_long_term <- build_matcher(
    exact = "Long-term Time Deposits",
    prefix = "Long-term Time Deposits, maturing start of:"
  )
  matcher_wholesale <- build_matcher(
    exact = "Wholesale Deposits",
    prefix = "Wholesale Deposits, maturing start of:"
  )
  matcher_interbank <- build_matcher(exact = "Interbank Borrowing")
  matcher_discount <- build_matcher(exact = "Discount Window Advances")
  
  matcher_total_assets <- build_matcher(exact = "TOTAL ASSETS")
  matcher_equity <- build_matcher(exact = "Net Worth and Retained Earnings")
  
  matcher_interest_income <- list(
    build_matcher(exact = "Business Loans - Fixed-Rate"),
    build_matcher(exact = "Business Loans - Floating-Rate"),
    build_matcher(exact = "Consumer Loans"),
    build_matcher(exact = c("Mortgages", "Mortgage Loans")),
    build_matcher(exact = "Government Bonds"),
    build_matcher(exact = "Interbank Lending (at CIBOR)")
  )
  
  matcher_interest_expense <- list(
    matcher_interbank,
    matcher_corp_dd,
    matcher_retail_dd,
    matcher_wholesale,
    matcher_savings,
    matcher_cds,
    matcher_long_term,
    matcher_discount
  )
  
  matcher_net_income <- build_matcher(exact = "TOTAL NET INCOME")
  
  sum_matchers <- function(df, quarter, matchers, label) {
    values <- vapply(
      matchers,
      function(m) pull_value(df, quarter, m, label, default = 0, required = FALSE),
      numeric(1)
    )
    sum(values, na.rm = TRUE)
  }
  
  loans_fixed <- pull_value(balance_use, q_sel, matcher_loans_fixed, "Business Loans - Fixed-Rate", default = 0, required = FALSE)
  loans_float <- pull_value(balance_use, q_sel, matcher_loans_float, "Business Loans - Floating-Rate", default = 0, required = FALSE)
  loans_consumer <- pull_value(balance_use, q_sel, matcher_consumer, "Consumer Loans", default = 0, required = FALSE)
  loans_mortgage <- pull_value(balance_use, q_sel, matcher_mortgage, "Mortgages", default = 0, required = FALSE)
  loan_loss_allowance <- pull_value(balance_use, q_sel, matcher_lla, "Loan Loss Allowance", default = 0, required = FALSE)
  
  deposits_retail <- pull_value(balance_use, q_sel, matcher_retail_dd, "Retail Demand Deposits", default = 0, required = FALSE)
  deposits_corp <- pull_value(balance_use, q_sel, matcher_corp_dd, "Corporate Demand Deposits", default = 0, required = FALSE)
  deposits_savings <- pull_value(balance_use, q_sel, matcher_savings, "Savings Deposits", default = 0, required = FALSE)
  deposits_cds <- pull_value(balance_use, q_sel, matcher_cds, "Savings Certificates (CDs)", default = 0, required = FALSE)
  deposits_long_term <- pull_value(balance_use, q_sel, matcher_long_term, "Long-term Time Deposits", default = 0, required = FALSE)
  
  wholesale_funding <- pull_value(balance_use, q_sel, matcher_wholesale, "Wholesale Deposits", default = 0, required = FALSE)
  interbank_funding <- pull_value(balance_use, q_sel, matcher_interbank, "Interbank Borrowing", default = 0, required = FALSE)
  discount_window <- pull_value(balance_use, q_sel, matcher_discount, "Discount Window Advances", default = 0, required = FALSE)
  
  total_assets_curr <- pull_value(balance_use, q_sel, matcher_total_assets, "TOTAL ASSETS")
  equity_curr <- pull_value(balance_use, q_sel, matcher_equity, "Net Worth and Retained Earnings")
  
  total_assets_prev <- if (!is.na(q_prev)) {
    pull_value(balance_use, q_prev, matcher_total_assets, "TOTAL ASSETS", required = FALSE)
  } else {
    NA_real_
  }
  equity_prev <- if (!is.na(q_prev)) {
    pull_value(balance_use, q_prev, matcher_equity, "Net Worth and Retained Earnings", required = FALSE)
  } else {
    NA_real_
  }
  
  interest_income <- sum_matchers(income_use, q_sel, matcher_interest_income, "Interest Income")
  interest_expense <- sum_matchers(income_use, q_sel, matcher_interest_expense, "Interest Expense")
  net_income <- pull_value(income_use, q_sel, matcher_net_income, "TOTAL NET INCOME")
  
  loans_sum <- loans_fixed + loans_float + loans_consumer + loans_mortgage
  loans_net <- loans_sum - loan_loss_allowance
  deposits_core <- deposits_retail + deposits_corp + deposits_savings + deposits_cds + deposits_long_term
  funding_total <- deposits_core + wholesale_funding + interbank_funding + discount_window
  
  avg_assets <- if (!is.na(total_assets_prev) && !is.na(total_assets_curr)) {
    mean(c(total_assets_curr, total_assets_prev), na.rm = TRUE)
  } else {
    total_assets_curr
  }
  
  avg_equity <- if (!is.na(equity_prev) && !is.na(equity_curr)) {
    mean(c(equity_curr, equity_prev), na.rm = TRUE)
  } else {
    equity_curr
  }
  
  calc_ratio <- function(num_val, den_val) {
    if (is.na(num_val) || is.na(den_val)) {
      return(NA_real_)
    }
    if (abs(den_val) < .Machine$double.eps) {
      return(NA_real_)
    }
    num_val / den_val
  }
  
  out <- tibble::tibble(
    Quarter = q_sel,
    FundingLoanRatio = calc_ratio(loans_net, funding_total),
    LoanDepositRatio = calc_ratio(loans_net, deposits_core),
    NetInterestMargin = calc_ratio(interest_income - interest_expense, avg_assets),
    ROE = calc_ratio(net_income, avg_equity),
    ROA = calc_ratio(net_income, avg_assets)
  )
  
  if (unit == "percent") {
    out <- out %>%
      dplyr::mutate(dplyr::across(-.data$Quarter, ~ .x * 100))
  }
  
  out
}