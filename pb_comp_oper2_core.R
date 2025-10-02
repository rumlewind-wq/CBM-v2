# pb_comp_oper2_core.R
# Helper-driven rewrite of pb_comp_oper2 using pb_helpers_core utilities.

pb_comp_oper2_core <- function(res, entity = NULL) {
  context <- "pb_comp_oper2_core"
  pb_helper_require_packages(c("dplyr", "tidyr", "tibble"), context)
  
  al <- pb_helper_prepare_all_long(
    res,
    entity = entity,
    required_cols = c("Quarter", "Item", "Value", "__section", "__subsection", "__group"),
    context = context
  )
  
  quarter_levels <- pb_helper_quarter_levels(al)
  if (!length(quarter_levels)) {
    empty <- pb_helper_empty_result()
    attr(empty, "trace") <- tibble::tibble()
    return(empty)
  }
  
  product_spec <- tibble::tribble(
    ~Product,                       ~Side,      ~Maturity,     ~AnnualFactor,
    "Interbank lending",            "asset",   "1-quarter",  4,
    "Fixed-rate Corporate Loans",   "asset",   "1-quarter",  4,
    "Floating-rate Corporate Loans","asset",   "2-quarter",   4,
    "Consumer Loans",               "asset",   "4-quarter",  4,
    "Mortgage Loans",               "asset",   "8-quarter",  4,
    "Government Bonds",             "asset",   "8-quarter",  4,
    "Interbank borrowing",          "funding", "1-quarter",  4,
    "Retail Demand Deposits",       "funding", "No-Maturity",4,
    "Corporate Demand Deposits",    "funding", "No-Maturity",4,
    "Wholesale Deposits",           "funding", "1-quarter",  4,
    "Savings Deposits",             "funding", "No-Maturity",4,
    "Savings Certificates (CDs)",   "funding", "2-quarter",   4,
    "Long-term Time Deposits",      "funding", "8-quarter",  4,
    "Discount Window Advances",     "funding", "No-Maturity",4
  )
  
  expense_alias <- list(
    "interbank borrowing and lending"      = "__interbank__",
    "business loans fixed rate"            = "Fixed-rate Corporate Loans",
    "business loans floating rate"         = "Floating-rate Corporate Loans",
    "consumer loans"                       = "Consumer Loans",
    "mortgages"                            = "Mortgage Loans",
    "government bonds"                     = "Government Bonds",
    "interbank lending"                    = "Interbank lending",
    "interbank lending at cibor"           = "Interbank lending",
    "interbank borrowing"                  = "Interbank borrowing",
    "retail demand deposits"               = "Retail Demand Deposits",
    "corporate demand deposits"            = "Corporate Demand Deposits",
    "wholesale deposits"                   = "Wholesale Deposits",
    "savings deposits"                     = "Savings Deposits",
    "savings certificates cds"             = "Savings Certificates (CDs)",
    "long term time deposits"              = "Long-term Time Deposits",
    "discount window advances"             = "Discount Window Advances"
  )
  
  balance_alias <- list(
    "interbank lending"                    = "Interbank lending",
    "interbank lending at cibor"           = "Interbank lending",
    "interbank borrowing"                  = "Interbank borrowing",
    "business loans fixed rate"            = "Fixed-rate Corporate Loans",
    "business loans floating rate"         = "Floating-rate Corporate Loans",
    "consumer loans"                       = "Consumer Loans",
    "mortgages"                            = "Mortgage Loans",
    "government bonds"                     = "Government Bonds",
    "retail demand deposits"               = "Retail Demand Deposits",
    "corporate demand deposits"            = "Corporate Demand Deposits",
    "wholesale deposits"                   = "Wholesale Deposits",
    "savings deposits"                     = "Savings Deposits",
    "savings certificates cds"             = "Savings Certificates (CDs)",
    "long term time deposits"              = "Long-term Time Deposits",
    "discount window advances"             = "Discount Window Advances"
  )
  
  sec_norm <- pb_helper_norm(al$`__section`)
  sub_norm <- pb_helper_norm(al$`__subsection`)
  grp_norm <- pb_helper_norm(al$`__group`)
  
  map_expense_product <- function(label) {
    lbl <- pb_helper_norm(label)
    if (!length(lbl) || is.na(lbl) || lbl == "") {
      return(NA_character_)
    }
    if (grepl("dd cost", lbl) && grepl("offset", lbl)) {
      return("__skip__")
    }
    expense_alias[[lbl]] %||% NA_character_
  }
  
  expense_rows <- al[
    sec_norm == "income and expense report" &
      sub_norm == "current expenses" &
      grepl("other non interest operating costs", grp_norm),
    c("Quarter", "Item", "Value"),
    drop = FALSE
  ]
  if (nrow(expense_rows)) {
    expense_rows$Product <- vapply(expense_rows$Item, map_expense_product, character(1))
  }
  
  if (!nrow(expense_rows)) {
    expense_summary <- tibble::tibble(Quarter = character(), Product = character(), Expense = double())
    interbank_summary <- tibble::tibble(Quarter = character(), Expense = double())
  } else {
    expense_rows <- tibble::as_tibble(expense_rows)
    keep_rows <- !is.na(expense_rows$Product) & expense_rows$Product != "__skip__"
    expense_rows <- expense_rows[keep_rows, , drop = FALSE]
    
    interbank_rows <- expense_rows[expense_rows$Product == "__interbank__", , drop = FALSE]
    if (nrow(interbank_rows)) {
      interbank_summary <- interbank_rows |>
        dplyr::group_by(Quarter) |>
        dplyr::summarise(Expense = sum(Value, na.rm = TRUE), .groups = "drop")
    } else {
      interbank_summary <- tibble::tibble(Quarter = character(), Expense = double())
    }
    
    non_ib_rows <- expense_rows[expense_rows$Product != "__interbank__", , drop = FALSE]
    if (nrow(non_ib_rows)) {
      expense_summary <- non_ib_rows |>
        dplyr::group_by(Quarter, Product) |>
        dplyr::summarise(Expense = sum(Value, na.rm = TRUE), .groups = "drop")
    } else {
      expense_summary <- tibble::tibble(Quarter = character(), Product = character(), Expense = double())
    }
  }
  
  map_balance_product <- function(label) {
    lbl <- pb_helper_norm(label)
    if (!length(lbl) || is.na(lbl) || lbl == "") {
      return(NA_character_)
    }
    balance_alias[[lbl]] %||% NA_character_
  }
  
  balance_rows <- al[
    sec_norm == "summary balance sheet" &
      !is.na(al$Value),
    c("Quarter", "Item", "Value"),
    drop = FALSE
  ]
  if (nrow(balance_rows)) {
    balance_rows$Product <- vapply(balance_rows$Item, map_balance_product, character(1))
  }
  
  if (!nrow(balance_rows)) {
    balance_summary <- tibble::tibble(Quarter = character(), Product = character(), Balance = double())
  } else {
    balance_rows <- tibble::as_tibble(balance_rows)
    balance_rows <- balance_rows[!is.na(balance_rows$Product), , drop = FALSE]
    balance_summary <- balance_rows |>
      dplyr::group_by(Quarter, Product) |>
      dplyr::summarise(Balance = sum(Value, na.rm = TRUE), .groups = "drop")
  }
  
  quarters_tbl <- tibble::tibble(Quarter = quarter_levels)
  non_ib_spec <- dplyr::filter(product_spec, !Product %in% c("Interbank lending", "Interbank borrowing"))
  
  non_ib_calc <- tidyr::expand_grid(quarters_tbl, non_ib_spec) |>
    dplyr::left_join(expense_summary, by = c("Quarter", "Product")) |>
    dplyr::mutate(Expense = tidyr::replace_na(Expense, 0)) |>
    dplyr::left_join(balance_summary, by = c("Quarter", "Product")) |>
    dplyr::mutate(
      ValidDenom = is.finite(Balance) & !is.na(Balance) & Balance > 0,
      Value = dplyr::if_else(ValidDenom, (Expense / Balance) * AnnualFactor, NA_real_),
      Denominator = dplyr::if_else(ValidDenom, Balance, NA_real_)
    ) |>
    dplyr::select(Quarter, Side, Product, Maturity, Expense, Balance, Denominator, AnnualFactor, Value)
  
  ib_balances <- balance_summary |>
    dplyr::filter(Product %in% c("Interbank lending", "Interbank borrowing")) |>
    tidyr::pivot_wider(names_from = Product, values_from = Balance, values_fill = list(Balance = NA_real_))
  
  if (!nrow(ib_balances)) {
    ib_balances <- tibble::tibble(
      Quarter = character(),
      `Interbank lending` = numeric(),
      `Interbank borrowing` = numeric()
    )
  }
  
  ib_calc <- lapply(quarter_levels, function(q) {
    lend_raw <- ib_balances$`Interbank lending`[ib_balances$Quarter == q]
    borrow_raw <- ib_balances$`Interbank borrowing`[ib_balances$Quarter == q]
    lend_raw <- lend_raw[1]
    borrow_raw <- borrow_raw[1]
    if (!length(lend_raw)) lend_raw <- NA_real_
    if (!length(borrow_raw)) borrow_raw <- NA_real_
    
    exp_total <- interbank_summary$Expense[interbank_summary$Quarter == q]
    exp_total <- exp_total[1]
    if (!length(exp_total) || is.na(exp_total)) exp_total <- 0
    
    lend_pos <- if (is.finite(lend_raw) && lend_raw > 0) lend_raw else 0
    borrow_pos <- if (is.finite(borrow_raw) && borrow_raw > 0) borrow_raw else 0
    total_balance <- lend_pos + borrow_pos
    
    if (!is.finite(total_balance) || total_balance <= 0) {
      tibble::tibble(
        Quarter = rep(q, 2),
        Side = c("asset", "funding"),
        Product = c("Interbank lending", "Interbank borrowing"),
        Maturity = rep("1-quarter", 2),
        Expense = c(NA_real_, NA_real_),
        Balance = c(lend_raw, borrow_raw),
        Denominator = c(NA_real_, NA_real_),
        AnnualFactor = rep(4, 2),
        Value = c(NA_real_, NA_real_)
      )
    } else {
      rate_shared <- (exp_total / total_balance) * 4
      share_lend <- if (total_balance > 0) lend_pos / total_balance else 0
      share_borrow <- if (total_balance > 0) borrow_pos / total_balance else 0
      exp_lend <- exp_total * share_lend
      exp_borrow <- exp_total * share_borrow
      
      tibble::tibble(
        Quarter = c(q, q),
        Side = c("asset", "funding"),
        Product = c("Interbank lending", "Interbank borrowing"),
        Maturity = c("1-quarter", "1-quarter"),
        Expense = c(exp_lend, exp_borrow),
        Balance = c(lend_raw, borrow_raw),
        Denominator = c(lend_raw, borrow_raw),
        AnnualFactor = c(4, 4),
        Value = c(rate_shared, rate_shared)
      )
    }
  })
  ib_calc <- dplyr::bind_rows(ib_calc)
  
  full_calc <- dplyr::bind_rows(non_ib_calc, ib_calc)

  seq_quarter_labels <- function(n) {
    paste0(seq_len(n), "-quarter")
  }

  make_display_block <- function(product, side, maturities) {
    tibble::tibble(Side = side, Product = product, Maturity = maturities)
  }

  asset_display <- dplyr::bind_rows(
    make_display_block("Consumer Loans", "asset", seq_quarter_labels(4)),
    make_display_block("Fixed-rate Corporate Loans", "asset", "1-quarter"),
    make_display_block("Floating-rate Corporate Loans", "asset", seq_quarter_labels(2)),
    make_display_block("Government Bonds", "asset", seq_quarter_labels(8)),
    make_display_block("Interbank lending", "asset", "1-quarter"),
    make_display_block("Mortgage Loans", "asset", seq_quarter_labels(8))
  )

  funding_display <- dplyr::bind_rows(
    make_display_block("Interbank borrowing", "funding", "1-quarter"),
    make_display_block("Retail Demand Deposits", "funding", "No-Maturity"),
    make_display_block("Corporate Demand Deposits", "funding", "No-Maturity"),
    make_display_block("Savings Deposits", "funding", "No-Maturity"),
    make_display_block("Savings Certificates (CDs)", "funding", seq_quarter_labels(2)),
    make_display_block("Long-term Time Deposits", "funding", seq_quarter_labels(8)),
    make_display_block("Wholesale Deposits", "funding", seq_quarter_labels(4)),
    make_display_block("Discount Window Advances", "funding", "No-Maturity")
  )

  display_spec <- dplyr::bind_rows(asset_display, funding_display)

  grid <- tidyr::expand_grid(
    Quarter = quarter_levels,
    display_spec
  ) |>
    dplyr::left_join(full_calc, by = c("Quarter", "Product", "Side", "Maturity")) |>
    dplyr::mutate(
      Component = "oper",
      Value = Value
    ) |>
    dplyr::select(Quarter, Side, Product, Maturity, Component, Value)
  
  grid <- grid[order(match(grid$Quarter, quarter_levels), grid$Side, grid$Product, grid$Maturity), ]
  
  trace_tbl <- full_calc |>
    dplyr::mutate(Component = "oper") |>
    dplyr::select(Quarter, Side, Product, Maturity, Component, Expense, Balance, Denominator, AnnualFactor, Value)
  
  pb_helper_make_result(grid, trace = tibble::as_tibble(trace_tbl), context = context)
}