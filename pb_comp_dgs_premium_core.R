# pb_comp_dgs_premium_core.R
# Refactored version of pb_comp_dgs_premium() using pb_helpers_core utilities.

pb_comp_dgs_premium <- function(res, q = "latest", entity = NULL) {
  context <- "pb_comp_dgs_premium"
  pb_helper_require_packages(c("dplyr", "tibble", "tidyr"), context)
  
  al <- pb_helper_prepare_all_long(res, entity = entity, context = context)
  if (!nrow(al)) {
    return(pb_helper_empty_result())
  }
  
  quarter_order_all <- pb_helper_quarter_order(al$Quarter)
  if (!length(quarter_order_all)) {
    return(pb_helper_empty_result())
  }
  
  select_quarters <- function(all_quarters, q_arg, context) {
    all_unique <- unique(all_quarters)
    all_unique <- pb_helper_quarter_order(all_unique)
    if (!length(all_unique)) {
      stop(sprintf("%s: Ingen kvartaler fundet i datagrundlaget.", context), call. = FALSE)
    }
    if (is.null(q_arg) || (is.character(q_arg) && length(q_arg) == 1 && tolower(q_arg) == "latest")) {
      return(tail(all_unique, 1))
    }
    if (is.character(q_arg) && length(q_arg) == 1 && tolower(q_arg) == "all") {
      return(all_unique)
    }
    if (is.numeric(q_arg) && length(q_arg) == 1 && is.finite(q_arg) && q_arg >= 1) {
      idx <- as.integer(q_arg)
      idx <- min(idx, length(all_unique))
      return(tail(all_unique, idx))
    }
    if (is.character(q_arg)) {
      q_vec <- pb_helper_chr(q_arg)
      matches <- all_unique[all_unique %in% q_vec]
      if (!length(matches)) {
        stop(
          sprintf(
            "%s: Ingen af de angivne kvartaler matcher kendte labels (%s).",
            context,
            paste(q_vec, collapse = ", ")
          ),
          call. = FALSE
        )
      }
      return(unique(matches))
    }
    stop(
      sprintf(
        "%s: Argumentet q skal være 'latest', 'all', et tal eller en vektor af kendte kvartaler.",
        context
      ),
      call. = FALSE
    )
  }
  
  quarters_sel <- select_quarters(quarter_order_all, q, sprintf("%s kvartalsfilter", context))
  al <- al[al$Quarter %in% quarters_sel, , drop = FALSE]
  if (!nrow(al)) {
    return(pb_helper_empty_result())
  }
  quarters <- pb_helper_quarter_order(quarters_sel)
  
  section_norm <- pb_helper_norm(al$`__section`)
  subsection_norm <- pb_helper_norm(al$`__subsection`)
  group_norm <- pb_helper_norm(al$`__group`)
  item_norm <- pb_helper_norm(al$Item)
  
  target_income_section <- pb_helper_norm("INCOME AND EXPENSE REPORT")
  target_oper_cost <- pb_helper_norm("Other Non-interest Operating Costs")
  target_dgs_item <- pb_helper_norm("DGS Deposit Insurance Premium")
  
  dgs_idx <-
    section_norm == target_income_section &
    group_norm == target_oper_cost &
    item_norm == target_dgs_item &
    !is.na(al$Value)
  dgs_rows <- al[dgs_idx, c("Quarter", "Value"), drop = FALSE]
  if (!nrow(dgs_rows)) {
    stop("Unable to locate 'DGS Deposit Insurance Premium' in section 'Other Non-interest Operating Costs'.")
  }
  dgs_expense <- dgs_rows |>
    tibble::as_tibble() |>
    dplyr::group_by(.data$Quarter) |>
    dplyr::summarise(dgs_expense = sum(.data$Value, na.rm = TRUE), .groups = "drop")
  
  insured_items <- c(
    "Retail Demand Deposits",
    "Corporate Demand Deposits",
    "Savings Deposits",
    "Savings Certificates (CDs)",
    "Long-term Time Deposits",
    "Wholesale Deposits"
  )
  insured_norm <- pb_helper_norm(insured_items)
  
  target_balance_section <- pb_helper_norm("SUMMARY BALANCE SHEET")
  target_liabilities <- pb_helper_norm("LIABILITIES")
  dep_idx <-
    section_norm == target_balance_section &
    subsection_norm == target_liabilities &
    item_norm %in% insured_norm &
    !is.na(al$Value)
  dep_rows <- al[dep_idx, c("Quarter", "Item", "Value"), drop = FALSE]
  if (!nrow(dep_rows)) {
    stop("Unable to locate insured deposit balances in 'Summary Balance Sheet → Liabilities'.")
  }
  dep_rows <- tibble::as_tibble(dep_rows)
  dep_rows$Product <- insured_items[match(pb_helper_norm(dep_rows$Item), insured_norm)]
  
  dep_detail <- dep_rows |>
    dplyr::group_by(.data$Quarter, .data$Product) |>
    dplyr::summarise(insured_balance = sum(.data$Value, na.rm = TRUE), .groups = "drop")
  insured_base <- dep_detail |>
    dplyr::group_by(.data$Quarter) |>
    dplyr::summarise(insured_base = sum(.data$insured_balance, na.rm = TRUE), .groups = "drop")
  
  env_section <- pb_helper_norm("ECONOMIC ENVIRONMENT REPORT")
  env_item_prefix <- pb_helper_norm("Annualized DGS Premium")
  env_idx <-
    section_norm == env_section &
    !is.na(al$Value) &
    !is.na(item_norm) &
    startsWith(item_norm, env_item_prefix)
  env_rows <- al[env_idx, c("Quarter", "Value"), drop = FALSE]
  env_rate <- tibble::as_tibble(env_rows) |>
    dplyr::group_by(.data$Quarter) |>
    dplyr::summarise(env_rate = mean(.data$Value, na.rm = TRUE), .groups = "drop")
  
  format_num <- function(x) {
    out <- format(x, trim = TRUE, scientific = FALSE, digits = 15)
    out[is.na(x)] <- NA_character_
    out
  }
  
  funding_specs <- tibble::tribble(
    ~sort_index, ~Side,     ~Product,                      ~Maturity,    ~covered,
    100L,        "funding", "Interbank borrowing",        "No-Maturity", FALSE,
    101L,        "funding", "Retail Demand Deposits",     "No-Maturity", TRUE,
    102L,        "funding", "Corporate Demand Deposits",  "No-Maturity", TRUE,
    103L,        "funding", "Wholesale Deposits",         "1-quarter",   TRUE,
    104L,        "funding", "Wholesale Deposits",         "2-quarter",   TRUE,
    105L,        "funding", "Wholesale Deposits",         "3-quarter",   TRUE,
    106L,        "funding", "Wholesale Deposits",         "4-quarter",   TRUE,
    107L,        "funding", "Savings Deposits",           "No-Maturity", TRUE,
    108L,        "funding", "Savings Certificates (CDs)", "1-quarter",   TRUE,
    109L,        "funding", "Savings Certificates (CDs)", "2-quarter",   TRUE,
    110L,        "funding", "Long-term Time Deposits",    "1-quarter",   TRUE,
    111L,        "funding", "Long-term Time Deposits",    "2-quarter",   TRUE,
    112L,        "funding", "Long-term Time Deposits",    "3-quarter",   TRUE,
    113L,        "funding", "Long-term Time Deposits",    "4-quarter",   TRUE,
    114L,        "funding", "Long-term Time Deposits",    "5-quarter",   TRUE,
    115L,        "funding", "Long-term Time Deposits",    "6-quarter",   TRUE,
    116L,        "funding", "Long-term Time Deposits",    "7-quarter",   TRUE,
    117L,        "funding", "Long-term Time Deposits",    "8-quarter",   TRUE,
    118L,        "funding", "Discount window advance",    "No-Maturity", FALSE
  )
  
  asset_specs <- tibble::tribble(
    ~sort_index, ~Side,   ~Product,                       ~Maturity, ~covered,
    1L,          "asset", "Interbank lending",            "T+1",    FALSE,
    2L,          "asset", "Fixed-rate Corporate Loans",   "T+1",    FALSE,
    3L,          "asset", "Floating-rate Corporate Loans","T+1",    FALSE,
    4L,          "asset", "Floating-rate Corporate Loans","T+2",    FALSE,
    5L,          "asset", "Consumer Loans",               "T+1",    FALSE,
    6L,          "asset", "Consumer Loans",               "T+2",    FALSE,
    7L,          "asset", "Consumer Loans",               "T+3",    FALSE,
    8L,          "asset", "Consumer Loans",               "T+4",    FALSE,
    9L,          "asset", "Mortgage Loans",               "T+1",    FALSE,
    10L,         "asset", "Mortgage Loans",               "T+2",    FALSE,
    11L,         "asset", "Mortgage Loans",               "T+3",    FALSE,
    12L,         "asset", "Mortgage Loans",               "T+4",    FALSE,
    13L,         "asset", "Mortgage Loans",               "T+5",    FALSE,
    14L,         "asset", "Mortgage Loans",               "T+6",    FALSE,
    15L,         "asset", "Mortgage Loans",               "T+7",    FALSE,
    16L,         "asset", "Mortgage Loans",               "T+8",    FALSE,
    17L,         "asset", "Government Bonds",             "T+1",    FALSE,
    18L,         "asset", "Government Bonds",             "T+2",    FALSE,
    19L,         "asset", "Government Bonds",             "T+3",    FALSE,
    20L,         "asset", "Government Bonds",             "T+4",    FALSE,
    21L,         "asset", "Government Bonds",             "T+5",    FALSE,
    22L,         "asset", "Government Bonds",             "T+6",    FALSE,
    23L,         "asset", "Government Bonds",             "T+7",    FALSE,
    24L,         "asset", "Government Bonds",             "T+8",    FALSE
  )
  
  all_specs <- dplyr::bind_rows(asset_specs, funding_specs)
  
  rate_tbl <- dplyr::left_join(dgs_expense, insured_base, by = "Quarter") |>
    dplyr::mutate(
      dgs_rate = dplyr::if_else(
        is.finite(.data$insured_base) & .data$insured_base > 0,
        (.data$dgs_expense / .data$insured_base) * 4,
        as.numeric(NA)
      ),
      zero_base = !(is.finite(.data$insured_base) & .data$insured_base > 0)
    ) |>
    dplyr::right_join(tibble::tibble(Quarter = quarters), by = "Quarter") |>
    dplyr::left_join(env_rate, by = "Quarter")
  
  spec_map <- dplyr::mutate(all_specs, spec_index = dplyr::row_number())
  grid <- tidyr::expand_grid(Quarter = quarters, spec_index = spec_map$spec_index) |>
    dplyr::left_join(spec_map, by = "spec_index") |>
    dplyr::left_join(rate_tbl, by = "Quarter") |>
    dplyr::mutate(
      Value_num = dplyr::case_when(
        .data$Side == "funding" & .data$covered & !is.na(.data$dgs_rate) ~ .data$dgs_rate,
        .data$Side == "funding" & .data$covered                         ~ as.numeric(NA),
        TRUE                                                             ~ 0
      ),
      Value = dplyr::case_when(
        .data$Side == "funding" & .data$covered & !is.na(.data$Value_num) ~ format_num(.data$Value_num),
        .data$Side == "funding" & .data$covered &  is.na(.data$Value_num) ~ NA_character_,
        TRUE                                                              ~ "nul"
      )
    )
  
  out_tbl <- grid |>
    dplyr::transmute(
      Quarter   = pb_helper_chr(.data$Quarter),
      Side      = .data$Side,
      Product   = pb_helper_chr(.data$Product),
      Maturity  = pb_helper_chr(.data$Maturity),
      Component = "dgs",
      Value     = .data$Value,
      sort_index = .data$sort_index
    ) |>
    dplyr::arrange(.data$Quarter, .data$sort_index)
  
  out_tbl$sort_index <- NULL
  rownames(out_tbl) <- NULL
  
  recon <- rate_tbl |>
    dplyr::mutate(
      reconstructed_expense = .data$dgs_rate * .data$insured_base / 4,
      diff = .data$dgs_expense - .data$reconstructed_expense
    )
  
  tol <- 1e-6
  notes <- character()
  
  missing_rate_q <- recon$Quarter[is.na(recon$dgs_rate)]
  missing_rate_q <- sort(unique(missing_rate_q[!is.na(missing_rate_q)]))
  if (length(missing_rate_q)) {
    notes <- c(notes, sprintf(
      "No insured base available for quarter(s): %s → DGS rate set to NA.",
      paste(missing_rate_q, collapse = ", ")
    ))
  }
  
  mismatch_idx <- which(is.finite(recon$diff) & abs(recon$diff) > tol * pmax(1, abs(recon$dgs_expense)))
  if (length(mismatch_idx)) {
    notes <- c(notes, sprintf(
      "Reconciliation variance exceeds tolerance for quarter(s): %s.",
      paste(recon$Quarter[mismatch_idx], collapse = ", ")
    ))
  }
  
  if (nrow(env_rate)) {
    env_check <- dplyr::left_join(
      dplyr::select(recon, "Quarter", "dgs_rate"),
      env_rate,
      by = "Quarter"
    )
    gap_idx <- which(
      is.finite(env_check$dgs_rate) & is.finite(env_check$env_rate) &
        abs(env_check$dgs_rate - env_check$env_rate / 100) > tol * pmax(1, abs(env_check$dgs_rate))
    )
    if (length(gap_idx)) {
      notes <- c(notes, sprintf(
        "Economic environment rate differs materially in quarter(s): %s.",
        paste(env_check$Quarter[gap_idx], collapse = ", ")
      ))
    }
  }
  
  if (nrow(dep_detail)) {
    included_quarters <- sort(unique(dep_detail$Quarter[dep_detail$Product == "Wholesale Deposits"]))
    if (length(included_quarters)) {
      notes <- c(notes, sprintf(
        "Wholesale Deposits included in insured base for quarter(s): %s.",
        paste(included_quarters, collapse = ", ")
      ))
    } else {
      notes <- c(notes, "Wholesale Deposits not found in insured base for analysed quarters.")
    }
    
    missing_by_q <- lapply(split(dep_detail$Product, dep_detail$Quarter), function(products) {
      setdiff(insured_items, unique(products))
    })
    missing_by_q <- missing_by_q[lengths(missing_by_q) > 0 & lengths(missing_by_q) < length(insured_items)]
    if (length(missing_by_q)) {
      msg <- vapply(names(missing_by_q), function(qq) {
        sprintf("%s: %s", qq, paste(missing_by_q[[qq]], collapse = ", "))
      }, character(1))
      notes <- c(notes, sprintf("Missing insured deposit balances for: %s.", paste(msg, collapse = "; ")))
    }
  }
  
  trace <- list(
    step = "dgs",
    formula = "Value = dgs_rate_q",
    inputs = list(
      dgs_expense = dgs_expense,
      insured_base = insured_base,
      env_rate = env_rate,
      value_num = dplyr::transmute(grid, Quarter, Side, Product, Maturity, Component = "dgs", Value_num),
      deposit_detail = dep_detail
    ),
    reconciliation = recon
  )
  if (length(notes)) trace$notes <- notes
  
  pb_helper_make_result(out_tbl, trace = trace, context = context)
}