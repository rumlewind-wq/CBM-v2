# pb_capitalization_report_core.R
# Refactored capitalization report calculator using pb_helpers_core utilities.

pb_capitalization_require <- function(context = "pb_capitalization_flex") {
  pb_helper_require_packages(c("dplyr", "stringr", "tibble", "tidyr"), context)
  invisible(TRUE)
}

pb_capitalization_latest_quarters <- function(res, context = "pb_capitalization_flex") {
  qs <- pb_quarters(res, context = sprintf("%s::pb_quarters", context))
  if (length(qs) < 2L) {
    stop(sprintf("%s: Der findes kun ét kvartal i data.", context), call. = FALSE)
  }
  list(q0 = qs[[length(qs) - 1L]], q1 = qs[[length(qs)]])
}

pb_capitalization_load_table <- function(res, q0, q1, keep_prior = FALSE,
                                         context = "pb_capitalization_flex") {
  cap <- pb_get_table(
    res,
    section = "CAPITALIZATION REPORT",
    context = sprintf("%s::pb_get_table", context)
  )
  cap <- tibble::as_tibble(cap)
  
  if (!"Item" %in% names(cap)) {
    stop(sprintf("%s: CAP-data mangler kolonnen 'Item'.", context), call. = FALSE)
  }
  
  if (!isTRUE(keep_prior)) {
    cap <- dplyr::filter(cap, !stringr::str_detect(pb_helper_chr(Item), "Prior quarter"))
  }
  
  cols <- c("Item", q0, q1)
  cols <- cols[cols %in% names(cap)]
  if (!all(c(q0, q1) %in% cols)) {
    stop(sprintf("%s: Q0/Q1 kolonner mangler i CAP-data.", context), call. = FALSE)
  }
  
  cap %>%
    dplyr::select(dplyr::all_of(cols)) %>%
    dplyr::rename(Q0 = !!q0, Q1 = !!q1) %>%
    dplyr::mutate(
      Q0 = suppressWarnings(as.numeric(Q0)),
      Q1 = suppressWarnings(as.numeric(Q1)),
      Delta = Q1 - Q0,
      Pct = dplyr::if_else(Q0 == 0 | is.na(Q0), NA_real_, (Q1 - Q0) / Q0 * 100),
      Alpha = dplyr::case_when(
        is.na(Delta) ~ "",
        Delta > 0 ~ "↑",
        Delta < 0 ~ "↓",
        TRUE ~ ""
      ),
      Base = Item |>
        stringr::str_replace(":?\\s*Current quarter$", "") |>
        stringr::str_replace(":?\\s*Prior quarter$", "")
    )
}

pb_capitalization_add_category <- function(df) {
  df %>%
    dplyr::mutate(
      Category = dplyr::case_when(
        Base %in% c(
          "Price Per Share",
          "Market Value of Equity",
          "Book Value of Equity"
        ) ~ "Equity Market",
        stringr::str_detect(
          Base,
          "Number of Shares Sold|Price of Shares Sold|Shares Sold \\(total\\)|Avg Sale Price|Number of Shares Outstanding"
        ) ~ "Shares & Management",
        Base %in% c("Net Total Assets", "Risk-weighted Assets") ~ "Balance",
        Base %in% c("Basel Tier I Ratio", "Basel Tier I+II Ratio", "Capital/Asset Ratio") ~ "Capital Ratios",
        TRUE ~ "Other"
      )
    )
}

pb_capitalization_apply_materiality <- function(df, pct_thresh = 1, delta_thresh = 1000) {
  dplyr::filter(df, abs(Pct) >= pct_thresh | abs(Delta) >= delta_thresh)
}

pb_capitalization_aggregate_sold <- function(df) {
  num_m <- dplyr::filter(df, stringr::str_detect(Base, "^Number of Shares Sold by Management$"))
  num_r <- dplyr::filter(df, stringr::str_detect(Base, "^Number of Shares Sold by Regulators$"))
  prc_m <- dplyr::filter(df, stringr::str_detect(Base, "^Price of Shares Sold by Management$"))
  prc_r <- dplyr::filter(df, stringr::str_detect(Base, "^Price of Shares Sold by Regulators$"))
  
  if (nrow(num_m) + nrow(num_r) + nrow(prc_m) + nrow(prc_r) == 0) {
    return(df)
  }
  
  getv <- function(tbl, col) {
    if (nrow(tbl)) {
      suppressWarnings(as.numeric(tbl[[col]]))
    } else {
      0
    }
  }
  
  Q0_num_m <- getv(num_m, "Q0"); Q1_num_m <- getv(num_m, "Q1")
  Q0_num_r <- getv(num_r, "Q0"); Q1_num_r <- getv(num_r, "Q1")
  Q0_prc_m <- getv(prc_m, "Q0"); Q1_prc_m <- getv(prc_m, "Q1")
  Q0_prc_r <- getv(prc_r, "Q0"); Q1_prc_r <- getv(prc_r, "Q1")
  
  Q0_num_tot <- Q0_num_m + Q0_num_r
  Q1_num_tot <- Q1_num_m + Q1_num_r
  
  total_num <- tibble::tibble(
    Item = "Shares Sold (total)",
    Base = "Shares Sold (total)",
    Q0 = Q0_num_tot,
    Q1 = Q1_num_tot,
    Delta = Q1_num_tot - Q0_num_tot,
    Pct = dplyr::if_else(Q0_num_tot == 0, NA_real_, (Q1_num_tot - Q0_num_tot) / Q0_num_tot * 100),
    Alpha = dplyr::case_when(
      is.na(Q1 - Q0) ~ "",
      (Q1 - Q0) > 0 ~ "↑",
      (Q1 - Q0) < 0 ~ "↓",
      TRUE ~ ""
    ),
    Category = "Shares & Management"
  )
  
  wavg <- function(n_m, p_m, n_r, p_r) {
    denom <- n_m + n_r
    if (denom <= 0) {
      NA_real_
    } else {
      (n_m * p_m + n_r * p_r) / denom
    }
  }
  
  Q0_avg <- wavg(Q0_num_m, Q0_prc_m, Q0_num_r, Q0_prc_r)
  Q1_avg <- wavg(Q1_num_m, Q1_prc_m, Q1_num_r, Q1_prc_r)
  
  avg_price <- tibble::tibble(
    Item = "Avg Sale Price (weighted)",
    Base = "Avg Sale Price (weighted)",
    Q0 = Q0_avg,
    Q1 = Q1_avg,
    Delta = ifelse(is.na(Q0_avg) | is.na(Q1_avg), NA_real_, Q1_avg - Q0_avg),
    Pct = ifelse(is.na(Q0_avg) | Q0_avg == 0, NA_real_, (Q1_avg - Q0_avg) / Q0_avg * 100),
    Alpha = dplyr::case_when(
      is.na(Delta) ~ "",
      Delta > 0 ~ "↑",
      Delta < 0 ~ "↓",
      TRUE ~ ""
    ),
    Category = "Shares & Management"
  )
  
  dplyr::bind_rows(df, total_num, avg_price)
}

pb_capitalization_decompose_market_value <- function(df) {
  mv <- dplyr::filter(df, Base == "Market Value of Equity")
  price <- dplyr::filter(df, Base == "Price Per Share")
  shares <- dplyr::filter(df, Base == "Number of Shares Outstanding")
  
  if (
    nrow(mv) == 1 &&
    nrow(price) == 1 &&
    nrow(shares) == 1 &&
    all(c("Q0", "Q1") %in% names(price)) &&
    all(c("Q0", "Q1") %in% names(shares))
  ) {
    P0 <- price$Q0; P1 <- price$Q1
    Q0 <- shares$Q0; Q1 <- shares$Q1
    price_effect <- (P1 - P0) * Q0
    quantity_effect <- (Q1 - Q0) * P0
    
    tibble::tibble(
      Driver = c("Price Effect", "Quantity Effect"),
      Value = c(price_effect, quantity_effect),
      Alpha = c(
        ifelse(price_effect >= 0, "↑", "↓"),
        ifelse(quantity_effect >= 0, "↑", "↓")
      )
    )
  } else {
    tibble::tibble(Driver = character(), Value = numeric(), Alpha = character())
  }
}

pb_capitalization_flex <- function(res,
                                   mode = c("full", "changes", "kpi", "top5", "material", "signals"),
                                   top_n = 5,
                                   materiality = list(pct = 1, delta = 1000),
                                   signals = list(price_drop = -2, tier1 = 10, cap_asset = 9),
                                   keep_prior = FALSE) {
  context <- "pb_capitalization_flex"
  pb_capitalization_require(context)
  
  quarters <- pb_capitalization_latest_quarters(res, context)
  q0 <- quarters$q0
  q1 <- quarters$q1
  
  base <- pb_capitalization_load_table(res, q0, q1, keep_prior, context) %>%
    pb_capitalization_add_category() %>%
    pb_capitalization_aggregate_sold()
  
  mode <- match.arg(mode)
  out <- base
  
  if (mode == "changes") {
    out <- dplyr::select(out, Category, Item, Base, Delta, Pct, Alpha)
  }
  
  if (mode == "kpi") {
    out <- dplyr::filter(
      out,
      Base %in% c(
        "Price Per Share",
        "Market Value of Equity",
        "Number of Shares Outstanding",
        "Basel Tier I Ratio"
      )
    )
  }
  
  if (mode == "top5") {
    out <- out %>% dplyr::arrange(dplyr::desc(abs(Delta))) %>% dplyr::slice_head(n = top_n)
  }
  
  if (mode == "material") {
    out <- pb_capitalization_apply_materiality(out, materiality$pct %||% 1, materiality$delta %||% 1000)
  }
  
  if (mode == "signals") {
    out <- dplyr::filter(
      out,
      (Base == "Price Per Share" & Pct <= (signals$price_drop %||% -2)) |
        (Base == "Basel Tier I Ratio" & Q1 < (signals$tier1 %||% 10)) |
        (Base == "Capital/Asset Ratio" & Q1 < (signals$cap_asset %||% 9))
    )
  }
  
  mv_driver <- pb_capitalization_decompose_market_value(base)
  
  list(
    table = out,
    market_value_drivers = mv_driver,
    quarters = list(q0 = q0, q1 = q1)
  )
}

# Eksempler:
# pb_capitalization_flex(res, mode = "full")$table
# pb_capitalization_flex(res, mode = "changes")$table
# pb_capitalization_flex(res, mode = "kpi")$table
# pb_capitalization_flex(res, mode = "top5", top_n = 5)$table
# pb_capitalization_flex(res, mode = "material", materiality = list(pct = 2, delta = 5000))$table
# pb_capitalization_flex(res, mode = "signals")$table
# pb_capitalization_flex(res, keep_prior = TRUE)$table