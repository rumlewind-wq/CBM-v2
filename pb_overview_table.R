pb_overview_table <- function(res, side = "both", q = "latest", as_percent = FALSE) {
  required_pkgs <- c("dplyr", "tidyr", "tibble", "stringr")
  missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing_pkgs) > 0) {
    stop(sprintf("Følgende pakker mangler: %s", paste(missing_pkgs, collapse = ", ")))
  }
  if (!("all_long" %in% names(res)) || is.null(res$all_long)) {
    stop("res$all_long mangler. Sørg for at parseren er kørt korrekt.")
  }
  side_key <- stringr::str_to_lower(trimws(side))
  if (!side_key %in% c("both", "asset", "funding")) {
    stop("Parameteren 'side' skal være 'both', 'asset' eller 'funding'.")
  }
  empty_component <- tibble::tibble(Quarter = character(), Side = character(), Product = character(), Maturity = character(), Component = character(), Value = numeric())
  empty_final <- tibble::tibble(Quarter = character(), Type = character(), objekt = character(), maturity = character(), `Nom Rate` = numeric(), `Oper Cost` = numeric(), Adv = numeric(), Fee = numeric(), DGS = numeric(), Total = numeric(), `RR (%)` = numeric(), Default = numeric(), `NetRet Assets` = numeric(), `TotalCost Funds` = numeric())
  canon_text <- function(x) {
    out <- as.character(x)
    out[is.na(x)] <- NA_character_
    out <- trimws(out)
    out <- stringr::str_replace_all(out, "\\s+", " ")
    out
  }
  norm_lower <- function(x) stringr::str_to_lower(canon_text(x))
  norm_maturity <- function(x) {
    y <- norm_lower(x)
    y <- stringr::str_replace_all(y, "[()]", "")
    y <- stringr::str_replace_all(y, "\\s+/\\s+", "/")
    y <- stringr::str_replace_all(y, "\\s*-\\s*", "-")
    y <- stringr::str_replace_all(y, "\\s+", " ")
    y <- trimws(y)
    y <- stringr::str_replace_all(y, "^no\\s*-?\\s*maturity$", "no-maturity")
    y <- stringr::str_replace_all(y, "^t\\s*\\+\\s*(\\d+)$", "\\1-quarter")
    y <- stringr::str_replace_all(y, "^(\\d+)\\s*quarter$", "\\1-quarter")
    y <- stringr::str_replace_all(y, "^(\\d+)\\s*-\\s*quarter$", "\\1-quarter")
    y[y == ""] <- NA_character_
    y
  }
  
  format_maturity <- function(x) {
    out <- x
    out[is.na(out)] <- NA_character_
    idx_nm <- !is.na(out) & out == "no-maturity"
    out[idx_nm] <- "No-Maturity"
    mask <- !is.na(out) & out != "No-Maturity" & !stringr::str_detect(out, "^[0-9]+-quarter$")
    out[mask] <- stringr::str_to_title(out[mask])
    out
  }
  
  product_alias <- c(
    "mortgage loans" = "Mortgage Loans",
    "mortgages" = "Mortgage Loans",
    "interbank lending" = "Interbank Lending",
    "interbank lending (at cibor)" = "Interbank Lending",
    "interbank borrowing" = "Interbank Borrowing",
    "discount window advances" = "Discount Window Advances",
    "discount window advance" = "Discount Window Advances",
    "business loans - floating-rate" = "Business Loans - Floating-Rate",
    "business loans - fixed-rate" = "Business Loans - Fixed-Rate",
    "savings certificates (cds)" = "Savings Certificates (CDs)"
  )
  canon_product <- function(x) {
    key <- norm_lower(x)
    key <- stringr::str_replace(key, "(?i)\\s*\\(at cibor\\)$", "")
    key <- canon_text(key)
    mapped <- product_alias[key]
    fallback_idx <- is.na(mapped) | mapped == ""
    fallback <- canon_text(x)
    fallback <- stringr::str_replace(fallback, "(?i)\\s*\\(at cibor\\)$", "")
    mapped[fallback_idx] <- stringr::str_to_title(fallback[fallback_idx])
    mapped <- canon_text(mapped)
    mapped[mapped == ""] <- NA_character_
    mapped
  }
  canon_side <- function(x) {
    y <- norm_lower(x)
    dplyr::case_when(
      y %in% c("asset", "assets") ~ "asset",
      y %in% c("funding", "liability", "liabilities") ~ "funding",
      TRUE ~ y
    )
  }
  parse_value <- function(x) {
    if (is.null(x)) {
      return(numeric())
    }
    raw <- canon_text(x)
    raw[norm_lower(raw) %in% c("na", "nul", "null", "none", "")] <- NA_character_
    raw[raw == ""] <- NA_character_
    raw <- stringr::str_replace_all(raw, "%", "")
    raw <- stringr::str_replace_all(raw, "\\$", "")
    if (requireNamespace("readr", quietly = TRUE)) {
      readr::parse_number(raw, locale = readr::locale(decimal_mark = ".", grouping_mark = ","), na = c("", "na", "nul", "null"))
    } else {
      cleaned <- gsub(",", "", raw, fixed = TRUE)
      cleaned <- gsub("[^0-9\\-\\.]+", "", cleaned)
      out <- suppressWarnings(as.numeric(cleaned))
      out
    }
  }
  dedup_value <- function(v) {
    vals <- v[!is.na(v)]
    if (length(vals) == 0) {
      return(NA_real_)
    }
    uniq <- unique(vals)
    if (length(uniq) == 1) {
      return(uniq)
    }
    sel <- uniq[which.max(abs(uniq))]
    warning(sprintf("Dedup conflict: picking %.6f among %s", sel, paste(signif(uniq, 6), collapse = ", ")), call. = FALSE)
    sel
  }
  get_component <- function(fetch, component, side_filter = NULL) {
    tbl <- tryCatch(fetch(), error = function(e) NULL)
    if (is.null(tbl) || nrow(tbl) == 0) {
      return(empty_component)
    }
    required_cols <- c("Quarter", "Side", "Product", "Maturity", "Value")
    if (!all(required_cols %in% names(tbl))) {
      return(empty_component)
    }
    out <- dplyr::transmute(tbl, Quarter = as.character(Quarter), Side = as.character(Side), Product = as.character(Product), Maturity = as.character(Maturity), Component = component, Value = parse_value(Value))
    if (!is.null(side_filter)) {
      out <- dplyr::filter(out, canon_side(Side) %in% side_filter)
    }
    if (nrow(out) == 0) {
      return(empty_component)
    }
    out
  }
  components <- list(
    get_component(function() pb_comp_rate(res, q), "rate", NULL),
    get_component(function() pb_comp_oper2(res), "oper", NULL),
    get_component(function() pb_comp_adv_yaml(res), "adv", NULL),
    get_component(function() pb_fee_income(res), "fee", "funding"),
    get_component(function() pb_comp_dgs_premium(res), "dgs", "funding"),
    get_component(function() pb_comp_rr(res), "rr", "funding"),
    get_component(function() pb_asset_default_rates(res), "default", "asset")
  )
  combined <- dplyr::bind_rows(components)
  if (nrow(combined) == 0) {
    return(empty_final)
  }
  combined <- dplyr::mutate(combined, Side = canon_side(Side), Product = canon_product(Product), Maturity = norm_maturity(Maturity))
  combined <- dplyr::filter(combined, Side %in% c("asset", "funding"))
  if (side_key == "asset") {
    combined <- dplyr::filter(combined, Side == "asset")
  } else if (side_key == "funding") {
    combined <- dplyr::filter(combined, Side == "funding")
  }
  if (nrow(combined) == 0) {
    return(empty_final)
  }
  combined <- dplyr::group_by(combined, Quarter, Side, Product, Maturity, Component)
  combined <- dplyr::summarise(combined, Value = dedup_value(Value), .groups = "drop")
  wide <- tidyr::pivot_wider(combined, names_from = Component, values_from = Value)
  if (nrow(wide) > 0 && any(duplicated(wide[c("Quarter", "Side", "Product", "Maturity")]))) {
    stop("Deduplikeringsfejl: duplikerede nøgler i oversigten")
  }
  for (nm in c("rate", "oper", "adv", "fee", "dgs", "rr", "default")) {
    if (!nm %in% names(wide)) {
      wide[[nm]] <- NA_real_
    }
  }
  wide <- dplyr::mutate(wide, rate = as.numeric(rate), oper = as.numeric(oper), adv = as.numeric(adv), fee = as.numeric(fee), dgs = as.numeric(dgs), rr = as.numeric(rr), default = as.numeric(default), Type = dplyr::if_else(Side == "asset", "Assets", "Funding"))
  wide <- dplyr::mutate(wide, Total = dplyr::if_else(Side == "funding", dplyr::coalesce(rate, 0) + dplyr::coalesce(oper, 0) + dplyr::coalesce(adv, 0) - dplyr::coalesce(fee, 0) + dplyr::coalesce(dgs, 0), NA_real_), NetReturnAssets = dplyr::if_else(Side == "asset", dplyr::coalesce(rate, 0) - dplyr::coalesce(oper, 0) - dplyr::coalesce(adv, 0) - dplyr::coalesce(default, 0), NA_real_), rr_eff = dplyr::coalesce(rr, 0))
  wide <- dplyr::mutate(wide, TotalCostFunds = dplyr::case_when(Side != "funding" ~ NA_real_, !is.finite(rr_eff) ~ NA_real_, rr_eff >= 1 ~ NA_real_, (1 - rr_eff) == 0 ~ NA_real_, TRUE ~ Total/(1 - rr_eff)))
  result <- dplyr::transmute(wide, Quarter, Type, objekt = Product, maturity = format_maturity(Maturity), `Nom Rate` = rate, `Oper Cost` = oper, Adv = adv, Fee = fee, DGS = dgs, Total, `RR (%)` = rr, Default = default, `NetRet Assets` = NetReturnAssets, `TotalCost Funds` = TotalCostFunds)
  if (nrow(result) == 0) {
    return(empty_final)
  }
  metric_cols <- c("Nom Rate", "Oper Cost", "Adv", "Fee", "DGS", "Total", "RR (%)", "Default", "NetRet Assets", "TotalCost Funds")
  result <- dplyr::filter(result, dplyr::if_any(dplyr::all_of(metric_cols), ~ !is.na(.x)))
  if (nrow(result) == 0) {
    return(empty_final)
  }
  if (as_percent) {
    pct_cols <- intersect(metric_cols, names(result))
    result <- dplyr::mutate(result, dplyr::across(dplyr::all_of(pct_cols), ~ .x * 100))
  }
  result <- dplyr::arrange(result, Quarter, Type, objekt, maturity)
  tibble::as_tibble(result)
}