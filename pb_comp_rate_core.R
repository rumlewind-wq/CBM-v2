# pb_comp_rate_core.R
# Refactored rate component calculator using pb_helpers_core.R utilities.

pb_comp_rate_core <- function(res, q = "latest", include_bonds = TRUE,
                              context = "pb_comp_rate_core") {
  pb_helper_require_packages(c("dplyr", "stringr", "tibble"), context)
  
  al <- pb_helper_prepare_all_long(res, context = context)
  
  quarters_all <- pb_helper_quarter_levels(al)
  if (!length(quarters_all)) {
    stop(sprintf("%s: Ingen kvartaler fundet i res$all_long.", context), call. = FALSE)
  }
  
  q_param <- q
  if (is.numeric(q) && length(q) == 1 && is.finite(q) && q >= 1) {
    count <- min(as.integer(q), length(quarters_all))
    q_param <- tail(quarters_all, count)
  } else if (is.character(q) && length(q) == 1) {
    q_lower <- tolower(q)
    if (q_lower == "latest") {
      q_param <- tail(quarters_all, 1)
    } else if (q_lower == "all") {
      q_param <- quarters_all
    }
  }
  
  quarter_info <- pb_helper_filter_quarters(
    al,
    q = q_param,
    context = sprintf("%s: Kvartalsfilter", context)
  )
  al_q <- quarter_info$data
  
  is_rate_item <- function(items) {
    lbl <- pb_helper_chr(items)
    grepl("\\(rate\\)\\s*$", lbl, ignore.case = TRUE)
  }
  
  is_wholesale_group <- function(groups) {
    grp <- pb_helper_chr(groups)
    grepl("^Wholesale Deposits", grp) |
      grepl("^Woleslae depositis", grp, ignore.case = TRUE)
  }
  
  tplus_to_quarter <- function(labels) {
    lbl <- pb_helper_chr(labels)
    matched <- stringr::str_match(lbl, "T\\+(\\d+)")
    out <- ifelse(!is.na(matched[, 2]), paste0(matched[, 2], "-quarter"), NA_character_)
    out
  }
  
  norm_no_maturity <- function(values) {
    val <- pb_helper_chr(values)
    idx <- grepl("^no[- ]?maturity$", val, ignore.case = TRUE)
    val[idx] <- "No-Maturity"
    val
  }
  
  default_maturity_for <- function(side, product) {
    side_chr <- pb_helper_chr(side)
    prod_chr <- pb_helper_chr(product)
    combo <- ifelse(
      is.na(side_chr) | is.na(prod_chr),
      NA_character_,
      paste(side_chr, prod_chr, sep = "|")
    )
    
    combo_defaults <- c(
      "asset|Interbank lending" = "1-quarter",
      "asset|Fixed-rate Corporate Loans" = "1-quarter",
      "funding|Interbank borrowing" = "No-Maturity",
      "funding|Discount Window Advances" = "No-Maturity"
    )
    product_defaults <- c(
      "Retail Demand Deposits" = "No-Maturity",
      "Corporate Demand Deposits" = "No-Maturity",
      "Savings Deposits" = "No-Maturity"
    )
    
    out <- unname(combo_defaults[combo])
    missing <- is.na(out)
    if (any(missing)) {
      out[missing] <- unname(product_defaults[prod_chr[missing]])
    }
    out
  }
  
  build_bank_rates <- function(df) {
    if (!nrow(df)) {
      return(pb_helper_empty_result())
    }
    
    item_chr <- trimws(pb_helper_chr(df$Item))
    is_rate <- is_rate_item(item_chr)
    is_cibor <- item_chr == "Interbank Lending (CIBOR)"
    keep <- (is_rate | is_cibor) & !is.na(df$Value)
    if (!any(keep)) {
      return(pb_helper_empty_result())
    }
    
    bank <- df[keep, , drop = FALSE]
    item_chr <- trimws(pb_helper_chr(bank$Item))
    grp_chr <- trimws(pb_helper_chr(bank$`__group`))
    sub_chr <- pb_helper_chr(bank$`__subsection`)
    
    side <- ifelse(
      toupper(sub_chr) == "ASSETS", "asset",
      ifelse(toupper(sub_chr) == "LIABILITIES", "funding", NA_character_)
    )
    
    product <- rep(NA_character_, length(item_chr))
    product[item_chr == "Interbank Lending (CIBOR)"] <- "Interbank lending"
    product[item_chr == "Interbank Borrowing (rate)"] <- "Interbank borrowing"
    product[item_chr == "Discount Window Advances (rate)"] <- "Discount Window Advances"
    
    dd_idx <- grp_chr == "Demand Deposits"
    product[dd_idx & item_chr == "Retail (rate)"] <- "Retail Demand Deposits"
    product[dd_idx & item_chr == "Corporate (rate)"] <- "Corporate Demand Deposits"
    
    product[grp_chr == "Savings Deposits" & is_rate_item(item_chr)] <- "Savings Deposits"
    
    fixed_grp <- grepl("^Business Loans - Fixed[- ]Rate", grp_chr, ignore.case = TRUE)
    product[fixed_grp & is_rate_item(item_chr)] <- "Fixed-rate Corporate Loans"
    product[is_rate_item(item_chr) &
              grepl("^Business Loans - Fixed[- ]Rate", item_chr, ignore.case = TRUE)] <- "Fixed-rate Corporate Loans"
    
    product[grp_chr == "Business Loans - Floating-Rate, maturing start of:" & is_rate_item(item_chr)] <-
      "Floating-rate Corporate Loans"
    product[grp_chr == "Consumer Loans, maturing start of:" & is_rate_item(item_chr)] <- "Consumer Loans"
    product[grp_chr == "Mortgage Loans, maturing start of:" & is_rate_item(item_chr)] <- "Mortgage Loans"
    
    product[is_wholesale_group(grp_chr) & is_rate_item(item_chr)] <- "Wholesale Deposits"
    
    product[grp_chr == "Savings Certificates (CDs), maturing start of:" & is_rate_item(item_chr)] <-
      "Savings Certificates (CDs)"
    product[grp_chr == "Long-term Time Deposits, maturing start of:" & is_rate_item(item_chr)] <-
      "Long-term Time Deposits"
    
    maturity <- default_maturity_for(side, product)
    tplus_idx <- grepl("T\\+\\d+", item_chr)
    if (any(tplus_idx)) {
      maturity[tplus_idx] <- tplus_to_quarter(item_chr[tplus_idx])
    }
    maturity <- norm_no_maturity(maturity)
    
    valid <- !is.na(product) & !is.na(side) & !is.na(bank$Value)
    if (!any(valid)) {
      return(pb_helper_empty_result())
    }
    
    tibble::tibble(
      Quarter = pb_helper_chr(bank$Quarter[valid]),
      Side = side[valid],
      Product = product[valid],
      Maturity = maturity[valid],
      Component = "Rate",
      Value = bank$Value[valid]
    )
  }
  
  build_bond_rates <- function(df) {
    if (!nrow(df) || !isTRUE(include_bonds)) {
      return(pb_helper_empty_result())
    }
    item_chr <- pb_helper_chr(df$Item)
    keep <- grepl("^Market Yield \\(T\\+\\d+\\)$", item_chr) & !is.na(df$Value)
    if (!any(keep)) {
      return(pb_helper_empty_result())
    }
    bonds <- df[keep, , drop = FALSE]
    maturity <- norm_no_maturity(tplus_to_quarter(bonds$Item))
    
    tibble::tibble(
      Quarter = pb_helper_chr(bonds$Quarter),
      Side = rep("asset", nrow(bonds)),
      Product = rep("Government Bonds", nrow(bonds)),
      Maturity = maturity,
      Component = "Rate",
      Value = bonds$Value
    )
  }
  
  bank_rows <- build_bank_rates(
    pb_helper_filter_all_long(al_q, section = "BANK BALANCE SHEET")
  )
  bond_rows <- build_bond_rates(
    pb_helper_filter_all_long(al_q, section = "BOND MARKET REPORT")
  )
  
  combined <- dplyr::bind_rows(bank_rows, bond_rows)
  if (nrow(combined)) {
    combined$Value <- combined$Value / 100
  }
  
  trace <- list(
    quarters = quarter_info$quarters,
    quarters_all = quarter_info$quarters_all,
    include_bonds = include_bonds
  )
  
  pb_helper_make_result(
    if (nrow(combined)) combined else pb_helper_empty_result(),
    trace = trace,
    context = sprintf("%s: Resultatopbygning", context)
  )
}