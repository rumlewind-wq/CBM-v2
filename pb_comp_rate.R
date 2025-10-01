# pb_comp_rate.R
# Input : res = parse_pbreport(..., return_long = TRUE)
# Output: tibble(Quarter, Side{asset|funding}, Product, Maturity, Component="Rate", Value)
# q: "latest" (default), "all", integer n (seneste n), eller c("Q0","Q1",...)

pb_comp_rate <- function(res, q = "latest", include_bonds = TRUE) {
  stopifnot(is.list(res), !is.null(res$all_long))
  al <- res$all_long
  
  # ---- helpers ----
  to_chr <- function(x) if (is.factor(x)) as.character(x) else as.character(x)
  norm_no_maturity <- function(x) {
    y <- to_chr(x)
    y[grepl("^no[- ]?maturity$", y, ignore.case = TRUE)] <- "No-Maturity"
    y
  }
  tplus_to_quarter <- function(lbl) {
    s <- to_chr(lbl)
    m <- stringr::str_match(s, "T\\+(\\d+)")
    ifelse(!is.na(m[,2]), paste0(m[,2], "-quarter"), NA_character_)
  }
  is_wholesale_group <- function(g) {
    g2 <- to_chr(g)
    grepl("^Wholesale Deposits", g2) | grepl("^Woleslae depositis", g2, ignore.case = TRUE)
  }
  is_rate_item <- function(x) {
    grepl("\\(rate\\)\\s*$", to_chr(x), ignore.case = TRUE)
  }
  default_maturity_for <- function(side, product) {
    side_chr <- to_chr(side)
    prod_chr <- to_chr(product)
    combo <- ifelse(is.na(side_chr) | is.na(prod_chr), NA_character_,
                    paste(side_chr, prod_chr, sep = "|"))
    
    combo_defaults <- c(
      "asset|Interbank lending"        = "1-quarter",
      "asset|Fixed-rate Corporate Loans" = "1-quarter",
      "funding|Interbank borrowing"    = "No-Maturity",
      "funding|Discount Window Advances" = "No-Maturity"
    )
    product_defaults <- c(
      "Retail Demand Deposits"   = "No-Maturity",
      "Corporate Demand Deposits" = "No-Maturity",
      "Savings Deposits"         = "No-Maturity"
    )
    
    out <- unname(combo_defaults[combo])
    missing <- is.na(out)
    if (any(missing)) {
      prod_fallback <- unname(product_defaults[prod_chr[missing]])
      out[missing] <- prod_fallback
    }
    out
  }
  
  # ---- types ----
  al$Item           <- to_chr(al$Item)
  al$Quarter        <- to_chr(al$Quarter)
  al$`__section`    <- to_chr(al$`__section`)
  al$`__subsection` <- to_chr(al$`__subsection`)
  al$`__group`      <- to_chr(al$`__group`)
  suppressWarnings(al$Value <- as.numeric(al$Value))
  
  # ---- kvartalsfilter (adaptivt) ----
  q_chr <- al$Quarter
  q_num <- suppressWarnings(as.integer(sub("^Q", "", q_chr)))
  if (is.character(q) && length(q) == 1 && tolower(q) == "latest") {
    newest_q <- paste0("Q", max(q_num, na.rm = TRUE))
    keep <- q_chr == newest_q
  } else if (is.character(q) && length(q) == 1 && tolower(q) == "all") {
    keep <- rep(TRUE, nrow(al))
  } else if (is.numeric(q) && length(q) == 1 && is.finite(q) && q >= 1) {
    uniq <- sort(unique(q_num[!is.na(q_num)]), decreasing = TRUE)
    sel  <- uniq[seq_len(min(q, length(uniq)))]
    keep <- q_num %in% sel
  } else if (is.character(q) && length(q) >= 1) {
    keep <- q_chr %in% q
  } else {
    stop("Unsupported 'q' argument.")
  }
  al <- al[keep, , drop = FALSE]
  
  # --- BANK: (rate) + Interbank Lending (CIBOR) ---
  bank <- al[
    al$`__section` == "BANK BALANCE SHEET" &
      ( is_rate_item(al$Item) | al$Item %in% "Interbank Lending (CIBOR)" ),
    , drop = FALSE
  ]
  
  if (nrow(bank) == 0L) {
    bank_out <- tibble::tibble(Quarter=character(), Side=character(), Product=character(),
                               Maturity=character(), Component=character(), Value=double())
  } else {
    bank$Item <- trimws(bank$Item)
    bank$Side <- dplyr::case_when(
      toupper(bank$`__subsection`) == "ASSETS" ~ "asset",
      toupper(bank$`__subsection`) == "LIABILITIES" ~ "funding",
      TRUE ~ NA_character_
    )
    grp <- trimws(bank$`__group`)
    
    bank$Product <- dplyr::case_when(
      bank$Item == "Interbank Lending (CIBOR)"                    ~ "Interbank lending",
      bank$Item == "Interbank Borrowing (rate)"                   ~ "Interbank borrowing",
      bank$Item == "Discount Window Advances (rate)"              ~ "Discount Window Advances",
      
      grp == "Demand Deposits" & bank$Item == "Retail (rate)"     ~ "Retail Demand Deposits",
      grp == "Demand Deposits" & bank$Item == "Corporate (rate)"  ~ "Corporate Demand Deposits",
      
      grp == "Savings Deposits" & is_rate_item(bank$Item)            ~ "Savings Deposits",
      
      !is.na(grp) &
        grepl("^Business Loans - Fixed[- ]Rate", grp, ignore.case = TRUE) &
        is_rate_item(bank$Item)                                      ~ "Fixed-rate Corporate Loans",
      
      is_rate_item(bank$Item) &
        grepl("^Business Loans - Fixed[- ]Rate", bank$Item, ignore.case = TRUE) ~ "Fixed-rate Corporate Loans",
      
      grp == "Business Loans - Floating-Rate, maturing start of:" &
        is_rate_item(bank$Item)                                      ~ "Floating-rate Corporate Loans",
      
      grp == "Consumer Loans, maturing start of:" &
        is_rate_item(bank$Item)                                      ~ "Consumer Loans",
      
      grp == "Mortgage Loans, maturing start of:" &
        is_rate_item(bank$Item)                                      ~ "Mortgage Loans",
      
      is_wholesale_group(grp) &
        is_rate_item(bank$Item)                                      ~ "Wholesale Deposits",
      
      grp == "Savings Certificates (CDs), maturing start of:" &
        is_rate_item(bank$Item)                                      ~ "Savings Certificates (CDs)",
      
      grp == "Long-term Time Deposits, maturing start of:" &
        is_rate_item(bank$Item)                                      ~ "Long-term Time Deposits",
      
      TRUE ~ NA_character_
    )
    
    bank$Maturity <- default_maturity_for(bank$Side, bank$Product)
    tplus_idx <- grepl("T\\+\\d+", bank$Item)
    if (any(tplus_idx)) {
      bank$Maturity[tplus_idx] <- tplus_to_quarter(bank$Item[tplus_idx])
    }
    bank$Maturity <- norm_no_maturity(bank$Maturity)
    
    bank_out <- bank[!is.na(bank$Product) & !is.na(bank$Side) & !is.na(bank$Value),
                     c("Quarter","Side","Product","Maturity","Value"), drop = FALSE]
    bank_out$Component <- "Rate"
    bank_out <- bank_out[, c("Quarter","Side","Product","Maturity","Component","Value")]
  }
  
  # --- BONDS: Market Yield (T+N) ---
  if (isTRUE(include_bonds)) {
    bonds <- al[
      al$`__section` == "BOND MARKET REPORT" &
        grepl("^Market Yield \\(T\\+\\d+\\)$", al$Item),
      , drop = FALSE
    ]
    if (nrow(bonds)) {
      bonds$Side     <- "asset"
      bonds$Product  <- "Government Bonds"
      bonds$Maturity <- norm_no_maturity(tplus_to_quarter(bonds$Item))
      bonds_out <- bonds[!is.na(bonds$Value),
                         c("Quarter","Side","Product","Maturity","Value"), drop = FALSE]
      bonds_out$Component <- "Rate"
      bonds_out <- bonds_out[, c("Quarter","Side","Product","Maturity","Component","Value")]
    } else {
      bonds_out <- bank_out[0, ]
    }
  } else {
    bonds_out <- bank_out[0, ]
  }
  
  out <- dplyr::bind_rows(bank_out, bonds_out)
  out$Value <- out$Value / 100  # 10.0% -> 0.10
  rownames(out) <- NULL
  out
}