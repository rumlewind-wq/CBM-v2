#  Opgave 1 ----


# What was the impact on retail and corporate demand deposits as a result of 
# decreasing the percent of operating cost to customers 
# (providing cheaper services to customers)?
# Retail demand deposits decreased the most, in percent
# Retail demand deposits increased the most, in percent
#  Corporate demand deposits increased the most, in percent
# Equal impact, in percent

###### ---------- PB helpers (drop-in) ----------
pb_keys_match <- function(idx,
                          section=NULL, subsection=NULL, subsubsection=NULL,
                          group=NULL, entity=NULL) {
  f <- function(col, val) if (is.null(val)) rep(TRUE, nrow(idx)) else trimws(idx[[col]]) == val
  keep <- f("section", section) &
    f("subsection", subsection) &
    f("subsubsection", subsubsection) &
    f("group", group) &
    f("entity", entity)
  unique(idx$key[keep])
}

pb_get_table <- function(res,
                         section=NULL, subsection=NULL, subsubsection=NULL,
                         group=NULL, entity=NULL, quarter=NULL) {
  keys <- pb_keys_match(res$tables_index, section, subsection, subsubsection, group, entity)
  if (!length(keys)) stop("Ingen tabeller matchede de angivne filtre.")
  tbls <- lapply(keys, function(k) {
    t <- res$tables[[k]]
    t$`__table_key` <- k
    t
  })
  out <- dplyr::bind_rows(tbls)
  if (!is.null(quarter)) {
    qs <- setdiff(names(out), c("Item", "__table_key"))
    if (!(quarter %in% qs)) stop("Kvartal findes ikke i tabellen: ", quarter)
    out <- out[, c("Item", "__table_key", quarter), drop=FALSE]
  }
  out
}

pb_find_items <- function(res,
                          section=NULL, subsection=NULL, subsubsection=NULL,
                          group=NULL, entity=NULL, pattern=NULL, fixed=TRUE) {
  tbl <- pb_get_table(res, section, subsection, subsubsection, group, entity)
  items <- unique(tbl$Item)
  if (!is.null(pattern)) items <- items[grepl(pattern, items, fixed=fixed)]
  items
}

pb_get_value <- function(res, item,
                         section=NULL, subsection=NULL, subsubsection=NULL,
                         group=NULL, entity=NULL, quarter) {
  stopifnot(!missing(quarter))
  tbl <- pb_get_table(res, section, subsection, subsubsection, group, entity)
  if (!(quarter %in% names(tbl))) stop("Kvartal findes ikke i tabellen: ", quarter)
  hit <- tbl[tbl$Item == item, c("Item", "__table_key", quarter), drop=FALSE]
  if (!nrow(hit)) stop("Ingen rækker matchede '", item, "'. Prøv pb_find_items() for at lede bredere.")
  hit
}

pb_list_structure <- function(res) {
  idx <- res$tables_index
  idx[is.na(idx)] <- ""
  dplyr::arrange(idx, section, subsection, subsubsection, group, entity) |>
    dplyr::select(section, subsection, subsubsection, group, entity, n_rows)
}

pb_list_sections <- function(res) unique(res$tables_index$section)

pb_quarters <- function(res) unique(res$all_long$Quarter)
##### ---------- end helpers ----------

##### --- små helpers til ændringer/deltaer ---
pb_get_two <- function(res, item, q0="Q0", q1="Q1", ...) {
  v0 <- pb_get_value(res, item=item, quarter=q0, ...)[[q0]]
  v1 <- pb_get_value(res, item=item, quarter=q1, ...)[[q1]]
  tibble::tibble(Item=item, !!q0:=v0, !!q1:=v1, Delta=v1-v0,
                 PctDelta = dplyr::if_else(is.na(v0) | v0==0, NA_real_, (v1-v0)/v0))
}


pb_rows <- function(...) dplyr::bind_rows(list(...))


#### ---- Hent beslutningen (omkostningsprocenten) ----
cost_retail <- pb_get_two(
  res,
  item="Percent of Cost Charged to Retail DD",
  section="DECISIONS", subsection="Other Miscellaneous Decisions"
)

cost_corp <- pb_get_two(
  res,
  item="Percent of Cost Charged to Corporate DD",
  section="DECISIONS", subsection="Other Miscellaneous Decisions"
)

costs <- pb_rows(cost_retail, cost_corp)
costs



#### Effekt på indskud – MARKEDSNIVEAU (Market Average Balance Sheet) ----
mkt_retail <- pb_get_two(
  res, item="Retail", q0="Q0", q1="Q1",
  section="MARKET AVERAGE BALANCE SHEET", subsection="LIABILITIES", group="Demand Deposits"
)

mkt_corp <- pb_get_two(
  res, item="Corporate", q0="Q0", q1="Q1",
  section="MARKET AVERAGE BALANCE SHEET", subsection="LIABILITIES", group="Demand Deposits"
)

market_effect <- pb_rows(mkt_retail, mkt_corp)
market_effect



#### Effekt på indskud – OPSUMMERING (Summary Balance Sheet) -----
sum_retail <- pb_get_two(
  res, item="Retail Demand Deposits",
  section="SUMMARY BALANCE SHEET", subsection="LIABILITIES"
)

sum_corp <- pb_get_two(
  res, item="Corporate Demand Deposits",
  section="SUMMARY BALANCE SHEET", subsection="LIABILITIES"
)

summary_effect <- pb_rows(sum_retail, sum_corp)
summary_effect




#### Mini-rapport (alt samlet) -----
impact_report <- list(
  Decision_Costs = costs,
  Market_Average_DDs = market_effect,
  Summary_Balance_Sheet_DDs = summary_effect
)
impact_report



### Answer ----
# Retail demand deposits increased the most, in percent


# Opgave 2 -----
# Which type of deposit among Wholesale Deposits, Savings Deposits, 
# Savings Certificates (CDs), or Long-term Time Deposits decreased 
# the most as a result?

# Savings Deposits

# Savings Certificates (CDs)

# Long-term Time Deposits

# Wholesale Deposits

############### -
pb_get_table(res,
             section="SUMMARY BALANCE SHEET",
             subsection="LIABILITIES",
             quarter=NULL
) |>
  dplyr::filter(Item %in% c("Wholesale Deposits",
                            "Savings Deposits",
                            "Savings Certificates (CDs)",
                            "Long-term Time Deposits"))


### Answer ----

# Opgave 3 -----

# Long-term Time Deposits decreased

# as a result of 22,296.18 in deposits maturing and receiving 16,866.03 in new deposits

# as a result of 16,866.03 in deposits maturing and receiving 22,296.18 in new deposits

# as a result of 16,866.03 in deposits maturing and receiving 23,133.43 in new deposits

# as a result of 23,133.43 in deposits maturing and receiving 16,866.03 in new deposits



############### -



### Answer ----

# Opgave 4 -----

# The price per share of DKK 92.17 in quarter 1 is determined by market value of equity divided by the number of outstanding shares.


# an increase in market value and an increase in share price

# a decrease in market vallue and an increase in share price

# an increase in market value and a decrease in share price

# a decrease in market value and a decrease in share price

############### -




### Answer ----

# Opgave 5 -----

# Total Wholesale Deposits increased from 132,000 to 174,000 because


# 75,000 matured and we added 33,000 in new Wholesale Deposits

# 33,000 matured and we added 75,000 in new Wholesale Deposits

# 33,000 matured and we added 50,000 in new Wholesale Deposits

# 50,000 matured and we added 33,000 in new Wholesale Deposits

############### -



### Answer ----

