# ================================-
# 1) DEMAND DEPOSITS – IMPACT----
# ================================-
# Parser: pb_dd_impact.R
source("pb_dd_impact.R") 

# Market Average (seneste to kvartaler)
ans_m <- pb_dd_impact(res, level="market")
ans_m$table


# Summary Balance Sheet (seneste to kvartaler)
ans_s <- pb_dd_impact(res, level="summary")
ans_s$table


# Evt. brug specifikke kvartaler
# pb_dd_impact(res, level="market", q_from="Q0", q_to="Q1")


# ========================================== - - 
# 2) DEPOSITS – HVILKEN FALDER MEST? ----
# ========================================== -
# Parser: pb_deposits_decrease_most.R
source("pb_deposits_decrease_most.R")

# Standardkørsel: seneste to kvartaler
pb_deposits_decrease_most_latest()

# Vælg specifikke kvartaler manuelt
pb_deposits_decrease_custom(q_from = "Q1", q_to = "Q2")


# Kun pct-ændringer, hurtigt overblik
pb_deposits_pct_changes()


# ==================================-
# 3) CAPITALIZATION REPORT – FLEX----
# ==================================-
# Parser: pb_capitalization_report.R
source("pb_capitalization_report.R")

# Fuld visning (Q0/Q1/Delta/Pct/↑↓)
pb_capitalization_flex(res, mode = "full")$table

# Kun ændringer (Delta/Pct/↑↓)
pb_capitalization_flex(res, mode = "changes")$table

# KPI-kort (Pris, MV, Aktier, Tier I)
pb_capitalization_flex(res, mode = "kpi")$table

# Top-5 største bevægelser (absolut Delta)
pb_capitalization_flex(res, mode = "top5", top_n = 5)$table

# Materiality-filter
pb_capitalization_flex(res, mode = "material", materiality = list(pct = 2, delta = 5000))$table

# Signaler (prisfald, lave kapitalrater)
pb_capitalization_flex(res, mode = "signals")$table

# Inkludér “Prior quarter”-linjer i grundlaget
pb_capitalization_flex(res, keep_prior = TRUE)$table


# ========================================== -
# 4) FLOW-PARSERE – T+N (BANK & MARKET)----
# ========================================== -
# Parsere: pb_lttd_flow.R + pb_flow_generic.R
source("pb_lttd_flow.R")
source("pb_flow_generic.R")

# --- BANK BALANCE SHEET (seneste to kvartaler) ---
pb_lttd_flow_latest()          # Long-term Time Deposits (T+N)
pb_wholesale_flow_latest()     # Wholesale Deposits (T+N)
pb_cds_flow_latest()           # Savings Certificates (CDs) (T+N)
pb_blfloating_flow_latest()    # Business Loans - Floating-Rate (T+N)
pb_consumer_flow_latest()      # Consumer Loans (T+N)
pb_mortgage_flow_latest()      # Mortgage Loans (T+N)
pb_govbonds_flow_latest()      # Government Bonds (T+N)

# --- BANK BALANCE SHEET (vælg kvartaler) ---
pb_lttd_flow_custom(q_from = "Q0", q_to = "Q1")
pb_wholesale_flow_custom(q_from = "Q0", q_to = "Q1")
pb_cds_flow_custom(q_from = "Q0", q_to = "Q1")
pb_blfloating_flow_custom(q_from = "Q0", q_to = "Q1")
pb_consumer_flow_custom(q_from = "Q0", q_to = "Q1")
pb_mortgage_flow_custom(q_from = "Q0", q_to = "Q1")
pb_govbonds_flow_custom(q_from = "Q0", q_to = "Q1")

# --- MARKET AVERAGE BALANCE SHEET (seneste to kvartaler) ---
pb_lttd_flow_market_latest()
pb_wholesale_flow_market_latest()
pb_cds_flow_market_latest()
pb_blfloating_flow_market_latest()
pb_consumer_flow_market_latest()
pb_mortgage_flow_market_latest()
pb_govbonds_flow_market_latest()


# =======================================================-
# 5) LIABILITIES – AMOUNTS & RATES (print-venlige calls)----
# =======================================================-
# Parser: pb_liabilities_amounts_and_rates.R
source("pb_liabilities_amounts_and_rates.R")

# Én linje pr. visning (printer)
pb_liab_amounts()   # amounts
pb_liab_rates()     # rates
pb_liab_both()      # begge tabeller

# Alternativt: få objekterne (tibbles) tilbage
a <- pb_liabilities_amounts(); a   # amounts tibble
r <- pb_liabilities_rates();   r   # rates tibble
b <- pb_liabilities_all();     b   # list(amounts=..., rates=...)


# =======================================================-
# All-in costs and Net returns ----
# =======================================================-

# =======================================================-
# 6) Costs parser (operating + advertising) – calls
# =======================================================-

# =======================================================-
# 7) Funding costs parser (all-in for deposits)----
# =======================================================-


source("pb_comp_rate.R")
pb_comp_rate(res)|> dplyr::arrange(Side, Product, Maturity) |> print(n = 200) # Seneste kvartal


# DONE
source("pb_comp_dgs_premium.R")

# Standard: seneste kvartal
pb_comp_dgs_premium(res) |> dplyr::arrange(Side, Product, Maturity) |> print(n = 200) # Seneste N kvartaler
pb_comp_dgs_premium(res, q = 3) |> dplyr::arrange(Quarter, Product) |> print(n = 200)

# Alle kvartaler
pb_comp_dgs_premium(res, q = "all") |> dplyr::arrange(Quarter, Product) |> print(n = 200)

# Specifikke kvartaler
pb_comp_dgs_premium(res, q = c("Q0","Q1")) |> dplyr::arrange(Quarter, Product) |> print(n = 200)

# Kun ikke-nul værdier
pb_comp_dgs_premium(res, q = "all") |> dplyr::filter(Value > 0) |> dplyr::arrange(Quarter, Product) |> print(n = 200)


# Materialitet/threshold filter
pb_comp_dgs_premium(res, q = "all") |>
  dplyr::filter(abs(Value) >= 0.0005) |>
  dplyr::arrange(dplyr::desc(Value)) |> print(n = 200)

# Gennemsnit pr. produkt på tværs af kvartaler
pb_comp_dgs_premium(res, q = "all") |>
  dplyr::group_by(Product) |>
  dplyr::summarise(mean_dgs = mean(Value, na.rm = TRUE), .groups = "drop") |>
  dplyr::arrange(dplyr::desc(mean_dgs))




# Eksempelscript til pb_comp_oper2()
# Viser forskellige udtræk og transformationer på outputtet fra helperen.

# Load
source("pb_comp_oper2.R")
source("pb_comp_oper2.R")

# Fuldt output (alle kvartaler)
pb_comp_oper2(res) |>
  dplyr::arrange(Quarter, Side, Product, Maturity) |>
  print(n = 200)

# Seneste kvartal
op2 <- pb_comp_oper2(res)
q_latest <- with(op2, paste0("Q", max(as.integer(sub("^Q", "", unique(Quarter))), na.rm = TRUE)))
op2 |>
  dplyr::filter(Quarter == q_latest) |>
  dplyr::arrange(Side, Product, Maturity)

# Fuldt output (alle kvartaler)
pb_comp_oper2(res) |>
  dplyr::arrange(Quarter, Side, Product, Maturity) |>
  print(n = 200)

# Seneste kvartal
op2 <- pb_comp_oper2(res)
q_latest <- with(op2, paste0("Q", max(as.integer(sub("^Q", "", unique(Quarter))), na.rm = TRUE)))
op2 |>
  dplyr::filter(Quarter == q_latest) |>
  dplyr::arrange(Side, Product, Maturity)

# Seneste N kvartaler (fx 4)
uq <- op2$Quarter |>
  unique() |>
  (\(x) x[order(as.integer(sub("^Q", "", x)))])()
keep <- tail(uq, 4)
op2 |>
  dplyr::filter(Quarter %in% keep) |>
  dplyr::arrange(Quarter, Side, Product, Maturity)

# Specifikke kvartaler
pb_comp_oper2(res) |>
  dplyr::filter(Quarter %in% c("Q0", "Q1")) |>
  dplyr::arrange(Quarter, Product)

# Kun assets / funding
pb_comp_oper2(res) |>
  dplyr::filter(Side == "asset")
pb_comp_oper2(res) |>
  dplyr::filter(Side == "funding")

# Ikke-nul satser
pb_comp_oper2(res) |>
  dplyr::filter(Value > 0) |>
  dplyr::arrange(Quarter, Product)|>
  print(n = 200)

# Procentvisning
pb_comp_oper2(res) |>
  dplyr::mutate(pct = Value * 100) |>
  dplyr::arrange(Quarter, Product)|>
  print(n = 200)

# Pivot: produkter som kolonner pr. kvartal
pb_comp_oper2(res) |>
  tidyr::pivot_wider(names_from = Product, values_from = Value) |>
  dplyr::arrange(Quarter)|>
  print(n = 200)

# Pivot: tenorer som kolonner pr. produkt
pb_comp_oper2(res) |>
  tidyr::pivot_wider(names_from = Maturity, values_from = Value) |>
  dplyr::arrange(Product, Quarter)|>
  print(n = 200)



# Sammenlign v1 vs v2 (hvis pb_comp_oper findes)
if (exists("pb_comp_oper")) {
  op1 <- pb_comp_oper(res, q = "all")
  op2 <- pb_comp_oper2(res)
  dplyr::full_join(
    op1,
    op2,
    by = c("Quarter", "Side", "Product", "Maturity", "Component"),
    suffix = c("_v1", "_v2")
  ) |>
    dplyr::mutate(diff = Value_v2 - Value_v1) |>
    dplyr::arrange(Quarter, Product) |>
    print(n = 200)
}


# Load
source("pb_fee_income.R")

# Full table (all quarters)
pb_fee_income(res) |> dplyr::arrange(Quarter, Product, Maturity) |> print(n = 200)

# Latest quarter only
pb_fee_income(res) |>
  dplyr::filter(Quarter == paste0("Q", max(as.integer(sub("^Q","",Quarter)), na.rm = TRUE))) |>
  dplyr::arrange(Product, Maturity)

# Last N quarters (e.g., 4)
fi <- pb_fee_income(res)
keep <- fi$Quarter |> unique() |> (\(x) x[order(as.integer(sub("^Q","",x)))])() |> tail(4)
fi |> dplyr::filter(Quarter %in% keep) |> dplyr::arrange(Quarter, Product, Maturity)

# Specific quarters
pb_fee_income(res) |> dplyr::filter(Quarter %in% c("Q0","Q1")) |> dplyr::arrange(Quarter, Product)

# Entity filter
pb_fee_income(res, entity = "Bank 1") |> dplyr::arrange(Quarter, Product, Maturity) |> print(n = 200)



# Load
source("pb_comp_adv_yaml.R")

# Standard (alle kvartaler, default q="all")
pb_comp_adv_yaml(res) |> dplyr::arrange(Quarter, Side, Product, Maturity) |> print(n = 200)

# Seneste kvartal
pb_comp_adv_yaml(res, q = "latest") |> dplyr::arrange(Side, Product, Maturity)|> print(n = 200)

# Seneste N kvartaler
pb_comp_adv_yaml(res, q = 4) |> dplyr::arrange(Quarter, Side, Product, Maturity)

# Specifikke kvartaler
pb_comp_adv_yaml(res, q = c("Q0","Q1")) |> dplyr::arrange(Quarter, Side, Product, Maturity)

# Entity-filter
pb_comp_adv_yaml(res, entity = "Bank 1") |> dplyr::arrange(Quarter, Product)

# Eksplicit manifest
yaml_txt <- paste(readLines("manifest.yml"), collapse = "\n")
pb_comp_adv_yaml(res, manifest_yaml = yaml_txt, q = "all") |> dplyr::arrange(Quarter, Product)

# Kun funding / kun assets
pb_comp_adv_yaml(res) |> dplyr::filter(Side == "funding") |> dplyr::arrange(Quarter, Product)
pb_comp_adv_yaml(res) |> dplyr::filter(Side == "asset")   |> dplyr::arrange(Quarter, Product)

# Ikke-nul satser (Value er i procentpoint)
pb_comp_adv_yaml(res) |> dplyr::filter(Value > 0) |> dplyr::arrange(Quarter, Product)

# Pivot: produkter som kolonner pr. kvartal
pb_comp_adv_yaml(res, q = 4) |>
  tidyr::pivot_wider(names_from = Product, values_from = Value) |>
  dplyr::arrange(Quarter)

# Pivot: tenorer som kolonner pr. produkt
pb_comp_adv_yaml(res) |>
  tidyr::pivot_wider(names_from = Maturity, values_from = Value) |>
  dplyr::arrange(Product, Quarter)

# QoQ ændring i pp pr. produkt/tenor
pb_comp_adv_yaml(res) |>
  dplyr::group_by(Product, Maturity) |>
  dplyr::arrange(Quarter, .by_group = TRUE) |>
  dplyr::mutate(qoq_pp = Value - dplyr::lag(Value)) |>
  dplyr::ungroup()

# Materialitet
pb_comp_adv_yaml(res) |> dplyr::filter(abs(Value) >= 0.05) |> dplyr::arrange(dplyr::desc(Value))

# Trace-inspektion (kildestier, metode, noter)
adv_tbl <- pb_comp_adv_yaml(res, q = "all")
adv_tr  <- attr(adv_tbl, "trace")
adv_tr |> dplyr::select(Quarter, Product, Maturity, formula, notes) |> print(n = 200)
adv_tr$source_table[[1]]  # numerator/denominator paths for første række
adv_tr$parameters[[1]]    # annualiseringsfaktor, metode, percent=TRUE

# Sammenkør med andre komponenter (nøgler)
keys <- c("Quarter","Side","Product","Maturity")
rate <- pb_comp_rate(res, q = "all")
oper <- pb_comp_oper(res, q = "all")
fee  <- pb_fee_income(res)
dgs  <- pb_comp_dgs_premium(res, q = "all")
dplyr::full_join(rate, oper, by = keys) |>
  dplyr::full_join(fee,  by = keys) |>
  dplyr::full_join(dgs,  by = keys) |>
  dplyr::full_join(pb_comp_adv_yaml(res), by = keys) |>
  print(n = 200)


# Load DONE mangler at se om der er 
source("pb_asset_default_rates.R")

# Standard (alle kvartaler)
pb_asset_default_rates(res) |> dplyr::arrange(Quarter, Product) |> print(n = 200)

# Med manifest-aliaser
yaml_txt <- paste(readLines("manifest.yml"), collapse = "\n")
pb_asset_default_rates(res, manifest_yaml = yaml_txt) |> dplyr::arrange(Quarter, Product)

# Seneste kvartal
adr <- pb_asset_default_rates(res)
q_latest <- paste0("Q", max(as.integer(sub("^Q","", unique(adr$Quarter))), na.rm = TRUE))
adr |> dplyr::filter(Quarter == q_latest) |> dplyr::arrange(Product)

# Seneste N kvartaler (fx 4)
uq <- unique(adr$Quarter); keep <- uq[order(as.integer(sub("^Q","",uq)))] |> tail(4)
adr |> dplyr::filter(Quarter %in% keep) |> dplyr::arrange(Quarter, Product)

# Specifikke kvartaler
pb_asset_default_rates(res) |> dplyr::filter(Quarter %in% c("Q0","Q1")) |> dplyr::arrange(Quarter, Product)

# Kun produkter med værdi (ikke-NA)
pb_asset_default_rates(res) |> dplyr::filter(!is.na(Value)) |> dplyr::arrange(Quarter, Product)

# Materialitet (threshold)
pb_asset_default_rates(res) |> dplyr::filter(abs(Value) >= 0.05) |> dplyr::arrange(dplyr::desc(Value))

# Pivot: produkter som kolonner pr. kvartal
pb_asset_default_rates(res) |>
  tidyr::pivot_wider(names_from = Product, values_from = Value) |>
  dplyr::arrange(Quarter)

# QoQ ændring pr. produkt
pb_asset_default_rates(res) |>
  dplyr::group_by(Product) |>
  dplyr::arrange(Quarter, .by_group = TRUE) |>
  dplyr::mutate(qoq_pp = Value - dplyr::lag(Value)) |>
  dplyr::ungroup()

# Top-5 default-rate pr. kvartal
pb_asset_default_rates(res) |>
  dplyr::group_by(Quarter) |>
  dplyr::slice_max(order_by = Value, n = 5, with_ties = FALSE) |>
  dplyr::ungroup()

# Trace/notes (manglende np/pd)
adr <- pb_asset_default_rates(res)
attr(adr, "trace")$notes

# Sammenkør med andre komponenter (nøgler)
keys <- c("Quarter","Side","Product","Maturity")
def <- pb_asset_default_rates(res)
rate <- pb_comp_rate(res, q = "all")
oper <- pb_comp_oper(res, q = "all")
adv  <- pb_comp_adv_yaml(res, q = "all")
dplyr::full_join(rate, oper, by = keys) |>
  dplyr::full_join(adv,  by = keys) |>
  dplyr::full_join(def,  by = keys) |>
  print(n = 200)





source("pb_comp_rr.R")
pb_comp_rr(res) |>
  dplyr::arrange(Quarter, Product, Maturity) |>
  print(n = 100)


source("pb_asset_default_rates.R")
source("pb_comp_dgs_premium.R")
source("pb_comp_adv_yaml.R")
source("pb_fee_income.R")
source("pb_comp_oper2.R")
source("pb_comp_rate.R")
source("pb_comp_rr.R")
source("pb_asset_default_rates.R")

source("pb_overview_table.R")

pb_overview_table(res)                |>
  print(n = 200)                   # side="both", q="latest", as_percent=FALSE


source("pb_kpis.R")
# seneste kvartal, decimal
pb_kpis(res)


# seneste kvartal, procent
pb_kpis(res, unit = "percent")

# specifikt kvartal, decimal
pb_kpis(res, q = "Q1")

# specifikt kvartal, procent
pb_kpis(res, q = "Q1", unit = "percent")

# eksplicit “latest” via helper
pb_kpis(res, q = "latest")

# lille helper for alle kvartaler (én ad gangen) -> tidsserie
qs <- pb_quarters(res)
ts <- dplyr::bind_rows(lapply(qs, function(qq) pb_kpis(res, q = qq)))
ts


# =======================================================-
# 8) Loan performance parser----
# =======================================================-


Make a copy of "pb_dd_impact.R" where the code is reworked so it fits the new helper-file (called "pb_helpers_core.R")



source("pb_deposits_decrease_most.R")
source("pb_capitalization_report.R")
source("pb_lttd_flow.R")
source("pb_flow_generic.R")
source("pb_liabilities_amounts_and_rates.R")
source("pb_comp_rate.R")
source("pb_comp_dgs_premium.R")
source("pb_comp_oper2.R")
source("pb_fee_income.R")
source("pb_comp_adv_yaml.R")
source("pb_asset_default_rates.R")
source("pb_comp_rr.R")
source("pb_overview_table.R")
source("pb_kpis.R")

source("pb_dd_impact_core.R")

source("pb_dd_impact.R")
source("pb_deposits_decrease_most.R")
source("pb_capitalization_report.R")
source("pb_lttd_flow.R")
source("pb_flow_generic.R")
source("pb_liabilities_amounts_and_rates.R")
source("pb_comp_rate.R")
source("pb_comp_dgs_premium.R")
source("pb_comp_oper2.R")
source("pb_fee_income.R")
source("pb_comp_adv_yaml.R")
source("pb_asset_default_rates.R")
source("pb_comp_rr.R")
source("pb_overview_table.R")
source("pb_kpis.R")



