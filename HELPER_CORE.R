# ================================-
# 1) DEMAND DEPOSITS – IMPACT (CORE)----
# ================================-
# Parser: pb_dd_impact_core.R
source("pb_dd_impact_core.R")

# Markedsbenchmark (seneste to kvartaler)
pb_dd_impact(res, level = "market")$table  # Markedsniveau: brug til hurtig sammenligning af retail/corporate chok mellem de seneste kvartaler.

# Summary Balance Sheet (seneste to kvartaler)
pb_dd_impact(res, level = "summary")$table  # Bankspecifikt: anvend for at se hvordan egen balance påvirkes over samme periode.

# Evt. brug specifikke kvartaler
pb_dd_impact(res, level = "market", q_from = "Q0", q_to = "Q1")$table  # Tilpas hvis auto-valgte kvartaler ikke matcher ønsket analysevindue.


# ========================================== - -
# 2) DEPOSITS – HVILKEN FALDER MEST? (CORE)----
# ========================================== -
# Parser: pb_deposits_decrease_most_core.R
source("pb_deposits_decrease_most_core.R")

# Standardkørsel: seneste to kvartaler
pb_deposits_decrease_most(res)  # Returnerer detaljeret tabel; brug når du skal forklare både pct- og absolutte fald.

# Hurtig rapport (seneste to kvartaler)
pb_deposits_decrease_most_latest()  # Printer valg og tabel direkte; praktisk til ad-hoc mødenoter.

# Vælg specifikke kvartaler manuelt
pb_deposits_decrease_custom(q_from = "Q1", q_to = "Q2")  # Bruges når fokus er på ældre perioder eller stress-scenarier.

# Kun pct-ændringer, hurtigt overblik
pb_deposits_pct_changes()  # Leverer tabel med pct-udvikling; godt til dashboards hvor absolutte værdier ikke er nødvendige.


# ==================================-
# 3) CAPITALIZATION REPORT – FLEX (CORE)----
# ==================================-
# Parser: pb_capitalization_report_core.R
source("pb_capitalization_report_core.R")

# Fuld visning (Q0/Q1/Delta/Pct/↑↓)
pb_capitalization_flex(res, mode = "full")$table  # Brug til detaljeret bestyrelsesrapport med alle nøglekolonner.

# Kun ændringer (Delta/Pct/↑↓)
pb_capitalization_flex(res, mode = "changes")$table  # Fokusér på bevægelser når niveauer allerede er kendt.

# KPI-kort (Pris, MV, Aktier, Tier I)
pb_capitalization_flex(res, mode = "kpi")$table  # Hurtig oversigt til præsentationer hvor kun top-KPI'er er relevante.

# Top-5 største bevægelser (absolut Delta)
pb_capitalization_flex(res, mode = "top5", top_n = 5)$table  # Brug til materialitetsgennemgange hvor kun de største udsving er interessante.

# Materiality-filter
pb_capitalization_flex(res, mode = "material", materiality = list(pct = 2, delta = 5000))$table  # Filtrér støj væk når thresholds er aftalt på forhånd.

# Signaler (prisfald, lave kapitalrater)
pb_capitalization_flex(res, mode = "signals")$table  # Identificér issues som kræver handling uden at læse hele tabellen.

# Inkludér "Prior quarter"-linjer i grundlaget
pb_capitalization_flex(res, keep_prior = TRUE)$table  # Tag tidligere niveauer med når revisionssporet skal dokumenteres.


# ========================================== -
# 4) FLOW-PARSERE – T+N (CORE)----
# ========================================== -
# Parsere: pb_lttd_flow_core.R + pb_flow_generic_core.R
source("pb_lttd_flow_core.R")
source("pb_flow_generic_core.R")

# --- Direkte core-kald ---
pb_lttd_flow_core(res)  # Producerer komplet T+N-stige for long-term time deposits; brug ved dybdegående balanceanalyse.

# --- BANK BALANCE SHEET (seneste to kvartaler) ---
pb_lttd_flow_latest(res)          # Standard LT time deposits; brug til seneste kvartalsrapport.
pb_wholesale_flow_latest(res)     # Wholesale deposit flows; anvend til treasury-møder.
pb_cds_flow_latest(res)           # Savings Certificates (CDs); relevant for funding-strategiopfølgning.
pb_blfloating_flow_latest(res)    # Business loans floating-rate; brug når renteeksponering skal vurderes.
pb_consumer_flow_latest(res)      # Consumer loans ladder; anvend i retail lending analyser.
pb_mortgage_flow_latest(res)      # Mortgage flows; godt til boligporteføljerapporter.
pb_govbonds_flow_latest(res)      # Government bond ladder; brug i ALM compliance checks.

# --- BANK BALANCE SHEET (vælg kvartaler) ---
pb_lttd_flow_custom(res, q_from = "Q0", q_to = "Q1")  # Brug når perioden ikke er de seneste kvartaler.
pb_wholesale_flow_custom(res, q_from = "Q0", q_to = "Q1")  # Tilpas til specifik stresstestperiode.
pb_cds_flow_custom(res, q_from = "Q0", q_to = "Q1")  # Fokuser på valgte perioder for CD-beholdningen.
pb_blfloating_flow_custom(res, q_from = "Q0", q_to = "Q1")  # Custom vindue for erhvervslån.
pb_consumer_flow_custom(res, q_from = "Q0", q_to = "Q1")  # Segmentér consumer flows på ældre data.
pb_mortgage_flow_custom(res, q_from = "Q0", q_to = "Q1")  # Brug til historiske boligrapporter.
pb_govbonds_flow_custom(res, q_from = "Q0", q_to = "Q1")  # Evaluer statsobligationsløb ved valgte tidspunkter.

# --- MARKET AVERAGE BALANCE SHEET (seneste to kvartaler) ---
pb_lttd_flow_market_latest(res)      # Markedsbenchmark for LT time deposits; brug til peer-sammenligning.
pb_wholesale_flow_market_latest(res) # Wholesale markedsdata; anvend til funding-benchmarking.
pb_cds_flow_market_latest(res)       # CD-markedsflow; nyttig til planlægning af prisstrategi.
pb_blfloating_flow_market_latest(res)# Markedsgennemsnit for erhvervslån; brug i konkurrencestudier.
pb_consumer_flow_market_latest(res)  # Markedsflow for consumer-lån; relevant for retail positionering.
pb_mortgage_flow_market_latest(res)  # Markedsflow for boliglån; brug til makrooverblik.
pb_govbonds_flow_market_latest(res)  # Markedsflow for statsobligationer; anvend i investeringskomitéer.


# =======================================================-
# 5) LIABILITIES – AMOUNTS & RATES (CORE)----
# =======================================================-
# Parser: pb_liabilities_amounts_and_rates_core.R
source("pb_liabilities_amounts_and_rates_core.R")

# Én linje pr. visning (printer)
pb_liab_amounts(res)   # Printer mængdetabel; brug til hurtig gennemgang i mødelokalet.
pb_liab_rates(res)     # Printer rentetabel; anvend når fokus er på rentespænd.
pb_liab_both(res)      # Printer begge tabeller; godt til fuldt bilag.

# Alternativt: få objekterne (tibbles) tilbage
a <- pb_liabilities_amounts(res); a   # Hent dataobjekt til videre behandling i R.
r <- pb_liabilities_rates(res);   r   # Brug når du vil plotte rentekurver.
b <- pb_liabilities_all(res);     b   # Returnerer liste; ideel til programmeret eksport.


# =======================================================-
# 6) RATE & COST COMPONENTS (CORE VS. LEGACY)----
# =======================================================-

# --- Core rate komponent ---
source("pb_comp_rate_core.R")
pb_comp_rate_core(res) |> print(n = 200) # Core-beregning af nominelle satser; anvend når du ønsker trace-attributter og manifestkontrol.

# --- Core operating cost komponent ---
source("pb_comp_oper2_core.R")
pb_comp_oper2_core(res) |> print(n = 200) # Opdateret oper. cost parser; brug når du vil have ensartet langt format til pipelines.

# --- DGS premium komponent (core) ---
source("pb_comp_dgs_premium_core.R")
pb_comp_dgs_premium(res)  |> print(n = 200) # Beregner DGS-premier i langt format; anvend ved funding-cost analyser.

# --- Advertising component (core rewrite) ---
source("pb_comp_adv.R")
pb_comp_adv(res)  |> print(n = 200) # Reklameomkostninger fordelt på produkter; brug ved budgetopfølgning med manifest aliaser.

# --- Fee income komponent (core rewrite) ---
source("pb_fee_income_core.R")
pb_fee_income(res) |> print(n = 200)  # Core-versionen giver konsistente fee-rater; brug når du vil have trace-info i output.

# --- Default rate (core rewrite) ---
source("pb_asset_default_rates.R")
pb_asset_default_rates(res, q = "latest") |> print(n = 200)

# --- Required reserves ---
source("pb_comp_rr.R")
pb_comp_rr(res) |> print(n = 200)

# =======================================================-
# 7) OVERBLIK & KPI'ER (CORE VS. LEGACY)----
# =======================================================-

# --- KPI'er (core rewrite) ---
source("pb_kpis_core.R") # VIRKER IKKE
pb_kpis(res, q = "latest")  # Core-KPI'er med robust kvartalsvalg; brug til officielle nøgletalsrapporter.

# --- Overview table (core) ---
source("pb_overview_table_core.R")
pb_overview_table_core(res, side = "both", q = "latest") |> print(n = 200) # Core-oversigt der samler alle komponenter; perfekt til dashboards.

# --- Overview table (legacy) ---
source("pb_overview_table.R")
pb_overview_table(res, side = "both", q = "latest") |> print(n = 200) # Legacy-version til validering mod tidligere udsendte tabeller.


# DONE (CORE HELPER LIST)