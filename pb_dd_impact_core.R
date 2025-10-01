# pb_dd_impact_core.R
# Demand deposit impact calculator rewritten to leverage pb_helpers_core utilities.

pb_dd_impact_resolve_quarters <- function(res, q_from = NULL, q_to = NULL,
                                          context = "pb_dd_impact") {
  quarters_available <- pb_quarters(res, context = sprintf("%s: kvartaler", context))
  if (length(quarters_available) < 2L) {
    stop(
      sprintf(
        "%s: Datagrundlaget indeholder kun ét kvartal (%s). Tilføj mindst et ekstra kvartal.",
        context,
        paste(quarters_available, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  
  quarters_available <- pb_helper_chr(quarters_available)
  quarters_default <- tail(quarters_available, 2L)
  
  resolve_single <- function(value, label) {
    if (is.null(value)) {
      return(NULL)
    }
    candidates <- pb_helper_chr(value)
    norm_candidates <- pb_helper_norm(candidates)
    norm_available <- pb_helper_norm(quarters_available)
    idx <- match(norm_candidates, norm_available)
    if (anyNA(idx)) {
      stop(
        sprintf(
          "%s: Angivet %s-kvartal '%s' findes ikke. Kendte kvartaler: %s.",
          context,
          label,
          paste(value, collapse = ", "),
          paste(quarters_available, collapse = ", ")
        ),
        call. = FALSE
      )
    }
    quarters_available[idx[1]]
  }
  
  q0 <- resolve_single(q_from, "fra") %||% quarters_default[1]
  q1 <- resolve_single(q_to, "til")   %||% quarters_default[2]
  
  list(q0 = q0, q1 = q1, quarters_available = quarters_available)
}

pb_dd_impact_pull_value <- function(res, item, quarter,
                                    section, subsection = NULL, group = NULL, entity = NULL,
                                    context = "pb_dd_impact") {
  tbl <- pb_get_table(
    res,
    section = section,
    subsection = subsection,
    group = group,
    entity = entity,
    items = item,
    quarter = quarter,
    context = sprintf("%s: tabelopslag", context)
  )
  
  if (!nrow(tbl)) {
    stop(
      sprintf(
        "%s: Ingen rækker matcher Item='%s' i sektionen %s.",
        context,
        item,
        pb_helper_manifest_path(section, subsection, NULL, group, entity)
      ),
      call. = FALSE
    )
  }
  
  quarter_label <- pb_helper_chr(quarter)[1]
  if (!quarter_label %in% names(tbl)) {
    stop(
      sprintf(
        "%s: Kvartalet '%s' findes ikke i tabellen for Item='%s'.",
        context,
        quarter,
        item
      ),
      call. = FALSE
    )
  }
  
  items_norm <- pb_helper_norm(pb_helper_chr(tbl$Item))
  idx <- which(items_norm == pb_helper_norm(item))
  if (!length(idx)) {
    stop(
      sprintf(
        "%s: Item='%s' findes ikke efter filtrering.", context, item
      ),
      call. = FALSE
    )
  }
  
  values <- suppressWarnings(as.numeric(tbl[[quarter_label]][idx]))
  values <- values[!is.na(values)]
  if (!length(values)) {
    stop(
      sprintf(
        "%s: Ingen numeriske værdier fundet for Item='%s' @ %s.",
        context,
        item,
        quarter
      ),
      call. = FALSE
    )
  }
  values[1]
}

pb_dd_impact_item_summary <- function(res, item, q0, q1,
                                      section, subsection = NULL, group = NULL, entity = NULL,
                                      context = "pb_dd_impact") {
  pb_helper_require_packages("tibble", context)
  v0 <- pb_dd_impact_pull_value(
    res,
    item = item,
    quarter = q0,
    section = section,
    subsection = subsection,
    group = group,
    entity = entity,
    context = sprintf("%s (%s)", context, q0)
  )
  v1 <- pb_dd_impact_pull_value(
    res,
    item = item,
    quarter = q1,
    section = section,
    subsection = subsection,
    group = group,
    entity = entity,
    context = sprintf("%s (%s)", context, q1)
  )
  
  pct <- if (is.na(v0) || identical(v0, 0)) NA_real_ else (v1 - v0) / v0 * 100
  
  tibble::tibble(
    Item = item,
    !!q0 := v0,
    !!q1 := v1,
    Delta = v1 - v0,
    Pct = pct
  )
}

pb_dd_impact_choose_option <- function(pct_retail, pct_corp, tol = 1e-12) {
  pr <- as.numeric(pct_retail)[1]
  pc <- as.numeric(pct_corp)[1]
  if (is.na(pr) || is.na(pc) || abs(pr - pc) <= tol) {
    return("Equal impact, in percent")
  }
  if (pr > pc) {
    if (pr >= 0) {
      "Retail demand deposits increased the most, in percent"
    } else {
      "Retail demand deposits decreased the most, in percent"
    }
  } else {
    "Corporate demand deposits increased the most, in percent"
  }
}

pb_dd_impact_market <- function(res, q_from = NULL, q_to = NULL) {
  pb_helper_require_packages(c("dplyr", "tibble"), "pb_dd_impact_market")
  qs <- pb_dd_impact_resolve_quarters(res, q_from, q_to, context = "pb_dd_impact_market")
  q0 <- qs$q0
  q1 <- qs$q1
  
  retail <- pb_dd_impact_item_summary(
    res,
    item = "Retail",
    q0 = q0,
    q1 = q1,
    section = "MARKET AVERAGE BALANCE SHEET",
    subsection = "LIABILITIES",
    group = "Demand Deposits",
    context = "pb_dd_impact_market"
  )
  
  corporate <- pb_dd_impact_item_summary(
    res,
    item = "Corporate",
    q0 = q0,
    q1 = q1,
    section = "MARKET AVERAGE BALANCE SHEET",
    subsection = "LIABILITIES",
    group = "Demand Deposits",
    context = "pb_dd_impact_market"
  )
  
  tbl <- tibble::as_tibble(dplyr::bind_rows(retail, corporate))
  
  choice <- pb_dd_impact_choose_option(
    pct_retail = tbl$Pct[pb_helper_norm(tbl$Item) == pb_helper_norm("Retail")],
    pct_corp = tbl$Pct[pb_helper_norm(tbl$Item) == pb_helper_norm("Corporate")]
  )
  
  list(quarters = c(from = q0, to = q1), table = tbl, choice = choice)
}

pb_dd_impact_summary <- function(res, q_from = NULL, q_to = NULL) {
  pb_helper_require_packages(c("dplyr", "tibble"), "pb_dd_impact_summary")
  qs <- pb_dd_impact_resolve_quarters(res, q_from, q_to, context = "pb_dd_impact_summary")
  q0 <- qs$q0
  q1 <- qs$q1
  
  retail <- pb_dd_impact_item_summary(
    res,
    item = "Retail Demand Deposits",
    q0 = q0,
    q1 = q1,
    section = "SUMMARY BALANCE SHEET",
    subsection = "LIABILITIES",
    context = "pb_dd_impact_summary"
  )
  
  corporate <- pb_dd_impact_item_summary(
    res,
    item = "Corporate Demand Deposits",
    q0 = q0,
    q1 = q1,
    section = "SUMMARY BALANCE SHEET",
    subsection = "LIABILITIES",
    context = "pb_dd_impact_summary"
  )
  
  tbl <- tibble::as_tibble(dplyr::bind_rows(retail, corporate))
  
  choice <- pb_dd_impact_choose_option(
    pct_retail = tbl$Pct[pb_helper_norm(tbl$Item) == pb_helper_norm("Retail Demand Deposits")],
    pct_corp = tbl$Pct[pb_helper_norm(tbl$Item) == pb_helper_norm("Corporate Demand Deposits")]
  )
  
  list(quarters = c(from = q0, to = q1), table = tbl, choice = choice)
}

pb_dd_impact <- function(res, level = c("market", "summary"), q_from = NULL, q_to = NULL) {
  level <- match.arg(level)
  if (level == "market") {
    pb_dd_impact_market(res, q_from = q_from, q_to = q_to)
  } else {
    pb_dd_impact_summary(res, q_from = q_from, q_to = q_to)
  }
}