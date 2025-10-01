# pb_lttd_flow_core.R
# Long-term Time Deposits flow (T+N ladder) rewritten to rely on pb_helpers_core.R

pb_lttd_flow_core <- function(res,
                              q_from = NULL,
                              q_to = NULL,
                              section = "BANK BALANCE SHEET",
                              subsection = "LIABILITIES",
                              group = "Long-term Time Deposits, maturing start of:",
                              side = "Liabilities",
                              product = "Long-term Time Deposits",
                              entity = NULL) {
  context <- "pb_lttd_flow_core"
  pb_helper_require_packages(c("dplyr", "stringr", "tibble", "tidyr"), context)
  
  table <- pb_get_table(
    res,
    section = section,
    subsection = subsection,
    group = group,
    entity = entity,
    context = sprintf("%s::pb_get_table", context)
  )
  
  if (!nrow(table)) {
    stop(
      sprintf(
        "%s: Ingen rækker fundet for %s.",
        context,
        pb_helper_manifest_path(section, subsection, NULL, group, entity)
      ),
      call. = FALSE
    )
  }
  
  meta_cols <- intersect(
    c("__section", "__subsection", "__subsubsection", "__group", "__entity"),
    names(table)
  )
  quarter_cols <- setdiff(names(table), c(meta_cols, "Item"))
  quarter_cols <- pb_helper_quarter_order(quarter_cols)
  if (length(quarter_cols) < 2) {
    stop(
      sprintf(
        "%s: Mindst to kvartaler kræves for at beregne flow; fundet %d.",
        context,
        length(quarter_cols)
      ),
      call. = FALSE
    )
  }
  
  q_defaults <- tail(quarter_cols, 2)
  q0 <- if (is.null(q_from)) q_defaults[[1]] else q_from
  q1 <- if (is.null(q_to))   q_defaults[[2]] else q_to
  q_pair <- c(q0, q1)
  
  missing_quarters <- setdiff(q_pair, quarter_cols)
  if (length(missing_quarters)) {
    stop(
      sprintf(
        "%s: Kvartalet/kvartalerne %s findes ikke i gruppen %s.",
        context,
        paste(missing_quarters, collapse = ", "),
        group
      ),
      call. = FALSE
    )
  }
  
  base <- tibble::tibble(
    Item = pb_helper_chr(table$Item),
    Q0 = suppressWarnings(as.numeric(table[[q0]])),
    Q1 = suppressWarnings(as.numeric(table[[q1]]))
  ) |>
    dplyr::filter(
      stringr::str_detect(Item, "^T\\+\\d+$"),
      !stringr::str_detect(Item, "\\(rate\\)$")
    ) |>
    dplyr::mutate(
      T_index = suppressWarnings(as.integer(sub("^T\\+", "", Item)))
    ) |>
    dplyr::arrange(T_index) |>
    dplyr::mutate(Delta = Q1 - Q0)
  
  if (!nrow(base)) {
    stop(
      sprintf(
        "%s: Ingen T+N-beløb fundet i %s.",
        context,
        group
      ),
      call. = FALSE
    )
  }
  
  mature_label <- if (any(base$Item == "T+1")) {
    "T+1"
  } else {
    base$Item[which.min(base$T_index)]
  }
  new_label <- if (any(base$Item == "T+8")) {
    "T+8"
  } else {
    base$Item[which.max(base$T_index)]
  }
  
  maturing_val <- base |>
    dplyr::filter(Item == mature_label) |>
    dplyr::summarise(val = sum(Q0, na.rm = TRUE), .groups = "drop") |>
    dplyr::pull(val)
  if (!length(maturing_val)) maturing_val <- NA_real_
  
  new_val <- base |>
    dplyr::filter(Item == new_label) |>
    dplyr::summarise(val = sum(Q1, na.rm = TRUE), .groups = "drop") |>
    dplyr::pull(val)
  if (!length(new_val)) new_val <- NA_real_
  net_change <- sum(base$Delta, na.rm = TRUE)
  
  balances <- base |>
    dplyr::select(Item, Q0, Q1) |>
    tidyr::pivot_longer(
      cols = c(Q0, Q1),
      names_to = "Quarter_key",
      values_to = "Value"
    ) |>
    dplyr::mutate(
      Quarter = dplyr::case_when(
        Quarter_key == "Q0" ~ q0,
        Quarter_key == "Q1" ~ q1
      ),
      Component = "Balance"
    ) |>
    dplyr::select(-Quarter_key)
  
  changes <- base |>
    dplyr::transmute(
      Item,
      Quarter = q1,
      Component = sprintf("Change from %s", q0),
      Value = Delta
    )
  
  summaries <- tibble::tibble(
    Item = c(mature_label, new_label, "All maturities"),
    Quarter = c(q0, q1, q1),
    Component = c(
      sprintf("Maturing at %s", q0),
      sprintf("New at %s", q1),
      sprintf("Net change %s-%s", q0, q1)
    ),
    Value = c(maturing_val, new_val, net_change)
  )
  
  result_raw <- dplyr::bind_rows(balances, changes, summaries) |>
    dplyr::mutate(
      Side = side,
      Product = product,
      Maturity = Item
    ) |>
    dplyr::select(Quarter, Side, Product, Maturity, Component, Value)
  
  trace <- list(
    context = context,
    section = section,
    subsection = subsection,
    group = group,
    entity = entity,
    quarters = list(from = q0, to = q1),
    labels = list(maturing = mature_label, new = new_label)
  )
  
  pb_helper_make_result(result_raw, trace = trace, context = context)
}