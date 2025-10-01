# pb_deposits_decrease_most_core.R
# Refactored version of pb_deposits_decrease_most that relies on pb_helpers_core.R
# utilities. Provides identical functionality while delegating validation and data
# preparation to shared helpers.

pb_deposits_decrease_most <- function(res, q_from = NULL, q_to = NULL) {
  context <- "pb_deposits_decrease_most"
  pb_helper_require_packages(c("dplyr", "tibble"), context)
  
  quarters_all <- pb_quarters(res, context = sprintf("%s: kvartaler", context))
  if (length(quarters_all) < 2L) {
    stop("Der findes kun ét kvartal i data.", call. = FALSE)
  }
  
  resolve_quarter <- function(q, default_index, label) {
    if (is.null(q)) {
      if (default_index < 1L || default_index > length(quarters_all)) {
        stop(
          sprintf(
            "%s: Kan ikke vælge %s, fordi der ikke findes tilstrækkelige kvartaler i data.",
            context,
            label
          ),
          call. = FALSE
        )
      }
      return(quarters_all[[default_index]])
    }
    selected <- pb_helper_select_quarters(
      quarters_all,
      q,
      context = sprintf("%s: valg af %s", context, tolower(label))
    )
    if (!length(selected)) {
      stop(
        sprintf("%s: Ingen kvartaler matcher angivelsen for %s.", context, label),
        call. = FALSE
      )
    }
    if (length(selected) > 1L) {
      stop(
        sprintf("%s: Angiv præcis ét kvartal for %s.", context, label),
        call. = FALSE
      )
    }
    selected[[1]]
  }
  
  q1 <- resolve_quarter(q_to, length(quarters_all), "slutkvartalet")
  idx_q1 <- match(q1, quarters_all)
  q0 <- resolve_quarter(q_from, idx_q1 - 1L, "startkvartalet")
  
  summarise_group <- function(group_title, type_label, only_tn = FALSE, single_item = NULL) {
    table_context <- sprintf("%s: %s", context, type_label)
    tbl <- pb_get_table(
      res,
      section = "BANK BALANCE SHEET",
      subsection = "LIABILITIES",
      group = group_title,
      context = table_context
    )
    if (!nrow(tbl)) {
      values_q0 <- 0
      values_q1 <- 0
    } else {
      tbl <- tibble::as_tibble(tbl)
      tbl$Item <- pb_helper_chr(tbl$Item)
      if (!is.null(single_item)) {
        keep <- pb_helper_match_values(tbl$Item, single_item)
        tbl <- tbl[keep, , drop = FALSE]
      } else {
        rates <- grepl("\\(rate\\)$", tbl$Item)
        tbl <- tbl[!rates, , drop = FALSE]
        if (only_tn) {
          tn <- grepl("^T\\+\\d+$", tbl$Item)
          tbl <- tbl[tn, , drop = FALSE]
        }
      }
      if (!nrow(tbl)) {
        values_q0 <- 0
        values_q1 <- 0
      } else {
        values_q0 <- sum(suppressWarnings(as.numeric(tbl[[q0]])), na.rm = TRUE)
        values_q1 <- sum(suppressWarnings(as.numeric(tbl[[q1]])), na.rm = TRUE)
      }
    }
    delta <- values_q1 - values_q0
    pct <- if (values_q0 == 0) {
      NA_real_
    } else {
      (values_q1 - values_q0) / values_q0 * 100
    }
    tibble::tibble(
      Type = type_label,
      Q0 = values_q0,
      Q1 = values_q1,
      Delta = delta,
      Pct = pct
    )
  }
  
  tab <- dplyr::bind_rows(
    summarise_group("Wholesale Deposits, maturing start of:", "Wholesale Deposits", only_tn = TRUE),
    summarise_group("Savings Deposits", "Savings Deposits", single_item = "Savings Deposits"),
    summarise_group("Savings Certificates (CDs), maturing start of:", "Savings Certificates (CDs)", only_tn = TRUE),
    summarise_group("Long-term Time Deposits, maturing start of:", "Long-term Time Deposits", only_tn = TRUE)
  ) |> dplyr::mutate(Quarter_from = q0, Quarter_to = q1, .before = Q0)
  
  choice_pct <- if (any(tab$Pct < 0, na.rm = TRUE)) {
    dplyr::filter(tab, !is.na(Pct)) |>
      dplyr::slice_min(order_by = Pct, n = 1, with_ties = FALSE) |>
      dplyr::pull(Type)
  } else {
    "Ingen fald i procent"
  }
  
  choice_abs <- if (any(tab$Delta < 0, na.rm = TRUE)) {
    dplyr::slice_min(tab, order_by = Delta, n = 1, with_ties = FALSE) |>
      dplyr::pull(Type)
  } else {
    "Ingen fald i absolutte værdier"
  }
  
  list(table = tab, choice_pct = choice_pct, choice_abs = choice_abs)
}

pb_deposits_decrease_most_latest <- function() {
  ans <- pb_deposits_decrease_most(res)
  print(ans$table, n = nrow(ans$table))
  cat("\nStørste fald (pct):   ", ans$choice_pct, "\n", sep = "")
  cat("Største fald (absolut): ", ans$choice_abs, "\n", sep = "")
  invisible(ans)
}

pb_deposits_decrease_choice <- function() {
  ans <- pb_deposits_decrease_most(res)
  cat("Største fald (pct):   ", ans$choice_pct, "\n", sep = "")
  cat("Største fald (absolut): ", ans$choice_abs, "\n", sep = "")
  invisible(list(pct = ans$choice_pct, abs = ans$choice_abs))
}

pb_deposits_decrease_custom <- function(q_from, q_to) {
  ans <- pb_deposits_decrease_most(res, q_from = q_from, q_to = q_to)
  print(ans$table, n = nrow(ans$table))
  cat("\nStørste fald (pct):   ", ans$choice_pct, "\n", sep = "")
  cat("Største fald (absolut): ", ans$choice_abs, "\n", sep = "")
  invisible(ans)
}

pb_deposits_pct_changes <- function() {
  ans <- pb_deposits_decrease_most(res)
  dplyr::select(ans$table, Type, Pct)
}