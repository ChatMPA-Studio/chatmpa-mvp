# shared/interpretation/interpret.R
# Pure helper functions for computing interpretation blocks in skill.R files.
# No I/O, no side effects. Source this file from any skill that needs it.
#
# Functions exported:
#   before_after()    — before/after means, delta, direction
#   mk_trend()        — Mann-Kendall monotonic trend test
#   insight_sentence() — bilingual insight sentence pair

# ── before_after ──────────────────────────────────────────────────────────────
#
# Args:
#   df           data.frame with a year column and a metric column
#   metric_col   character — name of the metric column
#   year_col     character — name of the year column (default "year")
#   before_years integer vector — explicit set of "before" years; NULL uses
#                first recent_n years not in the after window
#   recent_n     integer — number of most recent years for "after" period
#
# Returns:
#   On success: list(before_mean, after_mean, delta, pct_change, direction,
#                    before_period, after_period)
#   On failure: list(status = "no_data" | "insufficient_data" | "na_means")

before_after <- function(df, metric_col, year_col = "year",
                          before_years = NULL, recent_n = 5L) {

  df <- df[!is.na(df[[metric_col]]) & !is.na(df[[year_col]]), , drop = FALSE]
  if (nrow(df) == 0) return(list(status = "no_data"))

  all_years <- sort(unique(as.integer(df[[year_col]])))
  n_total   <- length(all_years)

  after_yrs <- tail(all_years, as.integer(recent_n))

  if (!is.null(before_years)) {
    before_yrs <- intersect(as.integer(before_years), all_years)
  } else {
    before_yrs <- head(setdiff(all_years, after_yrs), as.integer(recent_n))
  }

  if (length(before_yrs) == 0 || length(after_yrs) == 0)
    return(list(status = "insufficient_data", n_total = n_total))

  before_vals <- df[[metric_col]][as.integer(df[[year_col]]) %in% before_yrs]
  after_vals  <- df[[metric_col]][as.integer(df[[year_col]]) %in% after_yrs]

  before_mean <- mean(before_vals, na.rm = TRUE)
  after_mean  <- mean(after_vals,  na.rm = TRUE)

  if (is.na(before_mean) || is.na(after_mean))
    return(list(status = "na_means"))

  delta      <- after_mean - before_mean
  pct_change <- if (abs(before_mean) > 1e-10)
    round(delta / abs(before_mean) * 100, 1) else NA_real_

  direction <- if (abs(delta) < 1e-10) "stable"
               else if (delta > 0) "increasing" else "decreasing"

  list(
    before_mean   = round(before_mean, 4),
    after_mean    = round(after_mean,  4),
    delta         = round(delta, 4),
    pct_change    = pct_change,
    direction     = direction,
    before_period = c(min(before_yrs), max(before_yrs)),
    after_period  = c(min(after_yrs),  max(after_yrs))
  )
}

# ── mk_trend ──────────────────────────────────────────────────────────────────
#
# Non-parametric Mann-Kendall monotonic trend test (requires Kendall package).
#
# Args:
#   df         data.frame
#   metric_col character — metric to test
#   year_col   character — year column used for ordering (default "year")
#
# Returns:
#   list(method, tau, p, direction)  — or list(status = ...) on failure

mk_trend <- function(df, metric_col, year_col = "year") {

  if (!requireNamespace("Kendall", quietly = TRUE))
    return(list(status = "Kendall_package_missing"))

  df_clean <- df[!is.na(df[[metric_col]]), , drop = FALSE]
  df_clean <- df_clean[order(as.integer(df_clean[[year_col]])), ]
  x        <- df_clean[[metric_col]]

  if (length(x) < 4)
    return(list(status = "insufficient_data", n = length(x)))

  mk <- Kendall::MannKendall(x)

  list(
    method    = "Mann-Kendall",
    tau       = round(as.numeric(mk$tau), 4),
    p         = round(as.numeric(mk$sl),  4),
    direction = if (mk$tau > 0) "increasing" else
                if (mk$tau < 0) "decreasing" else "stable"
  )
}

# ── insight_sentence ──────────────────────────────────────────────────────────
#
# Builds a before/after insight sentence in English and Spanish.
#
# Args:
#   var_label_en  English label, e.g. "Fish biomass"
#   var_label_es  Spanish label (defaults to var_label_en if omitted)
#   unit          unit string, e.g. "g/m²", "°C"
#   before_mean   numeric
#   after_mean    numeric
#   delta         numeric (after - before)
#   pct_change    numeric or NA
#   before_period integer c(start, end)
#   after_period  integer c(start, end)
#   extra_en      optional extra sentence appended to English text
#   extra_es      optional extra sentence appended to Spanish text
#   digits        decimal places for values (default 2)
#
# Returns: list(en = "...", es = "...")

insight_sentence <- function(var_label_en, var_label_es = NULL, unit,
                              before_mean, after_mean, delta, pct_change,
                              before_period, after_period,
                              extra_en = NULL, extra_es = NULL,
                              digits = 2L) {

  if (is.null(var_label_es)) var_label_es <- var_label_en

  fmt <- function(x) formatC(round(x, digits), format = "f",
                              digits = digits, big.mark = ",")
  u   <- if (nzchar(unit)) paste0(" ", unit) else ""   # space+unit, or empty
  pct <- if (!is.na(pct_change)) paste0(" (", abs(round(pct_change, 1)), "%)") else ""

  dir_en <- if (delta > 0) "increased" else
            if (delta < 0) "decreased" else "remained stable"
  dir_es <- if (delta > 0) "aumentó" else
            if (delta < 0) "disminuyó" else "se mantuvo estable"

  bp <- if (before_period[1] == before_period[2]) as.character(before_period[1])
        else paste0(before_period[1], "–", before_period[2])
  ap <- if (after_period[1] == after_period[2]) as.character(after_period[1])
        else paste0(after_period[1], "–", after_period[2])

  en <- trimws(paste0(
    var_label_en, " ", dir_en, " by ", fmt(abs(delta)), u, pct,
    " from ", bp, " (", fmt(before_mean), u,
    ") to ", ap, " (", fmt(after_mean), u, ").",
    if (!is.null(extra_en)) paste0(" ", extra_en) else ""
  ))

  es <- trimws(paste0(
    var_label_es, " ", dir_es, " ", fmt(abs(delta)), u, pct,
    " de ", bp, " (", fmt(before_mean), u,
    ") a ", ap, " (", fmt(after_mean), u, ").",
    if (!is.null(extra_es)) paste0(" ", extra_es) else ""
  ))

  list(en = en, es = es)
}
