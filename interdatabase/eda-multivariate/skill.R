# skill.R — eda-multivariate
#
# Exploratory data analysis for a set of aligned annual ecological time series.
# Input: aligned table from temporal_align() — one row per year, one column
# per variable. All variables are treated equally (no predictor/response assumption).
#
# Sections computed (all by default, subset via `sections` argument):
#   "summary"      — basic statistics per variable
#   "normality"    — Shapiro-Wilk test per variable (informative only)
#   "trends"       — Mann-Kendall with Hamed & Rao (1998) autocorrelation correction
#   "correlations" — Spearman rank correlation matrix (rho + p-values)
#   "outliers"     — years exceeding ± 2 SD per variable
#
# Dependencies: modifiedmk (for Hamed & Rao corrected Mann-Kendall)
# See SKILL.md for full data contract and do-not rules.

# ── Constants ─────────────────────────────────────────────────────────────────

.VALID_SECTIONS  <- c("summary", "normality", "trends", "correlations", "outliers", "crosscorr")
.OUTLIER_SD      <- 2     # SD threshold for outlier flagging (fixed)
.MK_MIN_N        <- 4L    # minimum n for Mann-Kendall
.SW_MIN_N        <- 3L    # minimum n for Shapiro-Wilk
.TREND_ALPHA     <- 0.05  # significance level for trend direction label
.CCF_LAG_MAX     <- 3L    # maximum lag (years) for cross-correlation
.CCF_MIN_N       <- 6L    # minimum pairwise n for CCF

# ── run_skill ─────────────────────────────────────────────────────────────────
#
# Args:
#   data      data.frame — aligned table from temporal_align()$aligned
#             must have a "year" column; all other columns are variables
#   sections  character vector — subset of .VALID_SECTIONS to compute
#             default: all sections
#
# Returns: list(value, method, params)

run_skill <- function(data,
                      sections = .VALID_SECTIONS,
                      ...) {

  # ── Input validation ───────────────────────────────────────────────────────

  if (!is.data.frame(data))
    stop("'data' must be a data.frame (output of temporal_align()$aligned).")
  if (!"year" %in% names(data))
    stop("'data' must have a 'year' column.")

  unknown_sections <- setdiff(sections, .VALID_SECTIONS)
  if (length(unknown_sections) > 0)
    stop("Unknown sections: ", paste(unknown_sections, collapse = ", "),
         ". Valid options: ", paste(.VALID_SECTIONS, collapse = ", "))

  var_cols <- setdiff(names(data), "year")
  if (length(var_cols) < 2)
    stop("'data' must have at least 2 variable columns (besides 'year').")

  # Drop rows where ALL variables are NA
  all_na_rows <- apply(data[, var_cols, drop = FALSE], 1,
                       function(r) all(is.na(r)))
  if (any(all_na_rows)) {
    warning(sum(all_na_rows), " row(s) dropped where all variables are NA: years ",
            paste(data$year[all_na_rows], collapse = ", "))
    data <- data[!all_na_rows, ]
  }

  if (!requireNamespace("modifiedmk", quietly = TRUE) && "trends" %in% sections)
    stop("Package 'modifiedmk' is required for the 'trends' section. ",
         "Install with: install.packages('modifiedmk')")

  value <- list()

  # ── summary ───────────────────────────────────────────────────────────────

  if ("summary" %in% sections) {
    value$summary <- do.call(rbind, lapply(var_cols, function(v) {
      x <- data[[v]]
      x_valid <- x[!is.na(x)]
      n <- length(x_valid)
      if (n == 0)
        return(data.frame(variable = v, n_valid = 0L,
                          mean = NA, median = NA, sd = NA,
                          min = NA, max = NA, skewness = NA,
                          stringsAsFactors = FALSE))
      m <- mean(x_valid)
      m2 <- mean((x_valid - m)^2)
      m3 <- mean((x_valid - m)^3)
      skew <- if (m2 == 0) NA_real_ else m3 / m2^1.5
      data.frame(
        variable = v,
        n_valid  = n,
        mean     = round(m, 4),
        median   = round(median(x_valid), 4),
        sd       = round(sd(x_valid), 4),
        min      = round(min(x_valid), 4),
        max      = round(max(x_valid), 4),
        skewness = round(skew, 4),
        stringsAsFactors = FALSE
      )
    }))
    rownames(value$summary) <- NULL
  }

  # ── normality ─────────────────────────────────────────────────────────────

  if ("normality" %in% sections) {
    value$normality <- do.call(rbind, lapply(var_cols, function(v) {
      x_valid <- data[[v]][!is.na(data[[v]])]
      n <- length(x_valid)
      if (n < .SW_MIN_N)
        return(data.frame(variable = v, n_valid = n,
                          W = NA, p_value = NA, note = "n < 3 — test not run",
                          stringsAsFactors = FALSE))
      sw <- shapiro.test(x_valid)
      data.frame(
        variable = v,
        n_valid  = n,
        W        = round(sw$statistic, 4),
        p_value  = round(sw$p.value, 4),
        note     = "Informative only. Temporal autocorrelation may inflate W.",
        stringsAsFactors = FALSE
      )
    }))
    rownames(value$normality) <- NULL
  }

  # ── trends ────────────────────────────────────────────────────────────────

  if ("trends" %in% sections) {
    value$trends <- do.call(rbind, lapply(var_cols, function(v) {
      x_valid <- data[[v]][!is.na(data[[v]])]
      n <- length(x_valid)
      if (n < .MK_MIN_N)
        return(data.frame(
          variable    = v, n_valid = n,
          tau         = NA, p_value_hr = NA, p_value_mk = NA,
          direction   = NA,
          note        = paste0("n < ", .MK_MIN_N, " — test not run"),
          stringsAsFactors = FALSE
        ))
      mk <- modifiedmk::mmkh(x_valid)
      tau      <- round(mk["Tau"],        4)
      p_hr     <- round(mk["P-value"],    4)
      p_mk     <- round(mk["old P-value"], 4)
      direction <- if (is.na(p_hr) || p_hr >= .TREND_ALPHA) {
        "none"
      } else if (tau > 0) "increasing" else "decreasing"
      data.frame(
        variable   = v,
        n_valid    = n,
        tau        = unname(tau),
        p_value_hr = unname(p_hr),
        p_value_mk = unname(p_mk),
        direction  = direction,
        note       = "Hamed & Rao (1998) correction for serial autocorrelation applied.",
        stringsAsFactors = FALSE
      )
    }))
    rownames(value$trends) <- NULL
  }

  # ── correlations ──────────────────────────────────────────────────────────

  if ("correlations" %in% sections) {
    n_vars <- length(var_cols)
    rho_mat <- matrix(NA_real_, n_vars, n_vars,
                      dimnames = list(var_cols, var_cols))
    p_mat   <- matrix(NA_real_, n_vars, n_vars,
                      dimnames = list(var_cols, var_cols))
    diag(rho_mat) <- 1
    diag(p_mat)   <- 0

    for (i in seq_len(n_vars)) {
      for (j in seq_len(n_vars)) {
        if (i >= j) next
        x <- data[[var_cols[i]]]
        y <- data[[var_cols[j]]]
        both <- !is.na(x) & !is.na(y)
        if (sum(both) < 3) next
        ct <- cor.test(x[both], y[both], method = "spearman", exact = FALSE)
        rho_mat[i, j] <- rho_mat[j, i] <- round(ct$estimate, 4)
        p_mat[i, j]   <- p_mat[j, i]   <- round(ct$p.value, 4)
      }
    }
    value$correlations <- list(
      rho     = as.data.frame(rho_mat),
      p_value = as.data.frame(p_mat)
    )
  }

  # ── outliers ──────────────────────────────────────────────────────────────

  if ("outliers" %in% sections) {
    value$outliers <- lapply(var_cols, function(v) {
      x       <- data[[v]]
      x_valid <- x[!is.na(x)]
      if (length(x_valid) < 2) return(data.frame(year = integer(), value = numeric()))
      m   <- mean(x_valid)
      s   <- sd(x_valid)
      flag <- !is.na(x) & abs(x - m) > .OUTLIER_SD * s
      data.frame(year = data$year[flag], value = round(x[flag], 4))
    })
    names(value$outliers) <- var_cols
  }

  # ── crosscorr ─────────────────────────────────────────────────────────────

  if ("crosscorr" %in% sections) {
    pairs <- combn(var_cols, 2, simplify = FALSE)
    value$crosscorr <- lapply(pairs, function(pair) {
      x      <- data[[pair[1]]]
      y      <- data[[pair[2]]]
      both   <- !is.na(x) & !is.na(y)
      n_both <- sum(both)
      if (n_both < .CCF_MIN_N)
        return(data.frame(lag = integer(0), ccf = numeric(0),
                          ci  = numeric(0),
                          note = paste0("n < ", .CCF_MIN_N, " — CCF not computed"),
                          stringsAsFactors = FALSE))
      cc <- stats::ccf(x[both], y[both], lag.max = .CCF_LAG_MAX, plot = FALSE)
      ci <- 1.96 / sqrt(n_both)
      data.frame(
        lag  = as.integer(cc$lag),
        ccf  = round(as.numeric(cc$acf), 4),
        ci   = round(ci, 4),
        note = paste0("ccf(", pair[1], ", ", pair[2],
                      ") — negative lag: ", pair[1], " leads ", pair[2]),
        stringsAsFactors = FALSE
      )
    })
    names(value$crosscorr) <- sapply(pairs, function(p) paste(p, collapse = " ~ "))
  }

  # ── Return ─────────────────────────────────────────────────────────────────

  list(
    value  = value,
    method = paste0(
      "EDA on aligned annual time series. Sections: ",
      paste(sections, collapse = ", "), ". ",
      "summary: mean/median/sd/min/max/skewness (moment estimator). ",
      "normality: Shapiro-Wilk (informative only). ",
      "trends: Mann-Kendall with Hamed & Rao (1998) autocorrelation correction ",
      "(modifiedmk::mmkh), alpha=", .TREND_ALPHA, ". ",
      "correlations: Spearman rank, pairwise complete obs (cor.test exact=FALSE). ",
      "outliers: |value - mean| > ", .OUTLIER_SD, " SD. ",
      "crosscorr: stats::ccf(), lag.max=", .CCF_LAG_MAX,
      ", CI=±1.96/sqrt(n), negative lag means first variable leads."
    ),
    params = list(
      sections_computed  = sections,
      variables          = var_cols,
      n_years            = nrow(data),
      outlier_sd         = .OUTLIER_SD,
      trend_alpha        = .TREND_ALPHA,
      correlation_method = "spearman",
      ccf_lag_max        = .CCF_LAG_MAX
    )
  )
}
