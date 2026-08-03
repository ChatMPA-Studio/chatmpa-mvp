# skill.R — model-diagnostics
#
# Computes standard OLS regression diagnostics from the pre-extracted residuals
# and fitted values returned by reef-drivers-lm. Does NOT require the lm object
# or the original data.
#
# Sections:
#   "normality"          — Shapiro-Wilk test on residuals (informative)
#   "durbin_watson"      — DW statistic + lag-1 autocorrelation of residuals
#   "acf_residuals"      — ACF of residuals at lags 1–3
#   "pacf_residuals"     — PACF of residuals at lags 1–3
#   "heteroscedasticity" — Score test: e² ~ ŷ, LM stat ~ χ²(1)
#
# See SKILL.md for the full data contract and do-not rules.

# ── Constants ──────────────────────────────────────────────────────────────────

.VALID_DIAG      <- c("normality", "durbin_watson", "acf_residuals",
                       "pacf_residuals", "heteroscedasticity")
.RESID_LAG_MAX   <- 3L
.DW_LOW          <- 1.5   # heuristic lower bound (positive autocorrelation concern)
.DW_HIGH         <- 2.5   # heuristic upper bound (negative autocorrelation concern)

# ── run_skill ──────────────────────────────────────────────────────────────────
#
# Args:
#   lm_output   list — complete output of reef-drivers-lm$run_skill()
#               required: lm_output$value$fitted  (data.frame: year, nrsi_obs,
#                                                   nrsi_fit, residual)
#                         lm_output$value$n_years_used  (integer)
#                         lm_output$value$formula_used  (character)
#                         lm_output$value$species       (character vector)
#   sections    character vector — subset of .VALID_DIAG to compute
#               default: all sections
#
# Returns: list(value, method, params)

run_skill <- function(lm_output,
                      sections = .VALID_DIAG,
                      ...) {

  # ── Input validation ──────────────────────────────────────────────────────

  if (!is.list(lm_output) || is.null(lm_output$value))
    stop("'lm_output' must be the list returned by reef-drivers-lm run_skill().")

  required_value <- c("fitted", "n_years_used", "formula_used", "species")
  missing_val <- setdiff(required_value, names(lm_output$value))
  if (length(missing_val) > 0)
    stop("lm_output$value missing: ", paste(missing_val, collapse = ", "))

  fitted_df <- lm_output$value$fitted
  required_cols <- c("year", "nrsi_obs", "nrsi_fit", "residual")
  missing_cols <- setdiff(required_cols, names(fitted_df))
  if (length(missing_cols) > 0)
    stop("lm_output$value$fitted missing columns: ",
         paste(missing_cols, collapse = ", "))

  unknown <- setdiff(sections, .VALID_DIAG)
  if (length(unknown) > 0)
    stop("Unknown sections: ", paste(unknown, collapse = ", "),
         ". Valid options: ", paste(.VALID_DIAG, collapse = ", "))

  # Sort chronologically — DW and ACF depend on order
  fitted_df <- fitted_df[order(fitted_df$year), ]

  e   <- fitted_df$residual      # residuals in time order
  yhat <- fitted_df$nrsi_fit     # fitted values
  n   <- length(e)

  if (n < 3)
    stop("n_years_used = ", n, ". At least 3 observations required for diagnostics.")

  value <- list()

  # ── normality ───────────────────────────────────────────────────────────

  if ("normality" %in% sections) {
    if (n < 3) {
      value$normality <- list(W = NA, p_value = NA,
                              note = "n < 3 — Shapiro-Wilk not run.")
    } else {
      sw <- shapiro.test(e)
      value$normality <- list(
        W       = round(sw$statistic, 4),
        p_value = round(sw$p.value, 4),
        note    = paste0(
          "Shapiro-Wilk on residuals (informative only). ",
          "Temporal autocorrelation in residuals may affect the W statistic. ",
          "Do not use as a gate on model adequacy."
        )
      )
    }
  }

  # ── durbin_watson ────────────────────────────────────────────────────────

  if ("durbin_watson" %in% sections) {
    dw <- sum(diff(e)^2) / sum(e^2)
    r1 <- if (n >= 3) cor(e[-n], e[-1]) else NA_real_

    interp <- if (is.na(dw)) {
      "Could not compute."
    } else if (dw < .DW_LOW) {
      paste0("DW = ", round(dw, 3), " < ", .DW_LOW,
             " — positive autocorrelation in residuals (concern).")
    } else if (dw > .DW_HIGH) {
      paste0("DW = ", round(dw, 3), " > ", .DW_HIGH,
             " — negative autocorrelation in residuals (concern).")
    } else {
      paste0("DW = ", round(dw, 3), " — within heuristic range [",
             .DW_LOW, ", ", .DW_HIGH, "] (no strong autocorrelation signal).")
    }

    value$durbin_watson <- list(
      dw             = round(dw, 4),
      r1             = round(r1, 4),
      interpretation = interp,
      note           = paste0(
        "DW = sum(diff(e)^2) / sum(e^2). r1 = lag-1 autocorrelation of residuals. ",
        "DW ≈ 2*(1 - r1). Formal critical values depend on n and k — ",
        "heuristic thresholds used here (< ", .DW_LOW, " / > ", .DW_HIGH, ")."
      )
    )
  }

  # ── acf_residuals ────────────────────────────────────────────────────────

  if ("acf_residuals" %in% sections) {
    lag_max_acf <- min(.RESID_LAG_MAX, n - 1L)
    ac <- stats::acf(e, lag.max = lag_max_acf, plot = FALSE)
    ci <- 1.96 / sqrt(n)
    # Drop lag 0 (always 1.0)
    lags_out <- as.integer(ac$lag[-1])
    acf_out  <- round(as.numeric(ac$acf[-1]), 4)
    value$acf_residuals <- data.frame(
      lag  = lags_out,
      acf  = acf_out,
      ci   = round(ci, 4),
      stringsAsFactors = FALSE
    )
  }

  # ── pacf_residuals ───────────────────────────────────────────────────────

  if ("pacf_residuals" %in% sections) {
    if (n < 4) {
      value$pacf_residuals <- data.frame(lag = integer(0), pacf = numeric(0),
                                          ci = numeric(0))
    } else {
      lag_max_pacf <- min(.RESID_LAG_MAX, n - 2L)
      pc <- stats::pacf(e, lag.max = lag_max_pacf, plot = FALSE)
      ci <- 1.96 / sqrt(n)
      value$pacf_residuals <- data.frame(
        lag  = as.integer(pc$lag),
        pacf = round(as.numeric(pc$acf), 4),
        ci   = round(ci, 4),
        stringsAsFactors = FALSE
      )
    }
  }

  # ── heteroscedasticity ───────────────────────────────────────────────────
  # Score test: regress e² ~ ŷ; LM = n * R² ~ χ²(1) under H0

  if ("heteroscedasticity" %in% sections) {
    e2   <- e^2
    aux  <- lm(e2 ~ yhat)
    r2   <- summary(aux)$r.squared
    lm_stat <- n * r2
    p_val   <- pchisq(lm_stat, df = 1, lower.tail = FALSE)

    value$heteroscedasticity <- list(
      lm_stat = round(lm_stat, 4),
      p_value = round(p_val, 4),
      note    = paste0(
        "Score test: auxiliary regression e² ~ ŷ (fitted values). ",
        "LM = n × R² ~ χ²(1) under H₀ (constant variance). ",
        "Simplified White-type test — does not require original predictors. ",
        "Small p-value suggests heteroscedasticity."
      )
    )
  }

  # ── Return ────────────────────────────────────────────────────────────────

  list(
    value = value,
    method = paste0(
      "OLS regression diagnostics derived from pre-extracted residuals and ",
      "fitted values (no lm object required). ",
      "normality: Shapiro-Wilk on residuals (informative only). ",
      "durbin_watson: DW = sum(diff(e)^2)/sum(e^2); r1 = lag-1 autocorrelation. ",
      "acf_residuals/pacf_residuals: stats::acf/pacf(), lag.max=", .RESID_LAG_MAX,
      ", CI=±1.96/sqrt(n). ",
      "heteroscedasticity: score test e² ~ ŷ, LM=n*R² ~ χ²(1)."
    ),
    params = list(
      sections_computed = sections,
      formula_used      = lm_output$value$formula_used,
      species           = lm_output$value$species,
      n_obs             = n,
      resid_lag_max     = .RESID_LAG_MAX,
      dw_thresholds     = c(low = .DW_LOW, high = .DW_HIGH)
    )
  )
}
