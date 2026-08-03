# skill.R — results-report
#
# Transforms the validated output of a fitting skill (e.g. reef-drivers-lm)
# and, optionally, model-diagnostics into a structured statistical report.
# Produces CONTENT (numbers, flags, provenance); Stage [5] of ORCHESTRATION.md
# produces FORMAT (markdown, DOCX, figures).
#
# No statistical tests are run here. All quantities are extracted or computed
# algebraically from the inputs.
#
# See SKILL.md for the full data contract and do-not rules.

# ── Constants ──────────────────────────────────────────────────────────────────

.VALID_SECTIONS <- c("coefficients", "fit_summary", "data_summary",
                     "diagnostics_flags")
.CI_LEVEL       <- 0.95          # 95% confidence intervals
.VIF_WARN       <- 5             # collinearity flag threshold
.DW_LOW         <- 1.5           # DW heuristic lower bound
.DW_HIGH        <- 2.5           # DW heuristic upper bound
.HETERO_ALPHA   <- 0.05          # White-type test significance level

# ── run_skill ──────────────────────────────────────────────────────────────────
#
# Args:
#   model_output        list — complete output of a fitting skill (e.g.
#                       reef-drivers-lm$run_skill()). Must contain $value,
#                       $method, $params. See SKILL.md for required fields.
#   diagnostics_output  list or NULL — complete output of model-diagnostics
#                       run_skill(). If NULL, diagnostics_flags section
#                       returns NULL.
#   sections            character vector — subset of .VALID_SECTIONS to include
#                       in output. Default: all sections.
#
# Returns: list(value, method, params)

run_skill <- function(model_output,
                      diagnostics_output = NULL,
                      sections = .VALID_SECTIONS,
                      ...) {

  # ── Input validation ──────────────────────────────────────────────────────

  if (!is.list(model_output) || is.null(model_output$value))
    stop("'model_output' must be the list returned by a fitting skill's ",
         "run_skill() — must contain $value, $method, $params.")

  required_value <- c("coefficients", "r_squared", "r_squared_adj",
                      "f_statistic", "f_p_value", "vif", "fitted",
                      "n_years_used", "years_excluded", "formula_used")
  missing_val <- setdiff(required_value, names(model_output$value))
  if (length(missing_val) > 0)
    stop("model_output$value missing required fields: ",
         paste(missing_val, collapse = ", "))

  coef_df <- model_output$value$coefficients
  required_coef_cols <- c("predictor", "estimate", "std_error",
                           "t_value", "p_value")
  missing_coef <- setdiff(required_coef_cols, names(coef_df))
  if (length(missing_coef) > 0)
    stop("model_output$value$coefficients missing columns: ",
         paste(missing_coef, collapse = ", "))

  fitted_df <- model_output$value$fitted
  required_fit_cols <- c("year", "nrsi_obs", "nrsi_fit", "residual")
  missing_fit <- setdiff(required_fit_cols, names(fitted_df))
  if (length(missing_fit) > 0)
    stop("model_output$value$fitted missing columns: ",
         paste(missing_fit, collapse = ", "))

  if (!is.null(diagnostics_output)) {
    if (!is.list(diagnostics_output) || is.null(diagnostics_output$value))
      stop("'diagnostics_output', if provided, must be the list returned by ",
           "model-diagnostics run_skill().")
  }

  unknown <- setdiff(sections, .VALID_SECTIONS)
  if (length(unknown) > 0)
    stop("Unknown sections: ", paste(unknown, collapse = ", "),
         ". Valid options: ", paste(.VALID_SECTIONS, collapse = ", "))

  # ── Derived quantities shared across sections ─────────────────────────────

  val       <- model_output$value
  n         <- val$n_years_used
  n_coefs   <- nrow(coef_df)          # includes intercept
  df_res    <- n - n_coefs

  residuals_vec <- fitted_df$residual
  rss           <- sum(residuals_vec^2)

  value <- list()

  # ── coefficients ──────────────────────────────────────────────────────────
  # Reproduces the fitting skill's table + 95% CIs via t-distribution

  if ("coefficients" %in% sections) {
    t_crit  <- qt((1 + .CI_LEVEL) / 2, df = df_res)
    coef_out <- coef_df

    coef_out$estimate  <- round(coef_out$estimate,  4)
    coef_out$std_error <- round(coef_out$std_error, 4)
    coef_out$t_value   <- round(coef_out$t_value,   4)
    coef_out$p_value   <- round(coef_out$p_value,   4)

    coef_out$ci_lower_95 <- round(coef_df$estimate - t_crit * coef_df$std_error, 4)
    coef_out$ci_upper_95 <- round(coef_df$estimate + t_crit * coef_df$std_error, 4)

    value$coefficients <- coef_out[, c("predictor", "estimate", "std_error",
                                        "t_value", "p_value",
                                        "ci_lower_95", "ci_upper_95")]
  }

  # ── fit_summary ───────────────────────────────────────────────────────────
  # Scalars extracted from fitting skill + AIC/BIC/RMSE computed here.
  # AIC/BIC formula matches R's stats::AIC for lm objects (Gaussian MLE):
  #   neg2loglik = n + n*log(2*pi*RSS/n)
  #   k          = n_coefs + 1  (regression coefs + estimated sigma)
  #   AIC = neg2loglik + 2*k
  #   BIC = neg2loglik + log(n)*k

  if ("fit_summary" %in% sections) {
    rmse       <- sqrt(mean(residuals_vec^2))
    k_info     <- n_coefs + 1L
    neg2loglik <- n + n * log(2 * pi * rss / n)
    aic        <- neg2loglik + 2 * k_info
    bic        <- neg2loglik + log(n) * k_info

    value$fit_summary <- list(
      r_squared     = val$r_squared,
      r_squared_adj = val$r_squared_adj,
      f_statistic   = val$f_statistic,
      f_p_value     = val$f_p_value,
      aic           = round(aic, 3),
      bic           = round(bic, 3),
      rmse          = round(rmse, 4),
      n_obs         = n,
      df_residual   = df_res
    )
  }

  # ── data_summary ──────────────────────────────────────────────────────────
  # Provenance fields from the fitting skill + VIF table with flag_high column

  if ("data_summary" %in% sections) {
    vif_df <- val$vif
    if (!is.null(vif_df) && nrow(vif_df) > 0) {
      vif_df$flag_high <- vif_df$vif > .VIF_WARN
    } else {
      # Ensure flag_high column exists even if VIF was not computed
      if (is.null(vif_df) || nrow(vif_df) == 0)
        vif_df <- data.frame(predictor = character(), vif = numeric(),
                             flag_high = logical(), stringsAsFactors = FALSE)
    }

    value$data_summary <- list(
      formula_used   = val$formula_used,
      n_years_used   = val$n_years_used,
      years_excluded = if (!is.null(val$years_excluded)) val$years_excluded
                       else integer(0),
      vif_table      = vif_df
    )
  }

  # ── diagnostics_flags ─────────────────────────────────────────────────────
  # Flags from model-diagnostics output. NULL if diagnostics_output not provided.
  # Flags are observations, not conclusions about model adequacy.

  if ("diagnostics_flags" %in% sections) {
    if (is.null(diagnostics_output)) {
      value$diagnostics_flags <- NULL
    } else {
      diag_val <- diagnostics_output$value

      # autocorrelation: DW heuristic (< DW_LOW or > DW_HIGH)
      dw <- if (!is.null(diag_val$durbin_watson)) diag_val$durbin_watson$dw
            else NA_real_
      ac_flag <- if (!is.na(dw)) dw < .DW_LOW || dw > .DW_HIGH else NA

      # heteroscedasticity: White-type test p-value
      hetero_p <- if (!is.null(diag_val$heteroscedasticity))
                    diag_val$heteroscedasticity$p_value
                  else NA_real_
      hetero_flag <- if (!is.na(hetero_p)) hetero_p < .HETERO_ALPHA else NA

      # high-VIF predictors (from fitting skill, not diagnostics)
      high_vif_preds <- if (!is.null(val$vif) && nrow(val$vif) > 0)
                          val$vif$predictor[val$vif$vif > .VIF_WARN]
                        else character(0)

      value$diagnostics_flags <- list(
        autocorrelation_flag    = ac_flag,
        dw_value                = if (!is.na(dw)) round(dw, 4) else NA_real_,
        heteroscedasticity_flag = hetero_flag,
        hetero_p_value          = if (!is.na(hetero_p)) round(hetero_p, 4)
                                  else NA_real_,
        high_vif_predictors     = high_vif_preds,
        note = paste0(
          "Flags are observations from model-diagnostics, not conclusions. ",
          "autocorrelation_flag: DW outside [", .DW_LOW, ", ", .DW_HIGH,
          "] (heuristic — formal critical values not computed). ",
          "heteroscedasticity_flag: simplified White-type test p < ",
          .HETERO_ALPHA, ". ",
          "Consult model-diagnostics output for full diagnostic details."
        )
      )
    }
  }

  # ── Return ────────────────────────────────────────────────────────────────

  diag_note <- if ("diagnostics_flags" %in% sections && is.null(diagnostics_output))
    " diagnostics_flags = NULL (diagnostics_output not provided)."
  else ""

  list(
    value = value,
    method = paste0(
      "Structured extraction from fitting skill output. No statistical tests ",
      "performed in this skill. ",
      "coefficients: estimate ± qt(", (1 + .CI_LEVEL) / 2,
      ", df=", df_res, ") × SE for 95% CI. ",
      "AIC/BIC: Gaussian MLE log-likelihood formula (matches R stats::AIC for lm). ",
      "RMSE: sqrt(mean(residual²)). ",
      "diagnostics_flags: DW heuristic thresholds [", .DW_LOW, ", ", .DW_HIGH,
      "]; White-type test α = ", .HETERO_ALPHA, ".",
      diag_note
    ),
    params = list(
      sections_computed = sections,
      model_method      = model_output$method,
      formula_used      = val$formula_used,
      n_obs             = n,
      df_residual       = df_res,
      ci_level          = .CI_LEVEL,
      vif_threshold     = .VIF_WARN,
      dw_thresholds     = c(low = .DW_LOW, high = .DW_HIGH),
      hetero_alpha      = .HETERO_ALPHA
    )
  )
}
