# plot_diagnostics.R — plotting helper for model-diagnostics
#
# Takes the output of run_skill() and the lm_output$value$fitted table, and
# generates diagnostic figures. Each function returns a ggplot object.
#
# Dependencies: ggplot2
# All functions are defensive: they check that the required section was computed.

# ── helpers ───────────────────────────────────────────────────────────────────

.require_diag_section <- function(diag_value, section) {
  if (is.null(diag_value[[section]]))
    stop("Section '", section, "' not found in diagnostics output. ",
         "Re-run run_skill() with sections including '", section, "'.")
}

.require_diag_pkg <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE))
    stop("Package '", pkg, "' is required. Install with: install.packages('", pkg, "')")
}

# ── plot_residuals_vs_fitted(lm_output, diag_output) ─────────────────────────
# Scatter of residuals vs fitted values with a loess smooth and a reference
# line at zero. Patterns (curvature, funnel shape) indicate model problems.
#
# Args:
#   lm_output   — list returned by reef-drivers-lm run_skill()
#   diag_output — list returned by model-diagnostics run_skill() (for params)

plot_residuals_vs_fitted <- function(lm_output, diag_output) {
  .require_diag_pkg("ggplot2")

  df <- lm_output$value$fitted

  ggplot2::ggplot(df, ggplot2::aes(x = nrsi_fit, y = residual)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
                        colour = "#888888", linewidth = 0.5) +
    ggplot2::geom_point(colour = "#2166ac", size = 2.5) +
    ggplot2::geom_smooth(method = "loess", formula = y ~ x,
                         colour = "#d73027", linewidth = 0.8,
                         se = FALSE) +
    ggplot2::geom_text(ggplot2::aes(label = year),
                       vjust = -0.7, size = 3, colour = "#555555") +
    ggplot2::labs(
      title   = "Residuals vs fitted values",
      subtitle = "Loess smooth in red. Patterns suggest model misspecification.",
      x = "Fitted NRSI", y = "Residual"
    ) +
    ggplot2::theme_bw(base_size = 11)
}

# ── plot_qqnorm(lm_output) ────────────────────────────────────────────────────
# Q-Q plot of standardised residuals against theoretical normal quantiles.
# Points should fall close to the diagonal if residuals are normal.
#
# Args:
#   lm_output — list returned by reef-drivers-lm run_skill()

plot_qqnorm <- function(lm_output) {
  .require_diag_pkg("ggplot2")

  e     <- lm_output$value$fitted$residual
  n     <- length(e)
  std_e <- (e - mean(e)) / sd(e)

  # Theoretical quantiles
  probs    <- (rank(std_e) - 0.5) / n
  theo_q   <- qnorm(probs)
  df_qq    <- data.frame(theoretical = theo_q, sample = sort(std_e))

  # Reference line through Q1 and Q3
  q_samp  <- quantile(std_e, c(0.25, 0.75))
  q_theo  <- qnorm(c(0.25, 0.75))
  slope   <- diff(q_samp) / diff(q_theo)
  intcpt  <- q_samp[1] - slope * q_theo[1]

  ggplot2::ggplot(df_qq, ggplot2::aes(x = theoretical, y = sample)) +
    ggplot2::geom_abline(slope = slope, intercept = intcpt,
                         colour = "#d73027", linewidth = 0.8) +
    ggplot2::geom_point(colour = "#2166ac", size = 2.5) +
    ggplot2::labs(
      title    = "Normal Q-Q plot of standardised residuals",
      subtitle = "Points should follow the red line if residuals are normal.",
      x = "Theoretical quantiles", y = "Standardised residuals"
    ) +
    ggplot2::theme_bw(base_size = 11)
}

# ── plot_acf_residuals(diag_output) ──────────────────────────────────────────
# Bar chart of ACF and PACF of residuals, side by side (two panels).
#
# Args:
#   diag_output — list returned by model-diagnostics run_skill()

plot_acf_residuals <- function(diag_output) {
  .require_diag_section(diag_output$value, "acf_residuals")
  .require_diag_section(diag_output$value, "pacf_residuals")
  .require_diag_pkg("ggplot2")

  acf_df  <- diag_output$value$acf_residuals
  pacf_df <- diag_output$value$pacf_residuals

  acf_df$type  <- "ACF"
  pacf_df$type <- "PACF"
  names(pacf_df)[names(pacf_df) == "pacf"] <- "acf"

  long <- rbind(acf_df, pacf_df)
  long$type <- factor(long$type, levels = c("ACF", "PACF"))

  ggplot2::ggplot(long, ggplot2::aes(x = lag, y = acf)) +
    ggplot2::geom_col(fill = "#2166ac", width = 0.4) +
    ggplot2::geom_hline(ggplot2::aes(yintercept =  ci),
                        linetype = "dashed", colour = "#d73027", linewidth = 0.5) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = -ci),
                        linetype = "dashed", colour = "#d73027", linewidth = 0.5) +
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.3) +
    ggplot2::facet_wrap(~ type, ncol = 2, scales = "free_y") +
    ggplot2::scale_x_continuous(breaks = seq_len(max(long$lag))) +
    ggplot2::labs(
      title    = "ACF and PACF of residuals",
      subtitle = "Dashed lines: 95% CI (\u00b11.96/\u221an). Bars outside CI suggest autocorrelation.",
      x = "Lag (years)", y = NULL
    ) +
    ggplot2::theme_bw(base_size = 11) +
    ggplot2::theme(strip.text = ggplot2::element_text(size = 10, face = "bold"))
}

# ── plot_obs_vs_fit(lm_output) ────────────────────────────────────────────────
# Observed vs fitted NRSI over time — useful to see how well the model tracks
# the annual trajectory.
#
# Args:
#   lm_output — list returned by reef-drivers-lm run_skill()

plot_obs_vs_fit <- function(lm_output) {
  .require_diag_pkg("ggplot2")

  df <- lm_output$value$fitted

  long <- rbind(
    data.frame(year = df$year, value = df$nrsi_obs, series = "Observed",
               stringsAsFactors = FALSE),
    data.frame(year = df$year, value = df$nrsi_fit, series = "Fitted",
               stringsAsFactors = FALSE)
  )
  long$series <- factor(long$series, levels = c("Observed", "Fitted"))

  ggplot2::ggplot(long, ggplot2::aes(x = year, y = value,
                                      colour = series, linetype = series)) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::geom_point(size = 2) +
    ggplot2::scale_colour_manual(values = c("Observed" = "#2166ac",
                                             "Fitted"   = "#d73027")) +
    ggplot2::scale_linetype_manual(values = c("Observed" = "solid",
                                               "Fitted"   = "dashed")) +
    ggplot2::labs(
      title   = "Observed vs fitted NRSI",
      x = "Year", y = "NRSI",
      colour = NULL, linetype = NULL
    ) +
    ggplot2::theme_bw(base_size = 11) +
    ggplot2::theme(legend.position = "bottom")
}
