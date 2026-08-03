# plot_eda.R — plotting helper for eda-multivariate
#
# Takes the output of run_skill() and generates figures for the Stage [5]
# Render step. Each function returns a ggplot object — the caller decides
# whether to print, save, or embed it in a report.
#
# Dependencies: ggplot2, ggcorrplot (for correlation heatmap)
# All functions are defensive: they check that the required section was
# computed before attempting to plot.

# ── helpers ───────────────────────────────────────────────────────────────────

.require_section <- function(eda_value, section) {
  if (is.null(eda_value[[section]]))
    stop("Section '", section, "' not found in EDA output. ",
         "Re-run run_skill() with sections including '", section, "'.")
}

.require_pkg <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE))
    stop("Package '", pkg, "' is required. Install with: install.packages('", pkg, "')")
}

# ── plot_series(data, eda_output) ────────────────────────────────────────────
# Time series line plot for all variables, one panel per variable (faceted).
# Outlier years are highlighted as red points if "outliers" section is present.
#
# Args:
#   data       — the aligned data.frame (temporal_align()$aligned)
#   eda_output — list returned by run_skill()

plot_series <- function(data, eda_output) {
  .require_pkg("ggplot2")

  var_cols <- eda_output$params$variables

  # Pivot to long format (base R, no tidyr dependency)
  long <- do.call(rbind, lapply(var_cols, function(v) {
    data.frame(year = data$year, variable = v, value = data[[v]],
               stringsAsFactors = FALSE)
  }))

  # Outlier flags
  outlier_pts <- NULL
  if (!is.null(eda_output$value$outliers)) {
    outlier_pts <- do.call(rbind, lapply(var_cols, function(v) {
      df <- eda_output$value$outliers[[v]]
      if (nrow(df) == 0) return(NULL)
      data.frame(year = df$year, variable = v, value = df$value,
                 stringsAsFactors = FALSE)
    }))
  }

  p <- ggplot2::ggplot(long, ggplot2::aes(x = year, y = value)) +
    ggplot2::geom_line(colour = "#2166ac", linewidth = 0.7, na.rm = TRUE) +
    ggplot2::geom_point(colour = "#2166ac", size = 1.5, na.rm = TRUE) +
    ggplot2::facet_wrap(~ variable, scales = "free_y", ncol = 2) +
    ggplot2::labs(title = "Annual time series — all variables",
                  x = "Year", y = NULL) +
    ggplot2::theme_bw(base_size = 11) +
    ggplot2::theme(strip.text = ggplot2::element_text(size = 9))

  if (!is.null(outlier_pts) && nrow(outlier_pts) > 0) {
    p <- p + ggplot2::geom_point(
      data    = outlier_pts,
      mapping = ggplot2::aes(x = year, y = value),
      colour  = "#d73027", size = 3, shape = 17
    ) +
      ggplot2::labs(caption = "Red triangles: values > 2 SD from variable mean")
  }

  p
}

# ── plot_correlations(eda_output) ─────────────────────────────────────────────
# Heatmap of Spearman correlation matrix.
# Cells are coloured by rho; non-significant pairs (p > 0.05) are crossed out.
#
# Args:
#   eda_output — list returned by run_skill()
#   alpha      — significance threshold for crossing out non-significant pairs

plot_correlations <- function(eda_output, alpha = 0.05) {
  .require_section(eda_output$value, "correlations")
  .require_pkg("ggplot2")

  rho_mat <- as.matrix(eda_output$value$correlations$rho)
  p_mat   <- as.matrix(eda_output$value$correlations$p_value)
  vars    <- rownames(rho_mat)
  n       <- length(vars)

  # Build long data.frame for ggplot
  rows <- do.call(rbind, lapply(seq_len(n), function(i) {
    lapply(seq_len(n), function(j) {
      data.frame(var1 = vars[i], var2 = vars[j],
                 rho  = rho_mat[i, j],
                 sig  = !is.na(p_mat[i, j]) && p_mat[i, j] <= alpha,
                 stringsAsFactors = FALSE)
    })
  }))
  long <- do.call(rbind, unlist(rows, recursive = FALSE))
  long$var1 <- factor(long$var1, levels = rev(vars))
  long$var2 <- factor(long$var2, levels = vars)

  p <- ggplot2::ggplot(long, ggplot2::aes(x = var2, y = var1, fill = rho)) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.5) +
    ggplot2::scale_fill_gradient2(
      low = "#d73027", mid = "white", high = "#1a9641",
      midpoint = 0, limits = c(-1, 1),
      name = "Spearman\nrho"
    ) +
    ggplot2::geom_text(
      data    = long[!long$sig & long$var1 != long$var2, ],
      mapping = ggplot2::aes(label = "×"),
      colour  = "#555555", size = 4
    ) +
    ggplot2::geom_text(
      data    = long[long$var1 != long$var2 & long$sig, ],
      mapping = ggplot2::aes(label = round(rho, 2)),
      colour  = "black", size = 3
    ) +
    ggplot2::labs(
      title   = "Spearman correlation matrix",
      caption = paste0("× = non-significant (p > ", alpha,
                       "). Numbers shown for significant pairs only."),
      x = NULL, y = NULL
    ) +
    ggplot2::theme_bw(base_size = 11) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 35, hjust = 1))

  p
}

# ── plot_trends(eda_output) ───────────────────────────────────────────────────
# Bar chart of Mann-Kendall tau per variable, coloured by trend direction.
# Significant trends (Hamed & Rao corrected p ≤ 0.05) are filled; non-
# significant ones are shown with reduced opacity.
#
# Args:
#   eda_output — list returned by run_skill()

plot_trends <- function(eda_output) {
  .require_section(eda_output$value, "trends")
  .require_pkg("ggplot2")

  df <- eda_output$value$trends
  df <- df[!is.na(df$tau), ]

  if (nrow(df) == 0) {
    message("No variables had sufficient data for Mann-Kendall test.")
    return(invisible(NULL))
  }

  df$colour <- ifelse(df$tau > 0, "#1a9641", "#d73027")
  df$alpha  <- ifelse(df$p_value_hr <= eda_output$params$trend_alpha, 1, 0.35)
  df$variable <- factor(df$variable, levels = df$variable[order(df$tau)])

  p <- ggplot2::ggplot(df, ggplot2::aes(x = variable, y = tau, fill = colour,
                                        alpha = alpha)) +
    ggplot2::geom_col(width = 0.6) +
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.4) +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_alpha_identity() +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title   = "Mann-Kendall tau per variable",
      subtitle = paste0("Hamed & Rao (1998) autocorrelation correction. ",
                        "Faded bars: p > ", eda_output$params$trend_alpha, "."),
      x = NULL, y = "Kendall tau"
    ) +
    ggplot2::theme_bw(base_size = 11)

  p
}

# ── plot_distributions(data, eda_output) ──────────────────────────────────────
# Histogram + density overlay per variable (faceted).
#
# Args:
#   data       — the aligned data.frame (temporal_align()$aligned)
#   eda_output — list returned by run_skill()

plot_distributions <- function(data, eda_output) {
  .require_pkg("ggplot2")

  var_cols <- eda_output$params$variables

  long <- do.call(rbind, lapply(var_cols, function(v) {
    vals <- data[[v]]
    data.frame(variable = v, value = vals[!is.na(vals)],
               stringsAsFactors = FALSE)
  }))

  ggplot2::ggplot(long, ggplot2::aes(x = value)) +
    ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)),
                            bins = 10, fill = "#2166ac", alpha = 0.5,
                            colour = "white") +
    ggplot2::geom_density(colour = "#d73027", linewidth = 0.8) +
    ggplot2::facet_wrap(~ variable, scales = "free", ncol = 2) +
    ggplot2::labs(title = "Variable distributions",
                  x = NULL, y = "Density") +
    ggplot2::theme_bw(base_size = 11) +
    ggplot2::theme(strip.text = ggplot2::element_text(size = 9))
}

# ── plot_ccf(eda_output) ──────────────────────────────────────────────────────
# Bar chart of cross-correlation (CCF) between all variable pairs, faceted by
# pair. Dashed lines mark the 95% CI (±1.96/√n). Negative lag means the first
# variable (left of "~") leads the second.
#
# Args:
#   eda_output — list returned by run_skill()

plot_ccf <- function(eda_output) {
  .require_section(eda_output$value, "crosscorr")
  .require_pkg("ggplot2")

  cc_list <- eda_output$value$crosscorr

  long <- do.call(rbind, lapply(names(cc_list), function(pair) {
    df <- cc_list[[pair]]
    if (nrow(df) == 0) return(NULL)
    data.frame(pair = pair, lag = df$lag, ccf = df$ccf, ci = df$ci[1L],
               stringsAsFactors = FALSE)
  }))

  if (is.null(long) || nrow(long) == 0) {
    message("No variable pairs had sufficient data for CCF.")
    return(invisible(NULL))
  }

  ggplot2::ggplot(long, ggplot2::aes(x = lag, y = ccf)) +
    ggplot2::geom_col(fill = "#2166ac", width = 0.4, na.rm = TRUE) +
    ggplot2::geom_hline(ggplot2::aes(yintercept =  ci),
                        linetype = "dashed", colour = "#d73027", linewidth = 0.5) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = -ci),
                        linetype = "dashed", colour = "#d73027", linewidth = 0.5) +
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.3) +
    ggplot2::facet_wrap(~ pair, scales = "free_y", ncol = 2) +
    ggplot2::scale_x_continuous(breaks = seq(-.CCF_LAG_MAX, .CCF_LAG_MAX, 1)) +
    ggplot2::labs(
      title    = "Cross-correlation function (CCF) — all variable pairs",
      subtitle = "Dashed lines: 95% CI (\u00b11.96/\u221an). Negative lag: left variable leads right.",
      x = "Lag (years)", y = "Cross-correlation"
    ) +
    ggplot2::theme_bw(base_size = 11) +
    ggplot2::theme(strip.text = ggplot2::element_text(size = 8))
}
