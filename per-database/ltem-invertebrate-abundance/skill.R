# skill.R — ltem-invertebrate-abundance
#
# Fits a GAM (quantity ~ s(year) + s(reef, bs="re"), REML) independently
# for each of 4 focal taxa, and computes observed mean KPIs.
#
# See SKILL.md for the full data contract, method, and do-not rules.
# Reference: ltem_report/workshops/datamares/2026/generate_fig_invertebrates.R

# ── Constants (fixed — do not change without updating SKILL.md) ──────────────

.K_MAX         <- 10L   # maximum smoothing knots for s(year)
.KPI_YEARS     <- 5L    # number of most recent survey years for KPI mean
.GAM_MIN_YEARS <- 5L    # minimum unique years to fit the GAM per taxon
.GRID_POINTS   <- 200L  # prediction grid resolution over year range

# Focal taxa — fixed (do not modify)
.FOCAL_TAXA <- c("Echinoidea", "Asteroidea", "Holaxonia", "Scleractinia")

# ── .gam_trend_taxon ──────────────────────────────────────────────────────────
# Internal helper: fit GAM for one taxon's reef-year data.
# Mirrors generate_fig_invertebrates.R GAM block.

.gam_trend_taxon <- function(df) {
  df$reef <- factor(df$reef)
  n_years  <- length(unique(df$time))
  k_use    <- min(.K_MAX, n_years - 1L)

  fit <- tryCatch(
    mgcv::gam(value ~ s(time, k = k_use) + s(reef, bs = "re"),
              data = df, family = gaussian(), method = "REML"),
    error = function(e) {
      stop("GAM fitting failed: ", conditionMessage(e))
    }
  )

  yr_range <- range(df$time)
  grid     <- data.frame(
    time = seq(yr_range[1], yr_range[2], length.out = .GRID_POINTS),
    reef = df$reef[1]
  )

  pred <- predict(fit, newdata = grid, se.fit = TRUE,
                  exclude = "s(reef)", newdata.guaranteed = TRUE)

  list(
    dev_expl  = round(summary(fit)$dev.expl * 100, 1),
    k_used    = k_use,
    n_years   = n_years,
    n_reefs   = length(unique(df$reef)),
    trend_grid = data.frame(
      year = grid$time,
      fit  = pred$fit,
      lwr  = pmax(pred$fit - 1.96 * pred$se.fit, 0),   # counts cannot be negative
      upr  = pred$fit + 1.96 * pred$se.fit
    )
  )
}

# ── run_skill ─────────────────────────────────────────────────────────────────
#
# Args:
#   data    data.frame — reef-year invertebrate data with columns:
#           time (int), reef (chr), taxa (chr), value (num),
#           region (chr), richness (num, optional)
#
# Returns: list(value, method, params)

run_skill <- function(data, ...) {

  if (!requireNamespace("mgcv", quietly = TRUE))
    stop("Package 'mgcv' is required.")

  # ── Input validation ───────────────────────────────────────────────────────

  required_cols <- c("time", "reef", "taxa", "value", "region")
  missing_cols  <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0)
    stop("Missing columns in data: ", paste(missing_cols, collapse = ", "))

  if (!is.numeric(data$value))
    stop("'value' (abundance count per transect) must be numeric.")

  unknown_taxa <- setdiff(unique(data$taxa), .FOCAL_TAXA)
  if (length(unknown_taxa) > 0)
    warning("Non-focal taxa excluded: ", paste(unknown_taxa, collapse = ", "))

  data <- data[data$taxa %in% .FOCAL_TAXA & !is.na(data$value) & !is.na(data$time), ]

  if (nrow(data) == 0)
    stop("No records remain after filtering to focal taxa and removing NAs.")

  has_richness <- "richness" %in% names(data)

  # ── Process each focal taxon independently ─────────────────────────────────

  results <- lapply(.FOCAL_TAXA, function(tx) {

    tx_data   <- data[data$taxa == tx, ]
    all_years <- sort(unique(tx_data$time))
    n_years   <- length(all_years)

    # ── KPI: observed mean, most recent N years ───────────────────────────────

    kpi_years  <- tail(all_years, .KPI_YEARS)
    kpi_data   <- tx_data[tx_data$time %in% kpi_years, ]
    annual_kpi <- tapply(kpi_data$value, kpi_data$time, mean, na.rm = TRUE)
    kpi_mean   <- mean(annual_kpi, na.rm = TRUE)
    kpi_sd     <- sd(annual_kpi, na.rm = TRUE)

    kpi <- list(
      mean_abundance_per_transect = round(kpi_mean, 3),
      sd                          = round(kpi_sd, 3),
      years_included              = as.integer(kpi_years),
      n_years_in_kpi              = length(kpi_years)
    )

    if (has_richness) {
      rich_kpi <- tx_data[tx_data$time %in% kpi_years, ]
      annual_rich <- tapply(rich_kpi$richness, rich_kpi$time, mean, na.rm = TRUE)
      kpi$mean_richness_per_transect <- round(mean(annual_rich, na.rm = TRUE), 3)
    }

    # ── Annual means ± SE (observed points for plotting) ─────────────────────

    annual_means <- do.call(rbind, lapply(all_years, function(yr) {
      vals <- tx_data$value[tx_data$time == yr]
      n    <- sum(!is.na(vals))
      data.frame(
        year                      = yr,
        mean_abundance_per_transect = round(mean(vals, na.rm = TRUE), 3),
        se                        = round(sd(vals, na.rm = TRUE) / sqrt(n), 3),
        n_reefs                   = n
      )
    }))

    # ── GAM trend ──────────────────────────────────────────────────────────────

    trend <- if (n_years < .GAM_MIN_YEARS) {
      list(
        status       = "insufficient_data",
        n_years      = n_years,
        min_required = .GAM_MIN_YEARS,
        note         = paste0("GAM requires >= ", .GAM_MIN_YEARS,
                              " unique survey years; only ", n_years,
                              " available for ", tx, ".")
      )
    } else {
      gam_out <- tryCatch(
        .gam_trend_taxon(tx_data),
        error = function(e) {
          list(status = "gam_error", message = conditionMessage(e))
        }
      )
      if (!is.null(gam_out$trend_grid)) {
        list(
          status       = "fitted",
          dev_expl_pct = gam_out$dev_expl,
          k_used       = gam_out$k_used,
          n_years      = gam_out$n_years,
          n_reefs      = gam_out$n_reefs,
          year_range   = range(tx_data$time),
          trend_grid   = gam_out$trend_grid
        )
      } else gam_out
    }

    list(kpi = kpi, trend = trend, annual_means = annual_means)
  })

  names(results) <- .FOCAL_TAXA

  # ── Trend summary across all taxa ─────────────────────────────────────────

  trend_summary <- do.call(rbind, lapply(.FOCAL_TAXA, function(tx) {
    tr <- results[[tx]]$trend
    data.frame(
      taxa         = tx,
      status       = tr$status,
      dev_expl_pct = if (!is.null(tr$dev_expl_pct)) tr$dev_expl_pct else NA_real_,
      n_years      = tr$n_years,
      stringsAsFactors = FALSE
    )
  }))

  # ── Return ────────────────────────────────────────────────────────────────

  list(
    value = list(
      by_taxa       = results,
      trend_summary = trend_summary
    ),
    method = paste0(
      "KPI: observed mean abundance per transect across most recent ", .KPI_YEARS,
      " survey years (arithmetic mean of per-year reef means), per taxon independently. ",
      "Trend: GAM with formula quantity ~ s(year, k = k_use) + s(reef, bs = 're'), ",
      "family = gaussian(), method = REML, fit independently for each of 4 focal taxa: ",
      paste(.FOCAL_TAXA, collapse = ", "), ". ",
      "k_use = min(10, n_years - 1) per taxon. ",
      "Prediction excludes reef random effect; CI lower bound clipped at 0. ",
      "MCP pre-processing: label=INV filtered, focal taxa classified, ",
      "consistent site filter (>= 5 years), aggregation: transect SUM -> reef MEAN."
    ),
    params = list(
      focal_taxa    = .FOCAL_TAXA,
      k_max         = .K_MAX,
      kpi_years     = .KPI_YEARS,
      gam_min_years = .GAM_MIN_YEARS,
      gam_family    = "gaussian",
      gam_method    = "REML",
      grid_points   = .GRID_POINTS,
      unit          = "individuals per transect",
      ci_lwr_floor  = 0
    )
  )
}
