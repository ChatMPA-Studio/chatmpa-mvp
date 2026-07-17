# skill.R — erddap-chlorophyll
#
# Computes annual chlorophyll-a geometric means and log10 anomalies at local
# (AMP polygon) and regional (LME polygon) scales from MODIS Aqua 8-day
# Science Quality composites (NOAA CoastWatch ERDDAP).
#
# See SKILL.md for the full data contract and method specification.
#
# Dependencies:
#   shared/spatial_join/spatial_join.R  — clip_to_geometry()

source(file.path(dirname(sys.frame(1)$ofile),
                 "../../shared/spatial_join/spatial_join.R"))

# ── Constants (fixed — do not change without updating SKILL.md) ──────────────

.COVERAGE_THRESHOLD  <- 30          # % valid pixel-composites below which year = NA
.BASELINE_START      <- 2003L       # baseline period start (fixed)
.BASELINE_END        <- 2020L       # baseline period end (fixed)
.HIGH_PROD_THRESHOLD <-  0.3        # log10 anomaly flagged as high productivity (~2× baseline)
.LOW_PROD_THRESHOLD  <- -0.3        # log10 anomaly flagged as low productivity (~0.5× baseline)

# ── run_skill ────────────────────────────────────────────────────────────────
#
# Args:
#   data              data.frame — minimal contract from ERDDAP MCP:
#                     columns: lat (num), lon (num), time (Date), chlorophyll (num, mg/m³)
#   geometry_local    sf object  — AMP polygon, from get_amp_geometry()
#   geometry_regional sf object  — LME polygon, from get_lme_geometry()
#
# Returns: list(value, method, params, geometry_source)

run_skill <- function(data, geometry_local, geometry_regional, ...) {

  # ── Input validation ───────────────────────────────────────────────────────

  required_cols <- c("lat", "lon", "time", "chlorophyll")
  missing_cols  <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing columns in data: ", paste(missing_cols, collapse = ", "),
         "\nExpected: ", paste(required_cols, collapse = ", "))
  }
  if (!inherits(geometry_local,    "sf")) stop("geometry_local must be an sf object")
  if (!inherits(geometry_regional, "sf")) stop("geometry_regional must be an sf object")

  # ── Clip to each scale ─────────────────────────────────────────────────────

  scales <- list(
    local    = geometry_local,
    regional = geometry_regional
  )

  results <- lapply(names(scales), function(escala) {
    geom      <- scales[[escala]]
    data_clip <- clip_to_geometry(data, geom)
    n_pixels  <- length(unique(paste(data_clip$lat, data_clip$lon)))

    # ── Baseline geometric mean (2003–2020, all valid pixel-composites) ──────

    data_valid <- data_clip[
      !is.na(data_clip$chlorophyll) & data_clip$chlorophyll > 0, ]
    data_valid$year <- as.integer(format(as.Date(data_valid$time), "%Y"))

    baseline_data <- data_valid[
      data_valid$year >= .BASELINE_START & data_valid$year <= .BASELINE_END, ]

    if (nrow(baseline_data) == 0) {
      stop("No valid chlorophyll data in baseline period (",
           .BASELINE_START, "-", .BASELINE_END, ") for scale: ", escala)
    }

    chl_baseline <- 10 ^ mean(log10(baseline_data$chlorophyll))

    # ── Annual aggregation ────────────────────────────────────────────────────

    years <- sort(unique(data_valid$year))
    # Expected pixel-composites per year: n_pixels × 46 (approx 8-day composites)
    expected_per_year <- n_pixels * 46

    annual <- do.call(rbind, lapply(years, function(yr) {
      yr_data <- data_valid[data_valid$year == yr, ]
      n_valid <- nrow(yr_data)
      cob_pct <- round(n_valid / expected_per_year * 100, 1)

      if (cob_pct < .COVERAGE_THRESHOLD) {
        warning("Year ", yr, " (", escala, "): cobertura_pct = ", cob_pct,
                "% < ", .COVERAGE_THRESHOLD, "% — returning NA.")
        return(data.frame(
          year              = yr,
          escala            = escala,
          chl_geomean       = NA_real_,
          anomalia_log10    = NA_real_,
          anomalia_mgm3     = NA_real_,
          n_pixels          = n_pixels,
          cobertura_pct     = cob_pct,
          prod_flag         = NA_character_
        ))
      }

      chl_geomean    <- 10 ^ mean(log10(yr_data$chlorophyll))
      anom_log10     <- round(log10(chl_geomean) - log10(chl_baseline), 4)
      anom_mgm3      <- round(chl_geomean - chl_baseline, 4)

      prod_flag <- dplyr::case_when(
        anom_log10 >  .HIGH_PROD_THRESHOLD ~ "high",
        anom_log10 <  .LOW_PROD_THRESHOLD  ~ "low",
        TRUE                               ~ "normal"
      )

      data.frame(
        year           = yr,
        escala         = escala,
        chl_geomean    = round(chl_geomean, 4),
        anomalia_log10 = anom_log10,
        anomalia_mgm3  = anom_mgm3,
        n_pixels       = n_pixels,
        cobertura_pct  = cob_pct,
        prod_flag      = prod_flag
      )
    }))

    attr(annual, "chl_baseline") <- round(chl_baseline, 4)
    annual
  })

  value <- do.call(rbind, results)
  rownames(value) <- NULL

  # ── Return ─────────────────────────────────────────────────────────────────

  list(
    value  = value,
    method = paste0(
      "Annual geometric mean chl-a (log10 space averaging, back-transformed) ",
      "and log10 anomaly vs baseline geometric mean (", .BASELINE_START, "-",
      .BASELINE_END, "), clipped to AMP polygon (local) and LME polygon (regional). ",
      "Years with cobertura_pct < ", .COVERAGE_THRESHOLD, "% returned as NA. ",
      "Dataset: erdMH1chla8day_R202SQ, NOAA CoastWatch ERDDAP."
    ),
    params = list(
      coverage_threshold_pct = .COVERAGE_THRESHOLD,
      baseline_period        = paste0(.BASELINE_START, "-", .BASELINE_END),
      high_prod_threshold    = .HIGH_PROD_THRESHOLD,
      low_prod_threshold     = .LOW_PROD_THRESHOLD,
      averaging_method       = "geometric mean (log10 space)"
    ),
    geometry_source = list(
      local    = attr(geometry_local,    "geometry_source"),
      regional = attr(geometry_regional, "geometry_source")
    )
  )
}
