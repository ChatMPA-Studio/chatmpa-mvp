# skill.R — erddap-sst-anomaly
#
# Computes annual SST means and anomalies at local (AMP polygon) and regional
# (LME polygon) scales from OISST v2.1 daily data (NOAA CoastWatch ERDDAP).
#
# See SKILL.md for the full data contract and method specification.
#
# Dependencies:
#   shared/spatial_join/spatial_join.R  — clip_to_geometry()

source(file.path(dirname(sys.frame(1)$ofile),
                 "../../shared/spatial_join/spatial_join.R"))

# ── Constants (fixed — do not change without updating SKILL.md) ──────────────

.COVERAGE_THRESHOLD <- 50   # % of valid pixel-days below which year = NA
.STRESS_THRESHOLD   <- 0.5  # anomaly (°C) above which year is flagged as stressful
.MIN_PIXELS_WARN    <- 4    # n_pixels below which a warning is issued (coarse OISST grid)

# ── run_skill ────────────────────────────────────────────────────────────────
#
# Args:
#   data              data.frame — minimal contract from ERDDAP MCP:
#                     columns: lat (num), lon (num), time (Date), sst (num), anom (num)
#   geometry_local    sf object  — AMP polygon, from get_amp_geometry()
#   geometry_regional sf object  — LME polygon, from get_lme_geometry()
#
# Returns: list(value, method, params, geometry_source)

run_skill <- function(data, geometry_local, geometry_regional, ...) {

  # ── Input validation ───────────────────────────────────────────────────────

  required_cols <- c("lat", "lon", "time", "sst", "anom")
  missing_cols  <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing columns in data: ", paste(missing_cols, collapse = ", "),
         "\nExpected: ", paste(required_cols, collapse = ", "))
  }
  if (!inherits(geometry_local,    "sf")) stop("geometry_local must be an sf object")
  if (!inherits(geometry_regional, "sf")) stop("geometry_regional must be an sf object")

  # ── Clip to each scale ────────────────────────────────────────────────────

  scales <- list(
    local    = geometry_local,
    regional = geometry_regional
  )

  results <- lapply(names(scales), function(escala) {
    geom      <- scales[[escala]]
    data_clip <- clip_to_geometry(data, geom)
    coverage  <- attr(data_clip, "cobertura_pct")
    n_pixels  <- length(unique(paste(data_clip$lat, data_clip$lon)))

    if (n_pixels < .MIN_PIXELS_WARN && escala == "local") {
      warning("n_pixels = ", n_pixels, " for local scale — OISST resolution (0.25°) ",
              "may be too coarse for this AMP polygon.")
    }

    # ── Annual aggregation ─────────────────────────────────────────────────

    data_clip$year <- as.integer(format(as.Date(data_clip$time), "%Y"))
    data_valid     <- data_clip[!is.na(data_clip$sst) & !is.na(data_clip$anom), ]
    years          <- sort(unique(data_clip$year))

    # Expected pixel-days per year: n_pixels × 365 (approximate)
    expected_per_year <- n_pixels * 365

    annual <- do.call(rbind, lapply(years, function(yr) {
      yr_data      <- data_valid[data_valid$year == yr, ]
      n_valid      <- nrow(yr_data)
      cob_pct      <- round(n_valid / expected_per_year * 100, 1)

      if (cob_pct < .COVERAGE_THRESHOLD) {
        warning("Year ", yr, " (", escala, "): cobertura_pct = ", cob_pct,
                "% < ", .COVERAGE_THRESHOLD, "% — returning NA.")
        return(data.frame(
          year             = yr,
          escala           = escala,
          sst_media        = NA_real_,
          anomalia_media   = NA_real_,
          n_pixels         = n_pixels,
          cobertura_pct    = cob_pct,
          stress_flag      = NA
        ))
      }

      sst_med  <- mean(yr_data$sst,  na.rm = TRUE)
      anom_med <- mean(yr_data$anom, na.rm = TRUE)

      data.frame(
        year           = yr,
        escala         = escala,
        sst_media      = round(sst_med,  3),
        anomalia_media = round(anom_med, 3),
        n_pixels       = n_pixels,
        cobertura_pct  = cob_pct,
        stress_flag    = anom_med > .STRESS_THRESHOLD
      )
    }))

    annual
  })

  value <- do.call(rbind, results)
  rownames(value) <- NULL

  # ── Return ────────────────────────────────────────────────────────────────

  list(
    value  = value,
    method = paste0(
      "Annual mean SST and NOAA anomaly (1971-2000 baseline) per pixel-day, ",
      "clipped to AMP polygon (local) and LME polygon (regional). ",
      "Years with cobertura_pct < ", .COVERAGE_THRESHOLD, "% returned as NA. ",
      "Dataset: ncdcOisst21Agg_LonPM180, NOAA CoastWatch ERDDAP."
    ),
    params = list(
      coverage_threshold_pct = .COVERAGE_THRESHOLD,
      stress_threshold_c     = .STRESS_THRESHOLD,
      anomaly_baseline       = "NOAA 1971-2000 (embedded in OISST anom variable)",
      min_pixels_warn        = .MIN_PIXELS_WARN
    ),
    geometry_source = list(
      local    = attr(geometry_local,    "geometry_source"),
      regional = attr(geometry_regional, "geometry_source")
    )
  )
}
