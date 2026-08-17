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

if (!exists("before_after", mode = "function")) {
  source(file.path(dirname(sys.frame(1)$ofile),
                   "../../shared/interpretation/interpret.R"))
}

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

  # ── Interpretation ────────────────────────────────────────────────────────
  # Uses local-scale annual means; before = 1982-2000 pre-warming baseline.

  local_val <- value[value$escala == "local" & !is.na(value$sst_media), ]

  ba_sst  <- before_after(local_val, "sst_media", year_col = "year",
                           before_years = 1982:2000, recent_n = 5L)
  mkt_sst <- mk_trend(local_val, "sst_media", year_col = "year")

  after_stress_n <- if (!is.null(ba_sst$after_period)) {
    after_rows <- local_val[
      local_val$year >= ba_sst$after_period[1] &
      local_val$year <= ba_sst$after_period[2], ]
    sum(after_rows$stress_flag == TRUE, na.rm = TRUE)
  } else 0L

  sst_status <- if (!is.null(ba_sst$status)) "neutral" else {
    if      (after_stress_n >= 3) "negative"
    else if (after_stress_n == 0 && abs(ba_sst$delta) <= 0.1) "positive"
    else "neutral"
  }

  sst_sentences <- if (!is.null(ba_sst$status)) {
    list(en = NA_character_, es = NA_character_)
  } else {
    stress_note_en <- paste0(after_stress_n, " of the last 5 years exceeded ",
                             "the +0.5°C stress threshold.")
    stress_note_es <- paste0(after_stress_n, " de los últimos 5 años superaron ",
                             "el umbral de estrés de +0.5°C.")
    insight_sentence(
      var_label_en  = "Mean SST",
      var_label_es  = "La temperatura superficial del mar (SST)",
      unit          = "°C",
      before_mean   = ba_sst$before_mean, after_mean = ba_sst$after_mean,
      delta         = ba_sst$delta,       pct_change = ba_sst$pct_change,
      before_period = ba_sst$before_period, after_period = ba_sst$after_period,
      extra_en      = stress_note_en,     extra_es   = stress_note_es,
      digits        = 2L
    )
  }

  interpretation <- list(
    before_period       = ba_sst$before_period,
    after_period        = ba_sst$after_period,
    metric_before       = ba_sst$before_mean,
    metric_after        = ba_sst$after_mean,
    delta               = ba_sst$delta,
    pct_change          = ba_sst$pct_change,
    direction           = if (!is.null(ba_sst$direction)) ba_sst$direction else "unknown",
    status              = sst_status,
    stress_years_recent = after_stress_n,
    significance        = mkt_sst,
    insight_es          = sst_sentences$es,
    insight_en          = sst_sentences$en
  )

  # ── Return ────────────────────────────────────────────────────────────────

  list(
    value  = value,
    method = paste0(
      "Annual mean SST and NOAA anomaly (1971-2000 baseline) per pixel-day, ",
      "clipped to AMP polygon (local) and LME polygon (regional). ",
      "Years with cobertura_pct < ", .COVERAGE_THRESHOLD, "% returned as NA. ",
      "Dataset: ncdcOisst21Agg_LonPM180, NOAA CoastWatch ERDDAP."
    ),
    interpretation = interpretation,
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
