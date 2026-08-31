# skill.R — erddap-sst-anomaly
#
# Computes annual SST means and anomalies at local (AMP bbox) and regional
# (LME bbox) scales from OISST v2.1 daily data (NOAA CoastWatch ERDDAP).
#
# Data arrives pre-aggregated (aggregate_spatial=TRUE from ERDDAP MCP):
#   data_local    — {time, sst, anom} daily series for the AMP bbox
#   data_regional — {time, sst, anom} daily series for the LME bbox
# No clip_to_geometry needed; the bbox IS the spatial filter.
#
# See SKILL.md for the full data contract and method specification.
#
# Dependencies:
#   shared/spatial_join/spatial_join.R  — get_amp_geometry(), get_lme_geometry()
#                                         (used for geometry_source metadata only)

source(file.path(dirname(sys.frame(1)$ofile),
                 "../../shared/spatial_join/spatial_join.R"))

if (!exists("before_after", mode = "function")) {
  source(file.path(dirname(sys.frame(1)$ofile),
                   "../../shared/interpretation/interpret.R"))
}

# ── Constants (fixed — do not change without updating SKILL.md) ──────────────

.COVERAGE_THRESHOLD <- 50   # % of valid daily values below which year = NA
.STRESS_THRESHOLD   <- 0.5  # anomaly (°C) above which year is flagged as stressful

# ── run_skill ────────────────────────────────────────────────────────────────
#
# Args:
#   data_local    data.frame — aggregate_spatial=TRUE from ERDDAP MCP (AMP bbox):
#                 columns: time (chr/Date), sst (num °C), anom (num °C)
#   data_regional data.frame — same format, for the LME bbox (optional)
#
# Returns: list(value, method, params, geometry_source)

run_skill <- function(data_local, data_regional = NULL, ...) {

  # ── Geometry resolution (metadata only, no spatial clip) ──────────────────
  args <- list(...)
  mpa      <- args$mpa
  lme_name <- if (!is.null(args$lme) && nzchar(args$lme)) args$lme else "Gulf of California"

  geometry_local <- if (!is.null(mpa) && nzchar(mpa)) {
    tryCatch(get_amp_geometry(mpa), error = function(e) NULL)
  } else NULL

  geometry_regional <- tryCatch(get_lme_geometry(lme_name), error = function(e) NULL)

  # ── Input validation ───────────────────────────────────────────────────────

  required_cols <- c("time", "sst", "anom")
  for (nm in c("data_local", "data_regional")) {
    df <- get(nm)
    if (is.null(df)) next
    missing_cols <- setdiff(required_cols, names(df))
    if (length(missing_cols) > 0) {
      stop(nm, " missing columns: ", paste(missing_cols, collapse = ", "),
           "\nExpected: ", paste(required_cols, collapse = ", "),
           " (aggregate_spatial=TRUE response from ERDDAP MCP)")
    }
  }

  # ── Per-scale annual aggregation ──────────────────────────────────────────

  .process_scale <- function(df, escala) {
    data_valid        <- df[!is.na(df$sst) & !is.na(df$anom), ]
    data_valid$year   <- as.integer(format(as.Date(data_valid$time), "%Y"))
    years             <- sort(unique(data_valid$year))
    expected_per_year <- 365L  # daily OISST; one value per day after bbox aggregation

    do.call(rbind, lapply(years, function(yr) {
      yr_data <- data_valid[data_valid$year == yr, ]
      n_valid <- nrow(yr_data)
      cob_pct <- round(n_valid / expected_per_year * 100, 1)

      if (cob_pct < .COVERAGE_THRESHOLD) {
        warning("Year ", yr, " (", escala, "): cobertura_pct = ", cob_pct,
                "% < ", .COVERAGE_THRESHOLD, "% — returning NA.")
        return(data.frame(
          year           = yr,
          escala         = escala,
          sst_media      = NA_real_,
          anomalia_media = NA_real_,
          n_pixels       = NA_integer_,
          cobertura_pct  = cob_pct,
          stress_flag    = NA
        ))
      }

      sst_med  <- mean(yr_data$sst,  na.rm = TRUE)
      anom_med <- mean(yr_data$anom, na.rm = TRUE)

      data.frame(
        year           = yr,
        escala         = escala,
        sst_media      = round(sst_med,  3),
        anomalia_media = round(anom_med, 3),
        n_pixels       = NA_integer_,   # bbox-aggregated; pixel count not applicable
        cobertura_pct  = cob_pct,
        stress_flag    = anom_med > .STRESS_THRESHOLD
      )
    }))
  }

  results <- list(.process_scale(data_local, "local"))
  if (!is.null(data_regional)) {
    results <- c(results, list(.process_scale(data_regional, "regional")))
  }

  value <- do.call(rbind, results)
  rownames(value) <- NULL

  # ── Interpretation ────────────────────────────────────────────────────────

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
      "Annual mean SST and NOAA anomaly (1971-2000 baseline) from OISST daily data, ",
      "pre-aggregated spatially by ERDDAP MCP (aggregate_spatial=TRUE). ",
      "Local scale: AMP bbox [-110.5,-108.5,22.5,24.5]. ",
      "Regional scale: Gulf of California bbox [-117.5,-106.0,22.0,32.0]. ",
      "Years with cobertura_pct < ", .COVERAGE_THRESHOLD, "% returned as NA. ",
      "Dataset: ncdcOisst21Agg_LonPM180, NOAA CoastWatch ERDDAP."
    ),
    interpretation = interpretation,
    params = list(
      coverage_threshold_pct = .COVERAGE_THRESHOLD,
      stress_threshold_c     = .STRESS_THRESHOLD,
      anomaly_baseline       = "NOAA 1971-2000 (embedded in OISST anom variable)",
      spatial_method         = "bbox_aggregate (aggregate_spatial=TRUE)"
    ),
    geometry_source = list(
      local    = if (!is.null(geometry_local))    attr(geometry_local,    "geometry_source") else "bbox_aggregate",
      regional = if (!is.null(geometry_regional)) attr(geometry_regional, "geometry_source") else "bbox_aggregate"
    )
  )
}
