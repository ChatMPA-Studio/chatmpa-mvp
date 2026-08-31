# skill.R — erddap-chlorophyll
#
# Computes annual chlorophyll-a geometric means and log10 anomalies at local
# (AMP bbox) and regional (LME bbox) scales from MODIS Aqua 8-day Science
# Quality composites (NOAA CoastWatch ERDDAP).
#
# Data arrives pre-aggregated (aggregate_spatial=TRUE from ERDDAP MCP):
#   data_local    — {time, chlorophyll} 8-day series for the AMP bbox
#   data_regional — {time, chlorophyll} 8-day series for the LME bbox
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

.COVERAGE_THRESHOLD  <- 30          # % valid composites below which year = NA
.BASELINE_START      <- 2003L       # baseline period start (fixed)
.BASELINE_END        <- 2020L       # baseline period end (fixed)
.HIGH_PROD_THRESHOLD <-  0.3        # log10 anomaly flagged as high productivity (~2× baseline)
.LOW_PROD_THRESHOLD  <- -0.3        # log10 anomaly flagged as low productivity (~0.5× baseline)

# ── run_skill ────────────────────────────────────────────────────────────────
#
# Args:
#   data_local    data.frame — aggregate_spatial=TRUE from ERDDAP MCP (AMP bbox):
#                 columns: time (chr/Date), chlorophyll (num, mg/m³)
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

  required_cols <- c("time", "chlorophyll")
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
    data_valid       <- df[!is.na(df$chlorophyll) & df$chlorophyll > 0, ]
    data_valid$year  <- as.integer(format(as.Date(data_valid$time), "%Y"))

    baseline_data <- data_valid[
      data_valid$year >= .BASELINE_START & data_valid$year <= .BASELINE_END, ]

    if (nrow(baseline_data) == 0) {
      stop("No valid chlorophyll data in baseline period (",
           .BASELINE_START, "-", .BASELINE_END, ") for scale: ", escala)
    }

    chl_baseline     <- 10 ^ mean(log10(baseline_data$chlorophyll))
    years            <- sort(unique(data_valid$year))
    expected_per_year <- 46L  # ~46 8-day composites per year after bbox aggregation

    annual <- do.call(rbind, lapply(years, function(yr) {
      yr_data <- data_valid[data_valid$year == yr, ]
      n_valid <- nrow(yr_data)
      cob_pct <- round(n_valid / expected_per_year * 100, 1)

      if (cob_pct < .COVERAGE_THRESHOLD) {
        warning("Year ", yr, " (", escala, "): cobertura_pct = ", cob_pct,
                "% < ", .COVERAGE_THRESHOLD, "% — returning NA.")
        return(data.frame(
          year           = yr,
          escala         = escala,
          chl_geomean    = NA_real_,
          anomalia_log10 = NA_real_,
          anomalia_mgm3  = NA_real_,
          n_pixels       = NA_integer_,
          cobertura_pct  = cob_pct,
          prod_flag      = NA_character_
        ))
      }

      chl_geomean <- 10 ^ mean(log10(yr_data$chlorophyll))
      anom_log10  <- round(log10(chl_geomean) - log10(chl_baseline), 4)
      anom_mgm3   <- round(chl_geomean - chl_baseline, 4)

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
        n_pixels       = NA_integer_,   # bbox-aggregated; pixel count not applicable
        cobertura_pct  = cob_pct,
        prod_flag      = prod_flag
      )
    }))

    attr(annual, "chl_baseline") <- round(chl_baseline, 4)
    annual
  }

  results <- list(.process_scale(data_local, "local"))
  if (!is.null(data_regional)) {
    results <- c(results, list(.process_scale(data_regional, "regional")))
  }

  value <- do.call(rbind, results)
  rownames(value) <- NULL

  # ── Interpretation ────────────────────────────────────────────────────────

  local_chl <- value[value$escala == "local" & !is.na(value$chl_geomean), ]

  ba_chl  <- before_after(local_chl, "chl_geomean", year_col = "year",
                           before_years = .BASELINE_START:.BASELINE_END, recent_n = 5L)
  mkt_chl <- mk_trend(local_chl, "chl_geomean", year_col = "year")

  chl_sentences <- if (!is.null(ba_chl$status)) {
    list(en = NA_character_, es = NA_character_)
  } else {
    mk_note_en <- if (!is.null(mkt_chl$tau))
      paste0("Mann-Kendall: τ = ", mkt_chl$tau, ", p = ", mkt_chl$p, ".") else NULL
    mk_note_es <- mk_note_en
    ambig_note_en <- "Note: Chl-a trends reflect both upwelling dynamics and land-based nutrient inputs — direction alone does not indicate ecosystem health."
    ambig_note_es <- "Nota: La clorofila puede reflejar surgencia o aportes de nutrientes terrestres — la dirección por sí sola no indica salud del ecosistema."
    extra_en <- paste(c(mk_note_en, ambig_note_en), collapse = " ")
    extra_es <- paste(c(mk_note_es, ambig_note_es), collapse = " ")
    insight_sentence(
      var_label_en  = "Chlorophyll-a",
      var_label_es  = "La clorofila-a",
      unit          = "mg/m³",
      before_mean   = ba_chl$before_mean, after_mean = ba_chl$after_mean,
      delta         = ba_chl$delta,       pct_change = ba_chl$pct_change,
      before_period = ba_chl$before_period, after_period = ba_chl$after_period,
      extra_en      = extra_en,           extra_es   = extra_es,
      digits        = 3L
    )
  }

  interpretation <- list(
    before_period = ba_chl$before_period,
    after_period  = ba_chl$after_period,
    metric_before = ba_chl$before_mean,
    metric_after  = ba_chl$after_mean,
    delta         = ba_chl$delta,
    pct_change    = ba_chl$pct_change,
    direction     = if (!is.null(ba_chl$direction)) ba_chl$direction else "unknown",
    status        = "ambiguous",
    significance  = mkt_chl,
    insight_es    = chl_sentences$es,
    insight_en    = chl_sentences$en
  )

  # ── Return ─────────────────────────────────────────────────────────────────

  list(
    value  = value,
    method = paste0(
      "Annual geometric mean chl-a (log10 space averaging, back-transformed) ",
      "and log10 anomaly vs baseline geometric mean (", .BASELINE_START, "-",
      .BASELINE_END, "), pre-aggregated spatially by ERDDAP MCP (aggregate_spatial=TRUE). ",
      "Local scale: AMP bbox [-110.5,-108.5,22.5,24.5]. ",
      "Regional scale: Gulf of California bbox [-117.5,-106.0,22.0,32.0]. ",
      "Years with cobertura_pct < ", .COVERAGE_THRESHOLD, "% returned as NA. ",
      "Dataset: erdMH1chla8day_R202SQ, NOAA CoastWatch ERDDAP."
    ),
    interpretation = interpretation,
    params = list(
      coverage_threshold_pct = .COVERAGE_THRESHOLD,
      baseline_period        = paste0(.BASELINE_START, "-", .BASELINE_END),
      high_prod_threshold    = .HIGH_PROD_THRESHOLD,
      low_prod_threshold     = .LOW_PROD_THRESHOLD,
      averaging_method       = "geometric mean (log10 space)",
      spatial_method         = "bbox_aggregate (aggregate_spatial=TRUE)"
    ),
    geometry_source = list(
      local    = if (!is.null(geometry_local))    attr(geometry_local,    "geometry_source") else "bbox_aggregate",
      regional = if (!is.null(geometry_regional)) attr(geometry_regional, "geometry_source") else "bbox_aggregate"
    )
  )
}
