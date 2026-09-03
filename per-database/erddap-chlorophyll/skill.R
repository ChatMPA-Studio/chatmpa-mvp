# skill.R — erddap-chlorophyll
#
# Computes annual chlorophyll-a geometric means and log10 anomalies at:
#   local    — AMP polygon clip of a small bbox (clip_to_geometry on data_local)
#   regional — gulf_mexico bbox pre-aggregated server-side (aggregate_spatial=TRUE)
#
# Two separate ERDDAP calls merged by the orchestrator:
#   data_local    — {lat, lon, time, chlorophyll} small bbox, real pixel grid
#   data_regional — {time, chlorophyll} gulf_mexico bbox, aggregate_spatial=TRUE
#
# See SKILL.md for the full data contract and method specification.

source(file.path(dirname(sys.frame(1)$ofile),
                 "../../shared/spatial_join/spatial_join.R"))

if (!exists("before_after", mode = "function")) {
  source(file.path(dirname(sys.frame(1)$ofile),
                   "../../shared/interpretation/interpret.R"))
}

# ── Constants (fixed — do not change without updating SKILL.md) ──────────────

.COVERAGE_THRESHOLD  <- 30          # % valid composites below which year = NA
.BASELINE_START      <- 2003L       # baseline period start (fixed — MODIS Aqua launch)
.BASELINE_END        <- 2020L       # baseline period end (fixed)
.HIGH_PROD_THRESHOLD <-  0.3        # log10 anomaly flagged as high productivity (~2× baseline)
.LOW_PROD_THRESHOLD  <- -0.3        # log10 anomaly flagged as low productivity (~0.5× baseline)
.MIN_PIXELS_WARN     <- 4L          # MODIS ~4 km; below this the AMP polygon is too small

# ── run_skill ────────────────────────────────────────────────────────────────
#
# Args:
#   data_local    data.frame — ERDDAP bbox_local response: lat, lon, time, chlorophyll
#   data_regional data.frame — ERDDAP gulf_mexico + aggregate_spatial=TRUE: time, chlorophyll
#   geometry_local sf object — AMP polygon from get_amp_geometry(). Used to clip
#                  data_local to the exact AMP boundary (bbox_local only bounded
#                  the download). No geometry needed for regional (server-side aggregate).
#
# Returns: list(value, method, params, interpretation, geometry_source)

run_skill <- function(data_local, data_regional, geometry_local, ...) {

  # ── Input validation ───────────────────────────────────────────────────────

  required_local   <- c("lat", "lon", "time", "chlorophyll")
  missing_local    <- setdiff(required_local, names(data_local))
  if (length(missing_local) > 0)
    stop("Missing columns in data_local: ", paste(missing_local, collapse = ", "),
         "\nExpected: ", paste(required_local, collapse = ", "))

  required_regional <- c("time", "chlorophyll")
  missing_regional  <- setdiff(required_regional, names(data_regional))
  if (length(missing_regional) > 0)
    stop("Missing columns in data_regional: ", paste(missing_regional, collapse = ", "),
         "\nExpected: ", paste(required_regional, collapse = ", "))

  if (!inherits(geometry_local, "sf"))
    stop("geometry_local must be an sf object")

  # ── Local: clip to real AMP polygon ───────────────────────────────────────

  data_local_clip <- clip_to_geometry(data_local, geometry_local)
  n_pixels_local  <- length(unique(paste(data_local_clip$lat, data_local_clip$lon)))

  if (n_pixels_local < .MIN_PIXELS_WARN)
    warning("n_pixels_local = ", n_pixels_local,
            " — MODIS resolution (~4 km) may be too coarse for this AMP polygon.")

  # ── Per-scale annual aggregation ──────────────────────────────────────────
  # n_pixels = 1 for regional: data_regional is already spatially averaged
  # server-side (aggregate_spatial=TRUE), so expected composites = 46/year.

  .process_scale <- function(df, escala, n_pixels) {
    data_valid      <- df[!is.na(df$chlorophyll) & df$chlorophyll > 0, ]
    data_valid$year <- as.integer(format(as.Date(data_valid$time), "%Y"))

    baseline_data <- data_valid[
      data_valid$year >= .BASELINE_START & data_valid$year <= .BASELINE_END, ]

    if (nrow(baseline_data) == 0) {
      # Expected for local scale when the rolling window misses 2003-2020.
      # Do not abort — chl_geomean remains valid, only anomaly is NA.
      warning("No valid chlorophyll data in baseline period (",
              .BASELINE_START, "-", .BASELINE_END, ") for scale: ", escala,
              " — anomalia_log10/anomalia_mgm3 will be NA.")
      chl_baseline <- NA_real_
    } else {
      chl_baseline <- 10 ^ mean(log10(baseline_data$chlorophyll))
    }

    years             <- sort(unique(data_valid$year))
    expected_per_year <- n_pixels * 46L   # ~46 8-day composites per year

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
          n_pixels       = n_pixels,
          cobertura_pct  = cob_pct,
          prod_flag      = NA_character_
        ))
      }

      chl_geomean <- 10 ^ mean(log10(yr_data$chlorophyll))

      if (is.na(chl_baseline)) {
        anom_log10 <- NA_real_
        anom_mgm3  <- NA_real_
        prod_flag  <- NA_character_
      } else {
        anom_log10 <- round(log10(chl_geomean) - log10(chl_baseline), 4)
        anom_mgm3  <- round(chl_geomean - chl_baseline, 4)
        prod_flag  <- dplyr::case_when(
          anom_log10 >  .HIGH_PROD_THRESHOLD ~ "high",
          anom_log10 <  .LOW_PROD_THRESHOLD  ~ "low",
          TRUE                               ~ "normal"
        )
      }

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

    attr(annual, "chl_baseline") <- if (is.na(chl_baseline)) NA_real_ else round(chl_baseline, 4)
    annual
  }

  local_series    <- .process_scale(data_local_clip, "local",    n_pixels_local)
  regional_series <- .process_scale(data_regional,   "regional", 1L)

  value <- rbind(local_series, regional_series)
  rownames(value) <- NULL

  # ── Interpretation (local scale only) ─────────────────────────────────────

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
    insight_sentence(
      var_label_en  = "Chlorophyll-a",
      var_label_es  = "La clorofila-a",
      unit          = "mg/m³",
      before_mean   = ba_chl$before_mean, after_mean = ba_chl$after_mean,
      delta         = ba_chl$delta,       pct_change = ba_chl$pct_change,
      before_period = ba_chl$before_period, after_period = ba_chl$after_period,
      extra_en      = paste(c(mk_note_en, ambig_note_en), collapse = " "),
      extra_es      = paste(c(mk_note_es, ambig_note_es), collapse = " "),
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
      .BASELINE_END, "). Local: clipped to real AMP polygon via clip_to_geometry() ",
      "(bbox_local bounds the download, polygon clip gives exact boundary). ",
      "Regional: server-side aggregate_spatial over gulf_mexico bbox, ",
      "full 2003-present history. ",
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
      n_pixels_local         = n_pixels_local
    ),
    geometry_source = list(
      local = attr(geometry_local, "geometry_source")
    )
  )
}
