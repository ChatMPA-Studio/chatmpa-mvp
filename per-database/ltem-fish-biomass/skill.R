# skill.R — ltem-fish-biomass
#
# Fits a GAM (biomass ~ s(year) + s(reef, bs="re"), REML) to LTEM reef-year
# biomass data and computes the observed mean KPI for the most recent 5 years.
#
# See SKILL.md for the full data contract, method, and do-not rules.
# Reference: ltem_report/workshops/datamares/2026/generate_datamares_2026.R
#            gam_trend() and FIG07 section.

if (!exists("before_after", mode = "function")) {
  source(file.path(dirname(sys.frame(1)$ofile),
                   "../../shared/interpretation/interpret.R"))
}

# ── Constants (fixed — do not change without updating SKILL.md) ──────────────

.K_MAX        <- 10L   # maximum smoothing knots for s(year)
.KPI_YEARS    <- 5L    # number of most recent survey years for KPI mean
.GAM_MIN_YEARS <- 5L   # minimum unique years to fit the GAM
.GRID_POINTS  <- 200L  # prediction grid resolution over year range
.MK_MIN_YEARS <- 8L    # minimum unique survey years to compute MK trend label

# ── .gam_trend ────────────────────────────────────────────────────────────────
# Internal helper: fit the GAM and return population-level predictions.
# Mirrors generate_datamares_2026.R::gam_trend().

.gam_trend <- function(df, y_col, year_col, reef_col) {
  df <- df[!is.na(df[[y_col]]) & !is.na(df[[year_col]]), ]
  df[[reef_col]] <- factor(df[[reef_col]])   # bs="re" requires factor

  n_years <- length(unique(df[[year_col]]))
  k_use   <- min(.K_MAX, n_years - 1L)

  form <- as.formula(paste0(
    y_col, " ~ s(", year_col, ", k = ", k_use, ") + s(", reef_col, ", bs = 're')"
  ))

  fit <- tryCatch(
    mgcv::gam(form, data = df, family = gaussian(), method = "REML"),
    error = function(e) {
      stop("GAM fitting failed: ", conditionMessage(e))
    }
  )

  yr_range <- range(df[[year_col]])
  grid     <- data.frame(
    seq(yr_range[1], yr_range[2], length.out = .GRID_POINTS),
    factor(df[[reef_col]][1], levels = levels(df[[reef_col]]))
  )
  names(grid) <- c(year_col, reef_col)

  pred <- predict(fit, newdata = grid, se.fit = TRUE,
                  exclude = paste0("s(", reef_col, ")"),
                  newdata.guaranteed = TRUE)

  list(
    fit       = fit,
    dev_expl  = round(summary(fit)$dev.expl * 100, 1),
    trend_grid = data.frame(
      year = grid[[year_col]],
      fit  = pred$fit,
      lwr  = pred$fit - 1.96 * pred$se.fit,
      upr  = pred$fit + 1.96 * pred$se.fit
    )
  )
}

# ── .mk_label_output ─────────────────────────────────────────────────────────
# Internal. Derives the three MK output scalars from an mk_trend() result and
# the number of unique survey years in the series.
#
# Threshold for the label is p < 0.05 (two-sided), distinct from the
# interpretation block which uses p < 0.10 as a softer signal.
# Direction is the sign of the Theil-Sen slope (trend::sens.slope) of the
# chronologically ordered annual means, as specified in SKILL.md.
#
# Args:
#   mkt     — return value of mk_trend(); list(tau, p, direction) or list(status)
#   n_years — number of unique survey years in the full series
#   x       — annual mean biomass ordered by year (one value per survey year)
#
# Returns: list(mk_p_value, mk_slope_direction, mk_label)

.mk_label_output <- function(mkt, n_years, x) {

  if (!requireNamespace("trend", quietly = TRUE)) {
    warning("Package 'trend' not installed — Theil-Sen slope unavailable; ",
            "MK label returned as insufficient_data.")
    mkt <- list(status = "trend_package_missing")
  }

  if (n_years < .MK_MIN_YEARS || !is.null(mkt$status)) {
    return(list(
      mk_p_value         = NA_real_,
      mk_slope_direction = "insufficient_data",
      mk_label           = "Datos insuficientes para tendencia"
    ))
  }

  sen_slope <- unname(trend::sens.slope(x[!is.na(x)])$estimates)

  direction <- if (mkt$p < 0.05 && sen_slope > 0) {
    "positive"
  } else if (mkt$p < 0.05 && sen_slope < 0) {
    "negative"
  } else {
    "none"
  }

  label <- if (direction == "positive") {
    "Biomasa \u00edctica en aumento"   # Biomasa íctica en aumento
  } else if (direction == "negative") {
    "Biomasa \u00edctica en declive"   # Biomasa íctica en declive
  } else {
    "Biomasa \u00edctica estable"      # Biomasa íctica estable
  }

  list(
    mk_p_value         = mkt$p,
    mk_slope_direction = direction,
    mk_label           = label
  )
}

# ── run_skill ─────────────────────────────────────────────────────────────────
#
# Args:
#   data           data.frame — reef-year biomass (time, reef, value, region)
#   data_behavioral data.frame — optional behavioral functional-group breakdown
#                  (year, functional_name, mean_biomass) from behavioral_group_biomass
#   data_family    data.frame — optional biomass by taxonomic family (year, family, mean_biomass)
#   data_species   data.frame — optional biomass by species (species_id, species_name,
#                  trophic_group, mean_biomass), sortable for top-N
#   data_func      data.frame — DEPRECATED legacy breakdown (Functional_groups, mean_biomass);
#                  used as fallback only when data_behavioral is absent
#
# Returns: list(value, method, params)

run_skill <- function(data, data_behavioral = NULL, data_family = NULL,
                      data_species = NULL, data_func = NULL, ...) {

  if (!requireNamespace("mgcv", quietly = TRUE))
    stop("Package 'mgcv' is required.")

  # ── Input validation ───────────────────────────────────────────────────────

  required_cols <- c("time", "reef", "value", "region")
  missing_cols  <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0)
    stop("Missing columns in data: ", paste(missing_cols, collapse = ", "))

  if (!is.numeric(data$value))
    stop("'value' (biomass T/ha) must be numeric.")

  data <- data[!is.na(data$value) & !is.na(data$time), ]
  # `value` already arrives in T/ha: the LTEM Biomass column is loaded as
  # Quantity * a * Size^b / (Area * 100) = g/m² / 100 = T/ha. No conversion here.

  n_unique_years <- length(unique(data$time))
  n_reefs        <- length(unique(data$reef))

  # ── Step 1: KPI — observed mean over most recent N years ──────────────────

  all_years <- sort(unique(data$time))
  kpi_years <- tail(all_years, .KPI_YEARS)
  kpi_data  <- data[data$time %in% kpi_years, ]

  annual_kpi <- tapply(kpi_data$value, kpi_data$time, mean, na.rm = TRUE)
  kpi_mean   <- mean(annual_kpi, na.rm = TRUE)
  kpi_sd     <- sd(annual_kpi, na.rm = TRUE)

  kpi <- list(
    mean_biomass_t_ha  = round(kpi_mean, 4),
    sd_t_ha            = round(kpi_sd, 4),
    years_included     = as.integer(kpi_years),
    n_years_in_kpi     = length(kpi_years)
  )

  # ── Step 2: Annual means ± SE (for observed points on the plot) ───────────

  annual_means <- do.call(rbind, lapply(all_years, function(yr) {
    vals <- data$value[data$time == yr]
    n    <- sum(!is.na(vals))
    data.frame(
      year              = yr,
      mean_biomass_t_ha = round(mean(vals, na.rm = TRUE), 4),
      se_t_ha           = round(sd(vals, na.rm = TRUE) / sqrt(n), 4),
      n_reefs           = n
    )
  }))

  # ── Step 3: GAM trend ─────────────────────────────────────────────────────

  trend <- if (n_unique_years < .GAM_MIN_YEARS) {
    list(
      status       = "insufficient_data",
      n_years      = n_unique_years,
      min_required = .GAM_MIN_YEARS,
      note         = paste0("GAM requires >= ", .GAM_MIN_YEARS,
                            " unique survey years; only ", n_unique_years, " available.")
    )
  } else {
    gam_out <- .gam_trend(data, y_col = "value", year_col = "time", reef_col = "reef")
    list(
      status      = "fitted",
      dev_expl_pct = gam_out$dev_expl,
      n_years     = n_unique_years,
      n_reefs     = n_reefs,
      year_range  = range(data$time),
      trend_grid  = gam_out$trend_grid   # 200-row data.frame: year, fit, lwr, upr
    )
  }

  # ── Step 4: Behavioral functional-group breakdown (optional) ──────────────
  # Feeds the TrophicYear card: one row per year × behavioral group, in T/ha.
  # Maps functional_name -> Spanish label. Falls back to the legacy data_func
  # (functional_group_biomass) only when data_behavioral is absent.

  .FN_LABELS <- c(
    GenPred_solitary  = "Depredadores solitarios",
    GenPred_schooling = "Depredadores en cardúmenes",
    EpiBent_schooling = "Omnívoros en cardumen",
    Crip_schooling    = "Herbívoros en cardumen",
    Crip_solitary     = "Crípticos solitarios",
    Plank             = "Planctívoros"
  )

  func_out <- NULL
  if (!is.null(data_behavioral)) {
    req_b <- c("functional_name", "mean_biomass")
    if (!all(req_b %in% names(data_behavioral))) {
      warning("data_behavioral missing required columns (functional_name, mean_biomass) — ",
              "behavioral breakdown skipped.")
    } else {
      fb <- data_behavioral
      fb$functional_group <- ifelse(fb$functional_name %in% names(.FN_LABELS),
                                    .FN_LABELS[fb$functional_name], fb$functional_name)
      fb$biomass_t_ha <- round(fb$mean_biomass, 4)   # already T/ha
      keep_b <- intersect(c("year", "functional_group", "biomass_t_ha"), names(fb))
      func_out <- fb[, keep_b, drop = FALSE]
    }
  } else if (!is.null(data_func)) {
    req_func <- c("Functional_groups", "mean_biomass")
    if (!all(req_func %in% names(data_func))) {
      warning("data_func missing required columns (Functional_groups, mean_biomass) — ",
              "functional group breakdown skipped.")
    } else {
      func_out <- data_func[, c("Functional_groups", "mean_biomass"), drop = FALSE]
      names(func_out) <- c("functional_group", "biomass_t_ha")
      func_out$biomass_t_ha <- round(func_out$biomass_t_ha, 4)   # already T/ha
    }
  }

  # ── Step 4b: Family & species biomass pass-through (optional) ──────────────
  # Feed GET /species/family-biomass and GET /biomass/top-species. Unit kept as
  # delivered by the MCP (T/ha); caller decides top-N and family filter.
  family_out <- NULL
  if (!is.null(data_family) && all(c("family", "mean_biomass") %in% names(data_family))) {
    keep_f <- intersect(c("year", "family", "mean_biomass", "n_transects"), names(data_family))
    family_out <- data_family[, keep_f, drop = FALSE]
  }
  species_out <- NULL
  if (!is.null(data_species) && all(c("species_name", "mean_biomass") %in% names(data_species))) {
    keep_s <- intersect(c("species_id", "species_name", "trophic_group", "mean_biomass", "n_transects"),
                        names(data_species))
    species_out <- data_species[, keep_s, drop = FALSE]
  }

  # ── Interpretation ────────────────────────────────────────────────────────

  ba  <- before_after(annual_means, "mean_biomass_t_ha",
                      before_years = head(all_years, .KPI_YEARS),
                      recent_n     = .KPI_YEARS)
  mkt    <- mk_trend(annual_means, "mean_biomass_t_ha")
  mk_out <- .mk_label_output(mkt, n_unique_years,
                             annual_means$mean_biomass_t_ha[order(annual_means$year)])

  bio_status <- if (!is.null(ba$status)) "neutral" else {
    p_val <- if (!is.null(mkt$p)) mkt$p else 1
    if      (ba$direction == "increasing" && p_val < 0.10) "positive"
    else if (ba$direction == "decreasing" && p_val < 0.10) "negative"
    else "neutral"
  }

  bio_sentences <- if (!is.null(ba$status)) {
    list(en = NA_character_, es = NA_character_)
  } else {
    mk_note_en <- if (!is.null(mkt$p) && mkt$p < 0.10)
      paste0("Mann-Kendall trend: τ = ", mkt$tau, ", p = ", mkt$p, ".") else NULL
    mk_note_es <- if (!is.null(mkt$p) && mkt$p < 0.10)
      paste0("Tendencia Mann-Kendall: τ = ", mkt$tau, ", p = ", mkt$p, ".") else NULL
    insight_sentence(
      var_label_en  = "Fish biomass",
      var_label_es  = "La biomasa de peces",
      unit          = "T/ha",
      before_mean   = ba$before_mean, after_mean = ba$after_mean,
      delta         = ba$delta,       pct_change = ba$pct_change,
      before_period = ba$before_period, after_period = ba$after_period,
      extra_en      = mk_note_en,     extra_es   = mk_note_es
    )
  }

  interpretation <- list(
    before_period = ba$before_period,
    after_period  = ba$after_period,
    metric_before = ba$before_mean,
    metric_after  = ba$after_mean,
    delta         = ba$delta,
    pct_change    = ba$pct_change,
    direction     = if (!is.null(ba$direction)) ba$direction else "unknown",
    status        = bio_status,
    significance  = mkt,
    insight_es    = bio_sentences$es,
    insight_en    = bio_sentences$en
  )

  # ── Return ────────────────────────────────────────────────────────────────

  k_used <- min(.K_MAX, n_unique_years - 1L)

  list(
    value = list(
      kpi               = kpi,
      trend             = trend,
      annual_means      = annual_means,
      functional_groups = func_out,
      family_biomass    = family_out,
      species_biomass   = species_out
    ),
    mk             = mk_out,
    interpretation = interpretation,
    method = paste0(
      "KPI: observed mean biomass across most recent ", length(kpi_years), " survey years ",
      "(arithmetic mean of per-year reef means). ",
      "Trend: GAM with formula biomass ~ s(year, k = ", k_used, ") + s(reef, bs = 're'), ",
      "family = gaussian(), method = REML. ",
      "Prediction excludes reef random effect (population-level trend). ",
      "95% CI: fit ± 1.96 × se.fit over ", .GRID_POINTS, "-point year grid. ",
      "MCP pre-processing: label = PEC, biomass not null; ",
      "analysis filters (size outliers, Corredor Haemulidae/Carangidae, reefs with >= 5 years) ",
      "are NOT applied. Aggregation: transect SUM -> reef MEAN."
    ),
    params = list(
      k_max        = .K_MAX,
      k_used       = k_used,
      kpi_years    = .KPI_YEARS,
      gam_min_years = .GAM_MIN_YEARS,
      gam_family   = "gaussian",
      gam_method   = "REML",
      grid_points  = .GRID_POINTS,
      unit         = "T/ha",
      unit_note    = "MCP output is already in T/ha (Biomass = g/m² / 100); no conversion"
    )
  )
}
