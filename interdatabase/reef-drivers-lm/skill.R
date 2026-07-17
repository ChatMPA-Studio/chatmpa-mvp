# skill.R — reef-drivers-lm
#
# Fits a multiple linear regression of reef trophic health (NRSI) against
# local fishing pressure (CPUE per species × fleet), SST anomaly, and
# chlorophyll anomaly. MAYORES and MENORES are always separate predictors.
# Species must be provided as canonical scientific names.
#
# See SKILL.md for the full data contract, method, and do-not rules.
#
# Dependencies: car (VIF calculation)

# ── Constants (fixed — do not change without updating SKILL.md) ──────────────

.MIN_YEARS_BUFFER <- 1   # n_years_used must exceed n_params by at least this
.MIN_YEARS_WARN   <- 10  # low power → warning()
.VIF_WARN         <- 5   # collinearity threshold

# ── run_skill ─────────────────────────────────────────────────────────────────
#
# Args:
#   data_nrsi   data.frame — value$nrsi_by_reef from ltem-nrsi-index
#               required cols: time (int, used as year), reef (chr), nrsi (num)
#   data_cpue   named list of data.frames — one element per target species,
#               each being the value from conapesca-cpue filtered to escala="local"
#               required cols per element: anio_corte (int), tipo_aviso (chr),
#               cpue_media (num)
#               names(data_cpue) must be canonical scientific names
#   data_sst    data.frame — value from erddap-sst-anomaly, escala="local"
#               required cols: year (int), anomalia_media (num)
#   data_chl    data.frame — value from erddap-chlorophyll, escala="local"
#               required cols: year (int), anomalia_log10 (num)
#
# Returns: list(value, method, params)

run_skill <- function(data_nrsi, data_cpue, data_sst, data_chl, ...) {

  # ── Input validation ────────────────────────────────────────────────────────

  .check_cols <- function(df, cols, name) {
    missing <- setdiff(cols, names(df))
    if (length(missing) > 0)
      stop(name, " missing columns: ", paste(missing, collapse = ", "))
  }

  # data_nrsi: accepts 'time' (ltem-nrsi-index native) or 'year'
  if ("time" %in% names(data_nrsi) && !"year" %in% names(data_nrsi))
    names(data_nrsi)[names(data_nrsi) == "time"] <- "year"
  .check_cols(data_nrsi, c("year", "reef", "nrsi"), "data_nrsi")

  if (!is.list(data_cpue) || is.data.frame(data_cpue))
    stop("'data_cpue' must be a named list of data.frames, one per species. ",
         "Each name must be the canonical scientific name.")
  if (is.null(names(data_cpue)) || any(!nzchar(names(data_cpue))))
    stop("All elements of 'data_cpue' must have non-empty names ",
         "(canonical scientific names, e.g. 'Lutjanus peru').")
  if (length(data_cpue) == 0)
    stop("'data_cpue' is empty — provide at least one species.")

  for (sp in names(data_cpue)) {
    .check_cols(data_cpue[[sp]],
                c("anio_corte", "tipo_aviso", "cpue_media"),
                paste0("data_cpue[['", sp, "']]"))
  }

  .check_cols(data_sst, c("year", "anomalia_media"), "data_sst")
  .check_cols(data_chl, c("year", "anomalia_log10"), "data_chl")

  # ── Step 1: Aggregate NRSI to AMP-year ─────────────────────────────────────

  nrsi_yr <- aggregate(nrsi ~ year, data = data_nrsi, FUN = mean, na.rm = TRUE)
  names(nrsi_yr)[2] <- "nrsi_mean"

  # ── Step 2: Build CPUE predictor columns (species × fleet) ─────────────────
  # Each species contributes cpue_menores_{sp_col} and cpue_mayores_{sp_col}

  cpue_wide <- nrsi_yr[, "year", drop = FALSE]   # start from year scaffold

  cpue_col_names <- character(0)   # track which predictor columns were created

  for (sp in names(data_cpue)) {
    sp_col <- gsub(" ", "_", sp)   # "Lutjanus peru" → "Lutjanus_peru"

    df_sp <- data_cpue[[sp]]
    df_sp <- df_sp[df_sp$escala == "local" | !("escala" %in% names(df_sp)), ]

    for (fleet in c("MENORES", "MAYORES")) {
      col_name <- paste0("cpue_", tolower(fleet), "_", sp_col)

      df_fleet <- df_sp[df_sp$tipo_aviso == fleet, c("anio_corte", "cpue_media")]
      if (nrow(df_fleet) == 0) {
        # Fleet absent for this species — column will be NA (dropped in complete cases)
        warning("No data for fleet='", fleet, "' / species='", sp,
                "' — predictor '", col_name, "' will be NA for all years.")
        cpue_wide[[col_name]] <- NA_real_
      } else {
        names(df_fleet) <- c("year", col_name)
        cpue_wide <- merge(cpue_wide, df_fleet, by = "year", all.x = TRUE)
      }
      cpue_col_names <- c(cpue_col_names, col_name)
    }
  }

  # ── Step 3: Join all predictors on year (complete cases) ───────────────────

  sst_yr <- data_sst[, c("year", "anomalia_media")]
  names(sst_yr)[2] <- "anom_sst"

  chl_yr <- data_chl[, c("year", "anomalia_log10")]
  names(chl_yr)[2] <- "anom_chl"

  all_years <- Reduce(function(a, b) merge(a, b, by = "year", all = FALSE),
                      list(nrsi_yr, cpue_wide, sst_yr, chl_yr))

  all_years_attempted <- sort(unique(c(
    data_nrsi$year,
    unlist(lapply(data_cpue, function(d) d$anio_corte)),
    data_sst$year,
    data_chl$year
  )))
  years_excluded <- setdiff(all_years_attempted, all_years$year)

  all_years <- all_years[complete.cases(all_years), ]
  n_used <- nrow(all_years)

  # ── Step 4: Sample-size check (depends on n predictors) ────────────────────

  # Drop columns that are all-NA after complete-case filter (absent fleets)
  active_cpue_cols <- cpue_col_names[
    cpue_col_names %in% names(all_years) &
    sapply(cpue_col_names[cpue_col_names %in% names(all_years)],
           function(col) any(!is.na(all_years[[col]])))]

  n_params <- length(active_cpue_cols) + 2L + 1L  # CPUE cols + SST + Chl + intercept

  if (n_used < n_params + .MIN_YEARS_BUFFER) {
    stop("Only ", n_used, " complete-case years available. Model has ", n_params,
         " parameters (", length(active_cpue_cols), " CPUE predictors + SST + Chl ",
         "+ intercept). Minimum required: ", n_params + .MIN_YEARS_BUFFER, ". ",
         "Reduce the number of species or provide a longer time series.")
  }

  if (n_used < .MIN_YEARS_WARN)
    warning("n_years_used = ", n_used, " < ", .MIN_YEARS_WARN,
            ". Results have low power — interpret with caution.")

  # ── Step 5: Build formula and fit OLS ──────────────────────────────────────

  predictors   <- c(active_cpue_cols, "anom_sst", "anom_chl")
  formula_str  <- paste("nrsi_mean ~", paste(predictors, collapse = " + "))
  formula_used <- as.formula(formula_str)

  fit <- lm(formula_used, data = all_years)
  s   <- summary(fit)

  coef_table <- as.data.frame(s$coefficients)
  names(coef_table) <- c("estimate", "std_error", "t_value", "p_value")
  coef_table$predictor <- rownames(coef_table)
  rownames(coef_table) <- NULL
  coef_table <- coef_table[, c("predictor", "estimate", "std_error",
                                "t_value", "p_value")]

  # ── Step 6: VIF ────────────────────────────────────────────────────────────

  if (!requireNamespace("car", quietly = TRUE)) {
    warning("Package 'car' not available — VIF not computed.")
    vif_vals <- data.frame(predictor = character(), vif = numeric())
  } else {
    vif_raw  <- car::vif(fit)
    vif_vals <- data.frame(predictor = names(vif_raw),
                           vif       = round(unname(vif_raw), 3))
    high_vif <- vif_vals$predictor[vif_vals$vif > .VIF_WARN]
    if (length(high_vif) > 0)
      warning("VIF > ", .VIF_WARN, " for: ", paste(high_vif, collapse = ", "),
              " — collinearity concern.")
  }

  # ── Step 7: Fitted values and residuals ────────────────────────────────────

  fitted_df <- data.frame(
    year      = all_years$year,
    nrsi_obs  = all_years$nrsi_mean,
    nrsi_fit  = round(fitted(fit), 4),
    residual  = round(residuals(fit), 4)
  )

  # ── Return ─────────────────────────────────────────────────────────────────

  list(
    value = list(
      coefficients   = coef_table,
      r_squared      = round(s$r.squared, 4),
      r_squared_adj  = round(s$adj.r.squared, 4),
      f_statistic    = round(s$fstatistic[["value"]], 3),
      f_p_value      = round(pf(s$fstatistic[1], s$fstatistic[2],
                                s$fstatistic[3], lower.tail = FALSE), 4),
      vif            = vif_vals,
      fitted         = fitted_df,
      n_years_used   = n_used,
      years_excluded = years_excluded,
      formula_used   = formula_str,
      species        = names(data_cpue)
    ),
    method = paste0(
      "OLS multiple linear regression: ", formula_str, ". ",
      "Species (", paste(names(data_cpue), collapse = "; "), ") provided as ",
      "canonical scientific names. MAYORES and MENORES are separate predictors ",
      "per species. Predictors: local CPUE per species × fleet type, ",
      "local SST anomaly (NOAA 1971-2000 baseline), ",
      "local chl-a log10 anomaly (2003-2020 baseline). ",
      "Complete cases only. No variable selection. Associations only — not causal."
    ),
    params = list(
      formula_used     = formula_str,
      species          = names(data_cpue),
      cpue_predictors  = active_cpue_cols,
      scale            = "local",
      aggregation_nrsi = "mean across reefs per year",
      n_params         = n_params,
      min_years_error  = n_params + .MIN_YEARS_BUFFER,
      min_years_warn   = .MIN_YEARS_WARN,
      vif_threshold    = .VIF_WARN
    )
  )
}
