# skill.R — ppr-ratio
#
# Computes %PPR: the fraction of the AMP's net primary production required
# to sustain observed fishery catches. Combines:
#   - conapesca-ppr output (PPR in kg C/year, per species × fleet)
#   - erddap-pp output (NPP geometric mean in mgC/m²/day, local scale)
#
# Formula:
#   PP_total_kg_C  = pp_geomean × amp_area_km2 × 365 / 1000
#   PPR_total_kg_C = sum(ppr_kg_c)  [all species × fleets per year]
#   ppr_pct        = (PPR_total_kg_C / PP_total_kg_C) × 100
#
# Reference: Pauly & Christensen (1995). Nature, 374, 255–257.
# See SKILL.md for full data contract and do-not rules.

# ── run_skill ─────────────────────────────────────────────────────────────────
#
# Args:
#   data_ppr      data.frame — value from conapesca-ppr, escala="local"
#                 required cols: anio_corte (int), tipo_aviso (chr),
#                 nombre_cientifico_canonico (chr), ppr_kg_c (num)
#   data_pp       data.frame — value from erddap-pp, escala="local"
#                 required cols: year (int), pp_geomean (num), cobertura_pct (num)
#   amp_area_km2  numeric scalar — AMP polygon area in km²
#
# Returns: list(value, method, params)

run_skill <- function(data_ppr, data_pp, amp_area_km2, ...) {

  # ── Input validation ───────────────────────────────────────────────────────

  .check_cols <- function(df, cols, name) {
    missing <- setdiff(cols, names(df))
    if (length(missing) > 0)
      stop(name, " missing columns: ", paste(missing, collapse = ", "))
  }

  .check_cols(data_ppr, c("anio_corte", "tipo_aviso",
                           "nombre_cientifico_canonico", "ppr_kg_c"), "data_ppr")
  .check_cols(data_pp,  c("year", "pp_geomean", "cobertura_pct"),      "data_pp")

  if (!is.numeric(amp_area_km2) || length(amp_area_km2) != 1 || amp_area_km2 <= 0)
    stop("'amp_area_km2' must be a single positive number.")

  # ── Step 1: Convert PP to total areal production (kg C/year) ──────────────
  # pp_geomean (mgC/m²/day) × area (km²) × 1e6 (m²/km²) × 365 (days) / 1e9 (mg→kg)
  # = pp_geomean × amp_area_km2 × 365 / 1000

  data_pp <- data_pp[!is.na(data_pp$pp_geomean), ]
  data_pp$pp_total_kg_c <- data_pp$pp_geomean * amp_area_km2 * 365

  # ── Step 2: Sum PPR across species × fleets per year ──────────────────────

  ppr_by_year <- aggregate(ppr_kg_c ~ anio_corte,
                           data = data_ppr,
                           FUN  = sum, na.rm = TRUE)
  names(ppr_by_year)[2] <- "ppr_total_kg_c"

  # ── Step 3: Join on year (complete cases only) ─────────────────────────────

  all_years_attempted <- sort(union(data_ppr$anio_corte, data_pp$year))

  pp_yr  <- data_pp[,  c("year", "pp_geomean", "pp_total_kg_c", "cobertura_pct")]
  ppr_yr <- ppr_by_year
  names(ppr_yr)[1] <- "year"

  joined <- merge(ppr_yr, pp_yr, by = "year", all = FALSE)
  joined <- joined[complete.cases(joined), ]

  years_excluded <- setdiff(all_years_attempted, joined$year)
  n_used <- nrow(joined)

  if (n_used == 0)
    stop("No overlapping complete-case years between data_ppr and data_pp. ",
         "Check that both skills cover the same time period.")

  # ── Step 4: Compute %PPR ──────────────────────────────────────────────────

  joined$ppr_pct <- round(joined$ppr_total_kg_c / joined$pp_total_kg_c * 100, 3)

  annual <- joined[, c("year", "pp_geomean", "pp_total_kg_c",
                       "ppr_total_kg_c", "ppr_pct", "cobertura_pct")]
  annual <- annual[order(annual$year), ]
  rownames(annual) <- NULL

  # ── Step 5: Decomposition by species × fleet ──────────────────────────────
  # Merge with PP total to allow per-species %PPR decomposition if needed

  decomp <- merge(
    data_ppr[, c("anio_corte", "tipo_aviso", "nombre_cientifico_canonico", "ppr_kg_c")],
    data_pp[,  c("year", "pp_total_kg_c")],
    by.x = "anio_corte", by.y = "year",
    all.x = FALSE
  )
  decomp <- decomp[complete.cases(decomp), ]
  decomp$ppr_pct_species <- round(decomp$ppr_kg_c / decomp$pp_total_kg_c * 100, 3)
  decomp <- decomp[order(decomp$anio_corte,
                         decomp$nombre_cientifico_canonico,
                         decomp$tipo_aviso), ]
  rownames(decomp) <- NULL

  # ── Return ─────────────────────────────────────────────────────────────────

  formula_str <- paste0(
    "PP_total_kg_C = pp_geomean x amp_area_km2 x 365; ",
    "PPR_total_kg_C = sum(ppr_kg_c) across species x fleets; ",
    "ppr_pct = (PPR_total_kg_C / PP_total_kg_C) x 100"
  )

  list(
    value = list(
      annual         = annual,
      decomposition  = decomp,
      n_years_used   = n_used,
      years_excluded = years_excluded
    ),
    method = paste0(
      "%PPR (Pauly & Christensen 1995): fraction of local net primary production ",
      "required to sustain observed fishery catches. ",
      formula_str, ". ",
      "PPR from conapesca-ppr (local scale); ",
      "PP from erddap-pp (local scale, annual geometric mean). ",
      "Complete cases only — years with NA pp_geomean excluded."
    ),
    params = list(
      amp_area_km2 = amp_area_km2,
      formula_used = formula_str,
      n_years_used = n_used,
      years_excluded = years_excluded,
      species = sort(unique(data_ppr$nombre_cientifico_canonico))
    )
  )
}
