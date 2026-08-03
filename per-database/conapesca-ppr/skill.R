# skill.R — conapesca-ppr
#
# Computes Primary Production Required (PPR) to sustain observed landings,
# at regional (state) and local (office) scales, for MAYORES and MENORES
# separately, per species.
#
# Formula: PPR_kg_C = (peso_total_kg / ww_to_carbon) × (1 / te)^(TL − 1)
# With defaults ww_to_carbon=9 and te=0.1, reduces to Pauly & Christensen (1995):
#   PPR_kg_C = (peso_total_kg / 9) × 10^(TL − 1)
#
# See SKILL.md for the full data contract, method, and do-not rules.

# ── Constants (fixed — do not change without updating SKILL.md) ──────────────

.VALID_AVISOS   <- c("MAYORES", "MENORES")
.MIN_TRIPS_WARN <- 5L

# ── run_skill ─────────────────────────────────────────────────────────────────
#
# Args:
#   data          data.frame — minimal contract from CONAPESCA MCP:
#                 required cols: anio_corte, tipo_aviso, nombre_estado,
#                 nombre_oficina, nombre_cientifico_canonico,
#                 trophic_level, peso_desembarcado_kg
#   species       character vector — canonical scientific names
#   state_filter  character — nombre_estado
#   office_filter character or NULL — nombre_oficina; NULL = regional only
#   ww_to_carbon  numeric — wet weight to carbon ratio (default: 9)
#   te            numeric — trophic transfer efficiency (default: 0.1)
#   tl_override   named numeric or NULL — user-supplied TL per species,
#                 e.g. c("Lutjanus peru" = 3.2). Takes precedence over catalog.
#
# Returns: list(value, method, params)

run_skill <- function(data,
                      species,
                      state_filter,
                      office_filter = NULL,
                      ww_to_carbon  = 9,
                      te            = 0.1,
                      tl_override   = NULL,
                      ...) {

  # ── Input validation ───────────────────────────────────────────────────────

  required_cols <- c("anio_corte", "tipo_aviso", "nombre_estado",
                     "nombre_oficina", "nombre_cientifico_canonico",
                     "trophic_level", "peso_desembarcado_kg")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0)
    stop("Missing columns in data: ", paste(missing_cols, collapse = ", "))

  if (missing(species) || length(species) == 0 || !any(nzchar(species)))
    stop("'species' is required — provide at least one nombre_cientifico_canonico.")
  if (missing(state_filter) || !nzchar(state_filter))
    stop("'state_filter' is required — provide nombre_estado.")
  if (!is.null(office_filter) && !nzchar(office_filter))
    stop("'office_filter' must be NULL or a non-empty string.")
  if (!is.numeric(ww_to_carbon) || ww_to_carbon <= 0)
    stop("'ww_to_carbon' must be a positive number.")
  if (!is.numeric(te) || te <= 0 || te >= 1)
    stop("'te' must be a number in (0, 1).")
  if (!is.null(tl_override)) {
    if (is.null(names(tl_override)) || any(!nzchar(names(tl_override))))
      stop("'tl_override' must be a named numeric vector ",
           "(names = canonical scientific names).")
    if (!is.numeric(tl_override))
      stop("'tl_override' values must be numeric trophic levels.")
  }

  # ── Step 1: Filter records ─────────────────────────────────────────────────

  data <- data[
    data$nombre_estado              == state_filter          &
    data$tipo_aviso                 %in% .VALID_AVISOS       &
    data$nombre_cientifico_canonico %in% species             &
    !is.na(data$peso_desembarcado_kg)                        &
    data$peso_desembarcado_kg       >  0, ]

  if (nrow(data) == 0)
    stop("No valid records after filtering for species=",
         paste(species, collapse = ", "),
         " in state='", state_filter, "'.")

  # ── Step 2: Resolve trophic level per species ─────────────────────────────

  tl_table <- data.frame(
    nombre_cientifico_canonico = species,
    stringsAsFactors = FALSE
  )

  # Catalog TL: one value per species (use first non-NA found)
  catalog_tl <- tapply(
    data$trophic_level,
    data$nombre_cientifico_canonico,
    function(x) {
      vals <- x[!is.na(x)]
      if (length(vals) == 0) NA_real_ else vals[1]
    }
  )
  tl_table$tl_catalog <- catalog_tl[tl_table$nombre_cientifico_canonico]

  # Apply overrides (take precedence over catalog)
  tl_table$tl_used   <- tl_table$tl_catalog
  tl_table$tl_source <- "catalog"

  if (!is.null(tl_override)) {
    for (sp in names(tl_override)) {
      if (sp %in% tl_table$nombre_cientifico_canonico) {
        tl_table$tl_used  [tl_table$nombre_cientifico_canonico == sp] <- tl_override[[sp]]
        tl_table$tl_source[tl_table$nombre_cientifico_canonico == sp] <- "override"
      }
    }
  }

  # Error if any species still has NA TL after override resolution
  missing_tl <- tl_table$nombre_cientifico_canonico[is.na(tl_table$tl_used)]
  if (length(missing_tl) > 0)
    stop("Trophic level not found in catalog for: ",
         paste(missing_tl, collapse = ", "), ". ",
         "Please supply values via tl_override, e.g.: ",
         "tl_override = c(", paste0('"', missing_tl, '" = <TL>', collapse = ", "), ").")

  # ── Step 3: Aggregate catch and apply PPR formula ─────────────────────────

  .compute_ppr <- function(df, escala_label) {
    years_species_fleets <- unique(
      df[, c("anio_corte", "nombre_cientifico_canonico", "tipo_aviso")]
    )

    result <- do.call(rbind, lapply(seq_len(nrow(years_species_fleets)), function(i) {
      yr    <- years_species_fleets$anio_corte[i]
      sp    <- years_species_fleets$nombre_cientifico_canonico[i]
      aviso <- years_species_fleets$tipo_aviso[i]

      sub <- df[
        df$anio_corte                  == yr    &
        df$nombre_cientifico_canonico  == sp    &
        df$tipo_aviso                  == aviso, ]

      n_registros    <- nrow(sub)
      peso_total_kg  <- sum(sub$peso_desembarcado_kg, na.rm = TRUE)

      tl_row   <- tl_table[tl_table$nombre_cientifico_canonico == sp, ]
      tl_used  <- tl_row$tl_used
      tl_source <- tl_row$tl_source

      ppr_kg_c <- (peso_total_kg / ww_to_carbon) * (1 / te)^(tl_used - 1)

      if (n_registros < .MIN_TRIPS_WARN)
        warning("escala=", escala_label, " | ", yr, " | ", aviso, " | ", sp,
                ": n_registros=", n_registros,
                " — PPR based on very few records, interpret with caution.")

      data.frame(
        anio_corte                 = yr,
        tipo_aviso                 = aviso,
        escala                     = escala_label,
        nombre_cientifico_canonico = sp,
        trophic_level_used         = tl_used,
        tl_source                  = tl_source,
        peso_desembarcado_kg_total = round(peso_total_kg, 2),
        ppr_kg_c                   = round(ppr_kg_c, 2),
        n_registros                = n_registros,
        stringsAsFactors           = FALSE
      )
    }))

    rownames(result) <- NULL
    result
  }

  # Regional (full state)
  result_regional <- .compute_ppr(data, "regional")

  # Local (by office, if requested)
  result_local <- NULL
  if (!is.null(office_filter)) {
    data_local <- data[data$nombre_oficina == office_filter, ]
    if (nrow(data_local) == 0)
      warning("No records for office_filter='", office_filter,
              "' — local series not produced.")
    else
      result_local <- .compute_ppr(data_local, "local")
  }

  value <- rbind(result_regional, result_local)
  rownames(value) <- NULL

  # ── Return ─────────────────────────────────────────────────────────────────

  formula_str <- paste0(
    "PPR_kg_C = (peso_total_kg / ", ww_to_carbon, ") x ",
    "(1 / ", te, ")^(TL - 1)"
  )

  list(
    value  = value,
    method = paste0(
      "Primary Production Required (Pauly & Christensen 1995). ",
      formula_str, ". ",
      "Catch aggregated by anio_corte x tipo_aviso x escala x species. ",
      "COSECHA excluded. MAYORES and MENORES always separate. ",
      "Only records with peso_desembarcado_kg > 0 included."
    ),
    params = list(
      species       = species,
      state_filter  = state_filter,
      office_filter = office_filter,
      ww_to_carbon  = ww_to_carbon,
      te            = te,
      tl_override   = tl_override,
      formula_used  = formula_str,
      tl_resolution = tl_table[, c("nombre_cientifico_canonico",
                                   "tl_used", "tl_source")]
    )
  )
}
