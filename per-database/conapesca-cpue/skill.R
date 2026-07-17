# skill.R — conapesca-cpue
#
# Computes CPUE (kg / effective fishing day) as mean-of-ratios per folio,
# at regional (state) and local (office) scales, for MAYORES and MENORES
# separately. Fishing pressure proxy from CONAPESCA landing records.
#
# See SKILL.md for the full data contract, method, and do-not rules.

# ── Constants (fixed — do not change without updating SKILL.md) ──────────────

.MIN_TRIPS_WARN  <- 5L     # n_viajes below which year-fleet cell is flagged
.VALID_AVISOS    <- c("MAYORES", "MENORES")  # COSECHA excluded

# ── run_skill ─────────────────────────────────────────────────────────────────
#
# Args:
#   data          data.frame — minimal contract from CONAPESCA MCP (group_by="folio"):
#                 required cols: folio_aviso, anio_corte, tipo_aviso,
#                 nombre_estado, nombre_oficina, nombre_cientifico_canonico,
#                 peso_desembarcado_kg, dias_efectivos, dias_efectivos_fuente,
#                 flag_fecha_generica, flag_dias_efectivos_sospechoso, flag_periodo_futuro
#   species       character — canonical scientific name (nombre_cientifico_canonico)
#   state_filter  character — state name (nombre_estado)
#   office_filter character or NULL — landing office (nombre_oficina); NULL = regional only
#
# Returns: list(value, method, params)

run_skill <- function(data, species, state_filter, office_filter = NULL, ...) {

  # ── Input validation ───────────────────────────────────────────────────────

  required_cols <- c("folio_aviso", "anio_corte", "tipo_aviso",
                     "nombre_estado", "nombre_oficina",
                     "nombre_cientifico_canonico", "peso_desembarcado_kg",
                     "dias_efectivos", "dias_efectivos_fuente",
                     "flag_fecha_generica", "flag_dias_efectivos_sospechoso",
                     "flag_periodo_futuro")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0)
    stop("Missing columns in data: ", paste(missing_cols, collapse = ", "))

  if (missing(species)      || !nzchar(species))
    stop("'species' is required — provide nombre_cientifico_canonico.")
  if (missing(state_filter) || !nzchar(state_filter))
    stop("'state_filter' is required — provide nombre_estado.")
  if (!is.null(office_filter) && !nzchar(office_filter))
    stop("'office_filter' must be NULL or a non-empty string.")

  # ── Step 1: Filter ─────────────────────────────────────────────────────────

  # Count total folios for target species in state (before quality filters)
  folios_species_state <- data[
    data$nombre_estado              == state_filter &
    data$tipo_aviso                 %in% .VALID_AVISOS &
    data$nombre_cientifico_canonico == species &
    data$peso_desembarcado_kg       >  0, ]

  n_folios_total <- nrow(folios_species_state)

  # Apply quality filters
  data_clean <- folios_species_state[
    !folios_species_state$flag_fecha_generica            &
    !folios_species_state$flag_dias_efectivos_sospechoso &
    !is.na(folios_species_state$dias_efectivos), ]

  n_excluidos <- n_folios_total - nrow(data_clean)

  if (nrow(data_clean) == 0)
    stop("No valid records after quality filters for species='", species,
         "' in state='", state_filter, "'.")

  # ── Step 2: CPUE per folio ─────────────────────────────────────────────────
  # dias_efectivos is already folio-level — do NOT sum across rows

  data_clean$cpue_folio <- data_clean$peso_desembarcado_kg / data_clean$dias_efectivos

  # ── Step 3 & 4: Aggregate by year × fleet × scale ─────────────────────────

  .aggregate_cpue <- function(df, escala_label, n_excl) {
    years_fleets <- unique(df[, c("anio_corte", "tipo_aviso")])

    do.call(rbind, lapply(seq_len(nrow(years_fleets)), function(i) {
      yr    <- years_fleets$anio_corte[i]
      aviso <- years_fleets$tipo_aviso[i]

      sub <- df[df$anio_corte == yr & df$tipo_aviso == aviso, ]

      n_viajes  <- nrow(sub)
      n_recomp  <- sum(sub$dias_efectivos_fuente == "recomputado", na.rm = TRUE)

      if (n_viajes < .MIN_TRIPS_WARN)
        warning("escala=", escala_label, " | ", yr, " | ", aviso,
                ": n_viajes=", n_viajes, " < ", .MIN_TRIPS_WARN,
                " — CPUE mean unreliable with few trips.")

      data.frame(
        anio_corte                  = yr,
        tipo_aviso                  = aviso,
        escala                      = escala_label,
        nombre_cientifico_canonico  = species,
        cpue_media                  = round(mean(sub$cpue_folio), 4),
        cpue_sd                     = round(sd(sub$cpue_folio), 4),
        n_viajes                    = n_viajes,
        n_viajes_excluidos          = n_excl,
        peso_desembarcado_kg_total  = sum(sub$peso_desembarcado_kg),
        n_viajes_recomputado        = n_recomp
      )
    }))
  }

  # Regional series (full state)
  result_regional <- .aggregate_cpue(data_clean, "regional", n_excluidos)

  # Local series (by office, if requested)
  result_local <- NULL
  if (!is.null(office_filter)) {
    data_local <- data_clean[data_clean$nombre_oficina == office_filter, ]
    if (nrow(data_local) == 0)
      warning("No records for office_filter='", office_filter,
              "' — local series not produced.")
    else {
      n_excl_local <- sum(
        folios_species_state$nombre_oficina == office_filter) - nrow(data_local)
      result_local <- .aggregate_cpue(data_local, "local", n_excl_local)
    }
  }

  value <- rbind(result_regional, result_local)
  rownames(value) <- NULL

  # ── Return ────────────────────────────────────────────────────────────────

  list(
    value  = value,
    method = paste0(
      "Mean-of-ratios CPUE: mean(sum_kg_folio / dias_efectivos_folio) ",
      "per anio_corte × tipo_aviso × escala. ",
      "Quality filters: flag_fecha_generica=FALSE, ",
      "flag_dias_efectivos_sospechoso=FALSE, !is.na(dias_efectivos). ",
      "COSECHA excluded. MAYORES and MENORES always separate."
    ),
    params = list(
      species       = species,
      state_filter  = state_filter,
      office_filter = office_filter,
      flotas        = .VALID_AVISOS,
      min_trips_warn = .MIN_TRIPS_WARN
    )
  )
}
