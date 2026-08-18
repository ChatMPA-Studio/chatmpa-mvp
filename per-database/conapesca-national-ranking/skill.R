# skill.R — conapesca-national-ranking
#
# Computes the national position of a landing office by total landed volume
# (tonnes) and estimated production value (MXN).
#
# Without species filter: ranks the office by overall port importance.
# With species filter: ranks the office within that specific fishery.
# Both uses are valid — see SKILL.md.
#
# Data contract:
#   data — pre-aggregated from get_landings(group_by="office_year_fleet").
#          MCP handles all filtering (species, resource, year_range).
#          Data covers ALL offices — not just the focal one.
#
# See SKILL.md for the full data contract, method, and do-not rules.

# ── Constants ─────────────────────────────────────────────────────────────────

.VALID_FLEET_TYPES <- c("MAYORES", "MENORES", "COSECHA")

# ── run_skill ─────────────────────────────────────────────────────────────────
#
# Args:
#   data                       data.frame — pre-aggregated from
#                              get_landings(group_by="office_year_fleet").
#                              Required cols: nombre_oficina, nombre_estado,
#                              anio_corte, tipo_aviso, total_kg,
#                              total_valor_mxn, n_registros.
#   office_filter              character — focal office (nombre_oficina). Required.
#   state_filter               character — focal office state. Required.
#                              Office names are not unique across states.
#   nombre_principal           character or NULL — label only.
#   nombre_cientifico_canonico character or NULL — label only.
#                              Mutually exclusive with nombre_principal.
#   year_range                 integer vector or NULL — label only.
#   fleet_filter               character or NULL — one of MAYORES/MENORES/COSECHA,
#                              or NULL to aggregate all fleet types.
#
# Returns: list(value, ranking, method, params)
#   value   — full ranked table (all offices)
#   ranking — focal office scalars: rank_volumen, rank_valor, n_offices,
#             pct_volumen, pct_valor, total_toneladas, total_valor_mxn

run_skill <- function(data,
                      office_filter,
                      state_filter,
                      nombre_principal           = NULL,
                      nombre_cientifico_canonico = NULL,
                      year_range                 = NULL,
                      fleet_filter               = NULL,
                      ...) {

  # ── Input validation ───────────────────────────────────────────────────────

  data <- .normalize_ranking_cols(data)

  required_cols <- c("nombre_oficina", "nombre_estado", "anio_corte",
                     "tipo_aviso", "total_kg", "total_valor_mxn", "n_registros")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0)
    stop("Missing columns in data: ", paste(missing_cols, collapse = ", "))

  if (missing(office_filter) || is.null(office_filter) || !nzchar(office_filter))
    stop("'office_filter' is required.")
  if (missing(state_filter) || is.null(state_filter) || !nzchar(state_filter))
    stop("'state_filter' is required.")
  if (!is.null(nombre_principal) && !is.null(nombre_cientifico_canonico))
    stop("'nombre_principal' and 'nombre_cientifico_canonico' are mutually exclusive.")
  if (!is.null(fleet_filter) && !fleet_filter %in% .VALID_FLEET_TYPES)
    stop("'fleet_filter' must be one of: ", paste(.VALID_FLEET_TYPES, collapse = ", "),
         " or NULL.")

  if (nrow(data) == 0)
    stop("No records found in data.")

  # ── Step 1: Optional fleet filter ─────────────────────────────────────────

  if (!is.null(fleet_filter)) {
    data <- data[data$tipo_aviso == fleet_filter, ]
    if (nrow(data) == 0)
      stop("No records remain after fleet_filter='", fleet_filter, "'.")
  }

  # ── Step 2: Aggregate per office (sum across years and fleets) ─────────────

  offices <- unique(data[, c("nombre_oficina", "nombre_estado")])

  value <- do.call(rbind, lapply(seq_len(nrow(offices)), function(i) {
    of  <- offices$nombre_oficina[i]
    est <- offices$nombre_estado[i]
    sub <- data[data$nombre_oficina == of & data$nombre_estado == est, ]

    data.frame(
      nombre_oficina   = of,
      nombre_estado    = est,
      total_toneladas  = sum(sub$total_kg,        na.rm = TRUE) / 1000,
      total_valor_mxn  = sum(sub$total_valor_mxn, na.rm = TRUE),
      n_years          = length(unique(sub$anio_corte)),
      n_registros      = sum(sub$n_registros,     na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  }))

  # ── Step 3: Rank and compute national share ────────────────────────────────

  n_offices       <- nrow(value)
  total_nac_ton   <- sum(value$total_toneladas, na.rm = TRUE)
  total_nac_valor <- sum(value$total_valor_mxn, na.rm = TRUE)

  value$rank_volumen <- rank(-value$total_toneladas, ties.method = "min")
  value$rank_valor   <- rank(-value$total_valor_mxn, ties.method = "min")
  value$pct_volumen  <- round(value$total_toneladas / total_nac_ton   * 100, 2)
  value$pct_valor    <- round(value$total_valor_mxn / total_nac_valor * 100, 2)

  # ── Step 4: Extract focal office scalars ──────────────────────────────────

  focal_mask <- value$nombre_oficina == office_filter &
                value$nombre_estado  == state_filter
  if (!any(focal_mask))
    stop("Focal office '", office_filter, "' / '", state_filter,
         "' not found in data after aggregation.")

  focal <- value[focal_mask, ]

  ranking <- list(
    rank_volumen    = focal$rank_volumen,
    rank_valor      = focal$rank_valor,
    n_offices       = n_offices,
    pct_volumen     = focal$pct_volumen,
    pct_valor       = focal$pct_valor,
    total_toneladas = focal$total_toneladas,
    total_valor_mxn = focal$total_valor_mxn
  )

  # Sort by volume rank
  value <- value[order(value$rank_volumen), ]
  rownames(value) <- NULL

  # ── Return ─────────────────────────────────────────────────────────────────

  list(
    value   = value,
    ranking = ranking,
    method  = paste0(
      "National ranking by total landed volume and estimated value. ",
      "Data pre-aggregated by MCP (get_landings group_by='office_year_fleet'). ",
      "Aggregation: sum(total_kg)/1000 and sum(total_valor_mxn) per office ",
      "across all years",
      if (!is.null(fleet_filter)) paste0(" (fleet: ", fleet_filter, ")") else " (all fleets)",
      ". Ranked by descending total (ties.method='min'). ",
      "pct_volumen and pct_valor = office share of national total."
    ),
    params  = list(
      office_filter              = office_filter,
      state_filter               = state_filter,
      nombre_principal           = nombre_principal,
      nombre_cientifico_canonico = nombre_cientifico_canonico,
      year_range                 = year_range,
      fleet_filter               = fleet_filter
    )
  )
}

# ── .normalize_ranking_cols ───────────────────────────────────────────────────
# Accepts either MCP aggregate column names or raw column names.

.normalize_ranking_cols <- function(data) {
  if (!"total_kg" %in% names(data) && "peso_desembarcado_kg" %in% names(data))
    data$total_kg <- data$peso_desembarcado_kg
  if (!"total_valor_mxn" %in% names(data) && "valor_pesos_estimado" %in% names(data))
    data$total_valor_mxn <- data$valor_pesos_estimado
  if (!"n_registros" %in% names(data) && "n_records" %in% names(data))
    data$n_registros <- data$n_records
  data
}
