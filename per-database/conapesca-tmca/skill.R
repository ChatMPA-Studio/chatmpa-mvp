# skill.R — conapesca-tmca
#
# Computes the Mean Annual Growth Rate (TMCA — Tasa Media de Crecimiento Anual)
# of landed volume for a landing office, and classifies the trend into an
# interpretable category.
#
# Formula: TMCA = ((V_end / V_start)^(1/n) - 1) x 100
# Where n = number of years between V_start and V_end.
#
# Without species filter: TMCA reflects the overall trend of the landing office
# (all species combined). Valid — CONAPESCA annual reports use TMCA at office
# and state level without species breakdown.
# With species filter: TMCA reflects the trend of that species/resource at this
# specific office — the MCP already filtered data to the office before this skill
# runs, so the species filter narrows what is measured, not where.
# Both uses are valid — see SKILL.md.
#
# Data contract:
#   data — pre-aggregated from get_landings(group_by="year_fleet").
#          MCP handles all filtering (office, state, species, year_range).
#
# See SKILL.md for the full data contract, method, and do-not rules.

# ── Constants (fixed — do not change without updating SKILL.md) ──────────────

.DEFAULT_WINDOW  <- 10L   # years back from most recent

# Categorization thresholds (annual TMCA in %)
.THRESH_FAST_UP   <-  3
.THRESH_MOD_UP    <-  1
.THRESH_MOD_DOWN  <- -1
.THRESH_FAST_DOWN <- -3

.VALID_AVISOS <- c("MAYORES", "MENORES", "COSECHA")

# ── run_skill ─────────────────────────────────────────────────────────────────
#
# Args:
#   data                       data.frame — pre-aggregated from
#                              get_landings(group_by="year_fleet").
#                              Required cols: anio_corte, tipo_aviso, total_kg.
#   window                     integer — years in the window (default: 10).
#                              Window runs from (yr_max - window) to yr_max.
#                              If the start year is absent, uses the nearest
#                              available year with warning().
#   fleet_filter               character or NULL — "MAYORES", "MENORES",
#                              "COSECHA", or NULL (all fleets).
#   office_filter              character or NULL — label only.
#   state_filter               character or NULL — label only.
#   resource_group             character or NULL — label only (nombre_principal).
#   species                    character or NULL — label only (nombre_cientifico_canonico).
#                              Mutually exclusive with resource_group.
#
# Returns: list(tmca, category, value, method, params)
#   tmca     — numeric: TMCA in % (rounded to 2 decimal places)
#   category — character: "growing" | "growing moderately" | "stable" |
#              "declining moderately" | "declining"
#   value    — data.frame with annual series used for computation

run_skill <- function(data,
                      window         = .DEFAULT_WINDOW,
                      fleet_filter   = NULL,
                      office_filter  = NULL,
                      state_filter   = NULL,
                      resource_group = NULL,
                      species        = NULL,
                      ...) {

  # ── Input validation ───────────────────────────────────────────────────────

  data <- .normalize_tmca_cols(data)

  required_cols <- c("anio_corte", "tipo_aviso", "total_kg")
  missing_cols  <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0)
    stop("Missing columns in data: ", paste(missing_cols, collapse = ", "))

  if (!is.null(resource_group) && !is.null(species))
    stop("'resource_group' and 'species' are mutually exclusive.")

  window <- as.integer(window)
  if (is.na(window) || window < 2L)
    stop("'window' must be an integer >= 2.")

  if (!is.null(fleet_filter) && !fleet_filter %in% .VALID_AVISOS)
    stop("'fleet_filter' must be one of: ", paste(.VALID_AVISOS, collapse = ", "),
         " or NULL.")

  if (nrow(data) == 0)
    stop("No records found in data.")

  # ── Step 1: Optional fleet filter ─────────────────────────────────────────

  data_valid <- data[data$tipo_aviso %in% .VALID_AVISOS, ]

  if (!is.null(fleet_filter)) {
    data_valid <- data_valid[data_valid$tipo_aviso == fleet_filter, ]
    if (nrow(data_valid) == 0)
      stop("No records remain after fleet_filter='", fleet_filter, "'.")
  }

  # ── Step 2: Annual totals ──────────────────────────────────────────────────

  all_years <- sort(unique(data_valid$anio_corte))

  annual <- do.call(rbind, lapply(all_years, function(yr) {
    sub <- data_valid[data_valid$anio_corte == yr, ]
    data.frame(
      anio_corte      = yr,
      total_tonnes    = sum(sub$total_kg, na.rm = TRUE) / 1000,
      stringsAsFactors = FALSE
    )
  }))

  # ── Step 3: Resolve window endpoints ──────────────────────────────────────

  yr_end   <- max(annual$anio_corte)
  yr_ideal <- yr_end - window

  if (yr_ideal %in% annual$anio_corte) {
    yr_start <- yr_ideal
  } else {
    # Use closest available year >= yr_ideal; fall back to earliest
    candidates <- annual$anio_corte[annual$anio_corte >= yr_ideal]
    yr_start   <- if (length(candidates) > 0) min(candidates) else min(annual$anio_corte)
    warning("Year ", yr_ideal, " not available. Using ", yr_start,
            " as start of TMCA window.")
  }

  n <- yr_end - yr_start

  if (n < 2L)
    stop("Window too short (n=", n, " years) — need at least 2 years for TMCA.")

  v_end   <- annual$total_tonnes[annual$anio_corte == yr_end]
  v_start <- annual$total_tonnes[annual$anio_corte == yr_start]

  if (is.na(v_start) || v_start <= 0)
    stop("V_start (", yr_start, ") = ", v_start,
         " — cannot compute TMCA (zero or NA).")
  if (is.na(v_end) || v_end <= 0)
    stop("V_end (", yr_end, ") = ", v_end,
         " — cannot compute TMCA (zero or NA).")

  # ── Step 4: TMCA ──────────────────────────────────────────────────────────

  tmca_val <- round(((v_end / v_start)^(1 / n) - 1) * 100, 2)

  # ── Step 5: Categorize ────────────────────────────────────────────────────

  category <- .categorize(tmca_val)

  # ── Return ─────────────────────────────────────────────────────────────────

  list(
    tmca     = tmca_val,
    category = category,
    value    = annual,
    method   = paste0(
      "TMCA = ((V_", yr_end, " / V_", yr_start, ")^(1/", n, ") - 1) x 100. ",
      "V_", yr_end, " = ", round(v_end, 1), " t; ",
      "V_", yr_start, " = ", round(v_start, 1), " t. ",
      if (!is.null(fleet_filter)) paste0("Fleet: ", fleet_filter, ". ")
      else "All fleets. ",
      "Thresholds: >3% growing, 1-3% growing moderately, ",
      "-1 to 1% stable, -3 to -1% declining moderately, <-3% declining."
    ),
    params = list(
      window                     = window,
      yr_start                   = yr_start,
      yr_end                     = yr_end,
      n_years                    = n,
      fleet_filter               = fleet_filter,
      office_filter  = office_filter,
      state_filter   = state_filter,
      resource_group = resource_group,
      species        = species
    )
  )
}

# ── .categorize ───────────────────────────────────────────────────────────────

.categorize <- function(tmca) {
  if      (tmca >  .THRESH_FAST_UP)   "growing"
  else if (tmca >  .THRESH_MOD_UP)    "growing moderately"
  else if (tmca >= .THRESH_MOD_DOWN)  "stable"
  else if (tmca >= .THRESH_FAST_DOWN) "declining moderately"
  else                                "declining"
}

# ── .normalize_tmca_cols ──────────────────────────────────────────────────────
# Accepts either MCP aggregate column names or raw column names.

.normalize_tmca_cols <- function(data) {
  if (!"total_kg" %in% names(data) && "peso_desembarcado_kg" %in% names(data))
    data$total_kg <- data$peso_desembarcado_kg
  data
}
