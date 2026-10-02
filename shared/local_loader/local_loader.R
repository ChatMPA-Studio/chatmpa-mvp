# local_loader.R
#
# Dev-mode data access layer. Reads local RDS copies of CONAPESCA and LTEM
# and returns data.frames in the same shape that MCP tools would return.
#
# USAGE: source this file in the orchestration stage 2 (Acquire) when running
# locally. In production, replace these calls with the equivalent MCP tool calls.
# Skills themselves never change — they receive the same data.frame either way.
#
# Files expected in data/ (gitignored):
#   data/conapesca_landings_2001_2026_2026-07-15.rds
#   data/ltem_historic_updated_2026-07-16.RDS
#
# To update the filename when a new database version arrives, change the
# constants below only.

# ── File paths (update when new DB versions arrive) ───────────────────────────

.CONAPESCA_FILE <- "data/conapesca_landings_2001_2026_2026-07-15.rds"
.LTEM_FILE      <- "data/ltem_historic_updated_2026-07-16.RDS"

# ── Cache (loaded once per session) ───────────────────────────────────────────

.cache <- new.env(parent = emptyenv())

.load_raw <- function(key, path) {
  if (!exists(key, envir = .cache)) {
    if (!file.exists(path))
      stop("Local database not found: '", path, "'.\n",
           "Place the RDS file in data/ (gitignored).")
    message("  Loading ", basename(path), " ...")
    .cache[[key]] <- readRDS(path)
    message("  Done: ", nrow(.cache[[key]]), " rows.")
  }
  .cache[[key]]
}

# =============================================================================
# CONAPESCA
# =============================================================================

# ── get_conapesca_landings ─────────────────────────────────────────────────────
#
# Returns landing records in the shape the conapesca-lfo-ca skill expects.
# Column mapping:  nombre_oficina → nombre_oficina_canonico (upper-trimmed)
#
# Args:
#   year_min, year_max  integer — filter to anio_corte in [year_min, year_max]
#   fleet               character vector — subset of c("MAYORES","MENORES","COSECHA")
#                       Default excludes COSECHA (aquaculture).
#   columns             character vector — columns to return. NULL = all skill cols.
#
# Returns data.frame with columns:
#   anio_corte, tipo_aviso, nombre_oficina_canonico,
#   nombre_especie, nombre_cientifico, peso_desembarcado_kg,
#   dias_efectivos, flag_fecha_generica, flag_dias_efectivos_sospechoso,
#   latitud_oficina, longitud_oficina
#   (plus any extras requested via `columns`)

get_conapesca_landings <- function(year_min  = 2001L,
                                   year_max  = 2026L,
                                   fleet     = c("MAYORES", "MENORES"),
                                   columns   = NULL) {

  raw <- .load_raw("conapesca", .CONAPESCA_FILE)

  # Standardise office name → nombre_oficina_canonico
  raw$nombre_oficina_canonico <- toupper(trimws(raw$nombre_oficina))

  # Filter
  out <- raw[
    raw$anio_corte >= year_min &
    raw$anio_corte <= year_max &
    !is.na(raw$tipo_aviso) &
    raw$tipo_aviso %in% fleet,
  ]

  # Default columns returned (skill contract + diagnostics)
  skill_cols <- c(
    "anio_corte", "tipo_aviso", "nombre_oficina_canonico",
    "nombre_especie", "nombre_cientifico",
    "peso_desembarcado_kg", "dias_efectivos",
    "flag_fecha_generica", "flag_dias_efectivos_sospechoso",
    "latitud_oficina", "longitud_oficina"
  )

  keep <- if (is.null(columns)) skill_cols else union(skill_cols, columns)
  keep <- intersect(keep, names(out))

  out[, keep, drop = FALSE]
}

# ── get_lfo_coords ─────────────────────────────────────────────────────────────
#
# Returns one row per landing office with its lat/lon.
# Derived from the raw database — no separate file needed.
# Used as the optional `lfo_coords` argument in conapesca-lfo-ca.

get_lfo_coords <- function() {
  raw <- .load_raw("conapesca", .CONAPESCA_FILE)
  raw$nombre_oficina_canonico <- toupper(trimws(raw$nombre_oficina))

  coords <- raw[!is.na(raw$latitud_oficina) & !is.na(raw$longitud_oficina),
                c("nombre_oficina_canonico", "latitud_oficina", "longitud_oficina")]
  coords <- coords[!duplicated(coords$nombre_oficina_canonico), ]
  names(coords)[2:3] <- c("lat", "lon")
  coords[order(coords$nombre_oficina_canonico), ]
}

# =============================================================================
# LTEM
# =============================================================================
# Column mapping (raw → skill contract):
#   Year   → time
#   Reef   → reef
#   Transect → transect
#   Region → region
#   Biomass → value  (for fish biomass and NRSI skills)
#   Quantity → value (for invertebrate skill)
#   TrophicLevelF → TrophicLevelF (unchanged)

# ── get_ltem_fish ─────────────────────────────────────────────────────────────
#
# Returns fish (PEC) records for the NRSI and fish-biomass skills.
# Pre-processing applied here (matches ltem_report filtering):
#   - label == "PEC", Family != "Carangidae"
#   - Corredor: Haemulidae/Carangidae with Biomass > 3 excluded
#
# Args:
#   region   character — LTEM region name (NULL = all regions)
#   year_min, year_max  integer
#
# Returns data.frame with columns:
#   time, reef, transect, region, value (Biomass T/ha),
#   TrophicLevelF, Functional_groups, Family, lat (Latitude), lon (Longitude)

get_ltem_fish <- function(region   = NULL,
                          year_min = 1998L,
                          year_max = 2025L) {

  raw <- .load_raw("ltem", .LTEM_FILE)

  out <- raw[
    !is.na(raw$Label) & raw$Label == "PEC" &
    !is.na(raw$Family) & raw$Family != "Carangidae" &
    raw$Year >= year_min & raw$Year <= year_max,
  ]

  # Corredor: exclude schooling species outliers (matches ltem_report filter)
  corredor_flag <- out$Region == "Corredor" &
    out$Family %in% c("Haemulidae", "Carangidae") &
    !is.na(out$Biomass) & out$Biomass > 3
  out <- out[!corredor_flag, ]

  if (!is.null(region))
    out <- out[out$Region == region, ]

  data.frame(
    time             = out$Year,
    reef             = out$Reef,
    transect         = out$Transect,
    region           = out$Region,
    value            = out$Biomass,
    TrophicLevelF    = out$TrophicLevelF,
    functional_group = out$Functional_groups,
    family           = out$Family,
    lat              = out$Latitude,
    lon              = out$Longitude,
    stringsAsFactors = FALSE
  )
}

# ── get_ltem_invertebrates ────────────────────────────────────────────────────
#
# Returns invertebrate (INV) records for the ltem-invertebrate-abundance skill.
# Classifies focal taxa and aggregates to reef-year level.
#
# Args:
#   region   character — LTEM region name (NULL = all regions)
#   year_min, year_max  integer
#
# Returns data.frame (reef-year-taxa level) with columns:
#   time, reef, taxa, value (mean abundance per transect),
#   richness (mean species richness per transect), region

get_ltem_invertebrates <- function(region   = NULL,
                                   year_min = 1998L,
                                   year_max = 2025L) {

  raw <- .load_raw("ltem", .LTEM_FILE)

  inv <- raw[!is.na(raw$Label) & raw$Label == "INV" &
             raw$Year >= year_min & raw$Year <= year_max, ]

  if (!is.null(region))
    inv <- inv[inv$Region == region, ]

  # Classify focal taxa (mutually exclusive, ordered)
  inv$taxa <- ifelse(
    !is.na(inv$Taxa3) & inv$Taxa3 == "Holaxonia", "Holaxonia",
    ifelse(!is.na(inv$Taxa3) & inv$Taxa3 == "Scleractinia", "Scleractinia",
    ifelse(!is.na(inv$Taxa2) & inv$Taxa2 == "Asteroidea", "Asteroidea",
    ifelse(!is.na(inv$Taxa2) & inv$Taxa2 == "Echinoidea", "Echinoidea",
           NA_character_))))

  inv <- inv[!is.na(inv$taxa) & !is.na(inv$Reef) & !is.na(inv$Year) &
             !is.na(inv$Transect), ]

  if (nrow(inv) == 0) return(data.frame())

  # Transect level: sum quantity, count species
  keys_t <- c("Year", "Region", "Reef", "Transect", "taxa")
  trans <- do.call(rbind, lapply(
    split(inv, inv[, keys_t]),
    function(g) data.frame(
      Year      = g$Year[1],
      Region    = g$Region[1],
      Reef      = g$Reef[1],
      Transect  = g$Transect[1],
      taxa      = g$taxa[1],
      quantity  = sum(g$Quantity, na.rm = TRUE),
      richness  = length(unique(g$Species[!is.na(g$Species)])),
      stringsAsFactors = FALSE
    )
  ))
  rownames(trans) <- NULL

  # Reef-year level: mean across transects
  keys_r <- c("Year", "Region", "Reef", "taxa")
  reef_yr <- do.call(rbind, lapply(
    split(trans, trans[, keys_r]),
    function(g) data.frame(
      time     = g$Year[1],
      region   = g$Region[1],
      reef     = g$Reef[1],
      taxa     = g$taxa[1],
      value    = mean(g$quantity, na.rm = TRUE),
      richness = mean(g$richness, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  ))
  rownames(reef_yr) <- NULL
  reef_yr
}

# ── get_ltem_nrsi_input ───────────────────────────────────────────────────────
#
# Returns fish data in the exact shape ltem-nrsi-index/skill.R expects:
#   time, value (Biomass T/ha), TrophicLevelF, transect, reef
# (thin wrapper around get_ltem_fish with column selection only)

get_ltem_nrsi_input <- function(region = NULL, year_min = 1998L, year_max = 2025L) {
  fish <- get_ltem_fish(region = region, year_min = year_min, year_max = year_max)
  fish[, c("time", "value", "TrophicLevelF", "transect", "reef", "region")]
}
