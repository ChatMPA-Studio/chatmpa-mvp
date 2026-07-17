# spatial_join.R — shared spatial helpers
#
# Exposes:
#   get_amp_geometry(amp_name)  → sf object + geometry_source attribute
#   get_lme_geometry(lme_name)  → sf object from Marine Regions API (cached)
#   clip_to_geometry(data, geometry) → data filtered to pixels within geometry
#
# Skills receive sf objects — they never read named files directly.

library(sf)
library(dplyr)
library(readr)

.GEOMETRIES_DIR <- file.path(dirname(sys.frame(1)$ofile), "..", "geometries")

# ── get_amp_geometry ─────────────────────────────────────────────────────────
#
# Returns the best available polygon for an AMP as an sf object.
# Source priority: WDPA (via wdpar) if Jaccard >= threshold, else CONANP override.
# Adds a `geometry_source` attribute so the skill can report provenance.
#
# Args:
#   amp_name : character — CONANP name (NOMBRE column), case-insensitive partial match
#
# Returns: sf object (single polygon, CRS = WGS84) with attribute `geometry_source`

get_amp_geometry <- function(amp_name) {
  lookup_path <- file.path(.GEOMETRIES_DIR, "amp_geometry_lookup.csv")
  if (!file.exists(lookup_path)) {
    stop("amp_geometry_lookup.csv not found. Run shared/geometries/setup_amp_geometries.R first.")
  }

  lookup <- read_csv(lookup_path, show_col_types = FALSE)

  row <- lookup |>
    filter(grepl(amp_name, conanp_nombre, ignore.case = TRUE)) |>
    slice(1)

  if (nrow(row) == 0) {
    stop("AMP not found in lookup: '", amp_name, "'. ",
         "Check conanp_nombre column in amp_geometry_lookup.csv.")
  }

  source <- row$geometry_source

  if (source == "WDPA") {
    if (!requireNamespace("wdpar", quietly = TRUE)) {
      stop("Package 'wdpar' required for WDPA geometries. Install with: install.packages('wdpar')")
    }
    wdpa_name <- row$wdpa_nombre
    cat("Fetching WDPA geometry for:", wdpa_name, "\n")
    wdpa_mex <- wdpar::wdpa_fetch("MEX", wait = TRUE, download_dir = tempdir()) |>
      filter(grepl(wdpa_name, NAME, ignore.case = TRUE) |
             grepl(wdpa_name, NAME_ENG, ignore.case = TRUE)) |>
      st_transform(4326) |>
      slice(1)

    if (nrow(wdpa_mex) == 0) stop("WDPA match not found for: ", wdpa_name)
    geom <- wdpa_mex |> select(NAME, REP_M_AREA, geometry)

  } else {
    # CONANP_pending_review
    override_path <- file.path(.GEOMETRIES_DIR, "conanp_overrides.gpkg")
    if (!file.exists(override_path)) {
      stop("conanp_overrides.gpkg not found. Run shared/geometries/setup_amp_geometries.R first.")
    }
    geom <- st_read(override_path, quiet = TRUE) |>
      filter(grepl(amp_name, NOMBRE, ignore.case = TRUE)) |>
      slice(1)

    if (nrow(geom) == 0) stop("Override not found for: ", amp_name)
    cat("Using CONANP override (pending review) for:", geom$NOMBRE, "\n")
  }

  attr(geom, "geometry_source") <- source
  attr(geom, "amp_name")        <- row$conanp_nombre
  geom
}

# ── get_lme_geometry ─────────────────────────────────────────────────────────
#
# Returns an LME polygon from Marine Regions (mregions2), cached locally.
#
# Args:
#   lme_name : character — partial name match (e.g. "California Current",
#              "Gulf of California", "Gulf of Mexico")
#
# Returns: sf object (CRS = WGS84)

get_lme_geometry <- function(lme_name) {
  if (!requireNamespace("mregions2", quietly = TRUE)) {
    stop("Package 'mregions2' required. Install with: install.packages('mregions2')")
  }

  cache_path <- file.path(.GEOMETRIES_DIR, "lme",
                          paste0(gsub("[^a-zA-Z0-9]", "_", lme_name), ".gpkg"))

  if (file.exists(cache_path)) {
    cat("Loading LME from cache:", basename(cache_path), "\n")
    geom <- st_read(cache_path, quiet = TRUE)
  } else {
    cat("Fetching LME from Marine Regions API:", lme_name, "\n")
    results <- mregions2::gaz_search(lme_name, typeid = 142)  # 142 = LME type

    if (nrow(results) == 0) stop("LME not found: '", lme_name, "'")

    geom <- results |>
      filter(grepl(lme_name, preferredGazetteerName, ignore.case = TRUE)) |>
      slice(1) |>
      mregions2::gaz_geometry() |>
      st_transform(4326)

    dir.create(dirname(cache_path), showWarnings = FALSE, recursive = TRUE)
    st_write(geom, cache_path, delete_dsn = TRUE, quiet = TRUE)
    cat("Cached to:", cache_path, "\n")
  }

  attr(geom, "lme_name") <- lme_name
  geom
}

# ── clip_to_geometry ─────────────────────────────────────────────────────────
#
# Clips a data.frame with lat/lon columns to pixels within an sf polygon.
# Returns the filtered data.frame with a `cobertura_pct` attribute
# (% of input points retained — useful to flag sparse coverage).
#
# Args:
#   data     : data.frame with columns `lat` and `lon`
#   geometry : sf object (single polygon)

clip_to_geometry <- function(data, geometry) {
  pts <- st_as_sf(data, coords = c("lon", "lat"), crs = 4326, remove = FALSE)
  geometry_union <- st_union(st_transform(geometry, 4326))
  inside  <- st_within(pts, geometry_union, sparse = FALSE)[, 1]
  result  <- data[inside, ]
  attr(result, "cobertura_pct") <- round(mean(inside) * 100, 1)
  result
}
