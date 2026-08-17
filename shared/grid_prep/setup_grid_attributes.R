# setup_grid_attributes.R
#
# One-time setup script: stamps the coastal prosperity grid with the MPA that
# covers the largest share of each cell's area.
#
# Two-track architecture note:
#   - The coastal grid is used for socioeconomic/administrative data
#     (INEGI, municipal variables, CONAPESCA via georeferenced ports).
#     It covers the coastal zone only.
#   - Environmental satellite data (SST, Chl, NPP) is always extracted
#     directly from the MPA polygon — this works for ALL MPAs, including
#     offshore ones (Revillagigedo, Guadalupe) that have NO grid cells.
#   - Grid cells outside any MPA get mpa_id = NA, nombre_amp = NA.
#
# Inputs (git-ignored, in data/):
#   data/prosperity_grid_scored_v2_mun_corrected.gpkg  — coastal grid, 3 236 cells
#   data/mpas/MPA_v2_2024-03-20.shp                   — 466 MPA polygons (35 MPAs)
#
# Output (git-ignored, generated artifact):
#   data/prosperity_grid_attributed.gpkg
#     Same as input grid plus two new columns:
#       mpa_id     — ANP_ID (integer) of the MPA covering the most cell area; NA if none
#       nombre_amp — ANP name string; NA if none
#
# Run from repo root:  Rscript shared/grid_prep/setup_grid_attributes.R

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
})

cat("== setup_grid_attributes.R ==\n")

# ── Paths ─────────────────────────────────────────────────────────────────────

GRID_IN  <- "data/prosperity_grid_scored_v2_mun_corrected.gpkg"
MPA_IN   <- "data/mpas/MPA_v2_2024-03-20.shp"
GRID_OUT <- "data/prosperity_grid_attributed.gpkg"

# ── Load ──────────────────────────────────────────────────────────────────────

cat("Loading grid...\n")
grid <- st_read(GRID_IN, quiet = TRUE)
cat("  ", nrow(grid), "cells, CRS:", st_crs(grid)$input, "\n")

cat("Loading MPA polygons...\n")
mpas <- st_read(MPA_IN, quiet = TRUE)
cat("  ", nrow(mpas), "polygons |", length(unique(mpas$ANP_ID)), "unique MPAs\n")

# Repair degenerate/invalid geometries before intersection
n_invalid <- sum(!st_is_valid(mpas))
if (n_invalid > 0) {
  cat("  Repairing", n_invalid, "invalid geometries with st_make_valid()...\n")
  mpas <- st_make_valid(mpas)
}

# Keep only the columns we need from the MPA file
mpas_slim <- mpas %>%
  select(ANP_ID, ANP) %>%
  rename(mpa_id = ANP_ID, nombre_amp = ANP)

# ── Align CRS ────────────────────────────────────────────────────────────────

if (!identical(st_crs(grid), st_crs(mpas_slim))) {
  cat("Reprojecting MPAs to match grid CRS...\n")
  mpas_slim <- st_transform(mpas_slim, st_crs(grid))
}

# ── Spatial join: largest-overlap assignment ──────────────────────────────────
#
# For each grid cell, find every MPA polygon it intersects, compute the
# intersection area, and assign the MPA that covers the most of the cell.
# Cells that intersect no MPA get mpa_id = NA, nombre_amp = NA.

cat("Computing intersections (this may take ~30 s)...\n")

# st_intersection returns one row per (cell × MPA) pair
# We then pick the largest intersection per cell.
grid_slim <- grid %>% select(id)
grid_slim$.cell_area <- as.numeric(st_area(grid_slim))
inter <- st_intersection(grid_slim, mpas_slim)

if (nrow(inter) == 0) {
  cat("WARNING: No intersections found. Check that grid and MPAs overlap spatially.\n")
  grid$mpa_id     <- NA_integer_
  grid$nombre_amp <- NA_character_
} else {
  inter$.inter_area <- as.numeric(st_area(inter))

  # For each cell id, keep the MPA with the largest intersection area
  best_mpa <- inter %>%
    st_drop_geometry() %>%
    group_by(id) %>%
    slice_max(.inter_area, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(id, mpa_id, nombre_amp)

  cat("  ", nrow(best_mpa), "cells intersect at least one MPA\n")
  cat("  ", length(unique(best_mpa$nombre_amp)), "unique MPAs represented in grid\n")
  cat("  ", nrow(grid) - nrow(best_mpa), "cells outside all MPAs (mpa_id = NA)\n")

  grid <- grid %>%
    left_join(best_mpa, by = "id")
}

# ── Quick sanity check ────────────────────────────────────────────────────────

cat("\nMPA coverage summary (top 10 by cell count):\n")
grid %>%
  st_drop_geometry() %>%
  filter(!is.na(nombre_amp)) %>%
  count(nombre_amp, sort = TRUE) %>%
  slice_head(n = 10) %>%
  print()

# ── Write output ──────────────────────────────────────────────────────────────

cat("\nWriting attributed grid to", GRID_OUT, "...\n")
st_write(grid, GRID_OUT, delete_dsn = TRUE, quiet = TRUE)
cat("Done. Columns added: mpa_id, nombre_amp\n")
cat("Total columns now:", ncol(grid) - 1, "(excluding geometry)\n")
