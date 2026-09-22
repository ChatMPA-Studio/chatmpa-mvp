# generate_bbox_lookups.R
#
# One-time (re-run when the AMP or LME set changes) generator for two lookup
# tables that let ERDDAP-consuming skills call get_data(bbox=..., ...,
# aggregate_spatial=TRUE) without resolving any geometry at request time:
#
#   amp_bbox_lookup.csv — bbox per AMP (local scale), from amp_geometry_lookup.csv
#   lme_bbox_lookup.csv — bbox per LME (regional scale), from Marine Regions API
#
# Both are derived from the SAME sf polygons that get_amp_geometry() /
# get_lme_geometry() already resolve (shared/spatial_join/spatial_join.R) —
# this script does not introduce a new geometry source, it just precomputes
# the bounding box of each one, once, so it can be read as a flat lookup
# instead of recomputed per request.
#
# Usage: Rscript generate_bbox_lookups.R
# Requires: sf, dplyr, readr, wdpar (AMPs), mregions2 (LMEs)

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(readr)
})

.args <- commandArgs(trailingOnly = FALSE)
.script_path <- sub("^--file=", "", .args[grep("^--file=", .args)])
.HERE <- dirname(normalizePath(.script_path))
source(file.path(.HERE, "..", "spatial_join", "spatial_join.R"))

# ── AMP bboxes (local scale) ─────────────────────────────────────────────────

amp_lookup <- read_csv(file.path(.HERE, "amp_geometry_lookup.csv"), show_col_types = FALSE)

cat("Computing bbox for", nrow(amp_lookup), "AMPs...\n")

amp_bboxes <- lapply(amp_lookup$conanp_nombre, function(amp_name) {
  cat(" -", amp_name, "\n")
  result <- tryCatch({
    geom <- get_amp_geometry(amp_name)
    bb   <- st_bbox(geom)
    data.frame(
      amp_name         = amp_name,
      lon_min          = unname(bb["xmin"]),
      lon_max          = unname(bb["xmax"]),
      lat_min          = unname(bb["ymin"]),
      lat_max          = unname(bb["ymax"]),
      geometry_source  = attr(geom, "geometry_source"),
      stringsAsFactors = FALSE
    )
  }, error = function(e) {
    cat("   FAILED:", conditionMessage(e), "\n")
    data.frame(
      amp_name = amp_name, lon_min = NA, lon_max = NA, lat_min = NA, lat_max = NA,
      geometry_source = NA, stringsAsFactors = FALSE
    )
  })
  result
})

amp_bbox_df <- do.call(rbind, amp_bboxes)
n_failed <- sum(is.na(amp_bbox_df$lon_min))
cat("\nAMP bboxes: ", nrow(amp_bbox_df) - n_failed, " OK, ", n_failed, " failed.\n", sep = "")

write_csv(amp_bbox_df, file.path(.HERE, "amp_bbox_lookup.csv"))
cat("Wrote", file.path(.HERE, "amp_bbox_lookup.csv"), "\n\n")

# ── LME bboxes (regional scale) ──────────────────────────────────────────────
# Fixed, small set — the LMEs Mexican AMPs actually fall in. Add more names
# here if a future AMP needs a regional comparison outside these.

lme_names <- c("Gulf of California", "Gulf of Mexico", "California Current",
              "Pacific Central-American Coastal")  # note the hyphen — matches the
                                                     # Marine Regions gazetteer name;
                                                     # "Central American" (no hyphen)
                                                     # fails to match internally

cat("Computing bbox for", length(lme_names), "LMEs...\n")

lme_bboxes <- lapply(lme_names, function(lme_name) {
  cat(" -", lme_name, "\n")
  result <- tryCatch({
    geom <- get_lme_geometry(lme_name)
    bb   <- st_bbox(geom)
    data.frame(
      lme_name         = lme_name,
      lon_min          = unname(bb["xmin"]),
      lon_max          = unname(bb["xmax"]),
      lat_min          = unname(bb["ymin"]),
      lat_max          = unname(bb["ymax"]),
      stringsAsFactors = FALSE
    )
  }, error = function(e) {
    cat("   FAILED:", conditionMessage(e), "\n")
    data.frame(
      lme_name = lme_name, lon_min = NA, lon_max = NA, lat_min = NA, lat_max = NA,
      stringsAsFactors = FALSE
    )
  })
  result
})

lme_bbox_df <- do.call(rbind, lme_bboxes)
n_failed_lme <- sum(is.na(lme_bbox_df$lon_min))
cat("\nLME bboxes: ", nrow(lme_bbox_df) - n_failed_lme, " OK, ", n_failed_lme, " failed.\n", sep = "")

write_csv(lme_bbox_df, file.path(.HERE, "lme_bbox_lookup.csv"))
cat("Wrote", file.path(.HERE, "lme_bbox_lookup.csv"), "\n")
