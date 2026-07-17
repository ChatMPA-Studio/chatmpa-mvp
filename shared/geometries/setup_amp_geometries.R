# setup_amp_geometries.R
# Genera amp_geometry_lookup.csv y conanp_overrides.gpkg
# Correr una vez (o cuando cambie el shapefile de CONANP o el umbral de Jaccard)
#
# Requiere:
#   - comparar_amp_wdpa_conanp.csv  (generado por comparar_amp_wdpa_conanp.R)
#   - 232-ANP_ITRF08_19162026.zip   (shapefile CONANP)

library(sf)
library(dplyr)
library(readr)

JACCARD_THRESHOLD <- 0.95

metricas_path <- r"(C:\Users\carol\OneDrive\Documentos\CBMC\Projects\comparar_amp_wdpa_conanp.csv)"
conanp_zip    <- r"(C:\Users\carol\Downloads\232-ANP_ITRF08_19162026.zip)"
out_dir       <- r"(C:\Users\carol\OneDrive\Documentos\CBMC\Projects\chatmpa-mvp\shared\geometries)"

metricas <- read_csv(metricas_path, show_col_types = FALSE)

# ── Lookup: fuente por AMP ───────────────────────────────────────────────────

lookup <- metricas |>
  mutate(
    geometry_source = case_when(
      is.na(jaccard)          ~ "CONANP_pending_review",  # sin match en WDPA
      jaccard < JACCARD_THRESHOLD ~ "CONANP_pending_review",
      TRUE                    ~ "WDPA"
    ),
    jaccard_threshold = JACCARD_THRESHOLD
  ) |>
  select(conanp_nombre, wdpa_nombre, geometry_source,
         jaccard, jaccard_threshold, area_conanp_km2, area_wdpa_km2)

cat("Fuentes asignadas:\n")
print(table(lookup$geometry_source))

write_csv(lookup, file.path(out_dir, "amp_geometry_lookup.csv"))
cat("Guardado: amp_geometry_lookup.csv\n")

# ── Overrides: geometrías CONANP para AMPs con Jaccard < umbral ──────────────

nombres_override <- lookup |>
  filter(geometry_source == "CONANP_pending_review") |>
  pull(conanp_nombre)

cat("\nAMPs en override (", length(nombres_override), "):\n")
print(nombres_override)

conanp_full <- st_read(
  paste0("/vsizip/", conanp_zip, "/232-ANP_ITRF08_19162026.shp"),
  quiet = TRUE
) |>
  st_transform(4326) |>
  mutate(S_MARINA = as.numeric(S_MARINA)) |>
  filter(S_MARINA > 0)

overrides <- conanp_full |>
  filter(NOMBRE %in% nombres_override) |>
  select(NOMBRE, CAT_MANEJO, ESTADOS, SUPERFICIE, S_MARINA)

st_write(overrides,
         file.path(out_dir, "conanp_overrides.gpkg"),
         layer = "amp_overrides",
         delete_dsn = TRUE,
         quiet = TRUE)

cat("Guardado: conanp_overrides.gpkg (", nrow(overrides), "polígonos )\n")
