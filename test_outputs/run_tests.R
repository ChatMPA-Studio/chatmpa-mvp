# test_outputs/run_tests.R
#
# Run from repo root:
#   Rscript test_outputs/run_tests.R
#
# Generates test plots and KPI JSON for a single MPA (default: Cabo Pulmo).
# Outputs saved to test_outputs/.
#
# Required packages:
#   dplyr, ggplot2, fst, heatwaveR, jsonlite, tidyr, scales, mgcv, Kendall

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(fst)
  library(heatwaveR)
  library(jsonlite)
  library(scales)
  library(mgcv)
  library(Kendall)
})

# ── Config ─────────────────────────────────────────────────────────────────────

TEST_MPA    <- "Cabo Pulmo"
LTEM_REGION <- "Cabo Pulmo"

# Path to species → functional_name traits table (from ltem_report project)
TRAITS_CSV <- "../ltem_report/data/cluster_to_create_traits.csv"

MPA_LON_MIN <- -109.87; MPA_LON_MAX <- -109.35
MPA_LAT_MIN <-   22.95; MPA_LAT_MAX <-  23.48
PAD         <- 0.25

MHW_BASELINE_START <- "1998-01-01"
MHW_BASELINE_END   <- "2011-12-31"
CHL_BASELINE_YEARS <- 2004:2013
REPORT_YEAR        <- 2025L

OUT_DIR <- "test_outputs"

# Functional group palette (matches display_spec.json)
FG_COLORS <- c(
  GenPred_solitary  = "#D73027",
  GenPred_schooling = "#FC8D59",
  EpiBent_schooling = "#FEE08B",
  Crip_schooling    = "#91BFDB",
  Crip_solitary     = "#4575B4",
  Plank             = "#313695"
)
FG_LABELS <- c(
  GenPred_solitary  = "Depredadores solitarios",
  GenPred_schooling = "Depredadores en cardúmenes",
  EpiBent_schooling = "Omnívoros en cardúmen",
  Crip_schooling    = "Herbívoros en cardúmen",
  Crip_solitary     = "Crípticos solitarios",
  Plank             = "Planctívoros"
)
VALID_FG <- names(FG_COLORS)

cat("\n── chatMPA skill test runner ──────────────────────────────────────\n")
cat("MPA:", TEST_MPA, "\n\n")

save_plot <- function(p, name, width = 8, height = 4.5) {
  path <- file.path(OUT_DIR, paste0(name, ".png"))
  ggsave(path, p, width = width, height = height, dpi = 150, bg = "white")
  cat("  ✓ saved:", path, "\n")
}

# ── Load skill files ───────────────────────────────────────────────────────────

source("shared/local_loader/local_loader.R")
source("shared/interpretation/interpret.R")

# ── 1. Fish Biomass — functional group stacked area ───────────────────────────

cat("=== 1. Fish Biomass (functional groups) ===\n")

source("per-database/ltem-fish-biomass/skill.R")

fish_raw <- get_ltem_fish(region = LTEM_REGION)
cat("  Fish records (pre-traits join):", nrow(fish_raw), "\n")

# Join species-level traits to get functional_name (GenPred_solitary etc.)
# Functional_groups in the RDS uses a different classification (piscivores/planktivores etc.)
# The Functional_groups column in the RDS uses a different classification.
traits <- read.csv(TRAITS_CSV, stringsAsFactors = FALSE) %>%
  select(Species, functional_name)

# Species column is not in get_ltem_fish() output — join from raw RDS
raw_ltem <- readRDS("data/ltem_historic_updated_2026-07-16.RDS")
fish_species <- raw_ltem %>%
  filter(Label == "PEC", Family != "Carangidae",
         Region == LTEM_REGION, !is.na(Biomass)) %>%
  filter(!(Region == "Corredor" & Family %in% c("Haemulidae","Carangidae") & Biomass > 3)) %>%
  left_join(traits, by = "Species") %>%
  mutate(
    time   = Year,
    reef   = Reef,
    transect = Transect,
    value  = Biomass,
    functional_group = functional_name
  ) %>%
  filter(functional_group %in% VALID_FG)

cat("  After traits join — records with valid group:", nrow(fish_species), "\n")
cat("  Functional groups:", paste(sort(unique(fish_species$functional_group)), collapse = ", "), "\n")

# ── Aggregate for main data (all groups combined) — reef-year means
reef_year <- fish_species %>%
  group_by(time, reef, transect) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  group_by(time, reef) %>%
  summarise(value = mean(value, na.rm = TRUE), region = LTEM_REGION, .groups = "drop")

# ── Aggregate for data_func — year × functional_group means
data_func <- fish_species %>%
  group_by(time, reef, transect, functional_group) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  group_by(time, reef, functional_group) %>%
  summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
  group_by(time, functional_group) %>%
  summarise(value = mean(value, na.rm = TRUE), .groups = "drop")

cat("  Years:", min(data_func$time), "–", max(data_func$time), "\n")

# Run skill
biomass_result <- run_skill(reef_year, data_func = data_func)
kpi_biomass    <- biomass_result$value$kpi

cat("  KPI mean biomass:", kpi_biomass$mean_biomass_g_m2, "g/m² ≈",
    round(kpi_biomass$mean_biomass_g_m2 / 100, 4), "T/ha\n")

interp_biomass <- biomass_result$interpretation
cat("  Interpretation:", interp_biomass$direction, "|", interp_biomass$status, "\n")
cat("  EN:", interp_biomass$insight_en, "\n")
cat("  ES:", interp_biomass$insight_es, "\n")

# ── Stacked area plot
fg_plot <- data_func %>%
  mutate(functional_group = factor(functional_group, levels = VALID_FG))

p_biomass <- ggplot(fg_plot, aes(x = time, y = value / 100,
                                  fill = functional_group)) +
  geom_area(position = "stack", alpha = 0.85) +
  scale_fill_manual(
    values = FG_COLORS,
    labels = FG_LABELS,
    guide  = guide_legend(ncol = 2)
  ) +
  scale_x_continuous(breaks = seq(1998, 2026, 3),
                     expand = expansion(mult = c(0.01, 0.01))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)),
                     labels = scales::number_format(accuracy = 0.001)) +
  labs(
    title    = paste("Biomasa por Grupo Funcional ·", TEST_MPA),
    subtitle = "Área apilada — media anual regional (transecto → SUM → arrecife → MEAN → región → MEAN)",
    x = NULL, y = "Biomasa (T/ha)",
    fill = NULL,
    caption = "Fuente: LTEM · Grupos funcionales excluyen Pelágicos · Sin Carangidae"
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title    = element_text(face = "bold", hjust = 0.5, size = 12),
    plot.subtitle = element_text(hjust = 0.5, size = 9, color = "grey40"),
    plot.caption  = element_text(size = 8, color = "grey50"),
    legend.position  = "bottom",
    legend.text      = element_text(size = 8),
    panel.grid.minor = element_blank()
  )

save_plot(p_biomass, "biomass_functional_groups_cabo_pulmo", width = 9, height = 5)

# ── 2. NRSI — scalar KPI only ─────────────────────────────────────────────────

cat("\n=== 2. NRSI (scalar KPI) ===\n")

source("per-database/ltem-nrsi-index/skill.R", local = TRUE)

nrsi_data <- get_ltem_nrsi_input(region = LTEM_REGION)

# Consistent site filter: reefs monitored in ≥5 unique years
reef_years_count <- nrsi_data %>%
  group_by(reef) %>%
  summarise(n_years = n_distinct(time), .groups = "drop") %>%
  filter(n_years >= 5)
nrsi_data_consistent <- nrsi_data %>%
  filter(reef %in% reef_years_count$reef)
cat("  Reefs before filter:", n_distinct(nrsi_data$reef),
    "→ after ≥5 yr filter:", n_distinct(nrsi_data_consistent$reef), "\n")

nrsi_result <- run_skill(nrsi_data_consistent)
nrsi_df     <- nrsi_result$value$nrsi_by_reef

# Regional mean per year → 5-year recent mean as scalar KPI
nrsi_annual <- nrsi_df %>%
  group_by(time) %>%
  summarise(
    nrsi_mean  = mean(nrsi, na.rm = TRUE),
    health_cat = names(which.max(table(health_category))),
    .groups = "drop"
  )

kpi_nrsi_years  <- tail(sort(nrsi_annual$time), 5)
kpi_nrsi_value  <- mean(nrsi_annual$nrsi_mean[nrsi_annual$time %in% kpi_nrsi_years],
                        na.rm = TRUE)
latest_nrsi     <- tail(nrsi_annual$nrsi_mean, 1)
latest_nrsi_yr  <- max(nrsi_annual$time)
nrsi_health     <- ifelse(latest_nrsi > 0, "Saludable", "Degradado")

cat("  Latest year:", latest_nrsi_yr, "· NRSI:", round(latest_nrsi, 3),
    "·", nrsi_health, "\n")
cat("  5-yr KPI mean:", round(kpi_nrsi_value, 3), "\n")

interp_nrsi <- nrsi_result$interpretation
cat("  Interpretation:", interp_nrsi$direction, "|", interp_nrsi$status, "\n")
cat("  EN:", interp_nrsi$insight_en, "\n")
cat("  ES:", interp_nrsi$insight_es, "\n")

# ── 3. Marine Heatwave wave plot ───────────────────────────────────────────────

cat("\n=== 3. Marine Heatwaves ===\n")

oisst_raw <- read.fst("data/oisst_goc_1982_2025.fst", as.data.table = FALSE)

mpa_sst_daily <- oisst_raw %>%
  filter(
    lon >= MPA_LON_MIN - PAD, lon <= MPA_LON_MAX + PAD,
    lat >= MPA_LAT_MIN - PAD, lat <= MPA_LAT_MAX + PAD
  ) %>%
  group_by(t) %>%
  summarise(temp = mean(temp, na.rm = TRUE), .groups = "drop") %>%
  arrange(t)

cat("  Daily SST records:", nrow(mpa_sst_daily), "\n")

clim    <- ts2clm(mpa_sst_daily,
                  climatologyPeriod = c(MHW_BASELINE_START, MHW_BASELINE_END))
events  <- detect_event(clim, minDuration = 5, maxGap = 2)
clim_df <- events$climatology

annual_mhw <- clim_df %>%
  mutate(year = as.integer(format(t, "%Y"))) %>%
  group_by(year) %>%
  summarise(
    heatwave_days = sum(event == TRUE, na.rm = TRUE),
    mean_temp     = round(mean(temp, na.rm = TRUE), 2),
    n_events      = sum(diff(c(0L, as.integer(event == TRUE))) == 1L, na.rm = TRUE),
    .groups = "drop"
  )

cat("  MHW annual summary:", min(annual_mhw$year), "–", max(annual_mhw$year), "\n")

# Wave plot — most recent year
yr_use <- if (REPORT_YEAR %in% as.integer(format(clim_df$t, "%Y"))) {
  REPORT_YEAR
} else {
  max(annual_mhw$year)
}
year_data <- clim_df %>% filter(format(t, "%Y") == as.character(yr_use)) %>% arrange(t)
cat("  Wave plot year:", yr_use, "·", nrow(year_data), "days\n")

p_mhw <- ggplot(year_data, aes(x = t)) +
  geom_flame(aes(y = temp, y2 = thresh), fill = "salmon", alpha = 0.65) +
  geom_line(aes(y = temp),   color = "black",     linewidth = 0.55) +
  geom_line(aes(y = thresh), color = "darkred",   linewidth = 0.5, linetype = "dashed") +
  geom_line(aes(y = seas),   color = "steelblue", linewidth = 0.7) +
  scale_x_date(
    date_breaks = "2 months", date_labels = "%b.",
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  labs(
    title    = paste0("Olas de Calor Marinas (", yr_use, ") · ", TEST_MPA),
    subtitle = paste0(
      "Negro: SST observada  |  Azul: climatología  |  ",
      "Rojo discontinuo: umbral 90°p  |  Relleno: eventos de ola de calor"
    ),
    x = NULL, y = "Temperatura (°C)",
    caption = paste0(
      "Línea de base: ", MHW_BASELINE_START, " – ", MHW_BASELINE_END,
      " · ≥5 días consecutivos, ≤2 días de brecha · OISST v2.1 · Hobday et al. 2016"
    )
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title    = element_text(face = "bold", hjust = 0.5, size = 12),
    plot.subtitle = element_text(hjust = 0.5, size = 8.5, color = "grey40"),
    plot.caption  = element_text(size = 8, color = "grey50"),
    axis.text.x   = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  )

save_plot(p_mhw, "mhw_cabo_pulmo")

# KPI scalars (used in JSON only)
kpi_mean_sst        <- round(mean(mpa_sst_daily$temp, na.rm = TRUE), 2)
kpi_mhw_days_per_yr <- round(mean(annual_mhw$heatwave_days), 1)

# Interpretation: before=1982-2000, after=last 5 complete years
ba_sst  <- before_after(annual_mhw, "mean_temp", year_col = "year",
                         before_years = 1982:2000, recent_n = 5L)
mkt_sst <- mk_trend(annual_mhw, "mean_temp", year_col = "year")
after_stress_n <- if (!is.null(ba_sst$after_period))
  sum(annual_mhw$mean_temp[
    annual_mhw$year >= ba_sst$after_period[1] &
    annual_mhw$year <= ba_sst$after_period[2]] -
    ba_sst$before_mean > 0.5, na.rm = TRUE) else 0L
sst_status <- if (after_stress_n >= 3) "negative" else
              if (after_stress_n == 0 && isTRUE(abs(ba_sst$delta) <= 0.1)) "positive" else "neutral"
interp_sst <- list(
  before_period       = ba_sst$before_period,
  after_period        = ba_sst$after_period,
  metric_before       = ba_sst$before_mean,
  metric_after        = ba_sst$after_mean,
  delta               = ba_sst$delta,
  pct_change          = ba_sst$pct_change,
  direction           = if (!is.null(ba_sst$direction)) ba_sst$direction else "unknown",
  status              = sst_status,
  stress_years_recent = after_stress_n,
  significance        = mkt_sst,
  insight_en          = if (is.null(ba_sst$status)) insight_sentence(
    var_label_en = "Mean SST", unit = "°C",
    before_mean = ba_sst$before_mean, after_mean = ba_sst$after_mean,
    delta = ba_sst$delta, pct_change = ba_sst$pct_change,
    before_period = ba_sst$before_period, after_period = ba_sst$after_period,
    extra_en = paste0(after_stress_n, " of the last 5 years were above the +0.5°C stress threshold."),
    digits = 2L)$en else NA_character_,
  insight_es          = if (is.null(ba_sst$status)) insight_sentence(
    var_label_en = "La temperatura superficial del mar (SST)", unit = "°C",
    before_mean = ba_sst$before_mean, after_mean = ba_sst$after_mean,
    delta = ba_sst$delta, pct_change = ba_sst$pct_change,
    before_period = ba_sst$before_period, after_period = ba_sst$after_period,
    extra_en = paste0(after_stress_n, " de los últimos 5 años superaron el umbral de estrés de +0.5°C."),
    digits = 2L)$en else NA_character_
)
cat("  Interpretation:", interp_sst$direction, "|", interp_sst$status, "\n")
cat("  EN:", interp_sst$insight_en, "\n")

# ── 4. Chl-a wave plot (cyclic GAM seasonal climatology) ─────────────────────
#
# Method: Chl-a is log-normally distributed (see erddap-chlorophyll/SKILL.md).
# We fit a cyclic GAM on log10(Chl-a) over the baseline period to estimate
# the smooth seasonal climatology. bs="cc" (cyclic cubic spline) enforces
# periodicity (Dec connects to Jan). Anomaly = log10(obs) − log10(climatology).
# Annual trend assessed with Mann-Kendall on annual geometric means.
# Reference: per-database/erddap-chlorophyll/references/chl_seasonal_gam_method.md

cat("\n=== 4. Chlorophyll-a ===\n")

npp_raw <- readRDS("data/NPP_coastal_zone_2004-2023.RDS") %>% as.data.frame()

mpa_chl_monthly <- npp_raw %>%
  filter(
    longitude >= MPA_LON_MIN - PAD, longitude <= MPA_LON_MAX + PAD,
    latitude  >= MPA_LAT_MIN - PAD, latitude  <= MPA_LAT_MAX + PAD,
    mean_npp > 0                        # log10 requires positive values
  ) %>%
  group_by(year, month) %>%
  summarise(mean_chla = mean(mean_npp, na.rm = TRUE), .groups = "drop") %>%
  arrange(year, month)

cat("  Monthly Chl-a records:", nrow(mpa_chl_monthly), "\n")

# ── Cyclic GAM seasonal climatology on baseline (log10 space) ─────────────────

baseline_data <- mpa_chl_monthly %>%
  filter(year %in% CHL_BASELINE_YEARS) %>%
  mutate(log10_chla = log10(mean_chla))

# k=6: smooth seasonal curve without overfitting monthly data
gam_clim <- mgcv::gam(log10_chla ~ s(month, bs = "cc", k = 6),
                       data = baseline_data, method = "REML")

cat("  GAM seasonal fit — deviance explained:",
    round(summary(gam_clim)$dev.expl * 100, 1), "%\n")

pred_grid  <- data.frame(month = 1:12)
pred_out   <- predict(gam_clim, newdata = pred_grid, se.fit = TRUE)

clim_smooth <- data.frame(
  month     = 1:12,
  log_clim  = pred_out$fit,
  log_se    = pred_out$se.fit,
  clim_chla = 10^pred_out$fit,
  clim_lo   = 10^(pred_out$fit - 1.96 * pred_out$se.fit),
  clim_hi   = 10^(pred_out$fit + 1.96 * pred_out$se.fit)
)

# ── Most recent complete year: wave plot ──────────────────────────────────────

latest_chl_year <- mpa_chl_monthly %>%
  count(year) %>% filter(n >= 10) %>% pull(year) %>% max()

cat("  Wave plot year:", latest_chl_year, "\n")

chl_wave <- mpa_chl_monthly %>%
  filter(year == latest_chl_year) %>%
  left_join(clim_smooth, by = "month") %>%
  mutate(
    t             = as.Date(paste(year, month, "15", sep = "-")),
    log10_obs     = log10(mean_chla),
    anomaly_log10 = log10_obs - log_clim   # positive = above climatology
  )

p_chl <- ggplot(chl_wave, aes(x = t)) +
  # 95% CI band on climatology
  geom_ribbon(aes(ymin = clim_lo, ymax = clim_hi),
              fill = "steelblue", alpha = 0.15) +
  # Bidirectional fill — above/below smooth climatology
  geom_ribbon(
    aes(ymin = clim_chla, ymax = pmax(mean_chla, clim_chla)),
    fill = "#21925F", alpha = 0.50
  ) +
  geom_ribbon(
    aes(ymin = pmin(mean_chla, clim_chla), ymax = clim_chla),
    fill = "#C6892A", alpha = 0.50
  ) +
  geom_line(aes(y = mean_chla),  color = "black",     linewidth = 0.65) +
  geom_line(aes(y = clim_chla),  color = "steelblue", linewidth = 0.85) +
  geom_point(aes(y = mean_chla), color = "black",     size = 2.0) +
  scale_x_date(
    date_breaks = "2 months", date_labels = "%b.",
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  labs(
    title    = paste0("Clorofila-a (", latest_chl_year, ") · ", TEST_MPA),
    subtitle = paste0(
      "Negro: Chl-a observada  |  Azul: climatología estacional (GAM cíclico, 2004–2013)  |  ",
      "Verde: sobre climatología  |  Naranja: bajo climatología"
    ),
    x = NULL, y = bquote("Clorofila-a (mg m"^-3*")"),
    caption = paste0(
      "Climatología: GAM con spline cíclico (bs='cc', k=6) sobre log10(Chl-a) · ",
      "Banda azul: IC 95% · Anomalía en escala log10 · MODIS-Aqua mensual"
    )
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title    = element_text(face = "bold", hjust = 0.5, size = 12),
    plot.subtitle = element_text(hjust = 0.5, size = 8.5, color = "grey40"),
    plot.caption  = element_text(size = 8, color = "grey50"),
    axis.text.x   = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  )

save_plot(p_chl, "chla_cabo_pulmo")

# ── Annual geometric means + Mann-Kendall trend (for JSON KPIs) ──────────────

chl_annual <- mpa_chl_monthly %>%
  filter(mean_chla > 0) %>%
  group_by(year) %>%
  summarise(geomean_chla = 10^mean(log10(mean_chla), na.rm = TRUE), .groups = "drop")

# Baseline geometric mean (log10 space)
clim_geomean  <- 10^mean(log10(mpa_chl_monthly$mean_chla[
  mpa_chl_monthly$year %in% CHL_BASELINE_YEARS &
  mpa_chl_monthly$mean_chla > 0]), na.rm = TRUE)
chl_annual    <- chl_annual %>%
  mutate(anomaly_log10 = round(log10(geomean_chla) - log10(clim_geomean), 4))

mk_test       <- Kendall::MannKendall(chl_annual$geomean_chla)
mk_trend_dir  <- ifelse(mk_test$tau > 0, "increasing", "decreasing")
cat("  Mann-Kendall tau:", round(mk_test$tau, 3),
    "p =", round(mk_test$sl, 3), "→", mk_trend_dir, "\n")

kpi_mean_chla <- round(mean(chl_annual$geomean_chla, na.rm = TRUE), 3)

# Interpretation: before=2004-2013 (baseline), after=last 5 complete years
ba_chl  <- before_after(chl_annual, "geomean_chla", year_col = "year",
                         before_years = CHL_BASELINE_YEARS, recent_n = 5L)
mkt_chl <- mk_trend(chl_annual, "geomean_chla", year_col = "year")
interp_chl <- list(
  before_period = ba_chl$before_period,
  after_period  = ba_chl$after_period,
  metric_before = ba_chl$before_mean,
  metric_after  = ba_chl$after_mean,
  delta         = ba_chl$delta,
  pct_change    = ba_chl$pct_change,
  direction     = if (!is.null(ba_chl$direction)) ba_chl$direction else "unknown",
  status        = "ambiguous",
  significance  = mkt_chl,
  insight_en    = if (is.null(ba_chl$status)) insight_sentence(
    var_label_en = "Chlorophyll-a", unit = "mg/m³",
    before_mean = ba_chl$before_mean, after_mean = ba_chl$after_mean,
    delta = ba_chl$delta, pct_change = ba_chl$pct_change,
    before_period = ba_chl$before_period, after_period = ba_chl$after_period,
    extra_en = paste0(
      if (!is.null(mkt_chl$tau)) paste0("Mann-Kendall: τ = ", mkt_chl$tau,
                                         ", p = ", mkt_chl$p, ". ") else "",
      "Note: Chl-a trends reflect upwelling dynamics and land-based nutrient inputs."),
    digits = 3L)$en else NA_character_,
  insight_es    = if (is.null(ba_chl$status)) insight_sentence(
    var_label_en = "La clorofila-a", unit = "mg/m³",
    before_mean = ba_chl$before_mean, after_mean = ba_chl$after_mean,
    delta = ba_chl$delta, pct_change = ba_chl$pct_change,
    before_period = ba_chl$before_period, after_period = ba_chl$after_period,
    extra_en = paste0(
      if (!is.null(mkt_chl$tau)) paste0("Mann-Kendall: τ = ", mkt_chl$tau,
                                         ", p = ", mkt_chl$p, ". ") else "",
      "Nota: La clorofila puede reflejar surgencia u aportes de nutrientes terrestres."),
    digits = 3L)$en else NA_character_
)
cat("  Interpretation:", interp_chl$direction, "|", interp_chl$status, "\n")
cat("  EN:", interp_chl$insight_en, "\n")

# ── 5. KPI JSON ───────────────────────────────────────────────────────────────

cat("\n=== 5. KPI JSON ===\n")

kpis <- list(
  mpa       = TEST_MPA,
  generated = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ"),
  biomass = list(
    kpi_mean_biomass_g_m2 = kpi_biomass$mean_biomass_g_m2,
    kpi_mean_biomass_T_ha = round(kpi_biomass$mean_biomass_g_m2 / 100, 4),
    kpi_years             = kpi_biomass$years_included,
    interpretation        = interp_biomass,
    annual_by_group       = lapply(VALID_FG, function(fg) {
      rows <- data_func[data_func$functional_group == fg, ]
      list(
        group  = fg,
        label  = FG_LABELS[[fg]],
        year   = rows$time,
        T_ha   = round(rows$value / 100, 5)
      )
    })
  ),
  nrsi = list(
    kpi_value        = round(kpi_nrsi_value, 3),
    kpi_latest_year  = latest_nrsi_yr,
    kpi_latest_value = round(latest_nrsi, 3),
    health_category  = nrsi_health,
    interpretation   = interp_nrsi
  ),
  sst = list(
    kpi_mean_sst_c       = kpi_mean_sst,
    kpi_mhw_days_per_yr  = kpi_mhw_days_per_yr,
    kpi_years            = paste0(min(annual_mhw$year), "–", max(annual_mhw$year)),
    interpretation       = interp_sst,
    annual = list(
      year          = annual_mhw$year,
      heatwave_days = annual_mhw$heatwave_days,
      mean_temp     = annual_mhw$mean_temp,
      n_events      = annual_mhw$n_events
    )
  ),
  chl = list(
    kpi_geomean_chla_mg_m3 = kpi_mean_chla,
    kpi_baseline_geomean   = round(clim_geomean, 3),
    kpi_years              = paste0(min(chl_annual$year), "–", max(chl_annual$year)),
    trend = list(
      mann_kendall_tau = round(as.numeric(mk_test$tau), 3),
      mann_kendall_p   = round(as.numeric(mk_test$sl), 3),
      direction        = mk_trend_dir
    ),
    climatology_method = "cyclic GAM (bs='cc', k=6) on log10(Chl-a), baseline 2004-2013",
    interpretation     = interp_chl,
    annual = list(
      year          = chl_annual$year,
      geomean_chla  = round(chl_annual$geomean_chla, 4),
      anomaly_log10 = chl_annual$anomaly_log10
    )
  )
)

json_path <- file.path(OUT_DIR, "kpis_cabo_pulmo.json")
write_json(kpis, json_path, pretty = TRUE, auto_unbox = TRUE)
cat("  ✓ saved:", json_path, "\n")

cat("\n── Done ──────────────────────────────────────────────────────────────\n")
cat("Outputs in", OUT_DIR, ":\n")
for (f in list.files(OUT_DIR, pattern = "\\.(png|json)$", full.names = FALSE)) {
  sz <- round(file.size(file.path(OUT_DIR, f)) / 1024)
  cat("  ", f, paste0("(", sz, " KB)"), "\n")
}
