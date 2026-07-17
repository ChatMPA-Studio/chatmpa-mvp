# skill.R — ltem-nrsi-index
#
# Computes the Normalized Reef State Index (NRSI) per reef-year from LTEM
# biomass survey data, with bootstrap 95% confidence intervals.
#
# See SKILL.md for the full data contract, method, and do-not rules.
# Resolves TODOs from SKILL.md: seed=42, UTL=LTL=CTL=0 edge case, reference JSON.

# ── Constants (fixed — do not change without updating SKILL.md) ──────────────

.SEED   <- 42L   # fixed seed for bootstrap reproducibility
.N_BOOT <- 100L  # bootstrap resamples (default; range 100-500)

# Trophic level mapping (fixed — do not modify)
.TROPHIC_MAP <- list(
  LTL = "2-2.5",                              # Lower: herbivores, detritivores
  UTL = "4-4.5",                              # Upper: apex predators
  CTL = c("2.5-3", "3-3.5", "3.5-4")         # Consumer: all intermediate levels
)

# NRSI health categories (for annotation only — not used in computation)
.HEALTH_CATEGORIES <- c(
  "Severely degraded" = -1.0,
  "Degraded"          = -0.5,
  "Good"              =  0.0,
  "Excellent"         =  0.5
)

# ── run_skill ─────────────────────────────────────────────────────────────────
#
# Args:
#   data     data.frame — minimal contract from ltem-db-mcp:
#            columns: time (int, year), value (num, Biomass g/m²),
#                     TrophicLevelF (chr), transect (chr), reef (chr)
#            optional: lat (num), lon (num), region (chr)
#   n_boot   integer — bootstrap resamples (default: 100)
#
# Returns: list(value, method, params)

run_skill <- function(data, n_boot = .N_BOOT, ...) {

  # ── Input validation ───────────────────────────────────────────────────────

  required_cols <- c("time", "value", "TrophicLevelF", "transect", "reef")
  missing_cols  <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0)
    stop("Missing columns in data: ", paste(missing_cols, collapse = ", "))

  if (!is.numeric(data$value))
    stop("'value' (Biomass) must be numeric.")

  # ── Step 1: Classify trophic level ────────────────────────────────────────

  data$trophic_class <- ifelse(
    data$TrophicLevelF == .TROPHIC_MAP$LTL, "LTL",
    ifelse(data$TrophicLevelF == .TROPHIC_MAP$UTL, "UTL",
           ifelse(data$TrophicLevelF %in% .TROPHIC_MAP$CTL, "CTL", NA_character_))
  )

  n_unclassified <- sum(is.na(data$trophic_class))
  if (n_unclassified > 0)
    warning(n_unclassified, " rows have unrecognized TrophicLevelF values — excluded.")

  data <- data[!is.na(data$trophic_class), ]

  # ── Step 2: Sum biomass per transect per trophic class ───────────────────

  transect_sums <- aggregate(
    value ~ time + reef + transect + trophic_class,
    data = data, FUN = sum, na.rm = TRUE
  )

  # Reshape to wide: one row per transect, columns LTL/UTL/CTL
  transect_wide <- reshape(
    transect_sums,
    idvar     = c("time", "reef", "transect"),
    timevar   = "trophic_class",
    direction = "wide",
    v.names   = "value"
  )
  names(transect_wide) <- gsub("^value\\.", "", names(transect_wide))

  # Fill missing trophic classes with 0 (species absent = 0 biomass)
  for (col in c("LTL", "UTL", "CTL")) {
    if (!col %in% names(transect_wide)) transect_wide[[col]] <- 0
    transect_wide[[col]][is.na(transect_wide[[col]])] <- 0
  }

  # ── Step 3: Handle UTL=LTL=CTL=0 edge case (transect with no fish) ───────
  # Exclude from reef average — never emit silent NaN (resolves TODO in SKILL.md)

  total_biomass <- transect_wide$UTL + transect_wide$LTL + transect_wide$CTL
  n_empty       <- sum(total_biomass == 0)
  if (n_empty > 0)
    warning(n_empty, " transect(s) with UTL=LTL=CTL=0 (no fish) excluded ",
            "from NRSI computation.")
  transect_wide <- transect_wide[total_biomass > 0, ]

  # ── Step 4: Compute NRSI per transect ────────────────────────────────────

  total <- transect_wide$UTL + transect_wide$LTL + transect_wide$CTL
  UTL_p <- transect_wide$UTL / total
  LTL_p <- transect_wide$LTL / total
  CTL_p <- transect_wide$CTL / total

  # Standard formula; conditional when LTL dominates
  ltl_dominates       <- LTL_p > (UTL_p + CTL_p)
  denom_conditional   <- UTL_p + CTL_p

  transect_wide$nrsi <- ifelse(
    ltl_dominates & denom_conditional > 0,
    UTL_p / denom_conditional,                          # conditional
    (UTL_p + LTL_p - CTL_p) / (UTL_p + LTL_p + CTL_p) # standard
  )

  # ── Step 5: Bootstrap 95% CI per reef-year ───────────────────────────────
  # Resample transect-level NRSI within each reef-year (resolves TODO in SKILL.md)

  set.seed(.SEED)  # fixed seed — reproducible CIs

  reef_years <- unique(transect_wide[, c("time", "reef")])

  results <- do.call(rbind, lapply(seq_len(nrow(reef_years)), function(i) {
    yr   <- reef_years$time[i]
    reef <- reef_years$reef[i]

    transects_nrsi <- transect_wide$nrsi[
      transect_wide$time == yr & transect_wide$reef == reef]

    n_transects <- length(transects_nrsi)
    nrsi_mean   <- mean(transects_nrsi)

    # Bootstrap CI
    boot_means <- replicate(n_boot, mean(sample(transects_nrsi,
                                                size    = n_transects,
                                                replace = TRUE)))
    ci_lo <- quantile(boot_means, 0.025)
    ci_hi <- quantile(boot_means, 0.975)
    ci_includes_zero <- ci_lo <= 0 & ci_hi >= 0

    # Health category
    health <- names(.HEALTH_CATEGORIES)[
      findInterval(nrsi_mean, .HEALTH_CATEGORIES, rightmost.closed = TRUE)]
    if (length(health) == 0) health <- "Severely degraded"

    data.frame(
      time              = yr,
      reef              = reef,
      nrsi              = round(nrsi_mean, 4),
      ci_lo_95          = round(ci_lo, 4),
      ci_hi_95          = round(ci_hi, 4),
      ci_includes_zero  = ci_includes_zero,
      health_category   = health,
      n_transects       = n_transects,
      n_ltl_dominant    = sum(ltl_dominates[
        transect_wide$time == yr & transect_wide$reef == reef])
    )
  }))

  rownames(results) <- NULL

  # ── Step 6: Regional comparison (Kruskal-Wallis) if region column present ──

  kw_result <- NULL
  if ("region" %in% names(data)) {
    reef_region <- unique(data[, c("reef", "region")])
    results     <- merge(results, reef_region, by = "reef", all.x = TRUE)

    kw_data <- split(results$nrsi, results$region)
    if (length(kw_data) >= 2) {
      kw      <- kruskal.test(nrsi ~ region, data = results)
      kw_result <- data.frame(
        statistic = round(kw$statistic, 3),
        df        = kw$parameter,
        p_value   = round(kw$p.value, 4)
      )
    }
  }

  # ── Return ────────────────────────────────────────────────────────────────

  list(
    value = list(
      nrsi_by_reef    = results,
      kruskal_wallis  = kw_result
    ),
    method = paste0(
      "NRSI per transect: standard (UTL+LTL-CTL)/(UTL+LTL+CTL), ",
      "conditional UTL/(UTL+CTL) when LTL > UTL+CTL. ",
      "Transects with total biomass=0 excluded. ",
      "Reef-year mean from bootstrap (n_boot=", n_boot, ", seed=", .SEED, "). ",
      "95% CI: 2.5th-97.5th percentiles of bootstrap distribution."
    ),
    params = list(
      seed           = .SEED,
      n_boot         = n_boot,
      trophic_map    = .TROPHIC_MAP,
      ci_level       = 0.95
    )
  )
}
