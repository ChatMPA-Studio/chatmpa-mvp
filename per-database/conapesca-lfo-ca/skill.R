# skill.R — conapesca-lfo-ca
#
# Builds a landing-office × resource-group matrix from CONAPESCA records and
# runs DCA + CA to assess whether fishing offices show spatial structure in
# catch composition, following Erisman et al. (2011). Produces CA ordination
# scores and WK/KL indices to inform the user's choice of k in the downstream
# skill conapesca-lfo-regions.
#
# Dependencies: vegan (decorana, cca, downweight, scores, eigenvals)
#
# See SKILL.md for the full data contract, method, and do-not rules.

# ── Constants (fixed — do not change without updating SKILL.md) ───────────────

.DEFAULT_YEAR_MIN    <- 2001L  # PENDING: verify with Edu/Fabio
.DEFAULT_YEAR_MAX    <- 2010L  # PENDING: verify with Edu/Fabio
.DEFAULT_K_RANGE     <- 3L:12L
.GRADIENT_THRESHOLD  <- 3.0    # DCA MLG threshold: > 3 → CA preferred over RDA
.KMEANS_SEED         <- 42L    # seed for WK/KL k-means loop
.KMEANS_NSTART       <- 25L
.KMEANS_ITER         <- 100L
.MIN_LFO             <- 5L     # minimum LFOs required to run CA
.VALID_AVISOS        <- c("MAYORES", "MENORES")
.VALID_TAX_RES       <- c("nombre_especie", "genus", "family", "nombre_cientifico")
.CA_AXES             <- 2L     # number of CA axes used in WK/KL computation

# ── run_skill ─────────────────────────────────────────────────────────────────
#
# Args:
#   data            data.frame — landing records from CONAPESCA MCP.
#                   Required cols: anio_corte, tipo_aviso,
#                   nombre_oficina_canonico, nombre_especie, peso_desembarcado_kg.
#                   Additional cols required depending on tax_resolution:
#                     "genus"            → genus
#                     "family"           → family
#                     "nombre_cientifico"→ nombre_cientifico
#   tax_resolution  character — one of .VALID_TAX_RES (default: "nombre_especie")
#   year_min        integer — first year of analysis period (default: 2001, PENDING)
#   year_max        integer — last year of analysis period (default: 2010, PENDING)
#   lfo_coords      data.frame or NULL — columns: nombre_oficina_canonico, lat, lon.
#                   If provided, enables spatial correlation step.
#   k_range         integer vector — range of k for WK/KL indices (default: 3:12)
#
# Returns: list(value, method, params)

run_skill <- function(data,
                      tax_resolution = "nombre_especie",
                      year_min       = .DEFAULT_YEAR_MIN,
                      year_max       = .DEFAULT_YEAR_MAX,
                      lfo_coords     = NULL,
                      k_range        = .DEFAULT_K_RANGE,
                      ...) {

  # ── Input validation ──────────────────────────────────────────────────────

  if (!requireNamespace("vegan", quietly = TRUE))
    stop("Package 'vegan' is required. Install with: install.packages('vegan')")

  if (!tax_resolution %in% .VALID_TAX_RES)
    stop("'tax_resolution' must be one of: ",
         paste(.VALID_TAX_RES, collapse = ", "))

  required_base <- c("anio_corte", "tipo_aviso", "nombre_oficina_canonico",
                     "nombre_especie", "peso_desembarcado_kg")
  required_extra <- switch(tax_resolution,
    "genus"             = "genus",
    "family"            = "family",
    "nombre_cientifico" = "nombre_cientifico",
    character(0)
  )
  required_cols <- c(required_base, required_extra)
  missing_cols  <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0)
    stop("Missing columns in data: ", paste(missing_cols, collapse = ", "))

  if (!is.null(lfo_coords)) {
    lfo_coord_cols <- c("nombre_oficina_canonico", "lat", "lon")
    missing_coord  <- setdiff(lfo_coord_cols, names(lfo_coords))
    if (length(missing_coord) > 0)
      stop("lfo_coords missing columns: ", paste(missing_coord, collapse = ", "))
  }

  if (length(k_range) < 2)
    stop("'k_range' must contain at least 2 values.")

  # ── Step 1: Filter ────────────────────────────────────────────────────────

  d <- data[data$anio_corte >= year_min & data$anio_corte <= year_max, ]
  d <- d[d$tipo_aviso %in% .VALID_AVISOS, ]

  if (nrow(d) == 0)
    stop("No records remain after filtering for years [", year_min, ", ",
         year_max, "] and fleet types MAYORES/MENORES.")

  # ── Step 2: Resolve taxonomic column ─────────────────────────────────────

  tax_col <- switch(tax_resolution,
    "nombre_especie"     = "nombre_especie",
    "genus"              = "genus",
    "family"             = "family",
    "nombre_cientifico"  = "nombre_cientifico"
  )

  d$tax_group <- as.character(d[[tax_col]])
  d <- d[!is.na(d$tax_group) & nzchar(trimws(d$tax_group)), ]

  if (nrow(d) == 0)
    stop("No records remain after filtering empty '", tax_col, "' values.")

  # ── Step 3: Build mean annual kg matrix (LFO × resource group) ────────────

  # Annual totals per LFO × tax_group × year
  annual <- aggregate(
    peso_desembarcado_kg ~ nombre_oficina_canonico + tax_group + anio_corte,
    data = d, FUN = sum, na.rm = TRUE
  )

  # Mean across years (each year weighted equally — PENDING: verify with Edu/Fabio)
  mean_annual <- aggregate(
    peso_desembarcado_kg ~ nombre_oficina_canonico + tax_group,
    data = annual, FUN = mean, na.rm = TRUE
  )

  lfo_names <- sort(unique(mean_annual$nombre_oficina_canonico))
  tax_names <- sort(unique(mean_annual$tax_group))

  if (length(lfo_names) < .MIN_LFO)
    stop("Only ", length(lfo_names), " LFOs with data. Minimum required: ",
         .MIN_LFO, ". Check year range and fleet filters.")

  # Pivot to wide matrix: rows = LFO, cols = tax_group, values = mean annual kg
  mat <- matrix(0.0,
    nrow = length(lfo_names), ncol = length(tax_names),
    dimnames = list(lfo_names, tax_names)
  )
  for (i in seq_len(nrow(mean_annual))) {
    lfo <- mean_annual$nombre_oficina_canonico[i]
    tax <- mean_annual$tax_group[i]
    mat[lfo, tax] <- mean_annual$peso_desembarcado_kg[i]
  }

  # ── Step 4: DCA to determine maximum gradient length ──────────────────────

  dca <- vegan::decorana(mat)

  # MLG: range of axis 1 site scores (DCA decorana stores them in $rproj)
  axis1_scores <- dca$rproj[, 1]
  mlg          <- round(max(axis1_scores) - min(axis1_scores), 3)
  method_chosen <- if (mlg > .GRADIENT_THRESHOLD) "CA" else "RDA"

  if (mlg <= .GRADIENT_THRESHOLD)
    warning("MLG = ", mlg, " ≤ ", .GRADIENT_THRESHOLD,
            ". RDA would be preferred, but CA is used for comparability with ",
            "Erisman et al. (2011). Interpret with caution.")

  # ── Step 5: Correspondence Analysis with rare-species downweighting ────────

  mat_dw <- vegan::downweight(mat)
  ca     <- vegan::cca(mat_dw)

  eig     <- vegan::eigenvals(ca)
  pct_var <- round(as.numeric(eig) / sum(as.numeric(eig)) * 100, 2)

  scores_lfo <- vegan::scores(ca, display = "sites",   choices = 1:.CA_AXES)
  scores_sp  <- vegan::scores(ca, display = "species", choices = 1:.CA_AXES)

  eigenvalue_table <- data.frame(
    axis         = paste0("CA", seq_along(eig)),
    eigenvalue   = round(as.numeric(eig), 4),
    pct_variance = round(as.numeric(pct_var), 2),
    stringsAsFactors = FALSE
  )

  # ── Step 6: Spatial correlation (if lfo_coords provided) ──────────────────

  spatial_corr <- NULL

  if (!is.null(lfo_coords)) {
    scores_df <- as.data.frame(scores_lfo)
    scores_df$nombre_oficina_canonico <- rownames(scores_df)
    joined <- merge(scores_df, lfo_coords, by = "nombre_oficina_canonico")

    if (nrow(joined) < 3) {
      warning("Fewer than 3 LFOs matched lfo_coords — spatial correlation skipped.")
    } else {
      ca2_col <- paste0("CA", .CA_AXES)  # axis 2 as in Erisman
      ct_lat  <- cor.test(joined[[ca2_col]], joined$lat, method = "pearson")
      ct_lon  <- cor.test(joined[[ca2_col]], joined$lon, method = "pearson")

      spatial_corr <- list(
        axis2_vs_lat = list(
          r       = round(as.numeric(ct_lat$estimate), 3),
          p_value = round(ct_lat$p.value, 4),
          ci_95   = round(as.numeric(ct_lat$conf.int), 3)
        ),
        axis2_vs_lon = list(
          r       = round(as.numeric(ct_lon$estimate), 3),
          p_value = round(ct_lon$p.value, 4),
          ci_95   = round(as.numeric(ct_lon$conf.int), 3)
        ),
        n_lfo_matched = nrow(joined),
        note = paste0(
          "Pearson correlation of CA axis 2 scores with latitude and longitude. ",
          "Following Erisman et al. (2011): positive CA2 ~ higher latitude (temperate); ",
          "negative CA2 ~ higher longitude (Gulf coast). ",
          "n = ", nrow(joined), " LFOs matched between CA scores and lfo_coords."
        )
      )
    }
  }

  # ── Step 7: WK and KL indices ─────────────────────────────────────────────
  # k-means on CA axes 1–2 for k in (min(k_range)-1):(max(k_range)+1)
  # Seed 42 applied inside each iteration for reproducibility

  scores_mat <- scores_lfo[, 1:.CA_AXES, drop = FALSE]
  k_eval     <- max(2L, min(k_range) - 1L) : (max(k_range) + 1L)

  wk_by_k <- setNames(numeric(length(k_eval)), as.character(k_eval))
  for (k in k_eval) {
    set.seed(.KMEANS_SEED)
    km <- kmeans(scores_mat, centers = k,
                 nstart = .KMEANS_NSTART, iter.max = .KMEANS_ITER)
    wk_by_k[as.character(k)] <- km$tot.withinss
  }

  # KL index per k in k_range
  p_dim    <- .CA_AXES
  kl_rows  <- lapply(k_range, function(k) {
    k_prev <- as.character(k - 1L)
    k_curr <- as.character(k)
    k_next <- as.character(k + 1L)

    if (!k_prev %in% names(wk_by_k) || !k_next %in% names(wk_by_k))
      return(NULL)

    wk_prev <- wk_by_k[k_prev]
    wk_curr <- wk_by_k[k_curr]
    wk_next <- wk_by_k[k_next]

    diff_k   <- (k - 1)^(2 / p_dim) * wk_prev - k^(2 / p_dim) * wk_curr
    diff_kp1 <- k^(2 / p_dim) * wk_curr - (k + 1)^(2 / p_dim) * wk_next

    kl <- if (abs(diff_kp1) > 1e-10) abs(diff_k) / abs(diff_kp1) else NA_real_

    data.frame(k = k, WK = round(wk_curr, 3), KL = round(kl, 3),
               stringsAsFactors = FALSE)
  })
  kl_table <- do.call(rbind, Filter(Negate(is.null), kl_rows))

  k_optimal_kl <- if (any(!is.na(kl_table$KL)))
    kl_table$k[which.max(kl_table$KL)]
  else NA_integer_

  # ── Return ────────────────────────────────────────────────────────────────

  spatial_note <- if (is.null(lfo_coords))
    " spatial_correlation = NULL (lfo_coords not provided — PENDING)."
  else ""

  list(
    value = list(
      matrix_lfo          = as.data.frame(mat),
      ca_scores_lfo       = as.data.frame(scores_lfo),
      ca_scores_resources = as.data.frame(scores_sp),
      eigenvalues         = eigenvalue_table,
      mlg                 = mlg,
      method_chosen       = method_chosen,
      kl_table            = kl_table,
      k_optimal_kl        = k_optimal_kl,
      spatial_correlation = spatial_corr,
      n_lfo               = length(lfo_names),
      n_tax_groups        = length(tax_names),
      years_used          = year_min:year_max
    ),
    method = paste0(
      "Matrix: mean annual peso_desembarcado_kg per LFO × ", tax_resolution,
      " (MAYORES + MENORES combined), years ", year_min, "–", year_max,
      ". Each year weighted equally (mean, not period total) — PENDING validation. ",
      "DCA: vegan::decorana on raw matrix → MLG = ", mlg, " SD → ", method_chosen,
      " selected (threshold = ", .GRADIENT_THRESHOLD, " SD). ",
      "CA: vegan::cca(vegan::downweight(matrix)) — rare resource groups downweighted. ",
      "WK/KL: k-means on CA axes 1–2, seed = ", .KMEANS_SEED,
      ", nstart = ", .KMEANS_NSTART, ", iter.max = ", .KMEANS_ITER,
      ", k evaluated = ", min(k_eval), "–", max(k_eval), ".",
      spatial_note
    ),
    params = list(
      tax_resolution     = tax_resolution,
      year_min           = year_min,
      year_max           = year_max,
      k_range            = as.integer(k_range),
      ca_axes_used       = .CA_AXES,
      gradient_threshold = .GRADIENT_THRESHOLD,
      kmeans_seed        = .KMEANS_SEED,
      pending_params = list(
        year_min = paste0(
          "PENDING — 2001 used as default; verify with Edu/Fabio that data ",
          "coverage is adequate from this year across all LFOs."
        ),
        year_max = paste0(
          "PENDING — 2010 used as default; verify with Edu/Fabio whether this ",
          "is conservative enough to avoid strong depletion or climate-shift signals."
        ),
        temporal_resolution = paste0(
          "PENDING — mean annual kg used (each year weighted equally). ",
          "Verify with Edu/Fabio whether period totals are preferable."
        )
      )
    )
  )
}
