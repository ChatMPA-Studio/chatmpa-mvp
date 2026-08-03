# skill.R — conapesca-lfo-regions
#
# Clusters CONAPESCA landing offices into k fishing regions using k-means on
# CA scores from conapesca-lfo-ca. Regions are renumbered north-to-south by
# mean CA axis 2 score (which correlates positively with latitude). Produces
# a canonical lookup table for downstream skills.
#
# Seed 42 is fixed. Do not change without updating SKILL.md.
#
# See SKILL.md for the full data contract, method, and do-not rules.

# ── Constants (fixed — do not change without updating SKILL.md) ───────────────

.KMEANS_SEED   <- 42L
.KMEANS_NSTART <- 25L
.KMEANS_ITER   <- 100L

# ── run_skill ─────────────────────────────────────────────────────────────────
#
# Args:
#   ca_output   list — complete output of conapesca-lfo-ca$run_skill().
#               Required: ca_output$value$ca_scores_lfo (data.frame, CA1 CA2),
#                         ca_output$value$kl_table,
#                         ca_output$value$k_optimal_kl
#   k           integer — number of clusters, chosen by the user after
#               reviewing the KL/WK table from conapesca-lfo-ca.
#
# Returns: list(value, method, params)

run_skill <- function(ca_output, k, ...) {

  # ── Input validation ──────────────────────────────────────────────────────

  if (!is.list(ca_output) || is.null(ca_output$value))
    stop("'ca_output' must be the complete list returned by ",
         "conapesca-lfo-ca$run_skill().")

  required_fields <- c("ca_scores_lfo", "kl_table", "k_optimal_kl")
  missing_fields  <- setdiff(required_fields, names(ca_output$value))
  if (length(missing_fields) > 0)
    stop("ca_output$value missing fields: ",
         paste(missing_fields, collapse = ", "))

  scores_df <- ca_output$value$ca_scores_lfo
  if (!all(c("CA1", "CA2") %in% names(scores_df)))
    stop("ca_output$value$ca_scores_lfo must have columns CA1 and CA2.")

  k <- as.integer(k)
  n_lfo <- nrow(scores_df)

  if (k < 2L)
    stop("k must be at least 2.")
  if (k >= n_lfo)
    stop("k = ", k, " must be less than the number of LFOs (", n_lfo, ").")

  lfo_names <- rownames(scores_df)
  if (is.null(lfo_names))
    stop("ca_output$value$ca_scores_lfo must have LFO names as row names.")

  # ── Step 1: k-means clustering ────────────────────────────────────────────

  scores_mat <- as.matrix(scores_df[, c("CA1", "CA2")])

  set.seed(.KMEANS_SEED)
  km <- kmeans(scores_mat,
               centers  = k,
               nstart   = .KMEANS_NSTART,
               iter.max = .KMEANS_ITER)

  # ── Step 2: Geographic renumbering (north to south via CA2) ───────────────
  # CA axis 2 correlates positively with latitude (Erisman et al. 2011).
  # Rank clusters by descending mean CA2: rank 1 = highest CA2 = northernmost.

  raw_cluster   <- km$cluster   # named integer vector (names = LFO names)
  cluster_ca2   <- tapply(scores_df$CA2, raw_cluster, mean)
  # rank() with ties.method="first" for determinism
  north_rank    <- rank(-cluster_ca2, ties.method = "first")

  # Map raw cluster id → canonical region number
  cluster_to_region <- setNames(as.integer(north_rank), names(north_rank))

  region_num <- cluster_to_region[as.character(raw_cluster)]
  region_id  <- paste0("region_", region_num)

  # ── Build lookup table ────────────────────────────────────────────────────

  lookup <- data.frame(
    nombre_oficina_canonico = lfo_names,
    region_id               = region_id,
    region_num              = region_num,
    stringsAsFactors = FALSE,
    row.names = NULL
  )
  lookup <- lookup[order(lookup$region_num, lookup$nombre_oficina_canonico), ]

  # ── Step 3: MDS verification ──────────────────────────────────────────────

  dist_mat <- dist(scores_mat, method = "euclidean")
  mds      <- cmdscale(dist_mat, k = 2L)

  mds_df <- data.frame(
    nombre_oficina_canonico = lfo_names,
    MDS1      = round(mds[, 1], 4),
    MDS2      = round(mds[, 2], 4),
    region_id = region_id,
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  # ── Step 4: Cluster summary ───────────────────────────────────────────────

  # Mean annual landings per region (from matrix_lfo if available)
  has_matrix <- !is.null(ca_output$value$matrix_lfo)

  summary_rows <- lapply(seq_len(k), function(rn) {
    sub      <- lookup[lookup$region_num == rn, ]
    sub_lfo  <- sub$nombre_oficina_canonico
    ca2_vals <- scores_df$CA2[match(sub_lfo, lfo_names)]
    ca1_vals <- scores_df$CA1[match(sub_lfo, lfo_names)]

    mean_annual_kg <- NA_real_
    if (has_matrix) {
      mat_sub <- ca_output$value$matrix_lfo[
        rownames(ca_output$value$matrix_lfo) %in% sub_lfo, , drop = FALSE]
      if (nrow(mat_sub) > 0)
        mean_annual_kg <- round(sum(mat_sub), 1)
    }

    data.frame(
      region_id              = paste0("region_", rn),
      region_num             = rn,
      n_lfo                  = nrow(sub),
      lfo_names              = paste(sort(sub_lfo), collapse = "; "),
      mean_ca1               = round(mean(ca1_vals, na.rm = TRUE), 4),
      mean_ca2               = round(mean(ca2_vals, na.rm = TRUE), 4),
      total_mean_annual_kg   = mean_annual_kg,
      region_label_suggested = NA_character_,  # orchestrator fills this
      stringsAsFactors = FALSE
    )
  })
  cluster_summary <- do.call(rbind, summary_rows)

  # ── Return ────────────────────────────────────────────────────────────────

  k_opt <- ca_output$value$k_optimal_kl

  list(
    value = list(
      lookup          = lookup,
      cluster_summary = cluster_summary,
      mds_coords      = mds_df,
      k_used          = k,
      wk_final        = round(km$tot.withinss, 3)
    ),
    method = paste0(
      "k-means (k = ", k, ") on CA axes 1–2 scores from conapesca-lfo-ca. ",
      "seed = ", .KMEANS_SEED, ", nstart = ", .KMEANS_NSTART,
      ", iter.max = ", .KMEANS_ITER, ". ",
      "Regions renumbered north-to-south by mean CA axis 2 score ",
      "(CA2 correlates positively with latitude — Erisman et al. 2011). ",
      "MDS verification via stats::cmdscale on Euclidean distances in CA score space. ",
      "region_label_suggested is NA — to be filled by orchestrator for communication only."
    ),
    params = list(
      k                = k,
      k_optimal_kl     = k_opt,
      k_matches_kl_opt = isTRUE(k == k_opt),
      kmeans_seed      = .KMEANS_SEED,
      kmeans_nstart    = .KMEANS_NSTART,
      ordering_basis   = paste0(
        "Mean CA axis 2 score per cluster (descending). ",
        "Region 1 = northernmost (highest mean CA2). ",
        "Consistent regardless of lfo_coords availability."
      ),
      upstream_skill   = "conapesca-lfo-ca",
      upstream_params  = ca_output$params
    )
  )
}
