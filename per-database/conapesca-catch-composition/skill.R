# skill.R — conapesca-catch-composition
#
# Shows the production composition of a landing office: what percentage comes
# from capture fisheries (MAYORES + MENORES) vs aquaculture (COSECHA),
# in both volume and estimated value.
#
# Aggregates the full available period and displays composition as percentages.
# Output: two pie charts (volume and value) + composition table.
#
# Data contract:
#   data — pre-aggregated from get_landings(group_by="year_fleet").
#          MCP handles all filtering (office, state, species). Always aggregates
#          the full available period — no year input.
#
# See SKILL.md for the full data contract, method, and do-not rules.

# ── Constants ─────────────────────────────────────────────────────────────────

.CAPTURE_AVISOS <- c("MAYORES", "MENORES")
.AQUAC_AVISOS   <- "COSECHA"
.ALL_AVISOS     <- c(.CAPTURE_AVISOS, .AQUAC_AVISOS)

# Tableau 10 — consistent across all conapesca skills
.COLOR_CAPTURE <- "#4E79A7"
.COLOR_AQUAC   <- "#59A14F"

# ── run_skill ─────────────────────────────────────────────────────────────────
#
# Args:
#   data                       data.frame — pre-aggregated from
#                              get_landings(group_by="year_fleet").
#                              Required cols: anio_corte, tipo_aviso,
#                              total_kg, total_valor_mxn, n_registros.
#   office_filter              character or NULL — label only.
#   state_filter               character or NULL — label only.
#   resource_group             character or NULL — label only (nombre_principal).
#   species                    character or NULL — label only (nombre_cientifico_canonico).
#                              Mutually exclusive with resource_group.
#
# Returns: list(value, plot_volumen, plot_valor, method, params)

run_skill <- function(data,
                      office_filter  = NULL,
                      state_filter   = NULL,
                      resource_group = NULL,
                      species        = NULL,
                      ...) {

  # ── Input validation ───────────────────────────────────────────────────────

  data <- .normalize_comp_cols(data)

  required_cols <- c("anio_corte", "tipo_aviso", "total_kg",
                     "total_valor_mxn", "n_registros")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0)
    stop("Missing columns in data: ", paste(missing_cols, collapse = ", "))

  if (!is.null(resource_group) && !is.null(species))
    stop("'resource_group' and 'species' are mutually exclusive.")

  if (nrow(data) == 0)
    stop("No records found in data.")

  # ── Step 1: Aggregate across full period ──────────────────────────────────

  data_valid <- data[data$tipo_aviso %in% .ALL_AVISOS, ]

  cap_rows   <- data_valid[data_valid$tipo_aviso %in% .CAPTURE_AVISOS, ]
  aquac_rows <- data_valid[data_valid$tipo_aviso %in% .AQUAC_AVISOS, ]

  capture_kg    <- sum(cap_rows$total_kg,          na.rm = TRUE)
  aquac_kg      <- sum(aquac_rows$total_kg,        na.rm = TRUE)
  capture_valor <- sum(cap_rows$total_valor_mxn,   na.rm = TRUE)
  aquac_valor   <- sum(aquac_rows$total_valor_mxn, na.rm = TRUE)

  total_kg    <- capture_kg    + aquac_kg
  total_valor <- capture_valor + aquac_valor

  if (total_kg == 0)
    stop("Total landed weight is zero — cannot compute composition.")

  # ── Step 2: Composition table ─────────────────────────────────────────────

  value <- data.frame(
    component        = c("Capture", "Aquaculture"),
    total_tonnes     = c(capture_kg / 1000,  aquac_kg / 1000),
    total_value_mxn  = c(capture_valor,       aquac_valor),
    pct_volume       = round(c(capture_kg, aquac_kg) / total_kg    * 100, 1),
    pct_value        = round(c(capture_valor, aquac_valor) / total_valor * 100, 1),
    year_min         = min(data_valid$anio_corte),
    year_max         = max(data_valid$anio_corte),
    stringsAsFactors = FALSE
  )

  # ── Step 3: Plots ──────────────────────────────────────────────────────────

  filter_label <- if (!is.null(resource_group)) {
    paste0("Resource: ", resource_group)
  } else if (!is.null(species)) {
    paste0("Species: ", species)
  } else {
    "All resources"
  }

  period_label <- paste0(min(data_valid$anio_corte), "\u2013",
                         max(data_valid$anio_corte))

  subtitle <- paste0(filter_label, " | ", period_label)

  plot_volumen <- .build_pie(value$pct_volume, value$component, subtitle)
  plot_valor   <- .build_pie(value$pct_value,  value$component, subtitle)

  # ── Return ─────────────────────────────────────────────────────────────────

  list(
    value        = value,
    plot_volumen = plot_volumen,
    plot_valor   = plot_valor,
    method       = paste0(
      "Accumulated composition for period ", period_label, ". ",
      "Capture = sum(total_kg) where tipo_aviso IN ('MAYORES','MENORES'). ",
      "Aquaculture = sum(total_kg) where tipo_aviso = 'COSECHA'. ",
      "Percentages computed over total accumulated volume/value for the period. ",
      "Data pre-aggregated by MCP (get_landings group_by='year_fleet')."
    ),
    params = list(
      office_filter  = office_filter,
      state_filter   = state_filter,
      resource_group = resource_group,
      species        = species,
      period         = c(min(data_valid$anio_corte),
                         max(data_valid$anio_corte))
    )
  )
}

# ── .normalize_comp_cols ──────────────────────────────────────────────────────
# Accepts either MCP aggregate column names or raw column names.

.normalize_comp_cols <- function(data) {
  if (!"total_kg" %in% names(data) && "peso_desembarcado_kg" %in% names(data))
    data$total_kg <- data$peso_desembarcado_kg
  if (!"total_valor_mxn" %in% names(data) && "valor_pesos_estimado" %in% names(data))
    data$total_valor_mxn <- data$valor_pesos_estimado
  if (!"n_registros" %in% names(data) && "n_records" %in% names(data))
    data$n_registros <- data$n_records
  data
}

# ── .build_pie ────────────────────────────────────────────────────────────────
# Two-segment pie chart: capture vs aquaculture.
# Displays percentages only, not absolute values.

.build_pie <- function(pct, components, subtitle) {

  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("Package 'ggplot2' is required.")

  df <- data.frame(
    component = factor(components, levels = c("Capture", "Aquaculture")),
    pct       = pct,
    stringsAsFactors = FALSE
  )

  # Only label segments >= 2% to avoid overlap
  df$label <- ifelse(df$pct >= 2, paste0(df$pct, "%"), "")

  ggplot2::ggplot(df, ggplot2::aes(x = "", y = pct, fill = component)) +
    ggplot2::geom_col(width = 1, color = "white", linewidth = 0.5) +
    ggplot2::coord_polar(theta = "y", start = 0) +
    ggplot2::geom_text(
      ggplot2::aes(label = label),
      position = ggplot2::position_stack(vjust = 0.5),
      size     = 4.5,
      color    = "white",
      fontface = "bold"
    ) +
    ggplot2::scale_fill_manual(
      name   = NULL,
      values = c(Capture     = .COLOR_CAPTURE,
                 Aquaculture = .COLOR_AQUAC),
      labels = c(Capture     = "Capture (MAYORES + MENORES)",
                 Aquaculture = "Aquaculture (COSECHA)")
    ) +
    ggplot2::labs(subtitle = subtitle) +
    ggplot2::theme_void(base_size = 12) +
    ggplot2::theme(
      legend.position  = "bottom",
      legend.direction = "horizontal",
      plot.subtitle    = ggplot2::element_text(color  = "grey40",
                                               hjust  = 0.5,
                                               margin = ggplot2::margin(b = 8))
    )
}
