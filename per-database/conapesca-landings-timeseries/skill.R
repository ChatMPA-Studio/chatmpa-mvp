# skill.R — conapesca-landings-timeseries
#
# Produces annual time series of landed volume (tonnes) and estimated production
# value (MXN) for a landing office, disaggregated by fleet type (TOTAL, MAYORES,
# MENORES, COSECHA). Optionally includes CPUE series when folio-level data is
# provided.
#
# Data contract:
#   data       — pre-aggregated from get_landings(group_by="year_fleet").
#                MCP handles all filtering (office, state, species, year_range).
#   data_folio — optional, from get_landings(group_by="folio").
#                When provided, CPUE is computed internally via conapesca-cpue.
#
# See SKILL.md for the full data contract, method, and do-not rules.

# ── Constants (fixed — do not change without updating SKILL.md) ──────────────

.MIN_RECORDS_WARN <- 5L
.VALID_AVISOS_TS  <- c("MAYORES", "MENORES", "COSECHA")
.FLEET_ORDER      <- c("TOTAL", "MAYORES", "MENORES", "COSECHA")

# Tableau 10 palette — consistent across all conapesca skills
.SERIES_COLORS <- c(
  TOTAL   = "#E15759",
  MAYORES = "#4E79A7",
  MENORES = "#F28E2B",
  COSECHA = "#59A14F"
)
.SERIES_SHAPES <- c(
  TOTAL   = 15L,  # filled square
  MAYORES = 17L,  # filled triangle
  MENORES = 16L,  # filled circle
  COSECHA = 18L   # filled diamond
)

# ── run_skill ─────────────────────────────────────────────────────────────────
#
# Args:
#   data                       data.frame — pre-aggregated from
#                              get_landings(group_by="year_fleet").
#                              Required cols: anio_corte, tipo_aviso,
#                              total_kg (or peso_desembarcado_kg),
#                              total_valor_mxn (or valor_pesos_estimado),
#                              n_records (or n_registros).
#                              MCP filtering already applied — no row filtering done here.
#   data_folio                 data.frame or NULL — folio-level data from
#                              get_landings(group_by="folio"). When provided,
#                              CPUE series are computed and included in output.
#   office_filter              character — label only (for plot subtitle / params).
#                              Filtering was already done by MCP.
#   state_filter               character — label only.
#   nombre_principal           character or NULL — label only.
#   nombre_cientifico_canonico character or NULL — label only.
#   year_range                 integer vector c(start, end) or NULL — label only.
#
# Returns: list(value_volumen, value_valor, plot_volumen, plot_valor,
#               value_cpue, plot_cpue, method, params)
#   value_cpue and plot_cpue are NULL when data_folio is not provided.

run_skill <- function(data,
                      data_folio                 = NULL,
                      office_filter              = NULL,
                      state_filter               = NULL,
                      nombre_principal           = NULL,
                      nombre_cientifico_canonico = NULL,
                      year_range                 = NULL,
                      ...) {

  # ── Input validation ───────────────────────────────────────────────────────

  # Normalize column names — accept either MCP aggregate names or raw names
  data <- .normalize_ts_cols(data)

  required_cols <- c("anio_corte", "tipo_aviso", "total_kg",
                     "total_valor_mxn", "n_registros")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0)
    stop("Missing columns in data: ", paste(missing_cols, collapse = ", "))

  if (!is.null(nombre_principal) && !is.null(nombre_cientifico_canonico))
    stop("'nombre_principal' and 'nombre_cientifico_canonico' are mutually exclusive.")

  # ── Step 1: Exclude COSECHA from base data if not present ─────────────────
  # data already filtered by MCP; just ensure tipo_aviso is limited to valid set

  data <- data[data$tipo_aviso %in% .VALID_AVISOS_TS, ]

  if (nrow(data) == 0)
    stop("No records found in data after tipo_aviso filter.")

  # ── Step 2: Compute TOTAL series ──────────────────────────────────────────

  total_rows <- do.call(rbind, lapply(unique(data$anio_corte), function(yr) {
    sub <- data[data$anio_corte == yr, ]
    data.frame(
      anio_corte      = yr,
      tipo_aviso      = "TOTAL",
      total_kg        = sum(sub$total_kg,        na.rm = TRUE),
      total_toneladas = sum(sub$total_kg,        na.rm = TRUE) / 1000,
      total_valor_mxn = sum(sub$total_valor_mxn, na.rm = TRUE),
      n_registros     = sum(sub$n_registros,      na.rm = TRUE)
    )
  }))

  # ── Step 3: Combine fleet + TOTAL ─────────────────────────────────────────

  fleet_rows <- data
  fleet_rows$total_toneladas <- fleet_rows$total_kg / 1000

  value <- rbind(total_rows, fleet_rows[, c("anio_corte", "tipo_aviso",
                                             "total_kg", "total_toneladas",
                                             "total_valor_mxn", "n_registros")])
  value$tipo_aviso <- factor(value$tipo_aviso, levels = .FLEET_ORDER)
  value <- value[order(value$anio_corte, value$tipo_aviso), ]
  value$reliable <- value$n_registros >= .MIN_RECORDS_WARN
  rownames(value) <- NULL

  # Separate tables per metric
  value_volumen <- value[, c("anio_corte", "tipo_aviso", "total_toneladas",
                              "n_registros", "reliable")]
  value_valor   <- value[, c("anio_corte", "tipo_aviso", "total_valor_mxn",
                              "n_registros", "reliable")]

  # ── Step 4: Plots ──────────────────────────────────────────────────────────

  filter_label <- if (!is.null(nombre_principal)) {
    paste0("Recurso: ", nombre_principal)
  } else if (!is.null(nombre_cientifico_canonico)) {
    paste0("Especie: ", nombre_cientifico_canonico)
  } else {
    "Todos los recursos"
  }

  plot_volumen <- .build_ts_plot(
    value         = value_volumen,
    y_col         = "total_toneladas",
    y_axis_label  = "Toneladas desembarcadas",
    filter_label  = filter_label,
    n_col         = "n_registros",
    low_n_label   = "n < 5 registros"
  )

  plot_valor <- .build_ts_plot(
    value            = value_valor,
    y_col            = "total_valor_mxn",
    y_axis_label     = NULL,
    filter_label     = filter_label,
    n_col            = "n_registros",
    low_n_label      = "n < 5 registros",
    auto_scale_value = TRUE
  )

  # ── Step 5: CPUE (optional) ────────────────────────────────────────────────

  value_cpue <- NULL
  plot_cpue  <- NULL

  if (!is.null(data_folio)) {
    cpue_result <- tryCatch({
      cpue_env <- new.env(parent = emptyenv())
      source(file.path(dirname(sys.frames()[[1L]]$ofile %||%
                       "per-database/conapesca-cpue/skill.R"),
                       "../conapesca-cpue/skill.R"),
             local = cpue_env)
      cpue_env$run_skill(
        data                       = data_folio,
        office_filter              = office_filter,
        state_filter               = state_filter,
        nombre_principal           = nombre_principal,
        nombre_cientifico_canonico = nombre_cientifico_canonico,
        year_range                 = year_range
      )
    }, error = function(e) {
      warning("CPUE computation failed: ", conditionMessage(e))
      NULL
    })

    if (!is.null(cpue_result)) {
      value_cpue <- cpue_result$value
      plot_cpue  <- cpue_result$plot
    }
  }

  # ── Return ─────────────────────────────────────────────────────────────────

  list(
    value_volumen = value_volumen,
    value_valor   = value_valor,
    plot_volumen  = plot_volumen,
    plot_valor    = plot_valor,
    value_cpue    = value_cpue,
    plot_cpue     = plot_cpue,
    method        = paste0(
      "Annual sums of total_kg (converted to tonnes) and total_valor_mxn ",
      "by anio_corte x tipo_aviso. ",
      "TOTAL = MAYORES + MENORES + COSECHA. ",
      "Data pre-aggregated by MCP (get_landings group_by='year_fleet'). ",
      "No quality flag filtering applied — all records contribute.",
      if (!is.null(data_folio)) paste0(
        " CPUE computed from folio-level data via conapesca-cpue skill ",
        "(mean-of-ratios, quality flags applied)."
      )
    ),
    params = list(
      office_filter              = office_filter,
      state_filter               = state_filter,
      nombre_principal           = nombre_principal,
      nombre_cientifico_canonico = nombre_cientifico_canonico,
      year_range                 = year_range,
      has_cpue                   = !is.null(data_folio),
      min_records_warn           = .MIN_RECORDS_WARN
    )
  )
}

# ── .normalize_ts_cols ────────────────────────────────────────────────────────
# Accepts either MCP aggregate column names or raw column names.

.normalize_ts_cols <- function(data) {
  # total_kg: accept peso_desembarcado_kg if total_kg absent
  if (!"total_kg" %in% names(data) && "peso_desembarcado_kg" %in% names(data))
    data$total_kg <- data$peso_desembarcado_kg
  # total_valor_mxn: accept valor_pesos_estimado
  if (!"total_valor_mxn" %in% names(data) && "valor_pesos_estimado" %in% names(data))
    data$total_valor_mxn <- data$valor_pesos_estimado
  # n_registros: accept n_records
  if (!"n_registros" %in% names(data) && "n_records" %in% names(data))
    data$n_registros <- data$n_records
  data
}

# ── .auto_scale_value ─────────────────────────────────────────────────────────
# Returns divisor and axis label for MXN values based on magnitude.

.auto_scale_value <- function(x) {
  max_val <- max(x, na.rm = TRUE)
  if      (max_val >= 1e9) list(divisor = 1e9, label = "Miles de millones MXN")
  else if (max_val >= 1e6) list(divisor = 1e6, label = "Millones MXN")
  else if (max_val >= 1e3) list(divisor = 1e3, label = "Miles MXN")
  else                     list(divisor = 1,   label = "MXN")
}

# ── .build_ts_plot ────────────────────────────────────────────────────────────
#
# Internal. Builds a ggplot2 trend line chart for a single timeseries metric.
#
# Args:
#   value            data.frame with columns: anio_corte, tipo_aviso, <y_col>,
#                    n_registros, reliable
#   y_col            character — name of the metric column to plot
#   y_axis_label     character or NULL — axis label (NULL triggers auto-scale for value)
#   filter_label     character — subtitle text
#   n_col            character — column with record count (for hollow point threshold)
#   low_n_label      character — legend label for hollow points
#   auto_scale_value logical — if TRUE, auto-scale MXN axis

.build_ts_plot <- function(value, y_col, y_axis_label, filter_label,
                           n_col, low_n_label, auto_scale_value = FALSE) {

  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("Package 'ggplot2' is required.")
  if (!requireNamespace("scales", quietly = TRUE))
    stop("Package 'scales' is required.")

  # Auto-scale value axis if requested
  scale_info <- if (auto_scale_value) {
    .auto_scale_value(value[[y_col]])
  } else {
    list(divisor = 1, label = y_axis_label)
  }

  # Apply scaling to the metric column
  value$y_display <- value[[y_col]] / scale_info$divisor

  # Y-axis: log10 if range spans more than one order of magnitude
  y_range <- range(value$y_display[value$y_display > 0], na.rm = TRUE)
  use_log  <- length(y_range) == 2L && !any(is.na(y_range)) &&
              (y_range[2L] / y_range[1L]) > 10

  y_scale <- if (use_log) {
    ggplot2::scale_y_log10(
      labels = scales::label_number(accuracy = 0.1, big.mark = ","),
      name   = scale_info$label
    )
  } else {
    ggplot2::scale_y_continuous(
      labels = scales::label_number(accuracy = 0.1, big.mark = ","),
      name   = scale_info$label
    )
  }

  # Dummy row for "n < 5" legend entry
  dummy <- value[1L, ]
  dummy$anio_corte <- NA_integer_
  dummy$y_display  <- NA_real_
  dummy$tipo_aviso <- "n < 5 registros"
  dummy$reliable   <- FALSE

  fleet_levels <- c(as.character(.FLEET_ORDER), "n < 5 registros")
  value$tipo_aviso  <- factor(as.character(value$tipo_aviso), levels = fleet_levels)
  dummy$tipo_aviso  <- factor("n < 5 registros",              levels = fleet_levels)

  real_data         <- value
  unreliable        <- value[!value$reliable, ]
  filled_layer_data <- rbind(value[value$reliable, ], dummy)

  p <- ggplot2::ggplot(mapping = ggplot2::aes(
                         x     = anio_corte,
                         y     = y_display,
                         color = tipo_aviso,
                         shape = tipo_aviso)) +

    ggplot2::geom_line(data = real_data, linewidth = 0.8, na.rm = TRUE) +

    ggplot2::geom_point(data = filled_layer_data, size = 4, stroke = 1.2, na.rm = TRUE) +

    ggplot2::geom_point(data = unreliable, shape = 1L,
                        size = 4, stroke = 1.5, show.legend = FALSE) +


    y_scale +
    ggplot2::scale_x_continuous(breaks = scales::breaks_pretty(n = 8)) +

    ggplot2::scale_color_manual(
      name   = NULL,
      values = c(.SERIES_COLORS, "n < 5 registros" = "grey60"),
      labels = c(TOTAL   = "Total", MAYORES = "Flota mayor",
                 MENORES = "Flota menor", COSECHA = "Acuacultura",
                 "n < 5 registros" = low_n_label)
    ) +
    ggplot2::scale_shape_manual(
      name   = NULL,
      values = c(.SERIES_SHAPES, "n < 5 registros" = 1L),
      labels = c(TOTAL   = "Total", MAYORES = "Flota mayor",
                 MENORES = "Flota menor", COSECHA = "Acuacultura",
                 "n < 5 registros" = low_n_label)
    ) +

    ggplot2::labs(subtitle = filter_label, x = "A\u00f1o") +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position  = "bottom",
      legend.direction = "horizontal",
      panel.grid.minor = ggplot2::element_blank(),
      plot.subtitle    = ggplot2::element_text(color = "grey40")
    )

  p
}

# ── null-coalescing operator (base R doesn't have it) ────────────────────────
`%||%` <- function(x, y) if (!is.null(x)) x else y
