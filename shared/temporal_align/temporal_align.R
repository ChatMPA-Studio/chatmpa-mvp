# temporal_align.R — shared temporal-alignment helper
#
# Aligns a named list of annual data.frames to a common year axis via full
# outer join, and reports coverage per source.
#
# CONTRACT:
#   - Each data.frame in data_list MUST have exactly one column named "year"
#     (integer or numeric). The caller is responsible for renaming if needed
#     (e.g. anio_corte → year, time → year).
#   - Each data.frame MUST have exactly ONE row per year. If duplicates are
#     found, an error is raised — aggregate before calling this function.
#   - All non-"year" columns are treated as values to carry into the wide table.
#   - If two sources share a column name, it is disambiguated by prefixing the
#     source name: e.g. "cobertura_pct" from "sst" becomes "sst.cobertura_pct".
#
# Returns: list(
#   aligned        — data.frame, one row per year (full outer join), all sources
#   coverage       — data.frame, one row per year × source, with has_data (logical)
#   complete_years — integer vector of years where ALL sources have data
#   years_excluded — named list: year → character vector of sources missing data
# )

temporal_align <- function(data_list) {

  # ── Input validation ─────────────────────────────────────────────────────────

  if (!is.list(data_list) || is.data.frame(data_list))
    stop("'data_list' must be a named list of data.frames.")
  if (is.null(names(data_list)) || any(!nzchar(names(data_list))))
    stop("All elements of 'data_list' must have non-empty names.")
  if (length(data_list) < 2)
    stop("'data_list' must contain at least 2 data.frames to align.")

  for (src in names(data_list)) {
    df <- data_list[[src]]
    if (!is.data.frame(df))
      stop("Element '", src, "' is not a data.frame.")
    if (!"year" %in% names(df))
      stop("Element '", src, "' has no column named 'year'. ",
           "Rename the year column (e.g. anio_corte, time) to 'year' before calling.")
    if (any(duplicated(df$year)))
      stop("Element '", src, "' has duplicate year values: ",
           paste(df$year[duplicated(df$year)], collapse = ", "), ". ",
           "Aggregate to one row per year before calling temporal_align().")
    if (!is.numeric(df$year) && !is.integer(df$year))
      stop("Column 'year' in '", src, "' must be numeric or integer.")
  }

  # ── Disambiguate overlapping column names ────────────────────────────────────
  # Prefix value columns with source name if the same name appears in >1 source.

  all_value_cols <- unlist(lapply(names(data_list), function(src) {
    setdiff(names(data_list[[src]]), "year")
  }))
  duplicated_cols <- unique(all_value_cols[duplicated(all_value_cols)])

  src_names <- names(data_list)
  data_list <- setNames(
    lapply(src_names, function(src) {
      df <- data_list[[src]]
      value_cols <- setdiff(names(df), "year")
      cols_to_prefix <- intersect(value_cols, duplicated_cols)
      if (length(cols_to_prefix) > 0) {
        names(df)[names(df) %in% cols_to_prefix] <-
          paste0(src, ".", cols_to_prefix)
      }
      df
    }),
    src_names
  )

  # ── Full outer join on year ──────────────────────────────────────────────────

  aligned <- Reduce(
    function(a, b) merge(a, b, by = "year", all = TRUE),
    data_list
  )
  aligned <- aligned[order(aligned$year), ]
  rownames(aligned) <- NULL

  # ── Coverage report ──────────────────────────────────────────────────────────
  # One row per year × source: has_data = TRUE if source has a row for that year

  all_years <- sort(unique(aligned$year))

  coverage <- do.call(rbind, lapply(src_names, function(src) {
    src_years <- data_list[[src]]$year
    data.frame(
      year     = all_years,
      source   = src,
      has_data = all_years %in% src_years,
      stringsAsFactors = FALSE
    )
  }))
  rownames(coverage) <- NULL

  # ── Complete years and excluded years ────────────────────────────────────────

  coverage_wide <- tapply(coverage$has_data, list(coverage$year, coverage$source), identity)
  complete_mask <- apply(coverage_wide, 1, all)
  complete_years <- as.integer(rownames(coverage_wide)[complete_mask])

  years_excluded <- lapply(all_years[!complete_mask], function(yr) {
    row <- coverage[coverage$year == yr, ]
    row$source[!row$has_data]
  })
  names(years_excluded) <- as.character(all_years[!complete_mask])
  # Drop empty entries (shouldn't exist, but be safe)
  years_excluded <- years_excluded[sapply(years_excluded, length) > 0]

  # ── Return ───────────────────────────────────────────────────────────────────

  list(
    aligned        = aligned,
    coverage       = coverage,
    complete_years = complete_years,
    years_excluded = years_excluded
  )
}
