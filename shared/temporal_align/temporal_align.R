# temporal_align.R — shared temporal-alignment helper (STUB, no logic yet)
#
# Purpose (once implemented): align a table on the minimal data contract
# (lat, lon, time, value) to a fixed temporal unit (year, month, cell-month)
# so downstream skills aggregate on a fixed, non-optional time step.
#
# Reads the minimal data contract, NOT a named local file.

temporal_align <- function(data, unit = c("year", "month"), ...) {
  # TODO: implement temporal alignment. Scaffold only — no logic in this phase.
  stop("temporal_align() not implemented — scaffold stub.")
}
