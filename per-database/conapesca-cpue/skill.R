# skill.R — conapesca-cpue
#
# Computes CPUE (kg / effective fishing day) as a fishing pressure index
# from CONAPESCA landing records for a target species in Baja California Sur.
#
# See SKILL.md for the full data contract and method specification.

run_skill <- function(data, species, state_filter, office_filter = NULL, ...) {
  # TODO: implement the method fixed in SKILL.md. No degrees of freedom.
  #       This is a scaffold stub — no analysis logic in this phase.
  stop("run_skill() not implemented — this is a scaffold stub.")

  # Expected return shape (uncomment and fill when implementing):
  # list(
  #   value  = <data.frame with output structure from SKILL.md>,
  #   method = "mean-of-ratios CPUE: mean(sum_kg_folio / dias_efectivos_folio)",
  #   params = list(
  #     species       = species,
  #     state_filter  = state_filter,
  #     office_filter = office_filter,
  #     flotas        = c("MAYORES", "MENORES")
  #   )
  # )
}
