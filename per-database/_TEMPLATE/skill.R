# skill.R — per-database skill entry point (TEMPLATE)
#
# Every skill.R must expose a single top-level function `run_skill(data, ...)`
# that the validation harness (shared/validation/run_checks.R) can call.
#
# Contract with the harness:
#   - Input `data` is the MINIMAL DATA CONTRACT (a data.frame with the columns
#     declared in SKILL.md, e.g. lat, lon, time, value). NEVER read a named
#     local file here.
#   - Output is a list that MUST carry method metadata so check_coherence()
#     can confirm the methods used match those declared in SKILL.md:
#       list(
#         value  = <the computed result>,
#         method = "<name/formula matching SKILL.md 'Method' section>",
#         params = list(...)   # documented defaults actually used
#       )
#   - Any stochastic step MUST set the seed declared in SKILL.md.

run_skill <- function(data, ...) {
  # TODO: implement the method fixed in SKILL.md. No degrees of freedom.
  #       This is a scaffold stub — no analysis logic in this phase.
  stop("run_skill() not implemented — this is a template stub.")

  # Expected return shape (uncomment and fill when implementing):
  # list(
  #   value  = NA,
  #   method = "TODO: name from SKILL.md",
  #   params = list()
  # )
}
