# run_checks.R
# Shared validation harness. Every skill is validated with these 3 functions
# before it is considered ready for release.
#
# This phase: functional stubs with the structure of the 3 checks. Internal
# logic that depends on a concrete skill is marked TODO. The check contracts,
# return shapes, and the PASS/FAIL/SKIP semantics are already fixed here so no
# skill has to reinvent them.

# ---------------------------------------------------------------------------
# Result helpers — a uniform verdict object for every check.
# status is one of "PASS", "FAIL", "SKIP".
# ---------------------------------------------------------------------------

.check_result <- function(name, status, detail = "") {
  list(check = name, status = status, detail = detail)
}

# ---------------------------------------------------------------------------
# check_self_consistency(skill_fn, data, n = 10, tolerance)
#   Runs skill_fn(data) n times. Returns a result with status PASS if all
#   outputs match within tolerance, FAIL otherwise. Detects uncontrolled
#   stochasticity (loose seed): a correctly-seeded skill is identical across
#   runs; a leaky one drifts.
# ---------------------------------------------------------------------------

check_self_consistency <- function(skill_fn, data, n = 10, tolerance) {
  # TODO: call skill_fn(data) n times, extract the numeric `value` from each
  #       output, and compare against the first run within `tolerance`.
  #       Depends on a real skill's output shape (list(value=, method=, params=)).
  #
  # values <- vapply(seq_len(n), function(i) skill_fn(data)$value, numeric(1))
  # spread <- max(values) - min(values)
  # if (spread <= tolerance) PASS else FAIL with the observed spread.

  .check_result(
    "self-consistency",
    "SKIP",
    "TODO: stub — needs a concrete skill_fn to run n times."
  )
}

# ---------------------------------------------------------------------------
# check_reference(skill_fn, reference_case, tolerance)
#   Runs skill_fn on reference_case$input. Returns PASS if the output matches
#   reference_case$expected_value within tolerance.
#   If reference_case$status == "PENDING", returns SKIP with a warning — it
#   does NOT fail silently and does NOT treat a missing reference as a pass.
# ---------------------------------------------------------------------------

check_reference <- function(skill_fn, reference_case, tolerance) {
  if (is.null(reference_case$status) || reference_case$status == "PENDING") {
    warning(
      "Reference case '", reference_case$id %||% "?",
      "' is PENDING — no human-verified expected_value. Skipping (NOT a pass)."
    )
    return(.check_result(
      "reference",
      "SKIP",
      "reference_case status is PENDING — needs a value verified by a human (Edu/Fabio)."
    ))
  }

  # TODO: run skill_fn on reference_case$input and compare $value against
  #       reference_case$expected_value within `tolerance`.
  #       Depends on a real skill and a VERIFIED reference case.
  #
  # out <- skill_fn(reference_case$input)
  # if (abs(out$value - reference_case$expected_value) <= tolerance) PASS else FAIL

  .check_result(
    "reference",
    "SKIP",
    "TODO: stub — needs a concrete skill_fn and a VERIFIED reference case."
  )
}

# ---------------------------------------------------------------------------
# check_coherence(skill_output, skill_contract)
#   Verifies the output declares which methods it used and that they match the
#   ones the SKILL.md specifies. In this phase: a stub that checks for the
#   presence of method metadata in the output.
# ---------------------------------------------------------------------------

check_coherence <- function(skill_output, skill_contract) {
  # Minimal, already-functional part: the output must carry method metadata.
  has_method <- is.list(skill_output) &&
    !is.null(skill_output$method) &&
    nzchar(as.character(skill_output$method))

  if (!has_method) {
    return(.check_result(
      "coherence",
      "FAIL",
      "Output declares no `method` metadata — cannot check it against the SKILL.md contract."
    ))
  }

  # TODO: compare skill_output$method / $params against the methods and
  #       documented defaults declared in skill_contract (parsed SKILL.md).
  .check_result(
    "coherence",
    "SKIP",
    "Method metadata present; TODO: match it against the parsed SKILL.md contract."
  )
}

# ---------------------------------------------------------------------------
# run_all_checks(skill_dir)
#   Orchestrates the 3. Reads the SKILL.md and references/ of the skill's
#   directory, runs the checks, prints a PASS/FAIL/SKIP report per check.
# ---------------------------------------------------------------------------

run_all_checks <- function(skill_dir) {
  # TODO: source skill_dir/skill.R to obtain the skill's run function,
  #       parse skill_dir/SKILL.md into a contract object, load the reference
  #       cases from skill_dir/references/*.json, and load the fixed test data
  #       (from the minimal data contract, not a named local file).
  #
  # Wiring, once a real skill exists:
  #   skill_fn      <- <sourced from skill.R>
  #   contract      <- parse_skill_md(file.path(skill_dir, "SKILL.md"))
  #   ref_cases     <- load_reference_cases(file.path(skill_dir, "references"))
  #   data          <- <fixed test data on the minimal contract>
  #   results <- list(
  #     check_self_consistency(skill_fn, data, n = 10, tolerance = contract$tolerance),
  #     check_reference(skill_fn, ref_cases[[1]], tolerance = contract$tolerance),
  #     check_coherence(skill_fn(data), contract)
  #   )

  message("run_all_checks('", skill_dir, "') — scaffold stub.")
  results <- list(
    .check_result("self-consistency", "SKIP", "TODO: wire skill_fn + data."),
    .check_result("reference",        "SKIP", "TODO: wire reference cases."),
    .check_result("coherence",        "SKIP", "TODO: wire parsed contract.")
  )

  print_report(results)
  invisible(results)
}

# ---------------------------------------------------------------------------
# print_report(results) — uniform PASS/FAIL/SKIP report, one line per check.
# ---------------------------------------------------------------------------

print_report <- function(results) {
  for (r in results) {
    message(sprintf("[%-4s] %-16s %s", r$status, r$check, r$detail))
  }
  statuses <- vapply(results, function(r) r$status, character(1))
  message(sprintf(
    "Summary: %d PASS / %d FAIL / %d SKIP",
    sum(statuses == "PASS"), sum(statuses == "FAIL"), sum(statuses == "SKIP")
  ))
  invisible(results)
}

# Small null-coalescing helper (used above; base R has no %||%).
`%||%` <- function(a, b) if (is.null(a)) b else a
