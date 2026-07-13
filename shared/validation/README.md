# Validation harness

`run_checks.R` is the shared verification layer for **every** skill in this repo.
A skill is not "ready for release" until it passes these three checks. The harness
is deliberately skill-agnostic: it defines the check contracts once so no skill
reinvents what "validated" means.

## The three checks

1. **self-consistency** — run the skill N times on fixed data; all outputs must
   match within tolerance. Catches uncontrolled stochasticity (a leaky seed).
2. **reference** — run the skill on a known input; the output must match a
   **human-verified** `expected_value` within tolerance. A `PENDING` reference
   yields `SKIP` (never a silent pass).
3. **coherence** — the output must declare which methods/params it used, and
   those must match what `SKILL.md` specifies.

Each check returns `PASS`, `FAIL`, or `SKIP`. `run_all_checks(skill_dir)`
orchestrates all three and prints a per-check report plus a summary line.

## How a skill plugs in

Your skill lives in its own directory (e.g. `per-database/ltem-nrsi-index/`) and
must provide:

- `SKILL.md` — the contract (parsed for method, params, tolerance).
- `skill.R` — the code, exposing **one entry-point function** the harness calls.
- `references/*.json` — at least one reference case in the `example.json` schema.

### Required `skill.R` signature

`skill.R` must expose a single top-level function that:

- **takes the minimal data contract** as its first argument — a `data.frame`
  with the columns declared in `SKILL.md` (e.g. `lat`, `lon`, `time`, `value`).
  It must **never** read a named local file.
- **sets the seed** declared in `SKILL.md` before any stochastic step.
- **returns a list** carrying the result *and* method metadata:

```r
run_skill <- function(data, ...) {
  list(
    value  = <computed result>,      # the numeric/tabular output being checked
    method = "<name/formula, matching SKILL.md 'Method'>",
    params = list(...)               # documented defaults actually used
  )
}
```

The `method`/`params` metadata is what `check_coherence()` reads. Without it,
coherence fails by construction.

## Running the checks

```r
source("shared/validation/run_checks.R")
run_all_checks("per-database/ltem-nrsi-index")
```

> Current phase: the internal logic of each check is a **stub** (marked `TODO`).
> The signatures, return shapes, and PASS/FAIL/SKIP semantics are final; the
> per-check computation is wired in when the first real skill is implemented.
