# chatmpa-skills

Analysis **skills** for ChatMPA Studio (CBMC) — the MCP + Skills pipeline for
analyzing marine protected areas in the Gulf of California. Every skill in this
repo is written in **R**.

The **data does not live here.** Skills read it from a local staging copy during
development (`data/`, git-ignored) and from MCP servers in production. A skill
reads a *minimal data contract*, never a named local file.

## What a skill is

A skill is a **contract**, not a tutorial. It improves output by **removing
degrees of freedom**, not by adding words. This standard is the admission
criterion for the repo; it comes from our reproducibility manuscript
(*Skills as the new packages*, MEE).

Every skill fixes:

- **Data contract** — the minimal input interface (columns `lat`, `lon`, `time`,
  `value`, or whichever apply), the missing-data rule, and a fixed aggregation
  unit.
- **Method** — the exact formula/test/model with its documented default
  parameters. No optional knobs.
- **Random controls** — a fixed seed for any bootstrap/split/resample, or an
  explicit "Not applicable (deterministic skill)".
- **Reference value + tolerance** — a known input → a **human-verified**
  expected value, within a stated tolerance. Never invented.
- **Do-not rules** — the known traps and how to avoid them.
- **The 3 checks** — self-consistency, reference, coherence (see
  `shared/validation/`).

## Two cross-cutting disciplines (mandatory in every skill)

1. **Minimal data contract.** Read against columns (`lat`, `lon`, `time`,
   `value`), never against a named local file. The skill consumes an interface,
   so the same skill works on the staging copy and on MCP output unchanged.
2. **Trigger-oriented `description`.** The YAML `description` describes *which
   natural-language question fires the skill*, so a model can pick it blind.
   Good: "Assess the trophic health of a reef and answer whether it is healthy,
   degraded, or recovering." Bad: "Compute the NRSI."

## The 3 checks

Every skill is validated by `shared/validation/run_checks.R` before release:

1. **self-consistency** — N runs on fixed data match within tolerance (catches a
   leaky seed).
2. **reference** — output matches a **human-verified** reference within
   tolerance (a `PENDING` reference yields `SKIP`, never a silent pass).
3. **coherence** — the output declares its methods and they match `SKILL.md`.

See `shared/validation/README.md` for the `skill.R` signature the harness calls.

## Repository layout

```
per-database/      one skill per source metric (LTEM, SST, chl-a, CONAPESCA …)
  _TEMPLATE/       copy this to start a new per-database skill
  ltem-nrsi-index/ first fully-filled example — the visual gold standard
interdatabase/     skills that combine sources; output/report skills
  _TEMPLATE/       copy this to start a new interdatabase skill
shared/            cross-cutting helpers + the validation harness
  coverage/  spatial_join/  temporal_align/   (stubs for now)
  validation/      the 3-check harness
data/              local staging copy — GIT-IGNORED, never committed
```

## Who builds what

- **Carolina** builds **per-database** skills (biomass, CPUE, anomalies …),
  copying the `ltem-nrsi-index` shape.
- **Edu** builds **interdatabase** skills and **output** (report) skills.
- The **shared layer** and the **verification** harness are **joint** work.

## Sensitive data — CONAPESCA

The CONAPESCA database contains **economic-unit names, RNP identifiers, and
individual-level catch records**. This data must **never** enter version control
or any public output. The `.gitignore` (which excludes all of `data/` and the
common data formats) is the first barrier; `data/` is git-ignored by design.
Treat any derived output that could re-identify an economic unit as sensitive
too.

## Create a new skill

1. Copy the matching template:
   `cp -r per-database/_TEMPLATE per-database/<source-metric>`
   (or `interdatabase/_TEMPLATE`).
2. Fill in `SKILL.md` first (the contract), then `skill.R`.
3. Add at least one reference case in `references/` (`status: PENDING` until a
   human verifies the value).
4. Run the checks:
   `Rscript -e 'source("shared/validation/run_checks.R"); run_all_checks("per-database/<source-metric>")'`

See `CONVENTIONS.md` for naming and versioning rules.
