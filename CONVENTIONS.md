# Conventions

Rules every skill in this repo follows. These are not style preferences; they
keep skills selectable, reproducible, and safe to merge.

## Naming

- A skill directory is named **`source-metric`**, lowercase, hyphen-separated.
- Examples: `ltem-nrsi-index`, `conapesca-cpue`, `sst-marine-heatwaves`,
  `chla-anomalia`.
- The `name:` in the skill's YAML front-matter matches the directory name.

## Versioning

- **Semantic versioning per skill.** Each skill carries its own version
  (`skill_version` in its `references/*.json`).
- `0.1.0` while in development.
- `1.0.0` at the first **G3** release (first production-ready release).
- Bump per skill independently; the repo is not versioned as a whole.

## Order of work

- Each skill ships its **`SKILL.md` before its script**, not after. The contract
  is written and agreed first; the code implements the contract. A skill without
  a filled `SKILL.md` is not admissible.

## Reference values

- **No `expected_value` is filled without human verification.** A reference case
  starts `status: PENDING` and only becomes `VERIFIED` when a human (Edu or
  Fabio) confirms the value.
- Never fill an `expected_value` with the skill's own output — that turns the
  reference check into a disguised self-consistency check.

## The two cross-cutting disciplines (restated)

- Read against the **minimal data contract** (`lat`, `lon`, `time`, `value`),
  never a named local file.
- The `description` is **trigger-oriented**: it says which natural-language
  question fires the skill, so a model can pick it blind.
