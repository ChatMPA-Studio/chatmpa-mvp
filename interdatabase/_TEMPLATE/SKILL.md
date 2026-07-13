---
name: skill-name
description: >
  [Trigger-oriented. Describe WHICH user question fires this skill, in natural
  language, not just the technical name. Good example: "Assess the trophic health
  of a reef and answer whether it is healthy, degraded, or recovering." Bad
  example: "Compute the NRSI." The model selects the skill by reading this blind.]
---

# [Human-readable skill name]

## Purpose
[One or two sentences. What scientific question it answers.]

## Data contract (minimal interface, NOT the local file)
- Input: a table with columns [lat, lon, time, value] or [whichever apply].
  The skill reads this interface, not a file with a fixed name.
- Missing-data rule: [what happens with NA. Exclude / impute / flag. Be explicit.]
- Aggregation unit: [transect / reef-year / cell-month. Fixed, not optional.]

## Method (fixed, no degrees of freedom)
- [Exact formula / test / model, with the bibliographic reference if any.]
- [Parameters with their documented default value.]

## Random controls
- Seed: [fixed value, e.g. 42]. Mandatory if there is bootstrap, split, or resampling.
- [Scaling / split ratio if any.]
- If there is NO stochasticity, write: "Not applicable (deterministic skill)."

## Reference value and tolerance
- Reference case: [known input -> expected value]. Stored in references/.
- Tolerance: [acceptable range, e.g. +/- 0.01].
- If no verified reference exists yet: write "PENDING: needs a value verified by Edu/Fabio." Do NOT invent one.

## Do-not rules
- [Known trap 1 and how to avoid it.]
- [Known trap 2.]

## Validation checklist
- [ ] self-consistency: run N times on fixed data, outputs match within tolerance.
- [ ] reference: output matches the references/ value within tolerance.
- [ ] coherence: the output uses the methods this contract specifies.

## Success criteria
[What a complete analysis with this skill must contain to be considered done.]
