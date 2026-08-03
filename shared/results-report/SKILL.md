---
name: results-report
description: >
  Summarise the numerical results of a fitted regression model and its
  diagnostics into a structured, objective statistical report. Fires when the
  user asks for the model results to be reported, written up, or summarised —
  or when the analysis pipeline is ready to move from computation to output.
  Does NOT interpret the ecology; it only describes what the model says.
---

# Results Report — Structured statistical summary of a fitted model

## Purpose

Transforms the validated output of a fitting skill (e.g. `reef-drivers-lm`)
and, optionally, `model-diagnostics` into a structured list of statistical
results ready for rendering. Produces content (numbers, flags, provenance);
Stage [5] of ORCHESTRATION.md produces format (markdown, DOCX, figures).
These two are sequential, not redundant.

This skill is reusable across fitting methods — any fitting skill whose output
follows the declared minimal interface can be passed as `model_output`.

## Data contract (minimal interface, NOT the local file)

### Required: `model_output`
Full output list from a fitting skill — must include `value`, `method`,
`params`. Required fields in `model_output$value`:

- `coefficients` — data.frame with columns:
  `predictor` (chr), `estimate` (num), `std_error` (num),
  `t_value` (num), `p_value` (num)
- `r_squared` (num)
- `r_squared_adj` (num)
- `f_statistic` (num)
- `f_p_value` (num)
- `vif` — data.frame with columns: `predictor` (chr), `vif` (num);
  may be an empty data.frame if VIF was not computed
- `fitted` — data.frame with columns:
  `year` (int), `nrsi_obs` (num), `nrsi_fit` (num), `residual` (num)
- `n_years_used` (int)
- `years_excluded` (int vector, may be `integer(0)`)
- `formula_used` (chr)

### Optional: `diagnostics_output`
Full output list from `model-diagnostics`. If provided, the
`diagnostics_flags` section is populated. If `NULL`, that section
returns `NULL`.

### `sections`
Character vector controlling which sections appear in output.
Default: all four sections.
Options: `"coefficients"`, `"fit_summary"`, `"data_summary"`,
`"diagnostics_flags"`.
All requested sections are always computed (no lazy evaluation).

## Method (fixed, no degrees of freedom)

### coefficients
Reproduces the fitting skill's coefficient table and appends 95%
confidence intervals:
```
ci_lower_95 = estimate − qt(0.975, df_residual) × std_error
ci_upper_95 = estimate + qt(0.975, df_residual) × std_error
```
where `df_residual = n_years_used − number_of_coefficients`
(intercept counts as one coefficient).
Intercept is included as a row.

### fit_summary
Scalars extracted or derived from `model_output$value`:

| Field | Source |
|-------|--------|
| `r_squared` | extracted |
| `r_squared_adj` | extracted |
| `f_statistic` | extracted |
| `f_p_value` | extracted |
| `aic` | computed — see formula below |
| `bic` | computed — see formula below |
| `rmse` | `sqrt(mean(residual²))` |
| `n_obs` | `n_years_used` |
| `df_residual` | `n_years_used − n_coefficients` |

**AIC / BIC formula (matches R's `stats::AIC` for `lm` objects):**

Let `RSS = sum(residual²)`, `n = n_years_used`,
`k = n_coefficients + 1` (regression coefficients + estimated σ).

```
neg2loglik = n + n × log(2π × RSS/n)
AIC        = neg2loglik + 2 × k
BIC        = neg2loglik + log(n) × k
```

This is the Gaussian MLE log-likelihood formula used internally by R.

### data_summary
Structured provenance fields from the fitting skill:
- `formula_used` (chr)
- `n_years_used` (int)
- `years_excluded` (int vector)
- `vif_table` — the fitting skill's VIF data.frame with an added
  `flag_high` (logical) column: `TRUE` if VIF > 5.

### diagnostics_flags
Populated only when `diagnostics_output` is not `NULL`.
Flags are observations from `model-diagnostics`, not conclusions:

| Field | Rule |
|-------|------|
| `autocorrelation_flag` | `TRUE` if DW < 1.5 or DW > 2.5 (heuristic thresholds) |
| `dw_value` | raw DW statistic from `model-diagnostics` |
| `heteroscedasticity_flag` | `TRUE` if White-type test p-value < 0.05 |
| `hetero_p_value` | raw p-value from `model-diagnostics` |
| `high_vif_predictors` | chr vector of predictors with VIF > 5 (from `model_output$value$vif`) |
| `note` | fixed disclosure string explaining each flag's basis |

If `diagnostics_output` is `NULL`, this section is `NULL` (not an error).

## Random controls
Not applicable — deterministic skill. No statistical tests are run here;
all quantities are extracted or computed algebraically from the fitting
and diagnostics outputs.

## Reference value and tolerance
- Reference case: PENDING — full report object for Cabo Pulmo, to be
  verified by Edu/Fabio against a direct analysis in R.
- Tolerance: PENDING.
- Status: PENDING. Stored in `references/cabo_pulmo_report_reference.json`.

## Do-not rules
- Do NOT interpret coefficients ecologically — no statements about why
  a variable is positive or negative, or what it means for the ecosystem.
- Do NOT conclude model adequacy from diagnostic flags — flags are
  observations, not verdicts.
- Do NOT add information not present in the fitting or diagnostics output.
- Do NOT round beyond 4 significant figures on any reported statistic.
- Do NOT omit `diagnostics_flags = NULL` when `diagnostics_output` is
  not provided — the absence must be explicit.
- Do NOT silently skip a requested section — either compute it or stop
  with an informative error.

## Validation checklist
- [ ] self-consistency: run N times on fixed inputs, outputs match exactly
      (deterministic — tolerance = 0).
- [ ] reference: output matches `references/cabo_pulmo_report_reference.json`
      within tolerance. PENDING → SKIP with disclosure.
- [ ] coherence: output declares `method` and `params` matching this contract;
      `params$formula_used` matches `model_output$value$formula_used`.

## Success criteria
A complete results report includes:
- `coefficients`: data.frame with one row per predictor (including intercept),
  columns `predictor`, `estimate`, `std_error`, `t_value`, `p_value`,
  `ci_lower_95`, `ci_upper_95`.
- `fit_summary`: named list with all 9 scalars listed in the Method section.
- `data_summary`: named list with `formula_used`, `n_years_used`,
  `years_excluded`, `vif_table` (with `flag_high` column).
- `diagnostics_flags`: named list with all 6 fields listed above, or `NULL`
  if `diagnostics_output` was not provided — the absence disclosed in output.
