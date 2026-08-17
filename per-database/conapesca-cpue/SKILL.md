---
name: conapesca-cpue
version: 0.1.0
tier: 1
description: >
  Estimate fishing pressure on a target species from CONAPESCA landing records,
  by computing CPUE (catch per unit effort) as a time series at regional and
  local scales. Fires on questions about how fishing pressure has changed over
  time for a given species in a given state, how pressure near a marine protected
  area compares with the state-level trend, or which fleet type (industrial vs.
  artisanal) drives landings.
inputs:
  species:
    type: string
    required: true
    description: >
      Nombre científico canónico (`nombre_cientifico_canonico`), tal cual está
      en la base de CONAPESCA — p. ej. "Lutjanus peru". No acepta nombres
      comunes: "pargo" tiene que resolverse antes de llegar acá.
  state_filter:
    type: string
    required: true
    description: >
      Estado de la oficina de desembarque (`nombre_estado`), p. ej.
      "BAJA CALIFORNIA SUR". Define la escala regional, y es obligatorio
      también cuando se pide la local.
  office_filter:
    type: string
    required: false
    description: >
      Oficina de desembarque (`nombre_oficina`) dentro de `state_filter`,
      p. ej. "CABO SAN LUCAS". Si viene, se agrega la serie de escala local.
      Nunca va sin `state_filter`: los nombres de oficina no son únicos entre
      estados (EL ROSARIO existe en Baja California y en Sinaloa).
acquire:
  # El MCP de CONAPESCA ya devuelve este contrato completo y sin tope de filas
  # vía get_landings(group_by="folio"), así que el orquestador trae la tabla y
  # la manda en el body. No hay razón para que el servicio la baje de nuevo.
  - source: payload
    as: data
    columns:
      - folio_aviso
      - anio_corte
      - tipo_aviso
      - nombre_estado
      - nombre_oficina
      - nombre_cientifico_canonico
      - peso_desembarcado_kg
      - dias_efectivos
      - dias_efectivos_fuente
      - flag_fecha_generica
      - flag_dias_efectivos_sospechoso
      - flag_periodo_futuro
# Sin `output.table`: run_skill() devuelve un solo data.frame, no una lista de
# tablas, y ahí no hay nada que elegir.
#
# Skill determinista (sin controles aleatorios), así que las corridas deben
# coincidir exactamente. Se comparan las dos columnas calculadas: `cpue_media`
# sola dejaría pasar un cambio que solo afecte la dispersión.
comparable_value: [cpue_media, cpue_sd]
reference: references/cabo_pulmo_cpue_reference.json
# Con qué corre el ARNÉS contra el fixture. No son defaults del API: `species`
# y `state_filter` siguen siendo obligatorios en toda llamada real.
validation:
  params:
    species: Lutjanus peru
    state_filter: BAJA CALIFORNIA SUR
    office_filter: CABO SAN LUCAS
depends_on: []
---

# CONAPESCA CPUE — Índice de presión pesquera

## Purpose
Answers how fishing pressure on a target species has evolved over time, at
regional scale (all records for a given state) and at local scale (proxy for a
specific MPA via the nearest landing office within that state). CPUE (kg per
effective fishing day) is the proxy for fishing pressure and is computed
separately for industrial (MAYORES) and artisanal (MENORES) fleets.

The skill works for any state in the CONAPESCA database. The suggested default
context is Baja California Sur (AMPs: Cabo Pulmo, Loreto, Isla Espíritu Santo),
but `state_filter` and `office_filter` are fully user-specified.

## Parameters (required at call time)

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `species` | character | **Yes** | Canonical scientific name (`nombre_cientifico_canonico`). Must match the database exactly. |
| `state_filter` | character | **Yes** | State name (`nombre_estado`). Defines the regional scope. Required for both regional and local scale. |
| `office_filter` | character | No | Landing office name (`nombre_oficina`) within `state_filter`. If provided, adds a local-scale series. If absent, only regional series is returned. |

### Suggested defaults — MPA → state + office mapping
```
Cabo Pulmo          → estado: BAJA CALIFORNIA SUR  · oficina: CABO SAN LUCAS
Loreto              → estado: BAJA CALIFORNIA SUR  · oficina: LORETO
Isla Espíritu Santo → estado: BAJA CALIFORNIA SUR  · oficina: LA PAZ
```
The orchestrator presents these as suggestions when the user names an MPA but
does not specify a state or office. The user confirms or overrides in Stage 0.

## Data contract (minimal interface, NOT the local file)

Input columns required from the CONAPESCA database per record:

| Column | Type | Description |
|--------|------|-------------|
| `folio_aviso` | character | Trip/notice identifier (aggregation unit) |
| `anio_corte` | integer | Landing year |
| `tipo_aviso` | character | Fleet type: `MAYORES` or `MENORES` |
| `nombre_estado` | character | State of the landing office |
| `nombre_oficina` | character | Landing office (for local scale) |
| `nombre_cientifico_canonico` | character | Canonical species name |
| `peso_desembarcado_kg` | numeric | Landed weight in kg |
| `dias_efectivos` | integer | Effective fishing days (quality-controlled) |
| `dias_efectivos_fuente` | character | Source of effort value: `original`, `duracion`, `recomputado` |
| `flag_fecha_generica` | logical | TRUE = generic date placeholder detected; effort unreliable |
| `flag_dias_efectivos_sospechoso` | logical | TRUE = `dias_efectivos` likely derived from generic dates |
| `flag_periodo_futuro` | logical | TRUE = a period date is after `fecha_aviso`; informational only |

**Missing-data rule:**
- Records with `flag_fecha_generica = TRUE` or `flag_dias_efectivos_sospechoso = TRUE`
  or `is.na(dias_efectivos)` are **excluded** from CPUE computation.
- Records with `flag_periodo_futuro = TRUE` are **included** (effort may still be valid).
- Records with `dias_efectivos_fuente = "recomputado"` are **included** but counted
  separately in `n_viajes_recomputado` — their effort was recovered from date
  fields rather than directly captured, so treat with caution.
- If a folio has zero `peso_desembarcado_kg` for the target species after filtering,
  it is excluded (the species was not landed on that trip).

**Fleet scope:**
- Include: `MAYORES`, `MENORES`.
- Exclude: `COSECHA` (aquaculture production — not fishing effort).
- MAYORES and MENORES are always computed and reported **separately**.
  Their CPUEs are not comparable (different vessel size, effort scale, reporting unit).

**Aggregation unit:** `folio_aviso` (one fishing trip / notice). Fixed, not optional.

## Method (fixed, no degrees of freedom)

CPUE is computed as **mean of ratios** (not ratio of means) to treat each trip
as an independent observation and avoid large trips dominating the index.

### Step-by-step

```
1. FILTER
   nombre_estado  = <state_filter>
   AND tipo_aviso IN ('MAYORES', 'MENORES')
   AND flag_fecha_generica            = FALSE
   AND flag_dias_efectivos_sospechoso = FALSE
   AND NOT is.na(dias_efectivos)
   AND nombre_cientifico_canonico     = <species>
   AND peso_desembarcado_kg           > 0

2. AGGREGATE BY FOLIO
   For each folio_aviso:
     catch_folio  ← sum(peso_desembarcado_kg)   [kg landed for target species]
     effort_folio ← dias_efectivos               [already folio-level, no sum]
     cpue_folio   ← catch_folio / effort_folio   [kg / day]

3. REGIONAL SERIES (escala = "regional")
   All folios passing step 1 (full state)
   Group by: anio_corte, tipo_aviso
   cpue_media ← mean(cpue_folio)
   cpue_sd    ← sd(cpue_folio)
   n_viajes   ← count of folios included

4. LOCAL SERIES (escala = "local", only when office_filter is provided)
   Further filter: nombre_oficina = <office_filter>
   Group by: anio_corte, tipo_aviso
   Same aggregation as step 3.

5. COUNT EXCLUDED TRIPS (for transparency)
   n_viajes_excluidos ← count of folios for the species in the state that were
                        removed by the quality filters in step 1.
```

### Output structure

```
anio_corte | tipo_aviso | escala | nombre_cientifico_canonico |
cpue_media | cpue_sd | n_viajes | n_viajes_excluidos |
peso_desembarcado_kg_total | n_viajes_recomputado
```

- `escala`: `"regional"` (full state) or `"local"` (filtered by `office_filter`)
- `peso_desembarcado_kg_total`: total landed weight for context (not normalized)
- `n_viajes_recomputado`: trips where `dias_efectivos_fuente = "recomputado"`

## Random controls
Not applicable (deterministic skill).

## Reference value and tolerance
- Reference case: PENDING — needs a species + state + office + year combination
  with a hand-verified CPUE value.
- Tolerance: PENDING.
- Status: PENDING: needs a hand-verified value before this check can pass.
  Do NOT invent one. Store in `references/` with `status: PENDING` when available.

## Do-not rules
- Do NOT sum `dias_efectivos` across species rows of the same folio — effort is
  already at folio level. Summing produces inflated denominators.
- Do NOT mix MAYORES and MENORES into a single CPUE series — their effort units
  are not comparable.
- Do NOT include COSECHA records — they represent aquaculture production, not
  fishing effort.
- Do NOT use records with `flag_fecha_generica = TRUE` even if `dias_efectivos`
  looks reasonable — both the effort and duration fields are unreliable for
  these records.
- Do NOT run `office_filter` without `state_filter` — office names are not unique
  across states (e.g. EL ROSARIO exists in both Baja California and Sinaloa).
- Do NOT report a CPUE series with fewer than 5 trips per year-fleet cell
  without flagging it explicitly — small n makes the mean unreliable.
- Do NOT interpret local CPUE (by office) as spatially precise — landing offices
  record where fish were landed, not where they were caught.

## Validation checklist
- [ ] self-consistency: run twice on fixed data, outputs match exactly.
- [ ] reference: output matches the `references/` value within tolerance.
- [ ] coherence: mean-of-ratios formula used, not ratio-of-means.
- [ ] MAYORES and MENORES reported separately in all outputs.
- [ ] `n_viajes_excluidos` is present and non-zero for real data.
- [ ] No COSECHA records in filtered input.
- [ ] `office_filter` never used without `state_filter`.

## Success criteria
A complete CPUE analysis with this skill must include:
- Regional CPUE time series (MAYORES + MENORES separate) for the target species
  and state.
- Local CPUE time series for the specified office (if provided), with explicit
  note that office ≠ fishing ground.
- `n_viajes` and `n_viajes_excluidos` reported per year-fleet cell.
- Years with n < 5 trips flagged in the narrative.
- A brief comparison between regional and local trends (convergent / divergent).

---

## Planned scale architecture — PENDING: gradilla costera + base limpia

> Este bloque documenta la arquitectura objetivo del MVP. No modifica el
> contrato actual. Implementar cuando la gradilla y los puertos de desembarque
> georreferenciados estén disponibles.

### Cambio de escala local
- **Actual**: `office_filter` (nombre_oficina) como proxy del AMP
- **Planeado**: sitios de desembarque georreferenciados asociados a celdas de
  la gradilla donde `nombre_amp == <amp_name>`
- Requiere: puertos de desembarque limpios y georreferenciados en los folios CONAPESCA
- Supuesto mantenido: puerto de desembarque ≈ puerto de salida para flota artesanal

### Cambio de escala regional
- **Actual**: `state_filter` (nombre_estado) — estado completo
- **Planeado**: `region_id` de `conapesca-lfo-regions` — región de manejo pesquero
- Mecánica: folios se asignan a región vía `nombre_oficina` → lookup de lfo-regions
- Valor regional = promedio de CPUE de todos los folios cuya oficina pertenece a `region_id`
- Supuesto: no hay traslape de localidades entre oficinas dentro de una misma región

### Implicación para el output
- El campo `escala` pasará de `"regional"/"local"` a `"region_manejo"/"amp_local"`
  para distinguir claramente del esquema anterior
- AMPs en la misma región de manejo (ej. Loreto y Espíritu Santo si coinciden)
  tendrán el mismo valor regional por construcción — documentar explícitamente

### Lo que NO cambia
- Fórmula CPUE: mean-of-ratios por folio — idéntica
- Separación MAYORES/MENORES
- Reglas de exclusión por flags de calidad

### Dependencias para implementar
- [ ] Gradilla costera disponible (sf con `nombre_amp`, `region_id`, `nombre_oficina`)
- [ ] Puertos de desembarque georreferenciados en la base CONAPESCA
- [ ] `conapesca-lfo-regions` ejecutado (lookup oficina → region_id)
- [ ] Parámetro `region_filter` agregado como alternativa a `state_filter`
