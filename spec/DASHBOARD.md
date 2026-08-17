# chatMPA Dashboard — Display Specification

**Audience:** Front-end engineers building the chatMPA prototype dashboard.  
**Purpose:** Defines exactly what to display in the interface, for each data source, in each panel — without requiring any marine ecology knowledge.

---

## 1. Interface architecture

```
┌─────────────────── Top nav (68px) ──────────────────────────┐
│  chatMPA  [Demo]   Mission  How it works  Innovation …       │
└─────────────────────────────────────────────────────────────┘
┌──────────────── Map (flex: 1) ──────────────────┬── Panel (380px) ──┐
│                                                  │                   │
│   [MPA polygons]   [Prosperity grid]             │  (empty state)    │
│   [Fishing office dots]                          │  ← click anything │
│                                                  │                   │
│   [Layer toggles top-right]                      │  Panel A — MPA    │
│   [Legend bottom-left]                           │  Panel B — Office │
└──────────────────────────────────────────────────┴───────────────────┘
```

Two-column layout. Left: Leaflet map (full remaining height). Right: fixed 380 px scrollable panel.

**Two distinct panel types — triggered by what the user clicks:**

| Click target | Panel shown | Data sources |
|---|---|---|
| MPA polygon | **Panel A — Marine Protected Area** | LTEM + SST + Chl-a |
| Fishing office dot | **Panel B — Fisheries Office** | CONAPESCA + Economic KPIs |

Clicking the map background (no target) resets to the empty state.

---

## 2. Map layers

| Layer | What it shows | Toggle? | Default |
|-------|--------------|---------|---------|
| **MPA polygons** | Protected area boundaries, color-coded by data availability | Yes | ON |
| **Prosperity grid** | 0.05° coastal grid cells (3,236 total); teal inside an MPA, grey outside | Yes | ON |
| **Fishing offices** | Circle markers for 72 artisanal landing offices, colored by CONAPESCA region | Yes | ON |

### MPA polygon styles

| State | Fill | Border | Weight |
|-------|------|--------|--------|
| Has LTEM data | `#1D496B` at 28% opacity | `#0E82A8` | 1.6 px |
| No LTEM data | `#6488AC` at 10% opacity | `#9DB6CE` | 0.8 px |
| Selected (clicked) | `#0B2338` at 55% opacity | `#1E9EC4` | 2.5 px |

### Fishing office styles (by CONAPESCA region)

| Region ID | Name | Color |
|-----------|------|-------|
| 1 | Golfo de California Norte | `#1E9EC4` |
| 2 | Pacífico Baja Norte | `#0B2338` |
| 3 | Golfo de California Sur y BCS | `#21925F` |
| 4 | Pacífico Nayarit-Guerrero | `#C6892A` |
| 6 | Pacífico Central | `#7C5A93` |
| 7 | Pacífico Sur (Oaxaca-Chiapas) | `#CC4C43` |

Circle markers: 6 px radius, color fill at 80% opacity, white border 1 px. Selected: 9 px radius, 100% opacity, white border 2 px.

Region 5 (inland freshwater offices) is excluded from the map.

---

## 3. Panel A — Marine Protected Area

Triggered when the user clicks an **MPA polygon**. Content is stacked top to bottom.

```
┌─────────────────────────────────────────┐
│ [badge] Marine Protected Area           │
│ Name of the MPA                         │  ← panel header
│ Category · LTEM: <region name>          │
├──────────┬──────────┬────────────────────┤
│ BIOMASS  │ INVERTS  │  REEF HEALTH       │  ← KPI row 1 (3 cards)
│ x.xx     │ xx.x     │  x.xx              │
│ T/ha     │ ind./tr. │  NRSI index        │
├──────────┬──────────┴────────────────────┤
│ SST      │ CHL-A                         │  ← KPI row 2 (2 cards)
│ xx.x °C  │ x.xxx mg/m³                  │
├─────────────────────────────────────────┤
│ Fish Biomass — Trophic Structure (T/ha) │  ← stacked area chart
│ [6 functional groups, stacked]          │
├─────────────────────────────────────────┤
│ Marine Environment                      │  ← environment section
│ Marine Heatwaves [bar + line combo]     │
│ Chlorophyll-a [line chart]              │
└─────────────────────────────────────────┘
```

### 3.1 Panel header

| Element | Value |
|---------|-------|
| Badge label | `"Marine Protected Area"` (green, always) |
| Title | MPA name (from shapefile `nombre_amp` field) |
| Subtitle | `{categoria}  ·  LTEM: {ltem_region}` (omit LTEM part if no data) |

---

### 3.2 KPI grid — 5 cards in a 3+2 layout

Each card has: **label** (10px uppercase), **value** (18px bold), **unit** (10px), **sub** (10px smaller note).

#### Card 1 — Fish Biomass
| Field | Content |
|-------|---------|
| Label | `FISH BIOMASS` |
| Value | `{mean_biomass_g_m2}` rounded to 2 decimals |
| Unit | `T / ha` |
| Sub | `Mean · 5-yr surveys · LTEM` |
| Source | `mpa_data[name].biomass.kpi.mean_biomass_g_m2` |
| If no data | Show `—` |

> **T/ha**: tonnes per hectare. Field `mean_biomass_g_m2` is a legacy name — all values are in T/ha. Cabo Pulmo (well-recovered MPA) ≈ 0.09 T/ha. Unprotected reefs typically < 0.01 T/ha.

#### Card 2 — Invertebrates
| Field | Content |
|-------|---------|
| Label | `INVERTEBRATES` |
| Value | Echinoidea `mean_abundance_per_transect`, 1 decimal |
| Unit | `ind. / transect` |
| Sub | `Echinoidea · LTEM` |
| Source | `mpa_data[name].invertebrates.Echinoidea.kpi.mean_abundance_per_transect` |
| If no data | Show `—` |

#### Card 3 — Reef Health (NRSI)
| Field | Content |
|-------|---------|
| Label | `REEF HEALTH` |
| Value | `{nrsi_mean}` rounded to 2 decimals |
| Unit | `NRSI · index` |
| Sub | `−1 (depleted) to +1 (pristine) · LTEM` |
| Color | Green if value > 0, red if < −0.15, grey otherwise |
| Source | `mpa_data[name].nrsi.kpi.nrsi_mean` |
| If no data | Show `—` with tag `coming soon` |

#### Card 4 — SST
| Field | Content |
|-------|---------|
| Label | `SST` |
| Value | `{kpi_mean_sst_c}` rounded to 1 decimal |
| Unit | `°C · OISST` |
| Sub | `{kpi_years} · {kpi_mhw_days_per_yr} MHW days/yr` |
| Source | `mpa_data[name].sst` |
| If outside GoC | Show `—` with tag `GoC only` |

> SST coverage: lon −115.875 to −105.875, lat 22.125 to 31.625 (OISST). Islas Marietas, Huatulco, Revillagigedo fall outside this bbox.

#### Card 5 — Chl-a
| Field | Content |
|-------|---------|
| Label | `CHL-A` |
| Value | `{kpi_mean_chla_mg_m3}` rounded to 3 decimals |
| Unit | `mg/m³ · MODIS` |
| Sub | `{kpi_years} · annual mean` |
| Source | `mpa_data[name].chl` |
| If no data | Show `—` with tag `no data` |

---

### 3.3 Fish Biomass — Trophic Structure chart

**Primary display (when `functional_groups` data exists):**

**Chart type:** Stacked area chart  
**Height:** 190 px

| Element | Detail |
|---------|--------|
| X axis | Year (integer) |
| Y axis | Fish biomass in **T/ha** (label: `T/ha`), stacked |
| Series | 6 trophic functional groups (see table below) |
| Fill | Each series filled with 80% opacity of group color |
| Border | Group color, 0.8 px |
| Tension | 0.3 (slight smoothing) |
| Points | Hidden (`pointRadius: 0`) |
| Legend | Bottom, 9px font, reversed order |
| Stack | `stack: 'biomass'` on both axes |

**Trophic functional groups (stacking order, bottom to top):**

| Group key | Spanish label | Color |
|-----------|--------------|-------|
| `GenPred_solitary` | Depredadores solitarios | `#D73027` |
| `GenPred_schooling` | Depredadores en cardúmenes | `#FC8D59` |
| `EpiBent_schooling` | Omnívoros en cardúmen | `#FEE08B` |
| `Crip_schooling` | Herbívoros en cardúmen | `#91BFDB` |
| `Crip_solitary` | Crípticos solitarios | `#4575B4` |
| `Plank` | Planctívoros | `#313695` |

**Fallback (when `functional_groups` is absent but `biomass` exists):**

Show the GAM trend chart (scatter + smooth line + 95% CI band):

| Element | Detail |
|---------|--------|
| Height | 150 px |
| Scatter | Observed annual means — `#0B2338`, white border, 4 px |
| Trend line | GAM fit — `#1E9EC4`, 2 px |
| CI band | `rgba(30,158,196,0.15)` between `lwr` and `upr` |
| Sub-label | `GAM dev.expl.: {dev_expl_pct}%` in grey (10px) |

---

### 3.4 Marine Environment section

Two charts stacked vertically. Render only when data is available.

#### 3.4.1 Marine Heatwaves (MHW) chart

**Chart type:** Combo — bars (heatwave days) + line (mean SST)  
**Height:** 130 px  
**Shown when:** `mpa_data[name].sst` exists (GoC MPAs only)

| Element | Detail |
|---------|--------|
| Bars | Annual heatwave days/yr — `rgba(204,76,67,0.65)` fill, left Y axis |
| Line | Annual mean SST °C — `#1E9EC4`, 1.5 px, right Y axis |
| X axis | Year (1982–2025) |
| Left Y label | `MHW days` |
| Right Y label | `°C` |
| Legend | Bottom, 9px |
| Note below | `Baseline: 1998–2011 · min 5 consecutive days (Hobday et al. 2016)` |

#### 3.4.2 Chlorophyll-a time series

**Chart type:** Line chart with area fill  
**Height:** 130 px  
**Shown when:** `mpa_data[name].chl` exists

| Element | Detail |
|---------|--------|
| Line | Annual mean Chl-a — `#21925F`, 1.8 px |
| Fill | `rgba(33,146,95,0.12)` under the line |
| X axis | Year (2004–2023) |
| Y label | `mg/m³` |
| Tension | 0.3 |
| Points | 2.5 px radius, `#21925F` |
| Note below | `Monthly composites averaged annually · mg m⁻³` |

### 3.5 MPA panel — no-data state

If `demo_data.mpa_data[mpa_name]` is undefined:
- Show panel header (name + category)
- Show grey text: `"No precomputed data for this MPA. In production, the AI orchestrator runs the skills on demand."`
- Do not render any KPI cards or charts.

---

## 4. Panel B — Fisheries Office

Triggered when the user clicks a **fishing office dot**. Content is stacked top to bottom.

```
┌─────────────────────────────────────────┐
│ [badge] Fisheries Office                │
│ Office name                             │  ← panel header
│ [Region badge] GoC Sur y BCS           │
├─────────────────────────────────────────┤
│ [Species / group selector]              │  ← slider / dropdown
│ ○ All species   ● Pelágicos menores     │
│   Demersales    Escama    Invertebrados │
├──────────────┬──────────────────────────┤
│ ART. CPUE    │ IND. CPUE               │  ← KPI row (2 cards)
│ xxxx         │ xxxx                    │
│ kg/eff. day  │ kg/eff. day             │
├─────────────────────────────────────────┤
│ CPUE Trend 2001–2026                   │  ← dual line chart
│ [Artisanal (green) + Industrial (amber)]│
│ (filtered by selected species/group)    │
├─────────────────────────────────────────┤
│ Economic Indicators  [coming soon]      │  ← economic KPIs
│ [placeholder — Carolina's skills]       │
└─────────────────────────────────────────┘
```

### 4.1 Panel header

| Element | Value |
|---------|-------|
| Badge label | `"Fisheries Office"` (amber, always) |
| Title | Office name (`nombre_oficina_canonico` from `offices` data) |
| Region badge | Pill with colored dot + region name (colors from §2 table) |

---

### 4.2 Species / group selector

A horizontal pill-toggle or slim dropdown rendered between the header and the KPI cards. Changing the selection updates the KPI values and the CPUE trend chart.

**Options:**

| Value | Label (ES) | Description |
|-------|-----------|-------------|
| `all` | Todas las especies | Fleet aggregate (default) |
| `pelagicos_menores` | Pelágicos menores | Small pelagics (sardine, mackerel, anchovy) |
| `pelagicos_mayores` | Pelágicos mayores | Large pelagics (tuna, swordfish, marlin) |
| `demersales` | Demersales | Bottom fish (snapper, grouper, corvina) |
| `escama` | Escama | General finfish |
| `invertebrados` | Invertebrados | Shrimp, octopus, sea cucumber, urchin |
| `{species_name}` | (individual species) | Available once Carolina's skills are wired |

**Default:** `all` (fleet aggregate)  
**Data path:** `cpue_regions[region_id].by_group[selected_group]` (coming from Carolina's skills) or `cpue_regions[region_id]` for fleet aggregate.

> **Note for engineers:** Species-level and group-level CPUE time series are not yet in `demo_data.json`. For now, render the selector but use the fleet aggregate (`all`) for all selections. The selector UI is needed in the prototype so stakeholders can see the intended interaction.

---

### 4.3 KPI cards — 2 cards side by side

| Card | Label | Value | Unit | Sub | Color |
|------|-------|-------|------|-----|-------|
| Left | `ART. CPUE` | `{kpi_menores_mean_cpue}` | `kg / eff. day` | `MENORES · 5-yr mean` | `#21925F` (green) |
| Right | `IND. CPUE` | `{kpi_mayores_mean_cpue}` | `kg / eff. day` | `MAYORES · 5-yr mean` | `#C6892A` (amber) |

Source: `cpue_regions[conapesca_region_id]` (data is regional — all offices in the same CONAPESCA region share the same time series).

---

### 4.4 CPUE Trend chart

**Chart type:** Dual line  
**Height:** 150 px  
**Title:** `CPUE Trend 2001–2026`

| Element | Detail |
|---------|--------|
| Line 1 | MENORES (artisanal) — `#21925F` solid, 1.8 px |
| Line 2 | MAYORES (industrial) — `#C6892A` dashed (4,3), 1.8 px |
| X axis | Year |
| Y axis | kg / effective day |
| Y min | 0 |
| Legend | Bottom, 9px font |
| Note below | `Regional aggregate · CONAPESCA · {region_name}` |

When a species/group is selected and species-level data is available, this chart shows only that group's CPUE (both fleets filtered). Title becomes: `CPUE Trend — {selected_label}`.

---

### 4.5 Economic Indicators section *(coming soon)*

**Status:** Placeholder — Carolina's skills are in development.

**When available, this section will include:**

| KPI | Description | Unit |
|-----|-------------|------|
| Economic value of catch | Total landed value by fleet | MXN / USD million/yr |
| Revenue per fisher | Average income per registered fisher | MXN/yr |
| Employment (artisanal) | Registered fishers in the region | persons |
| Dependency index | % of municipal income from fishing | % |

**Spec will be finalized once the skills are complete.**  
Placeholder text to show in the panel for now: `"Economic indicators coming soon — skills in development."`

---

## 5. Data availability

### Pre-computed in `demo_data.json` (12 MPAs)

| MPA | LTEM | SST | Chl-a | CONAPESCA region |
|-----|------|-----|-------|-----------------|
| Alto Golfo | ✓ | ✓ GoC | ✓ | R1 GoC Norte |
| El Vizcaíno | ✓ | ✓ GoC | ✓ | R2 Pac. Baja Norte |
| Cabo Pulmo | ✓ | ✓ GoC | ✓ | R3 GoC Sur y BCS |
| Bahía de Loreto | ✓ | ✓ GoC | ✓ | R3 GoC Sur y BCS |
| Espíritu Santo | ✓ | ✓ GoC | ✓ | R3 GoC Sur y BCS |
| Balandra | ✓ | ✓ GoC | ✓ | R3 GoC Sur y BCS |
| Cabo San Lucas | ✓ | ✓ GoC | ✓ | R3 GoC Sur y BCS |
| Islas Marías | ✓ | ✓ GoC | ✓ | R4 Pac. Nayarit-Guerrero |
| Ventilas Hidrotermales | ✓ | ✓ GoC | ✓ | R3 GoC Sur y BCS |
| Islas Marietas | ✓ | — (outside GoC) | ✓ | R4 Pac. Nayarit-Guerrero |
| Huatulco | ✓ | — (outside GoC) | ✓ | R7 Pac. Sur |
| Revillagigedo | ✓ | — (outside GoC) | ✓ | R3 GoC Sur y BCS |

### CONAPESCA fishing offices (72 offices)

All 72 offices have: name, coordinates, region_id, region_name. Available in `demo_data.json → offices`.  
Regional CPUE time series (2001–2026) available in `demo_data.json → cpue_regions` (6 marine regions).  
Species/group-level CPUE: coming once Carolina's skills are complete.

### SST / MHW data

- Source: NOAA OISST v2.1 daily (1982–2025)
- MHW detection: `heatwaveR::ts2clm()` + `detect_event(minDuration=5, maxGap=2)`
- Baseline period: 1998-01-01 to 2011-12-31
- Coverage: GoC only (lon −115.875 to −105.875, lat 22.125 to 31.625)

### Chlorophyll-a data

- Source: MODIS-Aqua monthly (2004–2023)
- Variable: `mean_npp` (used as Chl-a proxy, mg/m³)
- Coverage: all coastal zone (lon −117.7 to −86.1, lat 14–33)

### Data update cadence

| Source | Cadence | Who triggers |
|--------|---------|-------------|
| LTEM | Biannual (May–Jun, Oct–Nov) | CBMC science team |
| CONAPESCA | Annual (~July) | CBMC science team |
| SST (OISST) | Every 15 days or daily | Automated ERDDAP pull |
| Chl-a (MODIS) | Every 15 days | Automated ERDDAP pull |

---

## 6. Data structures (for engineers)

### `demo_data.json` — top-level

```json
{
  "generated": "ISO datetime string",
  "mpas":        { /* GeoJSON FeatureCollection of all MPA polygons */ },
  "grid":        { /* GeoJSON FeatureCollection of prosperity grid cells */ },
  "offices": {
    "nombre_oficina_canonico": ["OFFICE A", ...],
    "lat": [28.4, ...],
    "lon": [-113.5, ...],
    "region_id": [3, ...],
    "region_name": ["Golfo de California Sur y BCS", ...]
  },
  "mpa_data":    { /* keyed by MPA name — Panel A source */ },
  "cpue_regions":{ /* keyed by region_id — Panel B source */ }
}
```

### `mpa_data[name]` — Panel A source

```json
{
  "ltem_region": "Cabo Pulmo",
  "conapesca_region_id": 3,
  "biomass": {
    "kpi": { "mean_biomass_g_m2": 0.09, "sd_g_m2": 0.03, "years_included": [...], "n_years_in_kpi": 5 },
    "annual_means": { "year": [...], "mean_biomass_g_m2": [...] },
    "trend": { "year": [...], "fit": [...], "lwr": [...], "upr": [...] },
    "dev_expl_pct": 74.2
  },
  "functional_groups": {
    "group_order": ["Depredadores solitarios", ...],
    "group_colors": ["#D73027", ...],
    "series": {
      "Depredadores solitarios": { "year": [...], "biomass": [...] },
      /* one entry per group */
    }
  },
  "invertebrates": {
    "Echinoidea":   { "kpi": {...}, "annual_means": {...}, "trend": {...} },
    "Asteroidea":   { /* same */ },
    "Holaxonia":    { /* same */ },
    "Scleractinia": { /* same */ }
  },
  "sst": {
    "kpi_mean_sst_c": 24.3,
    "kpi_mhw_days_per_yr": 12.4,
    "kpi_years": "1982–2025",
    "annual": { "year": [...], "heatwave_days": [...], "mean_temp": [...] }
  },
  "chl": {
    "kpi_mean_chla_mg_m3": 0.412,
    "kpi_years": "2004–2023",
    "annual": { "year": [...], "mean_chla": [...] }
  }
  /* nrsi: coming soon — not yet in demo_data.json */
}
```

### `cpue_regions[region_id]` — Panel B source

```json
{
  "region_name": "Golfo de California Norte",
  "menores": {
    "year": [2001, 2002, ...],
    "cpue": [1150.3, 1210.8, ...],
    "total_kg": [...],
    "total_dias": [...]
  },
  "mayores": { /* same structure */ },
  "kpi_menores_mean_cpue": 1284.32,
  "kpi_mayores_mean_cpue": 8713.04
  /* by_group: coming once Carolina's skills are wired */
}
```

---

## 7. Design tokens

```css
--ocean-800: #0B2338   /* headings, deep navy */
--ocean-600: #143A5C   /* secondary text */
--marine-400: #1E9EC4  /* primary accent, teal */
--marine-500: #0E82A8  /* borders, active */
--ink-400:    #808E97  /* muted labels */
--ink-100:    #E4E9EC  /* dividers */
--sand-50:    #FDFAF4  /* panel background */
--green-500:  #21925F  /* positive / artisanal */
--green-100:  #DBEFE5  /* green tag background */
--amber-500:  #C6892A  /* industrial / warning */
--amber-100:  #F6E9CF  /* amber tag background */
```

Font: Inter (system fallback: `system-ui, -apple-system, sans-serif`).

---

## 8. Reference implementation

A working proof-of-concept is in `dashboard/demo/`:

- **`demo.html`** — Self-contained map + panel. Loads `demo_data.json` via `fetch()`. Run with `python3 -m http.server 8080` from the `demo/` folder.
- **`chatMPA-site-standalone.html`** — Full site with demo embedded.
- **`demo_data.json`** — Pre-computed data for 12 MPAs (~1.1 MB).

The demo uses Leaflet.js + Chart.js 4.x. It is a display reference only — not production code.

**What the demo does not yet have:**
- Panel B (Fisheries Office panel) — new per this spec
- Species/group selector
- Economic KPIs (Carolina's skills pending)
- NRSI panel
- Invertebrate time-series charts (4-taxon)

---

## 9. How data flows in production

**Flow A — User clicks an MPA:**
1. Dashboard calls orchestrator with `{mpa_name, ltem_region}`
2. Orchestrator runs `ltem-fish-biomass` → biomass + trophic groups
3. Orchestrator runs `ltem-invertebrate-abundance` → 4-taxon KPIs + trends
4. Orchestrator runs `ltem-nrsi-index` → reef health index
5. Orchestrator runs `erddap-sst-anomaly` → SST + MHW series
6. Orchestrator runs `erddap-chlorophyll` → Chl-a series
7. Orchestrator returns JSON → dashboard renders Panel A

**Flow B — User clicks a Fishing Office:**
1. Dashboard calls orchestrator with `{office_name, conapesca_region_id, selected_group}`
2. Orchestrator runs `conapesca-cpue` skill (Carolina) → CPUE time series for selected group/species
3. Orchestrator runs `conapesca-economic` skill (Carolina) → economic KPIs
4. Orchestrator returns JSON → dashboard renders Panel B

`demo_data.json` short-circuits both flows with pre-computed results.

---

## 10. Skills reference

| Skill | Panel | What it computes |
|-------|-------|-----------------|
| `ltem-fish-biomass` | A | Biomass T/ha: annual means + GAM trend + trophic group breakdown |
| `ltem-invertebrate-abundance` | A | Abundance per transect for 4 taxa (Echinoidea, Asteroidea, Holaxonia, Scleractinia) |
| `ltem-nrsi-index` | A | Reef trophic health index (−1 to +1) |
| `erddap-sst-anomaly` | A | SST °C + MHW annual days (OISST, GoC only) |
| `erddap-chlorophyll` | A | Chl-a mg/m³ annual mean (MODIS) |
| `conapesca-cpue` *(Carolina)* | B | CPUE kg/day by species group and fleet |
| `conapesca-economic` *(Carolina)* | B | Economic KPIs: landed value, revenue per fisher, employment |
