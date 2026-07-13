# data/ — local staging copy (GIT-IGNORED)

This folder holds the **local copy** of the source datasets the skills read
during development. **Nothing in here is committed** — the repo `.gitignore`
excludes all of `data/` and the common data formats (`*.RData`, `*.rds`,
`*.csv`, `*.parquet`, `*.nc`). In production, skills read the same data from MCP
servers instead of this folder.

> ⚠️ **CONAPESCA is sensitive.** It contains economic-unit names, RNP
> identifiers, and individual-level catch records. It must never enter version
> control or any public output. Keeping it under this git-ignored folder is the
> first barrier — do not copy it elsewhere in the repo.

## What goes here

| Source | Contents | Notes |
|--------|----------|-------|
| **LTEM** | Long-Term Ecological Monitoring reef surveys (biomass, trophic levels, transects) | Gulf of California reef monitoring |
| **SST** | Sea-surface temperature grids | for marine-heatwave / thermal-stress skills |
| **chl-a** | Chlorophyll-a grids | for productivity / anomaly skills |
| **CONAPESCA** | Fisheries landings / catch records | **SENSITIVE — see warning above** |

## How to get it

The staging copy is provisioned outside this repo (shared drive / MCP export).
Place each dataset under `data/` using the layout your skill's `SKILL.md` data
contract expects. Because skills read the **minimal data contract** (columns,
not filenames), the exact filename here is not load-bearing — but keep sources
in clearly separated subfolders.

Ask Edu or Fabio for access to the current staging copy; do not redistribute
CONAPESCA data.
