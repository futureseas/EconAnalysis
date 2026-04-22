# Participation paper (Ecological Economics)

Nested logit / DCM for vessel participation and target-species decisions using PacFIN data across vessel clusters c4–c7.

## Main results

El driver principal (OOS + base para varias estimaciones) es:

**`code/R/estimation/main/nl_driver_allclusters.R`**

Desde ahí se alimentan / estiman:

- `nl_structure_c{4..7}.R` (estructura del nested logit por cluster — listas puras, sin paths)
- `nl_best_mean_avail_per_alternative*.R` (variantes: 30 días, last day, structural breaks closure/mhw, no unemployment, unique)
- `nl_elasticities*.R`, `nl_null.R`, `logit_tree.R`
- `scenario_heatwave*.R`, `scenario_dynamic_*.R` (escenarios)
- `n_logit_c4_RR.R`, `n_logit_c4_lastday_RR.R` (modelos Apollo c4 R&R)

> **Nota:** `n_logit_all_RR.R` (0 bytes) no tiene contenido recuperable desde git — siempre se committeó vacío. No es el script importante; el driver real es `nl_driver_allclusters.R`.

## Portable paths con `here()` (2026-04-22)

Todos los scripts en `code/R/estimation/main/` usan `library(here)` anclado en `EconAnalysis.Rproj` (raíz del repo). **No hay más rutas hardcoded del tipo `setwd("D:/GitHub/...")`** — los scripts corren igual en cualquier máquina siempre que se abran vía el `.Rproj`.

Patrón estándar que arma cada script al inicio:

```r
library(here)
part_dir      <- here::here("Papers", "Participation")
data_dir      <- file.path(part_dir, "data")
sdm_dir       <- file.path(part_dir, "code", "SDMs")
apollo_dir    <- file.path(part_dir, "Results", "apollo")
pred_dir      <- file.path(part_dir, "Results", "predictions")
r_output_dir  <- file.path(part_dir, "Results", "r_output")
shared_dir    <- here::here("Shared")   # sólo scenarios dinámicos y heatwave_delta_pp
setwd(part_dir)
```

### Dónde se esperan los archivos

| Variable | Contenido |
|---|---|
| `data_dir` | `CPUE_index.rds`, `.dta` datasets, `BlockAreas/`, `Unemployment/` |
| `sdm_dir` | `sdm_{msqd,psdn,nanc,jmck,cmck,phrg,albc}.rds` |
| `apollo_dir` | `apollo_{beta,fixed,probabilities}{,_null}_{c4..c7}.rds` |
| `pred_dir` | `res_c{4..7}.rds`, `oos_table.RDS`, `prediction_{NANC,PSDN}.rds` |
| `r_output_dir` | outputs de Apollo (`outputDirectory` del `apollo_control`) — `NL_participation_model_*`, `scenarios/`, `corr_heatmaps/`, `desc_stats/` |
| `shared_dir/Data/Ports/port_areas.csv` | coordenadas de puertos (para escenarios dinámicos) |

**Scripts fuera de `main/`** (`data_prep/`, `descriptive/`, `scenarios/`, `checks/`, `SDMs/`, `Stata/`, `Clustering/`, `Location/`) todavía tienen paths antiguos — aplicar el mismo patrón here() si se necesitan correr.

## Folder layout

```
Participation/
├── code/
│   ├── R/
│   │   ├── estimation/
│   │   │   ├── main/          — scripts R&R (resultados publicados, con here())
│   │   │   └── old/           — versiones anteriores (n_logit_c{4..7}, _30days, _lastday, dcm_estimation, etc.)
│   │   ├── data_prep/         — dcm_data, participation_database, calculate_currents/wind, etc.
│   │   ├── descriptive/       — annual_participation, map_participation, vessel_part_figure
│   │   ├── scenarios/         — future_predictions, simulated_shares_figure
│   │   └── checks/            — check_WAclosure, check_closures, check_comments, check_dominant
│   ├── Stata/                 — .do files (nlogit_estimations_cluster{4..7}, predict_changes_*, etc.)
│   ├── SDMs/                  — SDM R scripts por especie (PSDN, NANC, MSQD, etc.)
│   ├── Clustering/            — PAM clustering de vessels
│   └── Location/              — dcm_location, dcm_kernel_density
│
├── data/                      — inputs
│   ├── BlockAreas/            — shapefiles de áreas de pesca
│   ├── Unemployment/          — BLS series
│   ├── rdo_Stata_c4_full.dta  — main Stata dataset (64 MB)
│   ├── dbp_month.dta, predicted.dta, simulated{1,3}.dta
│   └── CPUE_index.rds
│
├── Results/
│   ├── apollo/                — apollo_{beta,fixed,probabilities}_{c4..c7}.rds (24 archivos)
│   ├── stata_nlogit/
│   │   ├── base/              — basemodel variants
│   │   ├── c4/ c5/ c6/ c7/    — .ster por cluster
│   │   └── _old/              — .ster experimentales sin cluster (_vN, _B*, _F*, _MRA*, etc.)
│   ├── predictions/           — prediction_{NANC,PSDN}.rds, res_c{4..7}.rds, oos_table.RDS
│   ├── r_output/              — output de Apollo (NL_participation_model_*, scenarios/, corr_heatmaps/, desc_stats/)
│   ├── Tables/                — .rtf de regresiones preliminares + catch_model.docx + price_model.docx
│   └── Logs/                  — contrmap.log, preliminary.log
│
├── manuscript/                — Switching_Behavior_Summary.docx
└── _archive/                  — Mixed_logit (test), NestedLogit_v1 (intentos viejos)
```

## Limpieza realizada (2026-04-22)

- Borrados 1 783 archivos `*_OLD*` de `R/output/` (717 MB → 97 MB).
- Borrada carpeta `R/n_logit_c4_files/` (HTML guardado de navegador, basura).
- Borrados duplicados en raíz de `basemodel.ster` y `Switching_Behavior_Summary.docx` (se conservó la versión de `Results/`).
- Renombrados: `descriptive_results .R` (espacio) → `descriptive_results.R`; `n_logit_c4 R_R.R` → `n_logit_c4_RR.R`; etc.
- **Refactor a `here()`**: 18 scripts en `main/` migrados de `setwd("D:/...")` a paths portables.
