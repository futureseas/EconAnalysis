# Participation paper (Ecological Economics)

Nested logit / DCM for vessel participation and target-species decisions using PacFIN data across vessel clusters c4–c7.

## Main results

El driver principal (OOS + base para varias estimaciones) es:
**`code/R/estimation/main/nl_driver_allclusters.R`**

Desde ahí se alimentan / estiman:
- `nl_structure_c{4..7}.R` (estructura del nested logit por cluster)
- `nl_best_mean_avail_per_alternative*.R` (variantes: 30 días, last day, structural breaks closure/mhw, no unemployment)
- `nl_elasticities*.R`, `nl_null.R`, `logit_tree.R`
- `scenario_heatwave*.R`, `scenario_dynamic_*.R` (escenarios)
- `n_logit_c4_RR.R`, `n_logit_c4_lastday_RR.R` (modelos Apollo c4 R&R)

> **Nota:** `n_logit_all_RR.R` (0 bytes) no tiene contenido recuperable desde git — siempre se committeó vacío. No es el script importante; el driver real es `nl_driver_allclusters.R`.

## Folder layout

```
Participation/
├── code/
│   ├── R/
│   │   ├── estimation/
│   │   │   ├── main/          — scripts R&R (resultados publicados)
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
