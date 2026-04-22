# EconAnalysis

Economic analysis of US West Coast CPS fisheries (Pacific sardine, Northern anchovy, Market squid, Pacific mackerel, etc.).

## Papers

- **`Papers/Landings/`** — Landings paper (CJFAS). How species availability and regulations affect landings of CPS species.
- **`Papers/Participation/`** — Participation paper (Ecological Economics). Nested logit / DCM for vessel location and target-species decisions using PacFIN data across vessel clusters c4–c7. Ver [Papers/Participation/README.md](Papers/Participation/README.md).
- **`Papers/FuturePredictions/`** — paper en progreso.

## Shared

- **`Shared/`** — recursos compartidos entre papers: `Data/`, `Functions/`, `MMSI/`, `Documents/`, `GameTheory/`, `Figures/`.

## Archive

- **`Archive/`** — código viejo o auxiliar: `AtlantisEcon/`, `Other_code/`, `EconProject.mmp`.

## Portable paths con `here()`

Todos los scripts R en `Papers/Participation/code/R/estimation/main/` usan el paquete [`here`](https://here.r-lib.org/) anclado en `EconAnalysis.Rproj` (raíz del repo). Esto hace los scripts portables entre máquinas — no se necesita editar `setwd("D:/...")` al cambiar de computador.

Patrón estándar al inicio de cada script:

```r
library(here)
part_dir      <- here::here("Papers", "Participation")
data_dir      <- file.path(part_dir, "data")
sdm_dir       <- file.path(part_dir, "code", "SDMs")
apollo_dir    <- file.path(part_dir, "Results", "apollo")
pred_dir      <- file.path(part_dir, "Results", "predictions")
r_output_dir  <- file.path(part_dir, "Results", "r_output")
shared_dir    <- here::here("Shared")  # sólo escenarios / datos compartidos
setwd(part_dir)
```

**Requisito:** abrir R a través de `EconAnalysis.Rproj` (o tener el working directory dentro del repo) para que `here()` localice la raíz correctamente.

## Reorganización 2026-04-22

Estructura plana (~25 carpetas en raíz) migrada a `Papers/` + `Shared/` + `Archive/`. La carpeta `Participation/` específicamente tuvo una reorganización profunda y limpieza de archivos viejos (717 MB → 97 MB en `r_output/`). Ver el README del paper para detalles.
