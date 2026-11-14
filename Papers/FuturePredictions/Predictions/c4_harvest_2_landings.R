###############################################################################
# Cluster 4 -- Step 2: expected landings, aggregate harvest, validation, figures
#
# Combines the monthly participation output of step 1 with the brms landing
# models (MSQD, PSDN, NANC). Landing models predict log monthly landings of a
# vessel in a port *conditional on landing that month*, so we need a rule that
# turns daily choice probabilities into monthly activity. Three are compared:
#   A  probabilistic : Pr(active) = 1 - prod_d (1 - e_vm p_d)
#   B  threshold     : active if expected fishing days >= 1
#   C  intensity     : harvest = E[days] * E[Q | active] / dbar_s
#                      (dbar_s = observed mean fishing days per active
#                       vessel-month for species s in the DCM data)   [main]
###############################################################################

rm(list = ls())
suppressMessages({
  library(tidyverse)
  library(data.table)
  library(brms)
})

ROOT <- "C:/GitHub/EconAnalysis/Papers/FuturePredictions"
GD   <- "G:/Mi unidad/Data/Anonymised data"
LAND <- "C:/GitHub/EconAnalysis/FuturePredictions/Landings/Data"
OUT  <- file.path(ROOT, "Predictions/output_c4")
set.seed(123)
NDRAWS <- 200

monthly <- readRDS(file.path(OUT, "monthly_participation_c4.rds")) %>% as_tibble() %>%
  mutate(alt = as.character(alt),
         SPECIES = toupper(substr(alt, 5, 8)), PORT = toupper(substr(alt, 1, 3)))
ves_len <- readRDS(file.path(OUT, "vessel_length_c4.rds"))

raw <- readRDS(file.path(GD, "part_model_c4.rds")) %>%
  mutate(set_date = as.Date(set_date), selection = tolower(gsub("-", "_", selection)),
         SPECIES = toupper(substr(selection, 5, 8)))

# --------------------------------------------------------------------------
# Observed benchmarks
# --------------------------------------------------------------------------
# (i) DCM data 2013-2017: active vessel-months and fishing days per active month
obs_vm <- raw %>% filter(fished == 1, selection != "no_participation") %>%
  count(fished_vessel_anon, year = year(set_date), month = month(set_date), SPECIES,
        name = "days")
dbar <- obs_vm %>% group_by(SPECIES) %>% summarise(dbar = mean(days))
obs_vm_year <- obs_vm %>% group_by(SPECIES, year) %>%
  summarise(obs_vessel_months = n(), obs_days = sum(days), .groups = "drop")

# (ii) Landings data: cluster-4 aggregate landings by year
land_dat <- list(MSQD = readRDS(file.path(LAND, "prediction_MSQD.rds")) %>% as_tibble(),
                 PSDN = readRDS(file.path(LAND, "prediction_PSDN.rds")) %>% as_tibble(),
                 NANC = readRDS(file.path(LAND, "prediction_NANC.rds")) %>% as_tibble())
obs_land <- bind_rows(lapply(c("MSQD", "PSDN"), function(s)
  land_dat[[s]] %>% filter(group_all == 4) %>%
    group_by(year = LANDING_YEAR) %>%
    summarise(obs_tons = sum(.data[[paste0(s, "_Landings")]]),
              obs_vessel_months = n(), obs_vessels = n_distinct(VESSEL_NUM)) %>%
    mutate(SPECIES = s)))

# --------------------------------------------------------------------------
# Monthly SDMs (mean of daily values) for the landing models
# --------------------------------------------------------------------------
msdm <- function(file, col, name) {
  fread(file.path(ROOT, file))[, .(v = mean(get(col), na.rm = TRUE)),
                               by = .(year = LANDING_YEAR, month = LANDING_MONTH, PORT = PORT_AREA_CODE)] %>%
    setnames("v", name)
}
sdm_month <- function(esm = NULL) {
  sfx <- if (is.null(esm)) "" else paste0("_", esm)
  if (is.null(esm)) {
    m <- msdm("SDM/Historical/MSQD_SDM_port_day.csv", "SDM_90", "MSQD_SDM_90")
    p <- msdm("SDM/Historical/PSDN_SDM_port_day.csv", "SDM_60", "PSDN_SDM_60")
    n <- msdm("SDM/Historical/NANC_SDM_port_day_merged.csv", "SDM_20", "NANC_SDM_20")
  } else {
    m <- msdm("SDM/Future/MSQD_FutureSDM_port_day.csv", paste0("SDM_90", sfx), "MSQD_SDM_90")
    p <- msdm("SDM/Future/PSDN_FutureSDM_port_day.csv", paste0("SDM_60", sfx), "PSDN_SDM_60")
    n <- msdm("SDM/Future/NANC_FutureSDM_port_day_20km.csv", paste0("SDM_20", sfx), "NANC_SDM_20")
  }
  as_tibble(m) %>% left_join(as_tibble(p), by = c("year", "month", "PORT")) %>%
    left_join(as_tibble(n), by = c("year", "month", "PORT"))
}
sdm_all <- bind_rows(sdm_month() %>% mutate(run = "VALID"),
                     sdm_month() %>% mutate(run = "HIST"),
                     sdm_month("GFDL") %>% mutate(run = "GFDL"),
                     sdm_month("IPSL") %>% mutate(run = "IPSL"),
                     sdm_month("HADL") %>% mutate(run = "HADL"))

# Regulatory states for the validation run (observed); projections: all open
regs_m <- land_dat$MSQD %>% group_by(year = LANDING_YEAR, month = LANDING_MONTH) %>%
  summarise(PSDN.Open = mean(PSDN.Open), PSDN.Total.Closure = mean(PSDN.Total.Closure), .groups = "drop")
regs_p <- land_dat$PSDN %>% group_by(year = LANDING_YEAR, month = LANDING_MONTH) %>%
  summarise(MSQD.Open = mean(MSQD.Open), .groups = "drop")

# Observed standardized prices (validation run only); projections use the mean
price_ym <- function(s) land_dat[[s]] %>%
  group_by(year = LANDING_YEAR, month = LANDING_MONTH) %>%
  summarise(pz = mean(.data[[paste0(s, "_Price_z")]], na.rm = TRUE), .groups = "drop")

# --------------------------------------------------------------------------
# Expected landings E[Q | active] from the brms models
# --------------------------------------------------------------------------
fits <- list(MSQD = readRDS(file.path(ROOT, "Landings/Estimations/fit_qMSQD_boost_GAM.rds")),
             PSDN = readRDS(file.path(ROOT, "Landings/Estimations/fit_qPSDN_boost_GAM.rds")),
             NANC = readRDS(file.path(ROOT, "Landings/Estimations/fit_qNANC_boost_GAM.rds")))

expected_landings <- function(s) {
  fit  <- fits[[s]]
  resp <- paste0("log", s, "Landings")
  lz   <- coef(lm(Length_z ~ Length, data = land_dat[[s]]))
  pmean <- mean(land_dat[[s]][[paste0(s, "_Price_z")]], na.rm = TRUE)
  nd <- monthly %>% filter(SPECIES == s) %>%
    distinct(run, vessel, year, month, PORT) %>%
    left_join(ves_len, by = c("vessel" = "fished_vessel_anon")) %>%
    left_join(sdm_all, by = c("run", "year", "month", "PORT")) %>%
    left_join(regs_m, by = c("year", "month")) %>%
    left_join(regs_p, by = c("year", "month")) %>%
    left_join(price_ym(s), by = c("year", "month")) %>%
    mutate(valid = run == "VALID",
           PSDN.Open          = ifelse(valid, coalesce(PSDN.Open, 1), 1),
           PSDN.Total.Closure = ifelse(valid, coalesce(PSDN.Total.Closure, 0), 0),
           MSQD.Open          = ifelse(valid, coalesce(MSQD.Open, 1), 1),
           WA.Restriction = 0,
           price_z = ifelse(valid, coalesce(pz, pmean), pmean),
           Length_z = lz[1] + lz[2] * Length,
           port_cluster_ID = paste0("4-", PORT), port_ID = PORT,
           Price.Fishmeal.AFI_z = 0,
           MSQD_Landings = 1, PSDN_Landings = 1, NANC_Landings = 1)
  nd[[paste0(s, "_Price_z")]] <- nd$price_z

  lev <- levels(factor(fit$data$port_cluster_ID))
  # Duan smearing factor: residuals in logs are left-skewed, so the lognormal
  # correction exp(sigma^2/2) overstates the mean; use the empirical factor.
  # (rows with |price_z| >= 5 excluded: a few extreme price outliers)
  fd <- fit$data
  pm_in <- colMeans(posterior_linpred(fit, resp = resp, ndraws = NDRAWS))
  okr <- abs(fd[[paste0(s, "_Price_z")]]) < 5
  duan <- mean(exp(log(fd[[paste0(s, "_Landings")]][okr]) - pm_in[okr]))
  duan_tab[[s]] <<- duan
  ids <- sample(seq_len(ndraws(fit)), NDRAWS)
  nd$EQ <- NA_real_
  for (known in c(TRUE, FALSE)) {
    idx <- which((nd$port_cluster_ID %in% lev) == known)
    if (!length(idx)) next
    mu <- posterior_linpred(fit, newdata = nd[idx, ], resp = resp,
                            re_formula = if (known) NULL else NA,
                            allow_new_levels = TRUE, draw_ids = ids)
    nd$EQ[idx] <- exp(colMeans(mu)) * duan
  }
  nd %>% mutate(SPECIES = s, re_level = port_cluster_ID %in% lev) %>%
    select(run, vessel, year, month, PORT, SPECIES, EQ, re_level,
           MSQD_SDM_90, PSDN_SDM_60, NANC_SDM_20)
}
duan_tab <- list()
EQ <- bind_rows(lapply(c("MSQD", "PSDN", "NANC"), expected_landings))
write_csv(tibble(SPECIES = names(duan_tab), duan = unlist(duan_tab)), file.path(OUT, "duan_factors.csv"))
saveRDS(EQ, file.path(OUT, "expected_landings_c4.rds"))

# --------------------------------------------------------------------------
# Aggregate harvest under the three activity rules
# --------------------------------------------------------------------------
H <- monthly %>% filter(SPECIES %in% c("MSQD", "PSDN", "NANC")) %>%
  left_join(EQ, by = c("run", "vessel", "year", "month", "PORT", "SPECIES")) %>%
  left_join(dbar, by = "SPECIES") %>%
  mutate(H_A = Pactive * EQ,
         H_B = as.numeric(Edays >= 1) * EQ,
         H_C = Edays * EQ / dbar,
         H_A_noexp = Pactive_noexp * EQ)
saveRDS(H, file.path(OUT, "harvest_vessel_month_c4.rds"))

annual <- H %>% group_by(run, SPECIES, year) %>%
  summarise(across(c(H_A, H_B, H_C, H_A_noexp), sum),
            vm_A = sum(Pactive), vm_B = sum(Edays >= 1), days = sum(Edays), .groups = "drop")
write_csv(annual, file.path(OUT, "annual_harvest_c4.csv"))

# ---------- Validation (VALID run vs observed, 2000-2019) ----------
val <- annual %>% filter(run == "VALID", SPECIES %in% c("MSQD", "PSDN")) %>%
  inner_join(obs_land, by = c("SPECIES", "year")) %>%
  mutate(sample = ifelse(year %in% 2013:2017, "In-sample (2013-17)", "Out-of-sample"))
val_stats <- val %>%
  pivot_longer(c(H_A, H_B, H_C, H_A_noexp), names_to = "rule", values_to = "pred") %>%
  group_by(SPECIES, rule, sample) %>%
  summarise(n_years = n(), obs_mean = mean(obs_tons), pred_mean = mean(pred),
            ratio = pred_mean / obs_mean,
            corr = ifelse(n() > 2, cor(pred, obs_tons), NA_real_),
            mape = mean(abs(pred - obs_tons) / obs_tons), .groups = "drop")
write_csv(val_stats, file.path(OUT, "validation_stats_c4.csv"))
write_csv(val, file.path(OUT, "validation_annual_c4.csv"))

# Activity check vs DCM data 2013-2017 (vessel-months and fishing days)
act <- annual %>% filter(run == "VALID", year %in% 2013:2017) %>%
  group_by(SPECIES) %>% summarise(pred_vm_A = sum(vm_A), pred_vm_B = sum(vm_B), pred_days = sum(days)) %>%
  left_join(obs_vm_year %>% group_by(SPECIES) %>%
              summarise(obs_vm = sum(obs_vessel_months), obs_days = sum(obs_days)), by = "SPECIES")
all_sp <- monthly %>% filter(run == "VALID", year %in% 2013:2017, alt != "no_participation") %>%
  group_by(SPECIES) %>% summarise(pred_days = sum(Edays), pred_vm_A = sum(Pactive)) %>%
  left_join(obs_vm_year %>% group_by(SPECIES) %>%
              summarise(obs_vm = sum(obs_vessel_months), obs_days = sum(obs_days)), by = "SPECIES")
write_csv(all_sp, file.path(OUT, "activity_check_c4.csv"))
print(all_sp)

# ---------- Projections: HIST (2000-19) vs ESMs (2070-90) ----------
proj_days <- monthly %>% filter(run != "VALID", alt != "no_participation") %>%
  group_by(run, SPECIES, year) %>% summarise(days = sum(Edays), .groups = "drop") %>%
  group_by(run, SPECIES) %>% summarise(days = mean(days), .groups = "drop")
proj_H <- annual %>% filter(run != "VALID") %>% group_by(run, SPECIES) %>%
  summarise(across(c(H_A, H_B, H_C), mean), .groups = "drop")
proj_port <- H %>% filter(run != "VALID") %>% group_by(run, SPECIES, PORT, year) %>%
  summarise(H_A = sum(H_A), H_C = sum(H_C), days = sum(Edays), .groups = "drop") %>%
  group_by(run, SPECIES, PORT) %>% summarise(H_A = mean(H_A), H_C = mean(H_C), days = mean(days), .groups = "drop")
write_csv(proj_days, file.path(OUT, "proj_days_by_species_c4.csv"))
write_csv(proj_H, file.path(OUT, "proj_harvest_by_species_c4.csv"))
write_csv(proj_port, file.path(OUT, "proj_harvest_by_port_c4.csv"))

# Decomposition of the change in harvest (rule C, main): participation vs landings margin
cl <- H %>% filter(run != "VALID") %>%
  group_by(run, SPECIES, PORT, vessel, month) %>%
  summarise(P = mean(Edays / dbar), Q = mean(EQ), .groups = "drop")   # rule C
base <- cl %>% filter(run == "HIST") %>% select(-run) %>% rename(P0 = P, Q0 = Q)
decomp <- cl %>% filter(run != "HIST") %>% left_join(base, by = c("SPECIES", "PORT", "vessel", "month")) %>%
  group_by(run, SPECIES) %>%
  summarise(H0 = sum(P0 * Q0), H1 = sum(P * Q),
            d_participation = sum((P - P0) * Q0), d_landings = sum(P0 * (Q - Q0)),
            d_interaction = sum((P - P0) * (Q - Q0)), .groups = "drop") %>%
  mutate(pct_change = 100 * (H1 / H0 - 1))
write_csv(decomp, file.path(OUT, "decomposition_c4.csv"))

# SDM means at the cluster-4 ports (context)
sdm_ctx <- sdm_all %>% filter(run != "VALID", PORT %in% c("LAA", "MNA", "SBA", "SFA", "MRA")) %>%
  group_by(run, PORT) %>% summarise(across(c(MSQD_SDM_90, PSDN_SDM_60, NANC_SDM_20), mean), .groups = "drop")
write_csv(sdm_ctx, file.path(OUT, "sdm_means_c4_ports.csv"))

# --------------------------------------------------------------------------
# Figures
# --------------------------------------------------------------------------
ink <- "#0b0b0b"; ink2 <- "#52514e"; grid <- "#e6e5e0"
th <- theme_minimal(base_size = 10) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = grid, linewidth = 0.3),
        axis.text = element_text(color = ink2), axis.title = element_text(color = ink2),
        strip.text = element_text(face = "bold", color = ink, hjust = 0),
        legend.position = "top", legend.title = element_blank(),
        plot.title = element_text(face = "bold", color = ink))
run_cols <- c(HIST = "#8a8984", GFDL = "#2a78d6", IPSL = "#eb6834", HADL = "#1baf7a")
rule_cols <- c("Observed" = ink, "A: probabilistic" = "#2a78d6",
               "B: threshold" = "#eb6834", "C: intensity" = "#1baf7a")

rule_cols <- c("Observed" = ink, "Predicted (rule C)" = "#2a78d6")
fv <- val %>% select(SPECIES, year, Observed = obs_tons, `Predicted (rule C)` = H_C) %>%
  pivot_longer(-c(SPECIES, year)) %>%
  mutate(name = factor(name, levels = names(rule_cols)), v = value / 1000)
g1 <- ggplot(fv, aes(year, v, color = name)) +
  annotate("rect", xmin = 2012.5, xmax = 2017.5, ymin = -Inf, ymax = Inf, fill = "#f1f0ec") +
  annotate("text", x = 2015, y = Inf, label = "DCM sample", vjust = 1.5, size = 2.6, color = ink2) +
  geom_line(aes(linewidth = name == "Observed")) + geom_point(size = 1.2) +
  scale_linewidth_manual(values = c(`TRUE` = 1, `FALSE` = 0.6), guide = "none") +
  scale_color_manual(values = rule_cols) +
  facet_wrap(~SPECIES, scales = "free_y", ncol = 2) +
  labs(x = NULL, y = "Cluster-4 landings (thousand t / yr)") + th
ggsave(file.path(OUT, "fig1_validation.png"), g1, width = 7, height = 3, dpi = 200, bg = "white")

fd <- proj_days %>% mutate(run = factor(run, levels = names(run_cols)),
                           SPECIES = factor(SPECIES, levels = c("MSQD", "NANC", "PSDN", "CMCK", "YTNA", "BTNA")))
g2 <- ggplot(fd, aes(SPECIES, days, fill = run)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.72, color = "white", linewidth = 0.4) +
  scale_fill_manual(values = run_cols, labels = c(HIST = "Current (2000-19)", GFDL = "GFDL 2070-90",
                                                  IPSL = "IPSL 2070-90", HADL = "HADL 2070-90")) +
  labs(x = NULL, y = "Expected fishing days / yr (fleet)") + th
ggsave(file.path(OUT, "fig2_days.png"), g2, width = 7, height = 2.8, dpi = 200, bg = "white")

fp <- proj_port %>% mutate(run = factor(run, levels = names(run_cols)), v = H_C / 1000)
g3 <- ggplot(fp, aes(PORT, v, fill = run)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.72, color = "white", linewidth = 0.4) +
  scale_fill_manual(values = run_cols, labels = c(HIST = "Current (2000-19)", GFDL = "GFDL 2070-90",
                                                  IPSL = "IPSL 2070-90", HADL = "HADL 2070-90")) +
  facet_wrap(~SPECIES, scales = "free", nrow = 1) +
  labs(x = NULL, y = "Harvest (thousand t / yr)") + th
ggsave(file.path(OUT, "fig3_harvest_port.png"), g3, width = 7, height = 2.8, dpi = 200, bg = "white")

print(val_stats, n = 50); print(proj_H); print(decomp); print(proj_days, n = 30)
cat("Step 2 finished\n")
