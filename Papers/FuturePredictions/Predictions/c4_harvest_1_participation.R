###############################################################################
# Cluster 4 -- Step 1: daily participation probabilities from the nested logit
#
# Runs:
#   VALID : historical SDMs 2000-2019, observed PSDN closures  (validation)
#   HIST  : historical SDMs 2000-2019, PSDN open               (current baseline)
#   GFDL / IPSL / HADL : future SDMs 2070-2090, PSDN open        (projections)
#
# Only the SDM-based availability (MSQD, NANC, PSDN, CMCK) changes across the
# HIST/future runs. Every other covariate is taken by sample enumeration from
# the observed 2013-2017 choice occasions of the same vessel and calendar month
# (K draws per predicted day), which avoids the Jensen bias of plugging in
# average covariates into a non-linear model.
#
# NL probabilities are computed in closed form with the estimated parameters
# (output/NL_participation_model_c4_for_predictions_model.rds); the code first
# checks that this reproduces the estimated log-likelihood.
#
# Output: Predictions/output_c4/monthly_participation_c4.rds
###############################################################################

rm(list = ls())
suppressMessages({
  library(tidyverse)
  library(data.table)
  library(zoo)
})

ROOT <- "C:/GitHub/EconAnalysis/Papers/FuturePredictions"
GD   <- "G:/Mi unidad/Data/Anonymised data"
LAND <- "C:/GitHub/EconAnalysis/FuturePredictions/Landings/Data"
OUT  <- file.path(ROOT, "Predictions/output_c4")
dir.create(OUT, showWarnings = FALSE)

model <- readRDS(file.path(ROOT, "output/NL_participation_model_c4_for_predictions_model.rds"))
b <- model$estimate

alts <- c("sfa_nanc", "laa_nanc", "laa_cmck", "laa_msqd", "laa_ytna", "mna_msqd",
          "sba_msqd", "laa_btna", "sfa_msqd", "mna_psdn", "sba_cmck", "mra_msqd",
          "laa_psdn", "mna_nanc")
nests <- list(cmck = c("laa_cmck", "sba_cmck"),
              msqd = c("laa_msqd", "mna_msqd", "mra_msqd", "sba_msqd", "sfa_msqd"),
              psdn = c("laa_psdn", "mna_psdn"),
              nanc = c("laa_nanc", "mna_nanc", "sfa_nanc"))
tuna  <- c("laa_ytna", "laa_btna")
sp_of <- function(a) substr(a, 5, 8)
pt_of <- function(a) toupper(substr(a, 1, 3))

# ---------------------------------------------------------------------------
# Nested logit probabilities (root = 1; part_cps -> species nests; part_tuna)
# V: list of utility vectors named by alternative; no_participation utility = 0
# ---------------------------------------------------------------------------
lse <- function(M) { m <- do.call(pmax, as.data.frame(M)); m + log(rowSums(exp(M - m))) }

nl_logprob <- function(V) {
  lc <- b["lambda_part_cps"]; lt <- b["lambda_part_tuna"]
  out <- list()
  IV <- sapply(names(nests), function(n) {
    ln <- b[paste0("lambda_", n)]
    lse(sapply(nests[[n]], function(a) V[[a]] / ln))
  })
  if (is.null(dim(IV))) IV <- matrix(IV, nrow = 1, dimnames = list(NULL, names(nests)))
  scaled <- sapply(names(nests), function(n) b[paste0("lambda_", n)] / lc * IV[, n])
  if (is.null(dim(scaled))) scaled <- matrix(scaled, nrow = 1, dimnames = list(NULL, names(nests)))
  IVc <- lse(scaled)
  IVt <- lse(sapply(tuna, function(a) V[[a]] / lt))
  top <- lse(cbind(0, lc * IVc, lt * IVt))
  lPc <- lc * IVc - top; lPt <- lt * IVt - top
  for (n in names(nests)) {
    ln <- b[paste0("lambda_", n)]
    for (a in nests[[n]]) out[[a]] <- V[[a]] / ln - IV[, n] + scaled[, n] - IVc + lPc
  }
  for (a in tuna) out[[a]] <- V[[a]] / lt - IVt + lPt
  out[["no_participation"]] <- 0 - top
  out
}

utility <- function(a, X) {
  # X: list with vectors mean_avail, mean_price, wind, d_d, d_cd, dist0, cog (for alt a),
  #    and weekend, unem, psdnclosure, btnaclosure (common)
  sp <- sp_of(a)
  v <- b[paste0("asc_", a)] + b[paste0("w_", sp)] * X$weekend +
    b["B_mean_avail"] * X$mean_avail + b["B_mean_price"] * X$mean_price +
    b["B_wind_max_220_mh"] * X$wind + b["B_d_d"] * X$d_d +
    b["B_dist_port_to_catch_area_zero"] * X$dist0 + b["B_unem_rate_part"] * X$unem +
    b["B_dist_to_cog"] * X$cog
  if (sp == "btna") v <- v + b["c_btna"] * X$btnaclosure
  if (sp == "psdn") v <- v + b["c_psdn"] * X$psdnclosure
  if (sp %in% c("ytna", "btna")) v <- v + b["B_d_cd"] * X$d_cd
  unname(v)
}

# ---------------------------------------------------------------------------
# SDM availability: 30-day moving average, lagged one day (as in estimation)
# ---------------------------------------------------------------------------
sdm_ma <- function(file, col) {
  x <- fread(file)
  x[, date := as.Date(date)]
  x[, val := get(col)]
  x <- x[, .(date, PORT_AREA_CODE, val)]
  # historical files skip a few calendar days (e.g. 29 Feb): complete and interpolate
  grid <- CJ(PORT_AREA_CODE = unique(x$PORT_AREA_CODE),
             date = seq(min(x$date), max(x$date), by = "day"))
  x <- x[grid, on = .(PORT_AREA_CODE, date)]
  setorder(x, PORT_AREA_CODE, date)
  x[, val := if (all(is.na(val))) val else na.approx(val, rule = 2), by = PORT_AREA_CODE]
  x[, ma := rollapplyr(val, 30, mean, partial = TRUE), by = PORT_AREA_CODE]
  x[, ma_lag := shift(ma, 1), by = PORT_AREA_CODE]
  x[is.na(ma_lag), ma_lag := ma]
  x[, .(date, PORT_AREA_CODE, ma_lag)]
}
hist_files <- list(MSQD = c("SDM/Historical/MSQD_SDM_port_day.csv", "SDM_90"),
                   PSDN = c("SDM/Historical/PSDN_SDM_port_day.csv", "SDM_60"),
                   NANC = c("SDM/Historical/NANC_SDM_port_day.csv", "SDM_60"),
                   CMCK = c("SDM/Historical/CMCK_SDM_port_day.csv", "SDM_60"))
fut_files  <- list(MSQD = c("SDM/Future/MSQD_FutureSDM_port_day.csv", "SDM_90"),
                   PSDN = c("SDM/Future/PSDN_FutureSDM_port_day.csv", "SDM_60"),
                   NANC = c("SDM/Future/NANC_FutureSDM_port_day.csv", "SDM_60"),
                   CMCK = c("SDM/Future/CMCK_FutureSDM_port_day.csv", "SDM_60"))

get_avail <- function(esm = NULL) {
  ff <- if (is.null(esm)) hist_files else fut_files
  rbindlist(lapply(names(ff), function(s) {
    col <- if (is.null(esm)) ff[[s]][2] else paste0(ff[[s]][2], "_", toupper(esm))
    sdm_ma(file.path(ROOT, ff[[s]][1]), col)[, SPECIES := s]
  }))
}

# ---------------------------------------------------------------------------
# Estimation data + log-likelihood check
# ---------------------------------------------------------------------------
raw <- readRDS(file.path(GD, "part_model_c4.rds")) %>%
  mutate(set_date = as.Date(set_date),
         selection = tolower(gsub("-", "_", selection)),
         PORT_AREA_CODE = toupper(substr(selection, 1, 3)),
         SPECIES = toupper(substr(selection, 5, 8)))

av_hist <- get_avail()
est <- raw %>%
  left_join(av_hist, by = c("set_date" = "date", "PORT_AREA_CODE", "SPECIES")) %>%
  mutate(mean_avail = ifelse(SPECIES %in% c("MSQD", "NANC", "PSDN", "CMCK"), ma_lag, mean_avail)) %>%
  filter(set_date != as.Date("2016-02-29"))

w <- est %>%
  select(fished_haul_anon, fished_vessel_anon, set_date, weekend, psdnclosure, btnaclosure,
         selection, fished, mean_avail, mean_price, wind_max_220_mh, d_d, d_cd,
         dist_port_to_catch_area_zero, dist_to_cog, unem_rate) %>%
  pivot_wider(names_from = selection,
              values_from = c(fished, mean_avail, mean_price, wind_max_220_mh, d_d, d_cd,
                              dist_port_to_catch_area_zero, dist_to_cog, unem_rate))

Xw <- function(a, df) list(mean_avail = df[[paste0("mean_avail_", a)]],
                          mean_price = df[[paste0("mean_price_", a)]],
                          wind = df[[paste0("wind_max_220_mh_", a)]],
                          d_d = df[[paste0("d_d_", a)]], d_cd = df[[paste0("d_cd_", a)]],
                          dist0 = df[[paste0("dist_port_to_catch_area_zero_", a)]],
                          cog = df[[paste0("dist_to_cog_", a)]],
                          weekend = df$weekend, unem = df$unem_rate_sfa_nanc,
                          psdnclosure = df$psdnclosure, btnaclosure = df$btnaclosure)
V <- setNames(lapply(alts, function(a) utility(a, Xw(a, w))), alts)
lP <- nl_logprob(V)
chosen <- sapply(c(alts, "no_participation"), function(a) w[[paste0("fished_", a)]])
LL <- sum(sapply(c(alts, "no_participation"), function(a) sum(lP[[a]] * chosen[, a])))
cat(sprintf("LL check: computed %.2f vs Apollo %.2f (N = %d)\n", LL, model$maximum, nrow(w)))
stopifnot(abs(LL - model$maximum) < 1)

insample_shares <- tibble(alt = c(alts, "no_participation"),
                          observed  = colMeans(chosen),
                          predicted = sapply(c(alts, "no_participation"), function(a) mean(exp(lP[[a]]))))
write_csv(insample_shares, file.path(OUT, "insample_shares.csv"))

# ---------------------------------------------------------------------------
# Covariate climatology (held fixed in all runs)
# ---------------------------------------------------------------------------
vessels <- sort(unique(raw$fished_vessel_anon))
ra <- raw %>% filter(selection != "no_participation")
alt_month <- ra %>% group_by(selection, set_month) %>%
  summarise(mean_price = mean(mean_price), wind = mean(wind_max_220_mh),
            avail_raw = mean(mean_avail), .groups = "drop")
alt_mean <- ra %>% group_by(selection) %>%
  summarise(across(c(mean_price, wind_max_220_mh, mean_avail), mean), .groups = "drop")
ves_alt <- ra %>% group_by(fished_vessel_anon, selection) %>%
  summarise(d_d = mean(d_d), d_cd = mean(d_cd), dist0 = mean(dist_port_to_catch_area_zero),
            cog = mean(dist_to_cog), .groups = "drop")
ves_unem <- raw %>% filter(selection == "sfa_nanc") %>%
  group_by(fished_vessel_anon) %>% summarise(unem = mean(unem_rate))

# Exposure: share of calendar days in month m on which vessel v is in the choice data
cal <- tibble(date = seq(as.Date("2013-04-23"), as.Date("2017-12-31"), by = "day")) %>%
  count(month = month(date), name = "cal_days")
exposure <- raw %>% distinct(fished_vessel_anon, set_date) %>%
  count(fished_vessel_anon, month = month(set_date), name = "obs_days") %>%
  complete(fished_vessel_anon = vessels, month = 1:12, fill = list(obs_days = 0)) %>%
  left_join(cal, by = "month") %>% mutate(expo = obs_days / cal_days)
write_csv(exposure, file.path(OUT, "exposure_vessel_month.csv"))

# Observed closures by year-month for the validation run
clos <- readRDS(file.path(LAND, "prediction_MSQD.rds")) %>% as_tibble() %>%
  group_by(LANDING_YEAR, LANDING_MONTH) %>%
  summarise(PSDN.Open = mean(PSDN.Open), .groups = "drop")

# ---------------------------------------------------------------------------
# Daily prediction for one run -> monthly vessel x alternative aggregates
# ---------------------------------------------------------------------------
predict_run <- function(run) {
  esm <- if (run %in% c("VALID", "HIST")) NULL else run
  years <- if (is.null(esm)) 2000:2019 else 2070:2090
  av <- if (is.null(esm)) av_hist else get_avail(esm)
  days <- tibble(date = seq(as.Date(paste0(min(years), "-01-01")),
                            as.Date(paste0(max(years), "-12-31")), by = "day")) %>%
    mutate(year = year(date), month = month(date),
           weekend = as.integer(wday(date) %in% c(1, 7)))
  if (run == "VALID") {
    days <- days %>% left_join(clos, by = c("year" = "LANDING_YEAR", "month" = "LANDING_MONTH")) %>%
      mutate(psdnclosure = 1 - coalesce(PSDN.Open, 1))
  } else days$psdnclosure <- 0
  days$btnaclosure <- 0

  res <- list()
  for (v in vessels) {
    D <- days
    # Sample enumeration: for every predicted day draw K observed choice
    # occasions of the same vessel in the same calendar month (fallback: any
    # vessel, same month) and use their non-SDM covariates. Only the SDM
    # availability differs between runs. Same seed per vessel in every run.
    set.seed(1000 + v)
    P <- 0
    for (k in seq_len(K_DRAWS)) {
      idx <- integer(nrow(D))
      for (m in 1:12) {
        i  <- which(D$month == m)
        rv <- which(w$fished_vessel_anon == v & w_month == m)
        if (!length(rv)) rv <- which(w_month == m)
        idx[i] <- rv[sample.int(length(rv), length(i), replace = TRUE)]
      }
      Xd <- w[idx, ]
      V <- list()
      for (a in alts) {
        sp <- toupper(sp_of(a))
        X <- Xw(a, Xd)
        if (sp %in% c("MSQD", "NANC", "PSDN", "CMCK")) {
          sdm <- av[SPECIES == sp & PORT_AREA_CODE == pt_of(a)]
          X$mean_avail <- sdm$ma_lag[match(D$date, sdm$date)]
        }
        X$weekend <- D$weekend
        X$psdnclosure <- D$psdnclosure
        X$btnaclosure <- D$btnaclosure
        V[[a]] <- utility(a, X)
      }
      lP <- nl_logprob(V)
      P <- P + sapply(c(alts, "no_participation"), function(a) exp(lP[[a]])) / K_DRAWS
    }
    ex <- exposure %>% filter(fished_vessel_anon == v)
    e <- ex$expo[match(D$month, ex$month)]
    long <- as.data.table(P)[, `:=`(year = D$year, month = D$month, expo = e)]
    long <- melt(long, id.vars = c("year", "month", "expo"), variable.name = "alt", value.name = "p")
    res[[as.character(v)]] <- long[, .(
      days_in_month = .N,
      Edays        = sum(p * expo),                   # expected fishing days (with exposure)
      Pactive      = 1 - exp(sum(log1p(-pmin(p * expo, 1 - 1e-12)))),
      Edays_noexp  = sum(p),
      Pactive_noexp = 1 - exp(sum(log1p(-pmin(p, 1 - 1e-12))))
    ), by = .(year, month, alt)][, vessel := v]
  }
  out <- rbindlist(res)[, run := run]
  cat(run, "done:", nrow(out), "rows\n")
  out
}

K_DRAWS <- 5
w_month <- month(w$set_date)
runs <- c("VALID", "HIST", "GFDL", "IPSL", "HADL")
monthly <- rbindlist(lapply(runs, predict_run))
saveRDS(monthly, file.path(OUT, "monthly_participation_c4.rds"))

# Vessel lengths (for the landings models)
ves_len <- raw %>% group_by(fished_vessel_anon) %>% summarise(Length = mean(vessellength))
saveRDS(ves_len, file.path(OUT, "vessel_length_c4.rds"))
cat("Step 1 finished\n")
