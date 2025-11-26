library(tidyverse)
library(purrr)
library(broom)
library(lme4)
library(glmmTMB)
library(performance)
library(DHARMa)
library(minerva)
library(corrplot)
library(car)
library(hydroGOF)

# options(ggplot2.discrete.colour= palette.colors(palette = "R4")[2:6])
# "#DF536B" "#61D04F" "#2297E6" "#28E2E5" "#CD0BBC"
# functions ----

fmt_p <- function(p) {
    if (p < 0.05) {
      "Fail (p < 0.05)"
    } else {
      "Pass (n.s.)"
    }
  }

check_lm_assumptions <- function(model) {
  if (!inherits(model, "lm")) stop("Input must be an lm object")
  
  library(lmtest)
  library(car)
  library(tibble)
  
  results <- list()
  
  cat("\n=== Linear Model Assumption Checks ===\n")
  
  # 1. Homoscedasticity
  has_intercept <- attr(terms(model), "intercept") == 1
  
  if (!has_intercept) {
    cat("\nNote: Model has no intercept. Adding intercept for Breusch-Pagan test.\n")
    predictors <- names(model$model)[-1]
    formula_bp <- as.formula(paste("~", paste(predictors, collapse = "+"), "+1"))
    bp <- bptest(model, formula_bp, data = model$model)
  } else {
    bp <- bptest(model)
  }
  
  results$Homoscedasticity <- bp
  
  cat("\nHomoscedasticity (Breusch-Pagan test):\n")
  cat(sprintf("  BP stat = %.3f, df = %d, p = %.4f\n",
              bp$statistic, bp$parameter, bp$p.value))
  if (bp$p.value >= 0.05) cat("  → No evidence of heteroscedasticity.\n") else
    cat("  → Heteroscedasticity detected!\n")
  plot(model, which = 1)
  
  # 2. Independence (Durbin-Watson) This tests autocorrelation of the residualts (over time or space not needed for binned data)
  dw <- dwtest(model)
  results$Independence <- dw
  
  cat("\nIndependence (Durbin-Watson test):\n")
  cat(sprintf("  DW = %.3f, p = %.4f\n", dw$statistic, dw$p.value))
  if (dw$p.value < 0.05) {
    if (dw$statistic < 2) {
      cat("  → Positive autocorrelation detected (statistically significant).\n")
    } else {
      cat("  → Negative autocorrelation detected (statistically significant).\n")
    }
  } else {
    cat("  → No significant autocorrelation detected.\n")
  }
  
  # 3. Normality (Shapiro-Wilk)
  sh <- shapiro.test(residuals(model))
  results$Normality <- sh
  
  cat("\nNormality (Shapiro-Wilk test):\n")
  cat(sprintf("  W = %.3f, p = %.4f\n", sh$statistic, sh$p.value))
  if (sh$p.value >= 0.05) cat("  → Residuals do not deviate significantly from normality.\n") else
    cat("  → Residuals deviate from normality!\n")
  plot(model, which = 2)
  hist(residuals(model), main = "Residuals Histogram", xlab = "Residuals",
       col = "lightblue", breaks = 15)
  
  cat("\n=== Assumption Checks Complete ===\n")
  
  # ---------------------------------------
  # Helper: convert p-values for output
  # ---------------------------------------

  # Returned table
  assumption_tbl <- tibble(
    Metric = c(
      "Homoscedasticity",
      "Independence",
      "Normality"
    ),
    Value = c(
      fmt_p(bp$p.value),
      fmt_p(dw$p.value),
      fmt_p(sh$p.value)
    )
  )
  
  return(list(
    tests = results,
    table = assumption_tbl
  ))
}

check_gls_assumptions <- function(gls_model) {
  if (!inherits(gls_model, "gls")) stop("Input must be a GLS object from nlme::gls")
  
  library(lmtest)
  library(tibble)
  
  results <- list()
  
  cat("\n=== GLS Model Assumption Checks ===\n")
  
  # Extract fitted and residuals
  y_fit <- fitted(gls_model)
  res <- residuals(gls_model)
  
  # 1. Heteroscedasticity note
  plot(y_fit, res, main = "Residuals vs Fitted",
       xlab = "Fitted values", ylab = "Residuals",
       col = "blue", pch = 19)
  abline(h = 0, col = "red", lwd = 2)
  cat("\n→ GLS models heteroscedasticity directly; residuals vs fitted plotted for visual inspection.\n")
  results$Homoscedasticity <- "Pass"
  
  # 2. Independence: Durbin-Watson test
  results$Independence <- 'NA' # not needed for time independent bins
  
  # 3. Normality of residuals
  sh <- shapiro.test(res)
  results$Normality <- sh
  cat("\nShapiro-Wilk Normality test:\n")
  cat(sprintf("  W = %.3f, p = %.4f\n", sh$statistic, sh$p.value))
  if (sh$p.value >= 0.05) cat("  → Residuals roughly normal.\n") else
    cat("  → Residuals deviate from normality!\n")
  
  # Histogram of residuals
  hist(res, main = "Residuals Histogram", xlab = "Residuals",
       col = "lightblue", breaks = 15)
  
  # ---------------------------------------
  # Output table
  # ---------------------------------------
  assumption_tbl <- tibble(
    Metric = c("Homoscedasticity", "Independence", "Normality"),
    Value = c("Pass", "NA", fmt_p(sh$p.value))
  )
  
  cat("\n=== GLS Assumption Checks Complete ===\n")
  
  return(list(
    tests = results,
    table = assumption_tbl
  ))
}


check_nls_assumptions <- function(model) {
  if (!inherits(model, "nls")) stop("Input must be an nls object")
  
  library(lmtest)
  library(tibble)
  
  results <- list()
  cat("\n=== NLS Model Assumption Checks ===\n")
  
  resid_nls <- residuals(model)
  fitted_nls <- fitted(model)
  
  # 1. Residual vs fitted plot (visual homoscedasticity)
  cat("\n1) Residual vs Fitted Plot (check homoscedasticity and patterns)\n")
  plot(fitted_nls, resid_nls, main="Residuals vs Fitted", 
       xlab="Fitted values", ylab="Residuals", pch=19, col="blue")
  abline(h=0, col="red", lwd=2)
  
  # Visual trend check with |residuals| ~ fitted
  abs_resid <- abs(resid_nls)
  lm_abs <- lm(abs_resid ~ fitted_nls)
  results$Homoscedasticity_lm <- summary(lm_abs)
  
  cat("  → Linear fit of |residuals| vs fitted: slope =", coef(lm_abs)[2], "\n")
  # No p-value, so no Pass/Fail, but used for heuristic
  homo_flag <- ifelse(coef(lm_abs)[2] > 0.01, TRUE, FALSE)
  
  # 2. Normality (Shapiro-Wilk)
  cat("\n2) Normality of residuals\n")
  sh <- shapiro.test(resid_nls)
  results$Normality <- sh
  
  cat(sprintf("  Shapiro-Wilk W = %.3f, p = %.4f\n", sh$statistic, sh$p.value))
  if (sh$p.value >= 0.05) cat("  → Residuals roughly normal\n") else
    cat("  → Residuals deviate from normality\n")
  qqnorm(resid_nls); qqline(resid_nls)
  
  # 3. Independence (Durbin-Watson)
  cat("\n3) Independence of residuals (Durbin-Watson)\n")
  dw <- tryCatch(dwtest(lm(resid_nls ~ fitted_nls)), error=function(e) NULL)
  
  if (!is.null(dw)) {
    results$Independence <- dw
    cat(sprintf("  DW = %.3f, p = %.4f\n", dw$statistic, dw$p.value))
  } else {
    cat("  → Durbin-Watson test failed\n")
  }
  acf(resid_nls)
  
  # 4. Convergence info
  cat("\n4) Convergence info\n")
  summary_model <- summary(model)
  
  converged <- summary_model$convInfo$stopMessage == 'converged'
  if (converged) {
    cat("  → Model converged successfully\n")
  } else {
    cat("  → Convergence warning: ", summary_model$convInfo$stopMessage, "\n")
  }
  
  cat("\n=== NLS Assumption Checks Complete ===\n")
  
  # -------------------------------------------------------------------
  # Helper: format p-values in same style as LM version
  # -------------------------------------------------------------------
  
  # Homoscedasticity: only heuristic, no p-value
  homo_text <- if (homo_flag) paste0("Fail (slope = ", coef(lm_abs)[2] |> round(2), ")") else "Pass"
  
  # Independence: DW may fail
  dw_p <- if (!is.null(dw)) fmt_p(dw$p.value) else "NA"
  
  # Returned summary table
  assumption_tbl <- tibble(
    Metric = c(
      "Homoscedasticity",
      "Independence",
      "Normality",
      "Converged?"
    ),
    Value = c(
      homo_text,
      dw_p,
      fmt_p(sh$p.value),
      if (converged) "Yes" else "No"
    )
  )
  
  return(list(
    tests = results,
    table = assumption_tbl
  ))
}

# account for no intercept as in Kozak1995
compute_r2 <- function(model) {
  y_fit <- fitted(model)

  # Determine model type
  if (inherits(model, "lm")) {
    y_obs <- model$model[[1]]
    
    # Check if model has intercept
    has_intercept <- attr(terms(model), "intercept") == 1
    
    # Compute TSS: for no-intercept model, TSS is sum(y^2)
    if (!has_intercept) {
      warning("No-intercept model: R² computed as 1 - RSS/TSS. Can be negative (see Kozak1995).")
    }
    
  } else if (inherits(model, "nls") || inherits(model, "gls")) {
        # Unified NLS + GLS branch
    if (inherits(model, "nls")) {
      y_obs <- model$m$getEnv()[[as.character(formula(model)[[2]])]]
    } else {  # GLS
      y_obs <- y_fit + residuals(model)
    }
    
  } else {
    stop("Model must be lm or nls")
  }

  TSS <- sum((y_obs - mean(y_obs))^2)
  RSS <- sum((y_obs - y_fit)^2)
  R2 <- 1 - RSS/TSS

  if (RSS > TSS) warning("R² is negative; interpret as pseudo-R².")
  
  return(round(R2, 3))
}

label_bin_fn <- function(bins){
  (bins[-1] + bins[-length(bins)]) / 2
}

to_long <- function(from,
                    to,
                    class,
                    quality,
                    bad_troughs,
                    weighed_tree_quality,
                    is_melt_event,
                    notes,
                    event_id) {
  datetime <- seq(from, to, 900)
  
  out <- data.frame(datetime,
                    class,
                    quality,
                    bad_troughs,
                    weighed_tree_quality,
                    is_melt_event,
                    notes,
                    event_id)
  
  return(out)
}

to_long_melt_subl <- function(from,
                              to,
                              class,
                              quality,
                              weighed_tree_quality,
                              mostly_sublimation,
                              bad_troughs,
                              is_melt_event,
                              notes,
                              event_id) {
  datetime <- seq(from, to, 900)
  
  out <- data.frame(datetime,
                    class,
                    quality,
                    weighed_tree_quality,
                    mostly_sublimation,
                    bad_troughs,
                    is_melt_event,
                    notes,
                    event_id)
  
  return(out)
}

# SETUP ----

# "#000000" "#E69F00" "#56B4E9" "#009E73" "#F0E442" "#0072B2" "#D55E00" "#CC79A7" "#999999"
load_suffix <- 'fsd_closed_0.88'

int_fig_width <- 6
int_fig_height <- 4

## axis labels ----
temp_bin_ax_lab <- 'Air Temperature Bins (°C)'
bin_wnd_ax_lab <- "Wind Speed Bins (m/s)"

temp_ax_lab <- 'Air Temperature (°C)'
wind_ax_lab <- 'Wind Speed (m/s)'
pretty_names_vect <- c(
  temp_ax_lab,
  "Relative Humidity (%)",
  wind_ax_lab,
  "Unloading Rate (mm/hr)",
  "Sublimation Rate (mm/hr)",
  "Snowmelt Rate (mm/hr)"
)

var_name_dict <-
  data.frame(
    name = c('t', 'rh', 'u', 'q_unl', 'q_subl', 'q_melt'),
    pretty_name = pretty_names_vect
  )

## thresholds and filters ----
min_snow_bin <- 0.1 # gives a max 10% error based on max scale output of 50 kg ... 50 * (0.02/100) = +/- 0.01 kg
snow_load_th <- 7
min_canopy_snow <- 1.5 # min mm in weighed tree for ablation analysis
min_qunld <- 0.025 # changes below this appear to be due to noise
manual_t_ice_th <- -7.5 # observed in the trough data for inc in unloading above ice bulb temp of -6 
manual_tau_th <- 0.05 # observed increase in trough unloading above this threshold
scl_names <- c('mixed', 'closed') # removed sparse trough here because was obviously leaking

# LOAD DATA ----

manual_event_types <- read.csv('tbls/select_event_met_stats_maxmin_manual.csv')
manual_event_types$type_short <- sub("^(\\w+)\\s*&.*$", "\\1", manual_event_types$manual_event_type)

ft_met <- 
  readRDS('../../analysis/met-data-processing/data/ffr_crhm_obs_qaqc_gap_fill.rds') 

pwl_met <- 
  readRDS('../../analysis/met-data-processing/data/pwl_crhm_modelling_obs.rds') 

canopy_snow_events <- 
  read.csv('data/raw-data/snow_in_canopy_post_snowfall_fltr_ablation.csv') |> 
  mutate(from =  as.POSIXct(from, tz = 'Etc/GMT+6'),
         to = as.POSIXct(to, tz = 'Etc/GMT+6'),
          event_id = format(from, "%Y-%m-%d %H")) 

events_fltr_long <-
  purrr::pmap_dfr(canopy_snow_events, to_long_melt_subl)

saveRDS(events_fltr_long, 'data/clean-data/ablation_events_long_post_snowfall.rds')

# met_binned <- readRDS('data/clean-data/met_binned_for_unloading_analysis.rds')

## BIN MET DATA ----

### wind ---- 

wind_binned <- ft_met |> 
  select(datetime, u)

# note zeros are not included in binning, to add set inlcude.lowest = T
min_wind <- round(
  min(wind_binned$u, na.rm = T),3)
max_wind <- round(
  max(wind_binned$u, na.rm = T),3)
wind_step <- 0.5

wind_breaks <- seq(
  min_wind,
  max_wind+wind_step,
  wind_step)

wind_labs_seq <- label_bin_fn(bins = wind_breaks)

stopifnot(tail(wind_breaks, 1) > max(wind_binned$u, na.rm = T))
stopifnot(length(wind_labs_seq) + 1 == length(wind_breaks))

wind_binned$wind_binned <- cut(wind_binned[,'u', drop = TRUE], wind_breaks)

wind_binned$wind_labs <- cut(wind_binned[,'u', drop = TRUE], 
                               wind_breaks, 
                               labels = wind_labs_seq)

wind_binned$wind_labs <- as.numeric(as.character(wind_binned$wind_labs))
wind_binned <- wind_binned |> select(-u)

### shear stress ----

# shear stress is calculated below as tau = wind^2 * coef (coef was found using
# observed tau at 3m at forest tower using OLS regression) since we have a lot
# of EC data gaps when we have unloading data

lm_mid_wnd_sqrd_low_tau <-
  readRDS('../../analysis/eddy-cov/data/est_tau_from_wnd/lm_mid_wnd_sqrd_low_tau.rds')

# since we forced the above through the origin we just need to multiply by the slope 

tau_binned <- ft_met |> 
  select(datetime, u) |> 
  mutate(tau = u^2 * lm_mid_wnd_sqrd_low_tau$slope) |> 
  select(-u)

ft_met$tau <- ft_met$u^2 * lm_mid_wnd_sqrd_low_tau$slope

min_mid_can_tau <- 0
max_mid_can_tau <- round(
  max(tau_binned$tau, na.rm = T))

mid_can_tau_step <- 0.05

mid_can_tau_breaks <- seq(
  min_mid_can_tau,
  max_mid_can_tau+0.2,
  mid_can_tau_step)

mid_can_tau_labs_seq <- label_bin_fn(bins = mid_can_tau_breaks)

stopifnot(tail(mid_can_tau_breaks, 1) > max(tau_binned$tau, na.rm = T))
stopifnot(length(wind_labs_seq) + 1 == length(wind_breaks))

tau_binned$tau_binned <- cut(tau_binned[,'tau', drop = TRUE], mid_can_tau_breaks)

tau_binned$tau_labs <- cut(tau_binned[,'tau', drop = TRUE], 
                        mid_can_tau_breaks, 
                        labels = mid_can_tau_labs_seq
)

tau_binned$tau_labs <- as.numeric(as.character(tau_binned$tau_labs))

tau_binned <- tau_binned |> select(-tau)

### temp ---- 

temp_binned <- ft_met |> 
  select(datetime, t)

# note zeros are not included in binning, to add set inlcude.lowest = T
min_temp <- round(
  min(temp_binned$t, na.rm = T),3)
max_temp <- round(
  max(temp_binned$t, na.rm = T),3)
temp_step <- 5

temp_breaks <- seq(
  min_temp,
  max_temp+temp_step,
  temp_step)

temp_labs_seq <- label_bin_fn(bins = temp_breaks)

stopifnot(tail(temp_breaks, 1) > max(temp_binned$t, na.rm = T))
stopifnot(length(temp_labs_seq) + 1 == length(temp_breaks))

temp_binned$temp_binned <- cut(temp_binned[,'t', drop = TRUE], temp_breaks)

temp_binned$temp_labs <- cut(temp_binned[,'t', drop = TRUE], 
                             temp_breaks, 
                             labels = temp_labs_seq)

temp_binned$temp_labs <- as.numeric(as.character(temp_binned$temp_labs))
temp_binned <- temp_binned |> select(-t)

### ice bulb depression ----

ti_dep_binned <- ft_met |> 
  mutate(ti_dep = t - t_ice_bulb) |> 
  select(datetime, ti_dep)

# note zeros are not included in binning, to add set inlcude.lowest = T
min_ti_dep <- round(
  min(ti_dep_binned$ti_dep, na.rm = T),3)
max_ti_dep <- round(
  max(ti_dep_binned$ti_dep, na.rm = T),3)
ti_dep_step <- 0.5

ti_dep_breaks <- seq(
  min_ti_dep,
  max_ti_dep+ti_dep_step,
  ti_dep_step)

ti_dep_labs_seq <- label_bin_fn(bins = ti_dep_breaks)

stopifnot(tail(ti_dep_breaks, 1) > max(ti_dep_binned$ti_dep, na.rm = T))
stopifnot(length(ti_dep_labs_seq) + 1 == length(ti_dep_breaks))

ti_dep_binned$ti_dep_binned <- cut(ti_dep_binned[,'ti_dep', drop = TRUE], ti_dep_breaks)

ti_dep_binned$ti_dep_labs <- cut(ti_dep_binned[,'ti_dep', drop = TRUE], 
                             ti_dep_breaks, 
                             labels = ti_dep_labs_seq)

ti_dep_binned$ti_dep_labs <- as.numeric(as.character(ti_dep_binned$ti_dep_labs))
ti_dep_binned <- ti_dep_binned |> select(-ti_dep)

## SCL data ----

bad_scl_events <- canopy_snow_events$event_id[canopy_snow_events$quality>2]

q_unld_scl <- 
  readRDS('data/clean-data/ft_scl_data_del_15_min.rds') |> 
  # inner_join(events_fltr_long |> select(datetime, event_id, quality, bad_troughs), by = 'datetime') |> 
  # remove some of the unloading obs where we observed one of the instruments to be faulty
  # mutate(value_flag = name == bad_troughs) |> # this is not a bug!
  filter(!event_id %in% bad_scl_events)

# need this for computing ins. error
scl_df_kg_m2_raw <- readRDS('data/raw-data/treefort_load_main.rds')
scl_df_kg_m2_raw_long <- scl_df_kg_m2_raw |>
  select(datetime, mixed = trough_1, sparse = trough_2, closed = trough_3) |> 
  pivot_longer(!datetime, values_to = 'scl_raw_kg_m2') |> 
  group_by(name) |> 
  fill(scl_raw_kg_m2, .direction = 'down') # NaN fill ok here as just used for error calc
  
## Weighed tree data ----

q_unld_tree <-
  readRDS( paste0(
    'data/clean-data/unloading_events_zero_weighed_tree_kg_m2_pre_post_cnpy_snow_',
    load_suffix,
    '.rds'
  )) |> 
  select(datetime:tree_mm) |> 
  left_join(events_fltr_long |> select(datetime, event_id, weighed_tree_quality, notes), by = 'datetime') |>
  filter(weighed_tree_quality < 3) 

q_unld_tree_all_troughs <-
  readRDS('data/clean-data/unloading_events_zero_weighed_tree_kg_m2_pre_post_cnpy_snow_fsd_cal_for_each_trough_vza_15.rds') |> 
  select(datetime, name = tree_cal_trough_name, tree_mm) |> 
  left_join(events_fltr_long |> select(datetime, event_id, weighed_tree_quality, notes), by = 'datetime') |>
  filter(weighed_tree_quality < 3) 


# need cal value to get weight of tree in kg for instrument error check
tree_cal_val_88 <- readRDS('./../../analysis/interception/data/loadcell/FFR_tree_cal_value_pluvio_fsd_closed.rds')

## obs ----

# warm tree specific events
# these ones differ from the cold ones below and may include some precip
warm_events <- c(
  '2022-04-21',
  '2022-04-23',
  '2022-06-14',
  '2022-06-23',
  '2022-06-24',
  '2023-03-14',
  '2023-03-25',
  '2023-03-26',
  '2023-03-28',
  '2023-04-13',
  '2023-04-17',
  '2023-05-08',
  '2023-06-15',
  '2023-06-21'
)

obs_tree_warm <-
  readRDS(paste0(
    'data/clean-data/warm_tree_events_zero_weighed_tree_',
    load_suffix,
    '_kg_m2_post_cnpy_snow.rds'
  )) |> 
  filter(event_id %in% warm_events)

all(warm_events %in% obs_tree_warm$event_id)

# cold tree events
cold_events <- c(
  # new ones
  #'2021-12-27', # wind event some precip, maybe blowing snow redist.
  # '2022-01-18', # wind event too much precip during (maybe blowing snow redistribution?)
  '2022-02-04', # wind event
  #'2022-02-21', # wind event unloading not associated with wind or other here
  # '2022-02-24', # wind event , tree increased due to vapour deposition likely
  # '2022-03-04', # unloading due to branch bending from warming
  # '2022-03-16', # wind event too much precip during (maybe blowing snow redistribution?
  
  # OG
  '2022-03-02', 
  '2022-03-09',
  '2022-03-20', 
  '2022-03-24',  
  '2022-03-29',  
  '2022-12-01',
  '2023-01-28',
  '2023-02-24',
  '2023-02-26'
)
obs_tree_cold <-
  readRDS(paste0(
    'data/clean-data/all_tree_events_zero_weighed_tree_',
    load_suffix,
    '_kg_m2_post_cnpy_snow.rds'
  )) |> 
  filter(event_id %in% cold_events)

obs_tree <- rbind(obs_tree_cold  |> 
                    select(datetime, event_id, observed = tree_mm),
                  obs_tree_warm |> 
                    select(datetime, event_id, observed = tree_mm)) 

w_tree_q_unld_15 <- obs_tree |>
  group_by(event_id) |> 
  mutate(
    dL = lag(observed) - observed,
    dL = ifelse(dL < 0, 0, dL)
  ) |>
  select(datetime, event_id, tree_mm = observed, dL)

saveRDS(obs_tree |> select(datetime, event_id), 'data/clean-data/ablation_events_long_warm_cold.rds')

# LOAD MODEL DATA ----

mod_d_drip_smry_frac <- readRDS('data/results/ablation_event_fraction_ablation_processes.rds')

# Select model run with all unloading events weighed tree snow load assimilated

prjs <- c('ffr_cc0.19_sparse_cansnobal',
          'ffr_cc0.54_closed_cansnobal',
          'ffr_cc0.63_mixed_cansnobal'
)
keywords <- c("sparse", "closed", "mixed")
# specify certain model run
run_tag <- "run_1"

paths <- list.files(
  paste0(
    "crhm/output/",
    prjs
  ),
  pattern = run_tag,
  full.names = T
)

read_crhm_output <- function(path){
  extracted <- str_extract(path, "sparse|closed|mixed")
  crhm_output <- CRHMr::readOutputFile(
    path,
    timezone = 'Etc/GMT+6') |> 
    mutate(name = extracted)
}

crhm_output <- map_dfr(paths, read_crhm_output) |> 
  select(datetime, name, hru_t.1, m_s_veg.1, hru_p.1,
         delsub_veg_int.1:delunld_subl_int.1) |> 
  mutate(q_subl_veg = -delsub_veg_int.1*4,
         q_melt_veg = delmelt_veg_int.1*4)

w_tree_path <- list.files(
  paste0(
    "crhm/output/",
    'ffr_closed_canopy_cc0.88_cansnobal'
  ),
  pattern = 'store_liquid_new_evap6',
  full.names = T
)

crhm_output_w_tree <- CRHMr::readOutputFile(
    w_tree_path,
    timezone = 'Etc/GMT+6') |> 
    mutate(name = 'w_tree') |> 
  select(datetime, name, hru_t.1, m_s_veg.1, hru_p.1,
         delsub_veg_int.1:delunld_subl_int.1) |> 
  mutate(q_subl_veg = -delsub_veg_int.1*4,
         q_melt_veg = delmelt_veg_int.1*4)

### bin mod subl  ----

# note zeros are not included in binning, to add set inlcude.lowest = T
min_subl <- round(
  min(crhm_output$q_subl_veg, na.rm = T),3)
max_subl <- round(
  max(crhm_output$q_subl_veg, na.rm = T),3)
subl_step <- 0.05

subl_breaks <- seq(
  min_subl,
  max_subl+subl_step,
  subl_step)

subl_labs_seq <- label_bin_fn(bins = subl_breaks)

stopifnot(tail(subl_breaks, 1) > max(crhm_output$q_subl_veg, na.rm = T))
stopifnot(length(subl_labs_seq) + 1 == length(subl_breaks))

crhm_output$subl_binned <- cut(crhm_output[,'q_subl_veg', drop = TRUE], subl_breaks)

crhm_output$subl_labs <- cut(crhm_output[,'q_subl_veg', drop = TRUE], 
                             subl_breaks, 
                             labels = subl_labs_seq)

crhm_output$subl_labs <- as.numeric(as.character(crhm_output$subl_labs))

### bin snowmelt rate (kg m-2 s-1) ----

# note zeros are not included in binning, to add set inlcude.lowest = T
min_sm <- 0
max_sm <- round(
  max(crhm_output$q_melt_veg, na.rm = T),3)
sm_step <- 0.5

sm_breaks <- seq(
  0,
  max_sm+0.5,
  sm_step)

sm_labs_seq <- label_bin_fn(bins = sm_breaks)

stopifnot(tail(sm_breaks, 1) > max(crhm_output$q_melt_veg, na.rm = T))
stopifnot(length(sm_labs_seq) + 1 == length(sm_breaks))

crhm_output$canopy_snowmelt_labs <- cut(crhm_output[,'q_melt_veg', drop = TRUE], 
                                          sm_breaks, 
                                          labels = sm_labs_seq)

crhm_output$canopy_snowmelt_labs <- as.numeric(as.character(crhm_output$canopy_snowmelt_labs))
crhm_output$canopy_snowmelt_labs <- ifelse(crhm_output$q_melt_veg == 0, 0, crhm_output$canopy_snowmelt_labs )

## Combine dfs ---- 

met_unld_no_melt <- 
  # q_unld_met_scl |> 
  # met_binned has duplicated data for each scaled weighed tree
  # q_unld_scl has been filtered to remove troughs for erroneous periods
  # left_join(q_unld_scl, crhm_output, by = c('datetime', 'name')) |>
  left_join(
    q_unld_scl |> select(datetime, name, q_unl, dU),
    crhm_output |> select(datetime,
                          name,
                          q_subl_veg,
                          subl_labs,
                          m_s_veg.1,
                          delmelt_veg_int.1),     by = c('datetime', 'name')
  ) |>
  left_join(
    q_unld_tree_all_troughs,
    by = c('datetime', 'name')
  ) |>
  left_join(wind_binned) |> 
  left_join(tau_binned) |> 
  left_join(temp_binned) |> 
  left_join(ti_dep_binned) |> 
  left_join(ft_met) |> 
  filter(
    # name %in% scl_names,
    q_unl < 7,
    q_unl > 0,
    # q_subl_veg > 0,
    delmelt_veg_int.1 == 0 # tried 2e-5, 0.01, and did not change tau unloading coef
    # hru_t.1 < -6
  ) #|> 
  # mutate(
  #   tree_mm = ifelse(is.na(tree_mm), m_s_veg.1, tree_mm) # fill missing tree data with model
  # )

## BIN TREE DATA ----

# note zeros are not included in binning, to add set inlcude.lowest = T
min_tree <- round(
  min(met_unld_no_melt$tree_mm, na.rm = T),3)
max_tree <- round(
  max(met_unld_no_melt$tree_mm, na.rm = T),3)
tree_step <- 5

tree_breaks <- seq(
  min_tree,
  max_tree+tree_step,
  tree_step)

# tree_breaks <- c(0, 5,20)
tree_breaks <- c(0, 2, 6,  20) # works well for tau but breaks sublimation fn

tree_labs_seq <- label_bin_fn(bins = tree_breaks)

stopifnot(tail(tree_breaks, 1) > max(met_unld_no_melt$tree_mm, na.rm = T))
stopifnot(length(tree_labs_seq) + 1 == length(tree_breaks))

met_unld_no_melt$tree_binned <- cut(met_unld_no_melt[,'tree_mm', drop = TRUE], tree_breaks, include.lowest = T)

met_unld_no_melt$tree_labs <- cut(met_unld_no_melt[,'tree_mm', drop = TRUE], 
                                  tree_breaks, include.lowest = T, 
                                  labels = tree_labs_seq)

met_unld_no_melt$tree_labs <- as.numeric(as.character(met_unld_no_melt$tree_labs))

met_unld_no_melt |> 
  group_by(tree_labs) |> 
  summarise(tree_mean = mean(tree_mm))

met_unld_no_melt |> 
  group_by(tree_labs) |> 
  tally()

## BIN TREE DATA FINE ----

# note zeros are not included in binning, to add set inlcude.lowest = T
min_tree <- round(
  min(met_unld_no_melt$tree_mm, na.rm = T),3)
max_tree <- round(
  max(met_unld_no_melt$tree_mm, na.rm = T),3)
tree_step <- 0.5

tree_breaks <- seq(
  min_tree,
  max_tree+tree_step,
  tree_step)

# tree_breaks <- c(0, 5,20)
# tree_breaks <- c(0, 2, 6,  20) # works well for tau but breaks sublimation fn

tree_labs_seq <- label_bin_fn(bins = tree_breaks)

stopifnot(tail(tree_breaks, 1) > max(met_unld_no_melt$tree_mm, na.rm = T))
stopifnot(length(tree_labs_seq) + 1 == length(tree_breaks))

met_unld_no_melt$tree_binned_fine <- cut(met_unld_no_melt[,'tree_mm', drop = TRUE], tree_breaks, include.lowest = T)

met_unld_no_melt$tree_labs_fine <- cut(met_unld_no_melt[,'tree_mm', drop = TRUE], 
                                  tree_breaks, include.lowest = T, 
                                  labels = tree_labs_seq)

met_unld_no_melt$tree_labs_fine <- as.numeric(as.character(met_unld_no_melt$tree_labs_fine))

met_unld_no_melt |> 
  group_by(tree_labs_fine) |> 
  summarise(tree_mean = mean(tree_mm))

met_unld_no_melt |> 
  group_by(tree_labs_fine) |> 
  tally()

# DF for canopy snowmelt periods ----

obs_mod_met_melt <- 
  # q_unld_met_scl |> 
  # met_binned has duplicated data for each scaled weighed tree
  # q_unld_scl has been filtered to remove troughs for erroneous periods
  # left_join(q_unld_scl, crhm_output, by = c('datetime', 'name')) |>
  left_join(
    q_unld_scl |> select(datetime, name, q_unl, dU),
    crhm_output |> select(datetime,
                          name,
                          q_subl_veg,
                          q_melt_veg,
                          subl_labs,
                          m_s_veg.1,
                          delmelt_veg_int.1),     by = c('datetime', 'name')
  ) |>
  left_join(
    q_unld_tree_all_troughs,
    by = c('datetime', 'name')
  ) |>
  left_join(wind_binned) |> 
  left_join(temp_binned) |> 
  left_join(tau_binned) |> 
  left_join(ti_dep_binned) |> 
  left_join(ft_met) |> 
  filter(
    # name %in% scl_names,
    q_unl < 7,
    q_unl > 0,
    # q_subl_veg > 0,
    delmelt_veg_int.1 > 0 # tried 0.01 as well and increases R2 of melt relationship but better to keep 0
    # hru_t.1 < -6
  ) |> 
  mutate(q_melt = delmelt_veg_int.1*4)

## bin predictors over melt events ----

### bin melt rate ---- 

# note zeros are not included in binning, to add set inlcude.lowest = T
min_sm <- 0
max_sm <- round(
  max(obs_mod_met_melt$q_melt, na.rm = T),3)
sm_step <- 0.5

sm_breaks <- seq(
  0,
  max_sm+0.5,
  sm_step)

sm_labs_seq <- label_bin_fn(bins = sm_breaks)

stopifnot(tail(sm_breaks, 1) > max(obs_mod_met_melt$q_melt, na.rm = T))
stopifnot(length(sm_labs_seq) + 1 == length(sm_breaks))

obs_mod_met_melt$canopy_snowmelt_binned <- cut(obs_mod_met_melt[,'q_melt', drop = TRUE], sm_breaks)

obs_mod_met_melt$canopy_snowmelt_labs <- cut(obs_mod_met_melt[,'q_melt', drop = TRUE], 
                                        sm_breaks, 
                                        labels = sm_labs_seq)

obs_mod_met_melt$canopy_snowmelt_labs <- as.numeric(as.character(obs_mod_met_melt$canopy_snowmelt_labs))

### bin dimensionless snowmelt rate (s-1) ----

# remove kg m-2 from snowmelt rate to avoid multi collinerity with snow load

plot(obs_mod_met_melt$m_s_veg.1, obs_mod_met_melt$q_melt)

obs_mod_met_melt$q_diml_melt_veg <- obs_mod_met_melt$q_melt / obs_mod_met_melt$m_s_veg.1
obs_mod_met_melt$q_diml_melt_veg <- ifelse(obs_mod_met_melt$q_diml_melt_veg > 10, NA, obs_mod_met_melt$q_diml_melt_veg)
obs_mod_met_melt$q_diml_melt_veg <- ifelse(obs_mod_met_melt$q_diml_melt_veg < 0, NA, obs_mod_met_melt$q_diml_melt_veg)

plot(obs_mod_met_melt$m_s_veg.1[obs_mod_met_melt$m_s_veg.1 > 0 & obs_mod_met_melt$q_melt > 0], obs_mod_met_melt$q_diml_melt_veg[obs_mod_met_melt$m_s_veg.1 > 0 & obs_mod_met_melt$q_melt > 0])

plot(obs_mod_met_melt$q_diml_melt_veg, obs_mod_met_melt$q_unl)

# note zeros are not included in binning, to add set inlcude.lowest = T
min_sm <- 0
max_sm <- round(
  max(obs_mod_met_melt$q_diml_melt_veg, na.rm = T),3)
sm_step <- 0.25

sm_breaks <- seq(
  0,
  max_sm+0.5,
  sm_step)

sm_dml_labs_seq <- label_bin_fn(bins = sm_breaks)

stopifnot(tail(sm_breaks, 1) > max(obs_mod_met_melt$q_diml_melt_veg, na.rm = T))
stopifnot(length(sm_dml_labs_seq) + 1 == length(sm_breaks))

obs_mod_met_melt$canopy_snowmelt_dml_labs <- cut(obs_mod_met_melt[,'q_diml_melt_veg', drop = TRUE], 
                                          sm_breaks, 
                                          labels = sm_dml_labs_seq)

obs_mod_met_melt$canopy_snowmelt_dml_labs <- as.numeric(as.character(obs_mod_met_melt$canopy_snowmelt_dml_labs))
obs_mod_met_melt$canopy_snowmelt_dml_labs <- ifelse(obs_mod_met_melt$q_diml_melt_veg == 0, 0, obs_mod_met_melt$canopy_snowmelt_dml_labs )

### bin weighed tree ----

# note zeros are not included in binning, to add set inlcude.lowest = T
min_tree <- 0
max_tree <- round(
  max(obs_mod_met_melt$tree_mm, na.rm = T),0)
tree_step <- 2.5

tree_breaks <- seq(
  min_tree,
  max_tree+2,
  tree_step)

tree_breaks <- c(0, 2, 4,  8) # works well for tau but breaks sublimation fn


tree_labs_seq <- label_bin_fn(bins = tree_breaks)

stopifnot(tail(tree_breaks, 1) > max(obs_mod_met_melt$tree_mm, na.rm = T))
stopifnot(length(tree_labs_seq) + 1 == length(tree_breaks))

obs_mod_met_melt$tree_binned <- cut(obs_mod_met_melt[,'tree_mm', drop = TRUE], tree_breaks)

obs_mod_met_melt$tree_labs <- cut(obs_mod_met_melt[,'tree_mm', drop = TRUE], 
                          tree_breaks, 
                          labels = tree_labs_seq)

obs_mod_met_melt$tree_labs <- as.numeric(as.character(obs_mod_met_melt$tree_labs))

### temp (again because diff temp range over melt events) ---- 

# note zeros are not included in binning, to add set inlcude.lowest = T
min_temp <- round(
  min(obs_mod_met_melt$t, na.rm = T),3)
max_temp <- round(
  max(obs_mod_met_melt$t, na.rm = T),3)
temp_step <- 0.25

temp_breaks <- seq(
  min_temp,
  max_temp+temp_step,
  temp_step)

temp_labs_seq <- label_bin_fn(bins = temp_breaks)

stopifnot(tail(temp_breaks, 1) > max(obs_mod_met_melt$t, na.rm = T))
stopifnot(length(temp_labs_seq) + 1 == length(temp_breaks))

obs_mod_met_melt$temp_labs <- cut(obs_mod_met_melt[,'t', drop = TRUE], 
                             temp_breaks, 
                             labels = temp_labs_seq)

obs_mod_met_melt$temp_labs <- as.numeric(as.character(obs_mod_met_melt$temp_labs))

### ice bulb temp ---- 

# note zeros are not included in binning, to add set inlcude.lowest = T
min_temp <- round(
  min(obs_mod_met_melt$t_ice_bulb, na.rm = T),3)
max_temp <- round(
  max(obs_mod_met_melt$t_ice_bulb, na.rm = T),3)
temp_step <- 0.5

temp_breaks <- seq(
  min_temp,
  max_temp+temp_step,
  temp_step)

temp_labs_seq <- label_bin_fn(bins = temp_breaks)

stopifnot(tail(temp_breaks, 1) > max(obs_mod_met_melt$t_ice_bulb, na.rm = T))
stopifnot(length(temp_labs_seq) + 1 == length(temp_breaks))

obs_mod_met_melt$ice_temp_labs <- cut(obs_mod_met_melt[,'t_ice_bulb', drop = TRUE], 
                             temp_breaks, 
                             labels = temp_labs_seq)

obs_mod_met_melt$ice_temp_labs <- as.numeric(as.character(obs_mod_met_melt$ice_temp_labs))

### shear stress ----

min_mid_can_tau <- 0
max_mid_can_tau <- round(
  max(obs_mod_met_melt$tau, na.rm = T), 3)

mid_can_tau_step <- 0.01

mid_can_tau_breaks <- seq(
  min_mid_can_tau,
  max_mid_can_tau+0.01,
  mid_can_tau_step)

mid_can_tau_labs_seq <- label_bin_fn(bins = mid_can_tau_breaks)

stopifnot(tail(mid_can_tau_breaks, 1) > max(obs_mod_met_melt$tau, na.rm = T))
stopifnot(length(wind_labs_seq) + 1 == length(wind_breaks))

obs_mod_met_melt$tau_binned <- cut(obs_mod_met_melt[,'tau', drop = TRUE], mid_can_tau_breaks)

obs_mod_met_melt$tau_labs <- cut(obs_mod_met_melt[,'tau', drop = TRUE], 
                        mid_can_tau_breaks, 
                        labels = mid_can_tau_labs_seq
)

obs_mod_met_melt$tau_labs <- as.numeric(as.character(obs_mod_met_melt$tau_labs))
