# This script analyzes the relationship between observed weighed tree ablation
# due to mass unloading of snow as it melts with canopy snowmelt and canopy load.
# Canopy snow unloading is determined as residual ablation after removing
# q_subl^veg, q_drip, q_unld^wind

# the theory here is that we could either use snowmelt as the predictor as in
# the other script or try and recreate this again here using temp and snow load.
# somewhat justified since snowmelt is a function of air temperature and canopy
# snow load anyway.

library(tidyverse)

load_suffix <- 'fsd_closed_0.88'

# LOAD DATA ----

## model ---- 

# Select model run with all unloading events weighed tree snow load assimilated
prj <- "ffr_closed_canopy_cc0.88_vector_based_new_ablation_psp"

# specify certain model run
run_tag <- "updated_q_unld_wind_pars"

path <- list.files(
  paste0(
    "../../analysis/crhm-analysis/output/",
    prj
  ),
  pattern = run_tag,
  full.names = T
)

crhm_output <- CRHMr::readOutputFile(
  path,
  timezone = 'Etc/GMT+6') |> 
  select(datetime, hru_t.1, hru_u.1, hru_p.1,
         Subl_Cpy.1:Snow_load.1)

### bin snowmelt rate (s-1) ----

# note zeros are not included in binning, to add set inlcude.lowest = T
min_sm <- 0
max_sm <- round(
  max(crhm_output$canopy_snowmelt.1*4, na.rm = T),3)
sm_step <- 0.2

sm_breaks <- seq(
  0.2,
  max_sm+0.02,
  sm_step)

sm_breaks <- c(0, 0.05, 0.1, 0.15, sm_breaks, 1.55)
sm_labs_seq <- label_bin_fn(bins = sm_breaks)

stopifnot(tail(sm_breaks, 1) > max(crhm_output$canopy_snowmelt.1*4, na.rm = T))
stopifnot(length(sm_labs_seq) + 1 == length(sm_breaks))

crhm_output$canopy_snowmelt_binned <- cut(crhm_output[,'canopy_snowmelt.1', drop = TRUE]*4, sm_breaks)

crhm_output$canopy_snowmelt_labs <- cut(crhm_output[,'canopy_snowmelt.1', drop = TRUE]*4, 
                                        sm_breaks, 
                                        labels = sm_labs_seq)

crhm_output$canopy_snowmelt_labs <- as.numeric(as.character(crhm_output$canopy_snowmelt_labs))

### bin temp  ----

# note zeros are not included in binning, to add set inlcude.lowest = T
min_temp <- 0
max_temp <- round(
  max(crhm_output$hru_t.1, na.rm = T),3)
temp_step <- 0.5

temp_breaks <- seq(
  0,
  max_temp+2,
  temp_step)

temp_labs_seq <- label_bin_fn(bins = temp_breaks)

stopifnot(tail(temp_breaks, 1) > max(crhm_output$hru_t.1, na.rm = T))
stopifnot(length(temp_labs_seq) + 1 == length(temp_breaks))

crhm_output$temp_binned <- cut(crhm_output[,'hru_t.1', drop = TRUE], temp_breaks)

crhm_output$temp_labs <- cut(crhm_output[,'hru_t.1', drop = TRUE], 
                             temp_breaks, 
                             labels = temp_labs_seq)

crhm_output$temp_labs <- as.numeric(as.character(crhm_output$temp_labs))

## obs ----

### FILTER EVENTS ---- 

# warm tree specific events
#' warm_events <- c(
#'   '2022-04-21',
#'   '2022-04-23',
#'   '2022-06-14',
#'   '2022-06-24',
#'   '2023-03-14',
#'   '2023-03-25',
#'   '2023-03-26',
#'   '2023-03-28',
#'   '2023-05-08',
#'   '2023-06-15',
#'   '2023-06-21'
#' )
#' 
#' obs_tree_warm <-
#'   readRDS(paste0(
#'     'data/clean-data/warm_tree_events_zero_weighed_tree_',
#'     load_suffix,
#'     '_kg_m2_post_cnpy_snow.rds'
#'   )) |> 
#'   filter(event_id %in% warm_events)
#' 
#' # cold tree events
#' cold_events <- c(
#'   '2022-03-02', 
#'   '2022-03-09',
#'   '2022-03-20', 
#'   '2022-03-24',  
#'   '2022-03-29',  
#'   '2022-12-01',
#'   '2023-01-28',
#'   '2023-02-24',
#'   '2023-02-26'
#'   #'2023-04-12' could add back if filter to start later.. also removed jjst so have clean 20 events
#' )
#' obs_tree_cold <-
#'   readRDS(paste0(
#'     'data/clean-data/all_tree_events_zero_weighed_tree_',
#'     load_suffix,
#'     '_kg_m2_post_cnpy_snow.rds'
#'   )) |> 
#'   filter(event_id %in% cold_events)

# obs_tree <- rbind(obs_tree_cold  |> 
#                     select(datetime, event_id, tree_mm),
#                   obs_tree_warm |> 
#                     select(datetime, event_id, tree_mm)) 
obs_tree <- q_unld_tree

# PLOT -----

w_tree_q_unld_15 <- obs_tree |>
  mutate(
    dL = lag(tree_mm) - tree_mm,
    dL = ifelse(dL < 0, 0, dL),
    q_unl = (dL / 15) * 60
  ) |>
  ungroup() |>
  filter(is.na(q_unl) == F,
         dL > 0) |> 
  select(datetime, event_id, tree_mm, dL)

obs_mod <- left_join(w_tree_q_unld_15, crhm_output, by = 'datetime') |> 
  mutate(est_q_unld_melt = dL - Subl_Cpy.1 - SUnloadWind.1 - canopy_snowmelt.1) 

# to hourly
# obs_mod <- obs_mod |> 
#   mutate(
#     datetime_hour = datetime - (15*60), # shift so datetime corresponds to start of measurement interval, this is to avoid measurements with the 00:00 timestamp which correspond to the average from 23:45 to 00:00 to be assigned incorrectly to the next day.
#     datetime_hour = format(datetime_hour, '%Y-%m-%d %H:00:00'),
#     datetime_hour = as.POSIXct(datetime_hour, tz = 'Etc/GMT+6'),
#     datetime_hour = datetime_hour + 60*60 
#     # datetime = lubridate::ceiling_date(datetime, unit = 'hour') # checked and this is the same as above
#   ) |> 
#   group_by(datetime_hour) |> 
#   mutate(n = n()) |> 
#   filter(n == 4) |> 
#   summarise(
#     tree_mm = nth(tree_mm, which.max(datetime)), # Get tree_mm with the largest datetime    t = mean(t),
#     dL = sum(dL),
#     hru_u.1 = mean(hru_u.1),
#     Subl_Cpy.1 = sum(Subl_Cpy.1),
#     SUnload.1 = sum(SUnload.1),
#     canopy_snowmelt.1 = sum(canopy_snowmelt.1),
#     SUnloadMelt.1 = sum(SUnloadMelt.1),
#     SUnloadWind.1 = sum(SUnloadWind.1)
#     ) |> 
#   as.data.frame() |> 
#   mutate(est_q_unld_melt = dL - Subl_Cpy.1 - SUnloadWind.1 - canopy_snowmelt.1) 

obs_mod_fltr <- obs_mod |> 
  filter(canopy_snowmelt.1 > 0,
         est_q_unld_melt >= 0
  ) |> 
  # convert mm/interval to mm/hour
  mutate(est_q_unld_melt = est_q_unld_melt*4,
         canopy_snowmelt.1 = canopy_snowmelt.1*4) |> 
  mutate(`Canopy Load (mm)` = case_when(
    tree_mm < 2 ~ 'Light',
    # tree_mm >= 5 & tree_mm <= 10 ~ 'Med',
    is.na(tree_mm) ~ 'NA',
    TRUE  ~ 'Heavy'
  )) |> group_by(`Canopy Load (mm)`) |> 
  mutate(avg_w_tree = mean(tree_mm))

obs_mod_fltr_avg <- obs_mod_fltr |> 
  group_by(`Canopy Load (mm)`, avg_w_tree, canopy_snowmelt_labs) |> 
  summarise(q_unl_avg = mean(est_q_unld_melt, na.rm = T),
            q_unl_sd = sd(est_q_unld_melt, na.rm = T),
            sd_low = ifelse((q_unl_avg - q_unl_sd)<0,0, q_unl_avg - q_unl_sd),
            sd_hi = q_unl_avg + q_unl_sd,
            n = n()) |> 
  filter(n >= 3,
    !is.na(`Canopy Load (mm)`),
    !is.na(avg_w_tree))

ggplot(obs_mod_fltr_avg) + 
  geom_point(aes(canopy_snowmelt_labs, q_unl_avg, colour = factor(avg_w_tree))) +
  geom_errorbar(aes(
    x = canopy_snowmelt_labs, 
    ymax = sd_hi,
    ymin = sd_low, colour = factor(avg_w_tree)), width = 0.05) +
  geom_point(
    data = obs_mod_fltr,
    aes(canopy_snowmelt.1, est_q_unld_melt),
    alpha = 0.1,
    size = 0.5
  ) + scale_color_viridis_d() 

lm_model_sm <- lm(est_q_unld_melt ~ 0 + canopy_snowmelt.1, data = obs_mod_fltr)
obs_mod_fltr$pred_sm <- predict(lm_model_sm)

lm_model_sm_L <- lm(est_q_unld_melt ~ 0 + canopy_snowmelt.1:avg_w_tree, data = obs_mod_fltr)
obs_mod_fltr$pred_sm_L <- predict(lm_model_sm_L)

lm_model_sm_L_bins <- lm(q_unl_avg ~ 0 + canopy_snowmelt_labs:avg_w_tree, data = obs_mod_fltr_avg)
a_sm <- coef(lm_model_sm_L_bins)[1]
obs_mod_fltr$pred_sm_L_bins <-
  obs_mod_fltr$canopy_snowmelt.1 * obs_mod_fltr$tree_mm * a_sm
saveRDS(lm_model_sm_L_bins, 'data/lm_q_drip_vs_q_unld_melt_load.rds')

# Non-linear saturation (Michaelis-Menten type) model function incorporating avg_w_tree
# y = (a * avg_w_tree * x) / (b + x)
nls_model <- minpack.lm::nlsLM(
  q_unl_avg ~ (a * avg_w_tree * canopy_snowmelt_labs) / (b + canopy_snowmelt_labs), 
  data = obs_mod_fltr_avg, 
  weights = n,  # Using sample size as weights
  start = list(
    a = max(data$q_unl_avg) / max(data$avg_w_tree), 
    b = 0.1
  ),
  control = list(maxiter = 1000)
)
nls_model_coefs <- coef(nls_model)
obs_mod_fltr$pred_nls <-
  nls_model_coefs[['a']] * obs_mod_fltr$tree_mm * obs_mod_fltr$canopy_snowmelt.1 / (nls_model_coefs[['a']] + obs_mod_fltr$canopy_snowmelt.1)

obs_mod_fltr_long <- pivot_longer(obs_mod_fltr, starts_with('pred'))

ggplot(obs_mod_fltr_long) +
  geom_point(
    aes(canopy_snowmelt.1, est_q_unld_melt),
    alpha = 0.1
  ) +
  geom_line(aes(canopy_snowmelt.1, value, colour = name))

ggplot(obs_mod_fltr_long) +
  geom_point(
    aes(est_q_unld_melt, value),
    alpha = 0.1
  ) +
  geom_abline() + 
  facet_wrap(~name)

lm_error_metrics <-   
  obs_mod_fltr_long |> 
  mutate(diff = est_q_unld_melt - value) |> 
  group_by(name) |>
  summarise(
    # runtag = run_tag_updt,
    MB = mean(diff, na.rm = T),
    MAE = mean(abs(diff), na.rm = T),
    RMSE = sqrt(mean(diff ^ 2, na.rm = T)),
    # NRMSE = `RMS Error` / (max(observed, na.rm = TRUE) - min(observed, na.rm = TRUE)),
    NRMSE = RMSE / mean(est_q_unld_melt, na.rm = T),
    R = cor(est_q_unld_melt, value),
    R2 = R^2) |> 
  mutate(across(MB:R2, round, digits = 3))
lm_error_metrics

# Create new data for predictions
new_data <- expand.grid(
  avg_w_tree = c(unique(obs_mod_fltr_avg$avg_w_tree)),
  canopy_snowmelt_labs = seq(
    0,
    max(obs_mod_fltr$canopy_snowmelt_labs),
    length.out = 100
  )
)

# Predict values
new_data$q_unl_pred <- predict(lm_model_sm_L_bins, newdata = new_data)

ggplot(obs_mod_fltr_avg) + 
  geom_point(aes(canopy_snowmelt_labs, q_unl_avg, colour = factor(round(avg_w_tree)))) +
  geom_errorbar(aes(
    x = canopy_snowmelt_labs, 
    ymax = sd_hi,
    ymin = sd_low, colour = factor(round(avg_w_tree))), width = 0.025) +
  geom_point(
    data = obs_mod_fltr,
    aes(canopy_snowmelt.1, est_q_unld_melt),
    alpha = 0.1,
    size = 0.5
  ) + 
  geom_line(
    data = new_data,
    aes(canopy_snowmelt_labs, q_unl_pred, colour = factor(round(avg_w_tree)))
  ) +
  labs(
    colour = expression(atop("Canopy Snow", "Load (kg m"^-2*")")),
    x = expression("Simulated Canopy Snowmelt Rate ("*kg ~ m^-2 ~ hr^-1*")"),
    # y = expression(q[unld]^{melt} ~ "(" ~ kg ~ m^-2 ~ hr^-1 ~ ")"),
    y = expression("Melt Unloading Rate ("*kg ~ m^-2 ~ hr^-1*")")
  ) 
ggsave(
  'figs/results/modelled_melt_load_unloading_w_obs.png',
  width = 5.5,
  height = 4,
  device = png
)

# Predict values NON LINEAR
new_data$q_unl_pred <- predict(nls_model, newdata = new_data)

ggplot(obs_mod_fltr_avg) + 
  geom_point(aes(canopy_snowmelt_labs, q_unl_avg, colour = factor(avg_w_tree))) +
  geom_errorbar(aes(
    x = canopy_snowmelt_labs, 
    ymax = sd_hi,
    ymin = sd_low, colour = factor(avg_w_tree)), width = 0.05) +
  geom_point(
    data = obs_mod_fltr,
    aes(canopy_snowmelt.1, est_q_unld_melt),
    alpha = 0.1,
    size = 0.5
  ) + 
  geom_line(
    data = new_data,
    aes(canopy_snowmelt_labs, q_unl_pred, colour = factor(avg_w_tree))
  )

# Plot
ggplot(obs_mod_fltr_avg, aes(x = canopy_snowmelt_labs, y = q_unl_avg, color = factor(avg_w_tree))) +
  geom_point(size = 3) +
  geom_line(data = new_data, aes(y = q_unl_pred), linetype = "solid") +
  labs(x = "Canopy Snowmelt Labels", y = "q_unl_avg",
       title = "Single Linear Model Fit",
       color = "Avg W Tree") +
  theme_minimal()

ggplot(obs_mod_fltr_avg) + 
  geom_point(aes(canopy_snowmelt_labs, q_unl_avg, colour = factor(avg_w_tree))) +
  geom_errorbar(aes(
    x = canopy_snowmelt_labs, 
    ymax = sd_hi,
    ymin = sd_low, 
    colour = factor(avg_w_tree)
    
  ), width = 0.05) +
  geom_point(
    data = obs_mod_fltr,
    aes(canopy_snowmelt.1, est_q_unld_melt),
    alpha = 0.1,
    colour = 'black'
  )

q_unld_melt_lm <- lm(est_q_unld_melt ~ 0 + canopy_snowmelt.1, data = obs_mod_fltr)
obs_mod_fltr$mod_q_unld_melt <- predict(q_unld_melt_lm)
saveRDS(q_unld_melt_lm, 'data/lm_q_drip_vs_q_unld_melt.rds')
# Extract the coefficient (slope) from the model
slope <- coef(q_unld_melt_lm)[1]


## ERROR TABLE ----

source('../../../Documents/code/stats/lm-through-the-origin/example-r2-from-lm-through-the-origin.R')

## first adjust the R2 

df_r2_adj <- r_squared_no_intercept(q_unld_melt_lm)

obs_mod_fltr_err_tbl <- obs_mod_fltr |> 
  mutate(diff = est_q_unld_melt - mod_q_unld_melt) |> 
  # group_by(avg_w_tree) |> 
  summarise(
    `Mean Bias` = mean(diff, na.rm = T),
    # `Max Error` = diff[which.max(abs(diff))],
    MAE = mean(abs(diff), na.rm = T),
    `RMS Error` = sqrt(mean(diff ^ 2, na.rm = T))) |> 
  # left_join(coefs_df, by = c('plot_name', 'name')) |> 
  # left_join(df_r2_adj, by = c('plot_name', 'name')) |> 
  select(
    `Mean Bias`,
    MAE,
    `RMS Error`
  ) |> 
  mutate(across(`Mean Bias`:`RMS Error`, round, digits = 3),
         R2 = df_r2_adj)

saveRDS(obs_mod_fltr_err_tbl,
        'data/modelled_melt_unloading_error_table.rds')


# Create observed vs predicted plot
ggplot(obs_mod_fltr, aes(x = est_q_unld_melt, y = mod_q_unld_melt)) +
  geom_point(color = "blue", size = 2) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(
    title = "Observed vs Predicted",
    x = "Observed Values",
    y = "Predicted Values"
  ) +
  theme_minimal()
