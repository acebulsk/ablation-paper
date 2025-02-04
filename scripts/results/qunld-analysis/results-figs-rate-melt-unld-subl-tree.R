# This script analyzes the relationship between observed weighed tree ablation
# due to mass unloading of snow as it melts with simulated canopy snowmelt.
# Canopy snow unloading is determined as residual ablation after removing
# q_subl^veg, q_drip, q_unld^wind

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
         Subl_Cpy.1:SUnloadWind.1)

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

## obs ----

### all events snow in canopy ----
# obs_tree <-
#   readRDS(paste0(
#     'data/clean-data/unloading_events_zero_weighed_tree_kg_m2_pre_post_cnpy_snow_',
#     load_suffix,
#     '.rds'
#   )) |>  filter(name == 'weighed tree')

# warm tree specific events
warm_events <- c(
  '2022-04-21',
  '2022-04-23',
  '2022-06-14',
  '2022-06-24',
  '2023-03-14',
  '2023-03-25',
  '2023-03-26',
  '2023-03-28',
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

# cold tree events
cold_events <- c(
  '2022-03-02', 
  '2022-03-09',
  '2022-03-20', 
  '2022-03-24',  
  '2022-03-29',  
  '2022-12-01',
  '2023-01-28',
  '2023-02-24',
  '2023-02-26'
  #'2023-04-12' could add back if filter to start later.. also removed jjst so have clean 20 events
)
obs_tree_cold <-
  readRDS(paste0(
    'data/clean-data/all_tree_events_zero_weighed_tree_',
    load_suffix,
    '_kg_m2_post_cnpy_snow.rds'
  )) |> 
  filter(event_id %in% cold_events)

obs_tree <- rbind(obs_tree_cold  |> 
                    select(datetime, event_id, tree_mm),
                  obs_tree_warm |> 
                    select(datetime, event_id, tree_mm)) 

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

obs_mod <- left_join(w_tree_q_unld_15, crhm_output) |> 
  mutate(est_q_unld_melt = dL - Subl_Cpy.1 - SUnloadWind.1 - canopy_snowmelt.1) 

obs_mod_fltr <- obs_mod |> 
  filter(canopy_snowmelt.1 > 0,
         est_q_unld_melt >= 0,
         # hru_u.1 < 0.5
  ) |> 
  # convert mm/interval to mm/hour
  mutate(est_q_unld_melt = est_q_unld_melt*4,
         canopy_snowmelt.1 = canopy_snowmelt.1*4) |> 
  mutate(`Canopy Load (mm)` = case_when(
    tree_mm < 10 ~ 'Light',
    # tree_mm >= 5 & tree_mm <= 10 ~ 'Med',
    is.na(tree_mm) ~ 'NA',
    TRUE  ~ 'Heavy'
  )) |> group_by(`Canopy Load (mm)`) |> 
  mutate(avg_w_tree = mean(tree_mm))


obs_mod_fltr |> 
  group_by(`Canopy Load (mm)`) |> 
  summarise(avg_w_tree = mean(tree_mm))

obs_mod_fltr_avg <- obs_mod_fltr |> 
  group_by(canopy_snowmelt_labs, `Canopy Load (mm)`, avg_w_tree) |> 
  summarise(q_unl_avg = mean(est_q_unld_melt, na.rm = T),
            q_unl_sd = sd(est_q_unld_melt, na.rm = T),
            sd_low = ifelse((q_unl_avg - q_unl_sd)<0,0, q_unl_avg - q_unl_sd),
            sd_hi = q_unl_avg + q_unl_sd,
            n = n()) |> 
  filter(#n >= 3,
         !is.na(`Canopy Load (mm)`),
         !is.na(avg_w_tree))

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

ggplot(obs_mod_fltr) +
  geom_point(
    aes(canopy_snowmelt.1, est_q_unld_melt),
    alpha = 0.1) 
  

linear_model <- lm(est_q_unld_melt ~ 0 + canopy_snowmelt.1, data = obs_mod_fltr)
quadratic_model <- lm(est_q_unld_melt ~ 0 + I(canopy_snowmelt.1 * Subl_Cpy.1 * tree_mm), 
                 data = obs_mod_fltr)
# quadratic_model <- lm(est_q_unld_melt ~ 0 + canopy_snowmelt.1 + I(canopy_snowmelt.1^2), data = obs_mod_fltr)
exp_model <- nls(
  est_q_unld_melt ~ tree_mm * canopy_snowmelt.1 * Subl_Cpy.1* a * exp(b * canopy_snowmelt.1) * exp(c * Subl_Cpy.1),
  data = obs_mod_fltr,
  start = list(a = 1, b = 0.1, c = 0.1)
)

rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

# Linear Model
linear_pred <- predict(linear_model)
linear_rmse <- rmse(obs_mod_fltr$est_q_unld_melt, linear_pred)
linear_r2 <- summary(linear_model)$r.squared

# Quadratic Model
quadratic_pred <- predict(quadratic_model)
quadratic_rmse <- rmse(obs_mod_fltr$est_q_unld_melt, quadratic_pred)
quadratic_r2 <- summary(quadratic_model)$r.squared

# Exponential Model
exp_pred <- predict(exp_model)
exp_rmse <- rmse(obs_mod_fltr$est_q_unld_melt, exp_pred)
# For nls, R2 isn't available directly, but we can compute it manually
ss_total <- sum((obs_mod_fltr$est_q_unld_melt - mean(obs_mod_fltr$est_q_unld_melt))^2)
ss_residual <- sum((obs_mod_fltr$est_q_unld_melt - exp_pred)^2)
exp_r2 <- 1 - (ss_residual / ss_total)

comparison <- data.frame(
  Model = c("Linear", "Quadratic", "Exponential"),
  R2 = c(linear_r2, quadratic_r2, exp_r2),
  RMSE = c(linear_rmse, quadratic_rmse, exp_rmse)
)

print(comparison)

library(ggplot2)

# Create a data frame with the predictions for plotting
plot_data <- data.frame(
  tree_mm = obs_mod_fltr$tree_mm,
  canopy_snowmelt = obs_mod_fltr$canopy_snowmelt.1,
  actual = obs_mod_fltr$est_q_unld_melt,
  linear = linear_pred,
  quadratic = quadratic_pred,
  exp = exp_pred
)

ggplot(plot_data, aes(x = canopy_snowmelt)) +
  geom_point(aes(y = actual, colour = tree_mm)) + 
  geom_line(aes(y = linear), color = "blue", linetype = "dashed") + 
  geom_line(aes(y = quadratic), color = "red", linetype = "dashed") + 
  geom_line(aes(y = exp), color = "purple", linetype = "dashed") + 
  labs(title = "Model Comparison", x = "Canopy Snowmelt", y = "Unloading") +
  theme_minimal()


# Create the plot
ggplot(obs_mod_fltr, aes(x = canopy_snowmelt.1, y = est_q_unld_melt)) +
  geom_point(color = "black", size = 2) +                      # Scatter plot of the data
  geom_abline(intercept = 0, slope = slope, color = "red",    # Model line
              linetype = "solid", size = 0.5) +
  labs(
    x = expression("Simulated Canopy Snowmelt Rate ("*kg ~ m^-2 ~ hr^-1*")"),
    # y = expression(q[unld]^{melt} ~ "(" ~ kg ~ m^-2 ~ hr^-1 ~ ")"),
    y = expression("Melt Unloading Rate ("*kg ~ m^-2 ~ hr^-1*")")
  ) 

ggsave(
  'figs/results/modelled_melt_unloading_w_obs.png',
  width = 4,
  height = 4,
  device = png
)

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
