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
run_tag <- "updt_canopy_unloading_wind_fn_pars"

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
  filter(is.na(q_unl) == F) |> 
  select(datetime, event_id, tree_mm, dL)

obs_mod <- left_join(w_tree_q_unld_15, crhm_output) |> 
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
  filter(canopy_snowmelt.1 > 0#,
         # hru_u.1 < 0.5
         ) |> 
  # convert mm/interval to mm/hour
  mutate(est_q_unld_melt = est_q_unld_melt*4,
         canopy_snowmelt.1 = canopy_snowmelt.1*4)

q_unld_melt_lm <- lm(est_q_unld_melt ~ 0 + canopy_snowmelt.1, data = obs_mod_fltr)
obs_mod_fltr$mod_q_unld_melt <- predict(q_unld_melt_lm)
saveRDS(q_unld_melt_lm, 'data/lm_q_drip_vs_q_unld_melt.rds')
# Extract the coefficient (slope) from the model
slope <- coef(q_unld_melt_lm)[1]

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
