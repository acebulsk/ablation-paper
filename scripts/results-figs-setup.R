library(tidyverse)

# SETUP ----

int_fig_width <- 6
int_fig_height <- 4
palette.colors(palette = "Okabe-Ito")

## axis labels ----
temp_bin_ax_lab <- 'Air Temperature Bins (°C)'
bin_unl_ax_lab <- "Unloading + Drip Rate (mm/hr)"
bin_wnd_ax_lab <- "Wind Speed Bins (m/s)"

## thresholds and filters ----
min_canopy_snow <- 1.5 # min mm in weighed tree for ablation analysis
min_qunld <- 0.025 # changes below this appear to be due to noise
manual_t_ice_th <- -7.5 # observed in the trough data for inc in unloading above ice bulb temp of -6 
manual_tau_th <- 0.05 # observed increase in trough unloading above this threshold
scl_names <- c('medium_density_forest', 'dense_forest') # removed sparse trough here because was obviously leaking

## load data ----

ffr_met_xtra <- 
  readRDS('../../analysis/met-data-processing/data/1000x_wnd_irtc.rds') |> 
  mutate(wind_flag = ifelse(USWindSpeed_Max > 10, T, F),
         USWindSpeed_Max = ifelse(wind_flag, NA, USWindSpeed_Max),
         USWindSpeed_Std = ifelse(wind_flag, NA, USWindSpeed_Std))

ffr_met <- 
  readRDS('../../analysis/met-data-processing/data/ffr_crhm_modelling_obs.rds') |> 
  left_join(ffr_met_xtra |> select(datetime, u_max = USWindSpeed_Max, u_std = USWindSpeed_Std))

met_unld <-
  readRDS('../../analysis/ablation/data/unloading_data_with_met_15min.rds')

q_unld_tree <-
  readRDS('../../analysis/ablation/data/weighed_tree_ablation_15min.rds')
canopy_snow_events <- 
  read.csv('../../analysis/ablation/data/snow_in_canopy_post_snowfall.csv') |> 
  mutate(from =  as.POSIXct(from, tz = 'Etc/GMT+6'),
         to = as.POSIXct(to, tz = 'Etc/GMT+6'),
         event_id = as.Date(from, tz = 'Etc/GMT+6')) 
events_fltr <- canopy_snow_events |> 
  filter(quality < 3, # these have all around bad ablation data
         weighed_tree_quality < 3) |>  # these have bad weighed tree observations and thus do not have sublimation measurements for the following paritioning equations, useful to look at these events later on when just looking at the rates
  pull(event_id)