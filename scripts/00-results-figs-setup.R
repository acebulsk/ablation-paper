library(tidyverse)

# functions ----

to_long <- function(from,
                    to,
                    class,
                    quality,
                    bad_troughs,
                    weighed_tree_quality,
                    notes,
                    event_id) {
  datetime <- seq(from, to, 900)
  
  out <- data.frame(datetime,
                    class,
                    quality,
                    bad_troughs,
                    weighed_tree_quality,
                    notes,
                    event_id)
  
  return(out)
}

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

ft_met_xtra <- 
  readRDS('../../analysis/met-data-processing/data/1000x_wnd_irtc.rds') |> 
  mutate(wind_flag = ifelse(USWindSpeed_Max > 10, T, F),
         USWindSpeed_Max = ifelse(wind_flag, NA, USWindSpeed_Max),
         USWindSpeed_Std = ifelse(wind_flag, NA, USWindSpeed_Std))

ft_met <- 
  readRDS('../../analysis/met-data-processing/data/ffr_crhm_modelling_obs.rds') |> 
  left_join(ft_met_xtra |> select(datetime, u_max = USWindSpeed_Max, u_std = USWindSpeed_Std))

ft_met_binned <- 
  readRDS('../../analysis/ablation/data/met_binned_for_unloading_analysis.rds') |> 
  ungroup() |> 
  select(datetime, ends_with('labs'))

canopy_snow_events <- 
  read.csv('../../analysis/ablation/data/snow_in_canopy_post_snowfall.csv') |> 
  mutate(from =  as.POSIXct(from, tz = 'Etc/GMT+6'),
         to = as.POSIXct(to, tz = 'Etc/GMT+6'),
         event_id = as.Date(from, tz = 'Etc/GMT+6')) 

events_fltr <- canopy_snow_events |> 
  filter(quality < 3) # these have all around bad ablation data

events_fltr_long <-
  purrr::pmap_dfr(events_fltr, to_long) |> 
  select(-quality)

q_unld_tree <-
  readRDS('../../analysis/ablation/data/weighed_tree_ablation_15min.rds') |> 
  left_join(events_fltr_long |> select(datetime, weighed_tree_quality, notes), by = 'datetime') |> 
  filter(weighed_tree_quality < 3)

q_unld_scl <- 
  readRDS('../../analysis/ablation/data/ft_lysimeter_data_del_15_min.rds') |> 
  filter(name != 'tree_mm') |> 
  select(-inst_type) |> 
  inner_join(events_fltr_long |> select(datetime, bad_troughs), by = 'datetime') |> 
  # remove some of the unloading obs where we observed one of the instruments to be faulty
  mutate(value_flag = name == bad_troughs) |> 
  filter(!value_flag, 
         name != 'sparse_forest'
         )

# this is already restricted to custom pre/post event periods 
q_subl <- 
  readRDS('../../analysis/ablation/data/obs_mod_canopy_load_and_sublimation_cnpy_snow.rds') 
  
met_unld <- q_unld_scl |> 
  left_join(ft_met) |> 
  left_join(ft_met_binned) |> 
  left_join(q_subl)

  
