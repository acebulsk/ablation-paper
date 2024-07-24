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

ffr_met <- 
  readRDS('../../analysis/met-data-processing/data/ffr_crhm_modelling_obs.rds')
met_unld <-
  readRDS('../../analysis/ablation/data/unloading_data_with_met_15min.rds') |> 
  rename(event_id = storm_id)
q_unld_tree <-
  readRDS('../../analysis/ablation/data/weighed_tree_ablation_15min.rds')
canopy_snow_events <- 
  read.csv('../../analysis/ablation/data/snow_in_canopy_post_snowfall.csv') |> 
  mutate(from =  as.POSIXct(from, tz = 'Etc/GMT+6'),
         to = as.POSIXct(to, tz = 'Etc/GMT+6'),
         storm_id = as.Date(from, tz = 'Etc/GMT+6')) |> filter(quality < 3)

to_long <- function(from, to, class, quality, notes, storm_id){
  datetime <- seq(from, to, 900)
  
  out <- data.frame(datetime, class, quality, notes, storm_id)
  
  return(out)
}

# these are periods where we know snow is in the canopy and unloading is possible
canopy_snow_long <- purrr::pmap_dfr(canopy_snow_events, to_long)
canopy_snow_long$storm_id <- as.Date(canopy_snow_long$storm_id)