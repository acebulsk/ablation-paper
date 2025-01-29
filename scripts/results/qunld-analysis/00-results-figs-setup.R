library(tidyverse)
options(ggplot2.discrete.colour= palette.colors(palette = "R4")[2:6])

# functions ----

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

# SETUP ----

# "#000000" "#E69F00" "#56B4E9" "#009E73" "#F0E442" "#0072B2" "#D55E00" "#CC79A7" "#999999"

int_fig_width <- 6
int_fig_height <- 4
palette.colors(palette = "Okabe-Ito")

## axis labels ----
temp_bin_ax_lab <- 'Air Temperature Bins (°C)'
bin_unl_ax_lab <- "Unloading Rate (mm/hr)"
bin_unl_drip_ax_lab <- "Unloading + Drip Rate (mm/hr)"
bin_wnd_ax_lab <- "Wind Speed Bins (m/s)"

## thresholds and filters ----
snow_load_th <- 7
min_canopy_snow <- 1.5 # min mm in weighed tree for ablation analysis
min_qunld <- 0.025 # changes below this appear to be due to noise
manual_t_ice_th <- -7.5 # observed in the trough data for inc in unloading above ice bulb temp of -6 
manual_tau_th <- 0.05 # observed increase in trough unloading above this threshold
scl_names <- c('mixed', 'closed') # removed sparse trough here because was obviously leaking

## load data ----

ft_met <- 
  readRDS('../../analysis/met-data-processing/data/ffr_crhm_modelling_obs.rds') 

pwl_met <- 
  readRDS('../../analysis/met-data-processing/data/pwl_crhm_modelling_obs.rds') 

ft_met_binned <- 
  readRDS('data/clean-data/met_binned_for_unloading_analysis.rds') |> 
  ungroup()

canopy_snow_events <- 
  read.csv('data/raw-data/snow_in_canopy_post_snowfall.csv') |> 
  mutate(from =  as.POSIXct(from, tz = 'Etc/GMT+6'),
         to = as.POSIXct(to, tz = 'Etc/GMT+6'),
         event_id = as.Date(from, tz = 'Etc/GMT+6')) 

events_fltr <- canopy_snow_events |> 
  filter(quality < 3) # these have all around bad ablation data

events_fltr_long <-
  purrr::pmap_dfr(events_fltr, to_long) |> 
  select(-quality)

q_unld_tree <-
  readRDS('data/clean-data/ft_w_tree_data_del_15_min.rds') |> 
  left_join(events_fltr_long |> select(datetime, weighed_tree_quality, notes), by = 'datetime') |> 
  filter(weighed_tree_quality < 3)

q_unld_scl <- 
  readRDS('data/clean-data/ft_scl_data_del_15_min.rds') |> 
  inner_join(events_fltr_long |> select(datetime, event_id, bad_troughs), by = 'datetime') |> 
  # remove some of the unloading obs where we observed one of the instruments to be faulty
  mutate(value_flag = name == bad_troughs) |> # this is not a bug!
  filter(!value_flag)

q_unld_scl_cml_event <- q_unld_scl |> 
  group_by(event_id, name) |> 
  mutate(cml_unld = cumsum(dU))

q_unld_met_scl <- readRDS('data/clean-data/unloading_data_with_met_15min.rds')



  
