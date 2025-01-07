library(tidyverse)

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

to_long_tb <- function(unloading_start_date, end_date, event_id){
  datetime <- seq(unloading_start_date, end_date, 900)
  
  out <- data.frame(datetime, event_id)
  
  return(out)
}

# SETUP ----

# "#000000" "#E69F00" "#56B4E9" "#009E73" "#F0E442" "#0072B2" "#D55E00" "#CC79A7" "#999999"

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
  select(datetime, time_elapsed_event, ends_with('labs'))

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
  inner_join(events_fltr_long |> select(datetime, event_id, bad_troughs), by = 'datetime') |> 
  # remove some of the unloading obs where we observed one of the instruments to be faulty
  mutate(value_flag = name == bad_troughs) |> # this is not a bug!
  filter(!value_flag, 
         name != 'sparse_forest'
         )

q_unld_scl_cml_event <- q_unld_scl |> 
  group_by(event_id, name) |> 
  mutate(cml_unld = cumsum(dU))

recent_tree_files <- 
  list.files('../../analysis/crhm-analysis/output/weighed_tree_vector_based_new_ablation_psp',
             full.names = T)
crhm_output_tree <- CRHMr::readOutputFile(
  tail(recent_tree_files, n = 1),
  timezone = 'Etc/GMT+6'
)

recent_baseline_tree_files <- list.files('../../analysis/crhm-analysis/output/weighed_tree_vector_based_baseline_ablation',
                                         full.names = T)
baseline_crhm_output_tree <- CRHMr::readOutputFile(
  tail(recent_baseline_tree_files, n = 1),
  timezone = 'Etc/GMT+6'
)

recent_scl_files <- list.files('../../analysis/crhm-analysis/output', pattern = 'scl_avg_closed_mixed', full.names = T)
crhm_output_scl <- CRHMr::readOutputFile(
  tail(recent_scl_files, n = 1),
  timezone = 'Etc/GMT+6'
)

# # TREE ---- 
# 
# # Select at commit id f3997966636d6793ce255102afb1b97d5b983d15 on branch ablation-w-obs 
# # where I added the IRTC canopy snow temperature to calculate canopy snowmelt
# path <- "../../analysis/crhm-analysis/output/2024-11-29-16-00-19_weighed_tree_vector_based_new_ablation_psp_output.txt"
# crhm_output_tree <- CRHMr::readOutputFile(
#   path,
#   timezone = 'Etc/GMT+6'
# ) 
# 
# # Select at commit id f3997966636d6793ce255102afb1b97d5b983d15 on branch ablation-w-obs 
# # where I added the IRTC canopy snow temperature to calculate canopy snowmelt
# path <- "../../analysis/crhm-analysis/output/2024-11-29-16-00-19_weighed_tree_vector_based_new_ablation_psp_output.txt"
# crhm_output_tree <- CRHMr::readOutputFile(
#   path,
#   timezone = 'Etc/GMT+6'
# ) 
# 
# # Select at commit id 8078809ac98c015e84d2598e78359649cbebe1d6 on branch class-trials
# # after major PSP changes but without observations
# path <- "../../analysis/crhm-analysis/output/2024-11-29-16-16-39_weighed_tree_vector_based_new_ablation_psp_output.txt"
# crhm_output_tree <- CRHMr::readOutputFile(
#   path,
#   timezone = 'Etc/GMT+6'
# ) 
# 
# # SCL ----- 
# # Select at commit id 510f3540f0f186f311d20a98e96a941c75388fec
# # Corresponds to state where we had changed wind profile to logarithmic, snow surface temp, and nighttime cloudy longwave
# path <- "../../analysis/crhm-analysis/output/2024-11-28-12-41-20_scl_avg_closed_mixed_vector_based_new_ablation_psp_output.txt"
# crhm_output_scl <- CRHMr::readOutputFile(
#   path,
#   timezone = 'Etc/GMT+6'
# ) 
# 
# # Select at commit id 0721ffe6930da7191f506dd945bc9a8d937fa252
# # Prior to major PSP changes
# path <- "../../analysis/crhm-analysis/output/2024-11-27-16-01-03_scl_avg_closed_mixed_vector_based_new_ablation_psp_output.txt"
# crhm_output_scl <- CRHMr::readOutputFile(
#   path,
#   timezone = 'Etc/GMT+6'
# )

# this is already restricted to custom pre/post event periods 
q_subl <- 
  readRDS('../../analysis/ablation/data/obs_mod_canopy_load_and_sublimation_cnpy_snow.rds') 
obs_tree <-
  readRDS('../../analysis/ablation/data/unloading_events_zero_weighed_tree_mm_pre_post_cnpy_snow.rds') |> 
  select(datetime, obs_tree = value)

met_unld <- q_unld_scl |> 
  left_join(ft_met) |> 
  left_join(ft_met_binned) |> 
  left_join(q_subl)

# warm tree specific events

obs_tree_warm_events <-
  readRDS('data/clean-data/warm_tree_events_zero_weighed_tree_fsd_closed_0.88_kg_m2_post_cnpy_snow.rds')

# load tipping bucket data
tb_data <-
  readRDS('../../analysis/ablation/data/tipping-bucket-data/tipping_bucket_qaqc_rough_cml_long.rds')
tb_events <-
  read.csv('../../analysis/ablation/data/tipping_bucket_evets_post.csv', skip = 1) |>
  filter(quality < 3) |> 
  mutate(
    across(unloading_start_date:end_date, ~as.POSIXct(.x, tz = 'Etc/GMT+6')),
    event_id = as.Date(unloading_start_date, tz = 'Etc/GMT+6'))
tb_events_long <-
  purrr::pmap_dfr(tb_events |>
                    select(c(unloading_start_date, end_date, event_id)),
                  to_long_tb)

tb_data_zeroed <- tb_data |> 
  left_join(tb_events_long) |> 
  group_by(event_id, name)

  
