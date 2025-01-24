# Load required inputs and data for CRHM analysis
library(tidyverse)

# SETUP ----

to_long_tb <- function(unloading_start_date, end_date, event_id){
  datetime <- seq(unloading_start_date, end_date, 900)
  
  out <- data.frame(datetime, event_id)
  
  return(out)
}

load_suffix <- 'fsd_closed_0.88'
# load_suffix <- 'fsd_cal_for_each_trough' # use this one if needing to compare the weighed tree to the troughs, mainly just used for the wind unloading analysis

# LOAD DATA ---- 

# recent_tree_files <- 
#   list.files('../../analysis/crhm-analysis/output/weighed_tree_vector_based_new_ablation_psp',
#              full.names = T)
# crhm_output_tree <- CRHMr::readOutputFile(
#   tail(recent_tree_files, n = 1),
#   timezone = 'Etc/GMT+6'
# )
# 
# recent_baseline_tree_files <- list.files('../../analysis/crhm-analysis/output/weighed_tree_vector_based_baseline_ablation',
#                                          full.names = T)
# baseline_crhm_output_tree <- CRHMr::readOutputFile(
#   tail(recent_baseline_tree_files, n = 1),
#   timezone = 'Etc/GMT+6'
# )
# 
# recent_scl_files <- list.files('../../analysis/crhm-analysis/output', pattern = 'scl_avg_closed_mixed', full.names = T)
# crhm_output_scl <- CRHMr::readOutputFile(
#   tail(recent_scl_files, n = 1),
#   timezone = 'Etc/GMT+6'
# )

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
# q_subl <- 
#   readRDS('data/clean-data/obs_mod_canopy_load_and_sublimation_cnpy_snow.rds') |> 
#   select()
obs_tree <-
  readRDS(paste0(
    'data/clean-data/unloading_events_zero_weighed_tree_kg_m2_pre_post_cnpy_snow_',
    load_suffix,
    '.rds'
  )) 

q_unld_met_scl <- readRDS('data/clean-data/unloading_data_with_met_15min.rds')

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

# load tipping bucket data
tb_data <-
  readRDS('../../analysis/ablation/data/tipping-bucket-data/tipping_bucket_qaqc_rough_cml_long.rds')
tb_events <-
  read.csv('../../analysis/ablation/data/tipping_bucket_evets_post.csv', skip = 1) |>
  filter(quality < 3) |> 
  mutate(
    unloading_start_date = as.POSIXct(unloading_start_date, tz = 'Etc/GMT+6'),
    end_date = as.POSIXct(end_date, tz = 'Etc/GMT+6'),
    event_id = as.Date(unloading_start_date, tz = 'Etc/GMT+6'))
tb_events_long <-
  purrr::pmap_dfr(tb_events |>
                    select(c(unloading_start_date, end_date, event_id)),
                  to_long_tb)

tb_data_zeroed <- tb_data |> 
  left_join(tb_events_long) |> 
  group_by(event_id, name)
