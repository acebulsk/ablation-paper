# Load required inputs and data for CRHM analysis
library(tidyverse)

options(ggplot2.discrete.colour= palette.colors(palette = "R4"))
# options(ggplot2.discrete.fill= palette.colors(palette = "R4")[2:6])

bad_events <- c(
  '2022-06-23', # crhm rain to snow paritioning is off here
  '2023-04-13',
  # '2022-06-24', # crhm rain to snow paritioning is off here
  '2023-03-25', # confirmed obs are good for this day but not getting the unloading right, maybe temp induced unloading due to slight melt, on part of the canopy unloading a lot of snow
  '2023-03-26', # confirmed obs are good for this day but not getting the unloading right, maybe temp induced unloading due to slight melt, on part of the canopy unloading a lot of snow
  '2023-03-28', # confirmed obs are good for this day but not getting the unloading right, maybe temp induced unloading due to slight melt, on part of the canopy unloading a lot of snow
  '2022-02-04', # baseline crhm gets this (write answer wrong reason) wind event as temp increases during the wind unloading. rm as is slightly misleading since it is observed to be wind unloading
  '2023-01-28' # crhm gets this again write answer wrong reason due to time intercepted and maybe some sublimation
)

# SETUP ----

# Select model run with all unloading events weighed tree snow load assimilated
# prj_updt <- "ffr_closed_canopy_cc0.88_vector_based_new_ablation_psp"
prj_updt <- "ffr_closed_canopy_cc0.88_cansnobal"


# specify certain model run
# run_tag_updt <- "psp_temp_as_canopy_snow_temp"
# run_tag_updt <- "air_temp_as_canopy_snow_temp"
# run_tag_updt <- "icebulb_temp_as_canopy_snow_temp"
# run_tag_updt <- "obs_irtc_trunk_temp_as_canopy_snow_temp"
run_tag_updt <- "new_event_set_output.txt"
run_tag_updt <- "test_LW_in_eq_vf_4pir"
run_tag_updt <- "ess03_vt0.0_avs_0.65_fix5"
run_tag_updt <- "init_run_cansnobal_v_1_1_unld_ratios_no_origin3"
run_tag_updt <- "no_subl_unld_updt_wind_pars2"

to_long_tb <- function(unloading_start_date, end_date, event_id, quality, TB1_flag, TB2_flag, TB3_flag, TB4_flag){
  datetime <- seq(unloading_start_date, end_date, 900)
  
  out <- data.frame(datetime, event_id, quality, TB1_flag, TB2_flag, TB3_flag, TB4_flag)
  
  return(out)
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

# events where snow is in the canopy post snowfall
canopy_snow_events <- 
  read.csv('data/raw-data/snow_in_canopy_post_snowfall.csv') |> 
  mutate(from =  as.POSIXct(from, tz = 'Etc/GMT+6'),
         to = as.POSIXct(to, tz = 'Etc/GMT+6'),
          event_id = format(from, "%Y-%m-%d %H")) 

events_fltr_long <-
  purrr::pmap_dfr(canopy_snow_events, to_long)

obs_tree <-
  readRDS(paste0(
    'data/clean-data/unloading_events_zero_weighed_tree_kg_m2_pre_post_cnpy_snow_',
    load_suffix,
    '.rds'
  )) 

obs_tree_post_sf <- obs_tree |> 
  select(datetime, tree_mm) |> 
  left_join(events_fltr_long |> select(datetime, event_id, weighed_tree_quality),
            by = 'datetime') |>
  filter(weighed_tree_quality < 3) |> select(-weighed_tree_quality)

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
  #'2022-02-28', # too small
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

all(cold_events %in% obs_tree_cold$event_id)


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
                    select(c(unloading_start_date, end_date, event_id, quality, TB1_flag:TB4_flag)),
                  to_long_tb)

tb_data_zeroed <- tb_data |> 
  left_join(tb_events_long) |> 
  filter(quality < 3) |> 
  mutate(
    dU = ifelse(name == 'TB1_mm' & TB1_flag == T, NA, dU),
    dU = ifelse(name == 'TB2_mm' & TB2_flag == T, NA, dU),
    dU = ifelse(name == 'TB3_mm' & TB3_flag == T, NA, dU),
    dU = ifelse(name == 'TB4_mm' & TB4_flag == T, NA, dU)
  )
mod_d_drip_smry_frac <- readRDS('data/ablation_event_fraction_ablation_processes.rds')


obs_tree <- rbind(obs_tree_cold  |> 
                    select(datetime, event_id, observed = tree_mm),
                  obs_tree_warm |> 
                    select(datetime, event_id, observed = tree_mm)) 

select_events_long <- obs_tree |> select(datetime, event_id)

ft_met_xtra <- readRDS('../../analysis/met-data-processing/data/1000x_wnd_irtc.rds')

irtc_temps <- ft_met_xtra |> 
  ungroup() |> 
  select(datetime, starts_with('IRTC')) 

base_path <- 'figs/crhm-analysis/ablation-events/'
fig_tbl_tag <- 'cansnobal_init_testing'

# LOAD DATA ----

## OBSERVED ----
obs_tree <- rbind(obs_tree_cold  |> 
                    select(datetime, event_id, observed = tree_mm),
                  obs_tree_warm |> 
                    select(datetime, event_id, observed = tree_mm)) |> 
  filter(!event_id %in% bad_events)
select_events_long <- obs_tree |> select(datetime, event_id)

# obs_tree <- obs_tree_post_sf
# select_events_long <- obs_tree |> select(datetime, event_id)

## MODELLED ----

path <- list.files(
  paste0(
    "crhm/output/",
    prj_updt
  ),
  pattern = run_tag_updt,
  full.names = T
)

stopifnot(length(path) == 1)

crhm_output_newsim <- CRHMr::readOutputFile(
  path,
  timezone = 'Etc/GMT+6')# |> filter(datetime %in% obs_tree$datetime)

mod_tree <- crhm_output_newsim |> 
  select(datetime, simulated_new = m_s_veg.1)

obs_mod_tree <- left_join(obs_tree, mod_tree)

## MODEL COMPARISON ----

### BASELINE CRHM ABLATION MODEL from Ellis2010/HP98 ----
# Select model run with all unloading events weighed tree snow load assimilated
prj_base <- "ffr_closed_canopy_cc0.88_crhm_baseline"

run_tag_base <- "new_event_set"

path <- list.files(
  paste0(
    "../../analysis/crhm-analysis/output/",
    prj_base
  ),
  pattern = run_tag_base,
  full.names = T
)

stopifnot(length(path) == 1)

crhm_output_baseline <- CRHMr::readOutputFile(
  path,
  timezone = 'Etc/GMT+6') |> filter(datetime %in% obs_tree$datetime)

mod_tree <- crhm_output_baseline |> 
  select(datetime, ellis2010 = Snow_load.1)

obs_mod_tree_comp <- left_join(obs_mod_tree, mod_tree)

### Roesch 2001 ----
# Select model run with all unloading events weighed tree snow load assimilated
prj_roesch <- "ffr_closed_canopy_cc0.88_roesch2001"

# specify certain model run
#run_tag <- "turn_off_duration_based_ablation_output.txt" # baseline prior to ratio based unloading
run_tag_roesch <- "new_event_set"
# run_tag <- "testing123"

path <- list.files(
  paste0(
    "../../analysis/crhm-analysis/output/",
    prj_roesch
  ),
  pattern = run_tag_roesch,
  full.names = T
)

stopifnot(length(path) == 1)

crhm_output_updated <- CRHMr::readOutputFile(
  path,
  timezone = 'Etc/GMT+6') |> filter(datetime %in% obs_tree$datetime)

mod_tree <- crhm_output_updated |> 
  select(datetime, roesch2001 = Snow_load.1)

obs_mod_tree_comp <- left_join(obs_mod_tree_comp, mod_tree)

### Andreadis 2009 ----
# Select model run with all unloading events weighed tree snow load assimilated
prj_andreadis <- "ffr_closed_canopy_cc0.88_andreadis2009"

# specify certain model run
#run_tag <- "turn_off_duration_based_ablation_output.txt" # baseline prior to ratio based unloading
run_tag_andreadis <- "new_event_set"
# run_tag <- "testing123"

path <- list.files(
  paste0(
    "../../analysis/crhm-analysis/output/",
    prj_andreadis
  ),
  pattern = run_tag_andreadis,
  full.names = T
)

stopifnot(length(path) == 1)

crhm_output_updated <- CRHMr::readOutputFile(
  path,
  timezone = 'Etc/GMT+6') |> filter(datetime %in% obs_tree$datetime)

mod_tree <- crhm_output_updated |> 
  select(datetime, andreadis2009 = Snow_load.1)

obs_mod_tree_comp <- left_join(obs_mod_tree_comp, mod_tree)

obs_mod_tree_comp <- left_join(obs_mod_tree_comp, mod_tree) |> 
  left_join(mod_d_drip_smry_frac)  |>
  mutate(event_type = case_when(
    melt >= 0.6 ~ 'melt',
    sublimation >= 0.6 ~ 'sublimation',
    wind >= 0.6 ~ 'wind',
    TRUE ~ 'mixed'
  )
  )

