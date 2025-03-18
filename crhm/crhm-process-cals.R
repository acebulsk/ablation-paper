# This script pulls in the model runs with various Pom 2009 hot tree B vals and
# unld_to_melt ratio vals. Error stats are returned comapred to the weighed
# tree. Events were selected based on those not influenced by wind and other
# outliers which we arent likely to get anyway. The results show that we need a
# much higher B value (0.2) and unld_to_melt_ratio should be ~0.13 to 0.16

# this gives us a over estimate of canopy snow load for spring events
# (underestimating ablation) and under estimate of snow load for warm summer
# events (over est. of ablation).

library(tidyverse)

# LOAD DATA ----

## Observation data ----
# rm outlier events and wind events
bad_events <- c(
  '2022-06-23', # crhm rain to snow paritioning is off here
  '2022-06-24', # crhm rain to snow paritioning is off here
  '2022-06-14', # outlier event
  '2022-04-21', # outlier event
  '2023-04-13', # not getting close with this one
  '2023-03-25', # edge case previous empirical models get hese better as are handling inc of unloading as air temp rises to melting point.
  '2023-03-26', # edge case previous empirical models get hese better as are handling inc of unloading as air temp rises to melting point.
  '2023-03-28', # edge case previous empirical models get hese better as are handling inc of unloading as air temp rises to melting point.
  '2022-02-04', # baseline crhm gets this (write answer wrong reason) wind event as temp increases during the wind unloading. rm as is slightly misleading since it is observed to be wind unloading
  '2023-01-28', # crhm gets this again write answer wrong reason due to time intercepted and maybe some sublimation
  '2022-12-01',
  '2023-02-24',
  '2023-02-26',
  '2022-03-09',
  '2023-04-17'
  )

load_suffix <- 'fsd_closed_0.88'

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

# cold tree events
cold_events <- c(
  # new ones
  #'2021-12-27', # wind event some precip, maybe blowing snow redist.
  # '2022-01-18', # wind event too much precip during (maybe blowing snow redistribution?)
  '2022-02-04', # wind event
  #'2022-02-21', # wind event unloading not associated with wind or other here
  # '2022-02-24', # wind event , tree increased due to vapour deposition likely
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

obs_tree <- rbind(obs_tree_cold  |> 
                    select(datetime, event_id, observed = tree_mm),
                  obs_tree_warm |> 
                    select(datetime, event_id, observed = tree_mm)) |> 
  filter(!event_id %in% bad_events)

## Calibration model output ----
prj_name <- "ffr_closed_canopy_cc0.88_cansnobal_for_calibration"
cal_run_id <- 'a'
cal_pattern <- paste0(prj_name, '_', cal_run_id)

cal_run_paths <- list.files('crhm/output/',
                            pattern = cal_pattern,
                            full.names = T,
                            recursive = T)

read_crhm_output <- function(file_name){
  crhm_output_updated <- CRHMr::readOutputFile(
    file_name,
    timezone = 'Etc/GMT+6')# |> filter(datetime %in% obs_tree$datetime)
  
  # Extract values from the file name using regular expressions
  pattern <- ".*_(a\\d{5})_unl_melt_r_(\\d+\\.\\d+)_B_(\\d+\\.\\d+)_output\\.txt"
  matches <- str_match(file_name, pattern)
  
  if (is.na(matches[1, 1])) {
    stop("File name does not match the expected pattern")
  }
  
  cal_id <- matches[1, 2]
  unld_to_melt_ratio <- as.numeric(matches[1, 3]) 
  SW_to_LW_fn <- as.numeric(matches[1, 4])
  
  # Add new columns to the data frame
  crhm_output_updated <- crhm_output_updated %>%
    mutate(
      cal_id = cal_id,
      unld_to_melt_ratio = unld_to_melt_ratio,
      SW_to_LW_fn = SW_to_LW_fn
    ) |> select(
      cal_id,
      datetime, 
      m_s_veg.1,
      unld_to_melt_ratio,
      SW_to_LW_fn
    )
  
}

  
mod_output <- map_dfr(cal_run_paths, read_crhm_output)

# saveRDS(
#   mod_output,
#   paste0(
#   'crhm/processed/cal_runs_combined_',
#   cal_pattern,
#   '.rds')
#   )

obs_mod_tree <- left_join(obs_tree, mod_output) |> 
  rename(modelled = m_s_veg.1)

# generate error table cal

obs_mod_tree_err_tbl_events <- obs_mod_tree |> 
  group_by(cal_id) |> 
  mutate(diff = observed - modelled) |> 
  # group_by(name) |> 
  summarise(
    MB = mean(diff, na.rm = T),
    MAE = mean(abs(diff), na.rm = T),
    RMSE = sqrt(mean(diff ^ 2, na.rm = T)),
    # NRMSE = RMSE / (max(observed, na.rm = TRUE) - min(observed, na.rm = TRUE)),
    NRMSE = RMSE / mean(observed, na.rm = T),
    R = cor(observed, modelled),
    R2 = R^2) |> 
  mutate(across(MB:R2, round, digits = 3))

# generate error table by event

obs_mod_tree_err_tbl_events <- obs_mod_tree |> 
  group_by(event_id, cal_id) |> 
  mutate(diff = observed - modelled) |> 
  # group_by(name) |> 
  summarise(
    MB = mean(diff, na.rm = T),
    MAE = mean(abs(diff), na.rm = T),
    RMSE = sqrt(mean(diff ^ 2, na.rm = T)),
    # NRMSE = RMSE / (max(observed, na.rm = TRUE) - min(observed, na.rm = TRUE)),
    NRMSE = RMSE / mean(observed, na.rm = T),
    R = cor(observed, modelled),
    R2 = R^2) |> 
  mutate(across(MB:R2, round, digits = 3))

write.csv(obs_mod_tree_err_tbl,
          paste0(
            'tbls/',
            'obs_mod_canopy_snow_load_err_tbl_by_event_',
            fig_tbl_tag,
            '.csv'
          ))

cal_smry <- obs_mod_tree_err_tbl_events |> 
  group_by(cal_id) |> 
  summarise(across(everything(), mean))
# obs vs mod weighed tree error table avg by event type

obs_mod_tree_err_tbl_avg_type <- obs_mod_tree_err_tbl_events |> 
  left_join(mod_d_drip_smry_frac)  |> 
  mutate(event_type = case_when(
    melt > 0.8 ~ 'melt',
    sublimation > 0.8 ~ 'sublimation',
    wind > 0.6 ~ 'wind',
    TRUE ~ 'mixed'
  )) |> 
  group_by(name, event_type) |> summarise(across(MB:R2, mean)) 

saveRDS(obs_mod_tree_err_tbl_avg_type,
        paste0(
          'tbls/',
          'obs_mod_canopy_snow_load_err_tbl_avg_type',
          fig_tbl_tag,
          '.rds'
        ))