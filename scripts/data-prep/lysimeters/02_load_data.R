
## Load Canopy Snow Periods ---- 

# load_suffix <- 'fsd_closed_0.88'
load_suffix <- 'fsd_cal_for_each_trough_vza_15'

scl_names <- c('mixed', 'sparse', 'closed')
good_loads <- c('mixed', 'sparse', 'closed', 'tree_mm')

raw_data_path <- 'data/raw-data'

# These events were defined with the SCLs in mind, so the events stop when the
# SCLs start loosing mass due to wind transport/melt
canopy_snow_events <- read.csv('data/raw-data/snow_in_canopy_post_snowfall_fltr_ablation.csv') |> 
  mutate(from =  as.POSIXct(from, tz = 'Etc/GMT+6'),
         to = as.POSIXct(to, tz = 'Etc/GMT+6'),
         event_id = format(from, "%Y-%m-%d %H")) |> filter(quality < quality_th)

# these are periods where we know snow is in the canopy and unloading is possible
canopy_snow_long <- purrr::pmap_dfr(canopy_snow_events, to_long_melt_subl)

# Load the canopy snow load periods during and after snowfall. We need this for
# calculating the sublimation rate.

# file.copy(from = '../../analysis/ablation/data/snow_in_canopy_pre_and_post_snowfall.csv',
#           to = raw_data_path,
#           recursive = F,
#           overwrite = F,
#           copy.date = T)
canopy_snow_events_pre_post <- read.csv('data/raw-data/snow_in_canopy_pre_and_post_snowfall.csv') |> 
  mutate(from =  as.POSIXct(from, tz = 'Etc/GMT+6'),
         to = as.POSIXct(to, tz = 'Etc/GMT+6'), 
          event_id = format(from, "%Y-%m-%d %H"),
         weighed_tree_quality = 1)

canopy_snow_events_pre_post$event_id <- as.Date(canopy_snow_events_pre_post$event_id)

canopy_snow_long_pre_post <- purrr::pmap_dfr(canopy_snow_events_pre_post, to_long)

# Check that we have canopy snow during and post data for all of our post
# snowfall periods where we know the weighed tree data is good quality (i.e., no dead)

for(row in 1:nrow(canopy_snow_events)){
  from_flag <- !canopy_snow_events$from[row] %in% canopy_snow_long_pre_post$datetime
  canopy_snow_events$from_flag[row] <- from_flag
  
  to_flag <- !canopy_snow_events$to[row] %in% canopy_snow_long_pre_post$datetime
  canopy_snow_events$to_flag[row] <- to_flag
}

bad_starts <- sum(canopy_snow_events$from_flag[canopy_snow_events$weighed_tree_quality < 3])
bad_ends <- sum(canopy_snow_events$to_flag[canopy_snow_events$weighed_tree_quality < 3])

stopifnot(bad_starts == 0)
stopifnot(bad_ends == 0)

# Load CRHM sublimation outputs ----

# this is the pom98 dimensionless sublimation we multiply by the obs tree load
# file.copy(from = '../../analysis/crhm-analysis/output/2023-08-09-16-29-11_fortress_forest_ridge_ac_output.txt',
#           to = 'data/crhm-output',
#           recursive = F,
#           overwrite = F,
#           copy.date = T)
# mod_df <- CRHMr::readOutputFile(
#   'data/crhm-output/2023-08-09-16-29-11_fortress_forest_ridge_ac_output.txt',
#   timezone = 'Etc/GMT+6'
# )
# 
# mod_subl_cpy <- mod_df |> select(datetime, diml_subl_rate = pot_subl_cpy.1)

weighed_tree_zeroed <- readRDS(paste0(
  'data/clean-data/unloading_events_zero_weighed_tree_kg_m2_pre_post_cnpy_snow_',
  load_suffix,
  '.rds'
)) |> 
  select(datetime:tree_mm, event_id)

## load met ----

# file.copy(from = '../../analysis/met-data-processing/data/ffr_crhm_obs_qaqc_gap_fill.rds',
#           to = raw_data_path,
#           overwrite = F,
#           copy.date = T)
ffr_met <- readRDS('data/raw-data/ffr_crhm_obs_qaqc_gap_fill.rds')

ffr_met_long <- ffr_met |>
  pivot_longer(!datetime)

# file.copy(from = '../../analysis/met-data-processing/data/pluvio-qaqc/pwl_pluvio_15_min_qaqc_undercatch_corr_ac.rds',
#           to = raw_data_path,
#           overwrite = F,
#           copy.date = T)
qc_pwl_plv <- readRDS('data/raw-data/pwl_pluvio_15_min_qaqc_undercatch_corr_ac.rds') |>
  mutate(name = 'pluvio_uc') |>
  select(datetime, name, value = pc)

ec_df_hi <- readRDS('../../analysis/eddy-cov/data/high-tower/ec_high_tower_30_min_2021_2023_qc_rough_semifill.rds') |>
    rename(tau_canopy_top = Tau)

ec_df_low <- readRDS('../../analysis/eddy-cov/data/low-tower/low_tower_15min_2021_2023_qc_rough_semifill.rds')|>
  rename(tau_canopy_low = tau)

ec_df <- left_join(ec_df_low, ec_df_hi, by = 'datetime') |>
  select(datetime, tau_canopy_top,  tau_canopy_low)

ec_df_long <- ec_df |> pivot_longer(!datetime)

# estimate shear stress using the regressions we created in
# eddy-cov/scripts/converting_mid_canopy_wind_to_tau.R

# file.copy(from = '../../analysis/eddy-cov/data/est_tau_from_wnd/lm_mid_wnd_sqrd_low_tau.rds',
#           to = raw_data_path,
#           overwrite = F,
#           copy.date = T)
lm_mid_wnd_sqrd_low_tau <-
  readRDS('data/raw-data/lm_mid_wnd_sqrd_low_tau.rds')

## load lysimeter data ----

# file.copy(from = paste0(
#             '../../analysis/interception/data/loadcell/treefort_weighed_tree_cal_kg_m2_plv_',
#             load_suffix,
#             '.rds'
#           ),
#           to = raw_data_path,
#           overwrite = F,
#           copy.date = T)
w_tree_kg_m2 <- readRDS(paste0('data/raw-data/treefort_weighed_tree_cal_kg_m2_plv_',
                         load_suffix,
                         '.rds')) |> 
  # just remove the one spike due to tower lowering
  filter(datetime !=as.POSIXct('2022-03-07 12:00:00', tz = 'Etc/GMT+6')) 


# file.copy('../../analysis/interception/data/loadcell/treefort_load_main.rds',
#           to = raw_data_path,
#           overwrite = F,
#           copy.date = T)
scl_df_kg_m2_raw <- readRDS('data/raw-data/treefort_load_main.rds')
# scl_df_kg_m2_raw |> select(-c(TB1_mm:TB4_mm)) |> pivot_longer(!datetime) |> 
#   rbind(ffr_met_long |> filter(name == 'p')) |> 
#   ggplot(aes(datetime, value, colour = name)) + 
#   geom_line() +
#   facet_grid(rows = vars(name), scales = 'free_y') +
#   theme(legend.position = 'none')
# plotly::ggplotly()
