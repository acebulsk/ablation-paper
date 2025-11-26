# Script to combine the data and plot outputs from the wind and tau scripts in
# this folder

# combine stats ---- 

## dry snow unloading ----

### just wind and tau as in original manuscript ----
wind_stats <- readRDS('data/results/modelled_wind_unloading_error_table.rds') |>
  rename(`Wind Speed` = Value) |>
  filter(Metric != 'Converged?')
tau_stats <- readRDS('data/results/modelled_tau_unloading_error_table_gls.rds') |> rename(`Shear Stress` = Value)
stats_out <- left_join(tau_stats, wind_stats) |> filter(Metric != 'Mean Absolute Error (mm/hr)')

saveRDS(stats_out, 'data/results/modelled_combined_wind_tau_unloading_error_table.rds')

### sublimation and load only
subl_stats <- readRDS('data/results/modelled_subl_unloading_error_table_lm.rds') |>
  rename(`Sublimation` = Value) |> filter(Metric != 'Mean Absolute Error (mm/hr)')
load_stats <- readRDS('data/results/modelled_load_unloading_error_table_gls.rds') |>
  rename(`Snow Load` = Value) |> filter(Metric != 'Mean Absolute Error (mm/hr)')

## tau and some other vars
tau_temp_stats <- readRDS('data/results/modelled_tau_temp_unloading_error_table.rds') |>
  rename(`Shear Stress, Air Temperature` = Value) |> filter(Metric != 'Mean Absolute Error (mm/hr)')
tau_ibd_stats <- readRDS('data/results/modelled_tau_ibd_unloading_error_table.rds') |>
  rename(`Shear Stress, Ice-Bulb dep.` = Value) |> filter(Metric != 'Mean Absolute Error (mm/hr)')
tau_subl_stats <- readRDS('data/results/modelled_tau_subl_unloading_error_table.rds') |>
  rename(`Shear Stress, Sublimation` = Value) |> filter(Metric != 'Mean Absolute Error (mm/hr)')

stats_out_all <- left_join(load_stats, stats_out) |>
  left_join(subl_stats) |> 
  left_join(tau_temp_stats) |>
  left_join(tau_ibd_stats) |> 
  left_join(tau_subl_stats)

saveRDS(stats_out_all, 'data/results/modelled_combined_wind_tau_others_unloading_error_table.rds')
write.csv(stats_out_all, 'data/results/modelled_combined_wind_tau_others_unloading_error_table.csv', row.names = F)

## combine melt event stats 

melt_stats <- readRDS('data/results/modelled_melt_unloading_melt_only_error_table.rds') |>
  rename(`Dimensionless Snowmelt Rate` = Value)
air_temp_stats <- readRDS('data/results/modelled_melt_unloading_air_temp_error_table.rds') |>
  rename(`Air Temperature` = Value)
ice_temp_stats <- readRDS('data/results/modelled_melt_unloading_ice_temp_error_table.rds') |>
  rename(`Ice-Bulb Temperature` = Value)
melt_tau_stats <- readRDS('data/results/modelled_melt_unloading_melt_w_tau_error_table.rds') |>
  rename(`Dimensionless Snowmelt Rate, Shear Stress` = Value)

melt_stats_out <- left_join(melt_tau_stats, melt_stats, by = 'Metric') |>
  left_join(air_temp_stats) |> 
  left_join(ice_temp_stats) |> 
  filter(Metric != 'Mean Absolute Error (mm/hr)')

saveRDS(melt_stats_out, 'data/results/modelled_melt_unloading_error_table.rds')
write.csv(melt_stats_out, 'data/results/modelled_melt_unloading_error_table.csv', row.names = F)
