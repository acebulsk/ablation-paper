# Combine new dataframes and save output

# Combine met and load data at 15 min timestep
obs_mod_subl <- obs_mod_subl_cpy_df_out |> 
  select(datetime, event_id, tree_cal_trough_name, tree_cal_cc,
         obs_tree_mod_subl_cpy_mm_hr
         )

q_unld_met_scl <- met_binned_cpy_snow |> 
  left_join(scl_q_unld, by = c('datetime', 'name'))

# check that data lines up between unloading rate and weighed tree sublimation 
# ggplot(
#   met_unld |> pivot_longer(
#     c(mod_canopy_load, dU),
#     names_to = 'new_name',
#     values_to = 'new_val'
#   ),
#   aes(datetime, new_val)
# ) + geom_line() + facet_wrap( ~ new_name, scales = 'free_y', nrow = 2)
# 
# ggplot(
#   met_unld |> 
#     filter(q_unl > 0.05,
#            name %in% scl_names) |> 
#     pivot_longer(
#     c(u, q_unl, obs_canopy_load),
#     names_to = 'new_name',
#     values_to = 'new_val'
#   ),
#   aes(datetime, new_val, colour = name)
# ) + geom_point() + facet_wrap( ~ new_name, scales = 'free_y', nrow = 3)
# plotly::ggplotly()

# qc unloading rate ----

# rm auto removal as was too strict.. we really just have the mar 7 spike due to tower raising on the weighed tree
# mean_unl <- mean(met_unld$q_unl)
# sd_unl <- sd(met_unld$q_unl)
# 
# stdep <- (met_unld$q_unl - mean_unl)/sd_unl
# # met_unld$stdep <- stdep
# 
# flag <- stdep > stdep_unl_fltr
# 
# met_unld$q_unl <- ifelse(flag, NA, met_unld$met_unld$q_unl)

# warning(
#   paste0(
#     'Number of rows removed using the unloading standardized departures filter: ',
#     sum(flag)
#   )
# )

# met_unld <- met_unld[!(met_unld$q_unl > q_unl_max_cold & 
#                       met_unld$pwl_ice_bulb < temp_th), ]

# met_unld <-  met_unld[!(met_unld$datetime >= as.POSIXct('2022-02-23 03:15:00', tz = 'Etc/GMT+6') & 
#                          met_unld$datetime <= as.POSIXct('2022-02-23 06:30:00', tz = 'Etc/GMT+6') &
#                           met_unld$q_unl > q_unl_max_cold & 
#                           met_unld$pwl_ice_bulb < temp_th),]
# 
# met_unld <-  met_unld[!(met_unld$datetime >= as.POSIXct('2023-02-22 00:45:00', tz = 'Etc/GMT+6') & 
#                          met_unld$datetime <= as.POSIXct('2023-02-24 10:15:00', tz = 'Etc/GMT+6') &
#                           met_unld$q_unl > q_unl_max_cold & 
#                            met_unld$pwl_ice_bulb < temp_th),]

saveRDS(q_unld_met_scl, 'data/clean-data/unloading_data_with_met_15min.rds')

# q_unld_tree <- met_unld |> filter(name == 'tree_mm') |> 
#   select(datetime, event_id, tree_cal_trough_name, q_tree_ablation = q_unl)

# saveRDS(q_unld_tree, 'data/clean-data/weighed_tree_ablation_15min.rds')

# look at daily averages ----

# since we are looking at daily aggregates we dont want to include any
# incomplete days on the edges

first_full_day <- date('2021-12-15') 
last_full_day <- date('2023-04-16') 

met_unld_daily <- q_unld_met_scl |> 
  filter(datetime >= first_full_day, datetime <= last_full_day) |> 
  group_by(date = date(datetime)) |> 
  summarise(t = mean(t),
            t_ice_bulb = mean(t_ice_bulb), 
            u = mean(u), 
            diml_subl_rate = mean(diml_subl_rate, na.rm = T),
            tau_canopy_top = mean(tau_canopy_top, na.rm = T),
            tau_canopy_low = mean(tau_canopy_low, na.rm = T),
            q_unl_daily = sum(dU, na.rm = T))

saveRDS(met_unld_daily, 'data/clean-data/unloading_data_with_met_daily.rds')

# met_unld |> 
#   left_join(canopy_snow_long) |> 
#   filter(is.na(event_id) == F,
#          tree_mm > 0) |> 
#   ggplot(aes(tree_mm, q_unl)) +
#   geom_point() +
#   xlab("Antecedent Snow Load (mm)") +
#   ylab("Unloading Rate (mm/hr)")
# 
# ggsave('figs/model-assessment/antecedent_snow_load_vs_average_unloading_rate.png', width = 4, height = 4)