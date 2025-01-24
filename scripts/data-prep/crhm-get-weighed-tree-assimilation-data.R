# Add in weighed tree obs for CRHM snowmelt initialization
# Filter this to just include data during the snowmelt events

# need to keep the below two datasets separate if including all events as there are some overlaps between events otherwise keep filtering
warm_events_select_agu24 <- c(
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
warm_tree_events_df <-
  readRDS('data/clean-data/warm_tree_events_zero_weighed_tree_fsd_closed_0.88_kg_m2_post_cnpy_snow.rds') |> 
  select(datetime, event_id, obs_snow_load = tree_mm) |> 
  group_by(event_id) |> 
  summarise(
    datetime = first(datetime),
    obs_snow_load = first(obs_snow_load)
  ) |> 
  filter(event_id %in% warm_events_select_agu24) |> 
  select(-event_id)

cold_events_select_agu24 <- c(
  '2022-03-02', 
  '2022-03-09',
  '2022-03-20', 
  '2022-03-24',  
  '2022-03-29',  
  '2022-12-01',
  '2023-01-28',
  '2023-02-24',
  '2023-02-26',
  '2023-04-12'
)
cold_tree_events_df <- 
  readRDS('data/clean-data/all_tree_events_zero_weighed_tree_fsd_closed_0.88_kg_m2_post_cnpy_snow.rds') |> 
  select(datetime, event_id, obs_snow_load = tree_mm) |> 
  group_by(event_id) |> 
  summarise(
    datetime = first(datetime),
    obs_snow_load = first(obs_snow_load)
  ) |> 
  filter(event_id %in% cold_events_select_agu24) |> 
  select(-event_id)

combined_tree_events <- rbind(
  warm_tree_events_df,
  cold_tree_events_df
)

# # Add in IRTC to model observation file for CRHM canopy snowmelt testing
# irtc <- readRDS('../../analysis/met-data-processing/data/1000x_wnd_irtc.rds') |> 
#   select(datetime, obs_canopy_temp = IRTC_midTree)
# 
# # need to qaqc this first
# # TODO move this to qc script
# irtc$obs_canopy_temp[irtc$datetime >= as.POSIXct('2022-07-18 16:30', tz = 'Etc/GMT+6') & 
#                        irtc$datetime <= as.POSIXct('2022-08-06 08:00', tz = 'Etc/GMT+6')] <- NA
# irtc$obs_canopy_temp[irtc$datetime >= as.POSIXct('2022-07-22 16:45', tz = 'Etc/GMT+6') & 
#                        irtc$datetime <= as.POSIXct('2022-07-23 07:00', tz = 'Etc/GMT+6')] <- NA
# irtc$obs_canopy_temp[irtc$datetime >= as.POSIXct('2022-08-04 10:00', tz = 'Etc/GMT+6') & 
#                        irtc$datetime <= as.POSIXct('2022-08-06 08:00', tz = 'Etc/GMT+6')] <- NA
# irtc$obs_canopy_temp[irtc$datetime >= as.POSIXct('2022-08-12 16:45', tz = 'Etc/GMT+6') & 
#                        irtc$datetime <= as.POSIXct('2022-08-13 09:00', tz = 'Etc/GMT+6')] <- NA
# irtc$obs_canopy_temp[irtc$datetime >= as.POSIXct('2022-08-21 17:15', tz = 'Etc/GMT+6') & 
#                        irtc$datetime <= as.POSIXct('2022-08-23 12:00', tz = 'Etc/GMT+6')] <- NA
# irtc$obs_canopy_temp[irtc$datetime >= as.POSIXct('2022-09-13 00:30', tz = 'Etc/GMT+6') & 
#                        irtc$datetime <= as.POSIXct('2022-09-17 10:30', tz = 'Etc/GMT+6')] <- NA
# irtc$obs_canopy_temp[irtc$datetime >= as.POSIXct('2022-09-29 11:15', tz = 'Etc/GMT+6') & 
#                        irtc$datetime <= as.POSIXct('2022-09-30 05:30', tz = 'Etc/GMT+6')] <- NA
# irtc$obs_canopy_temp[irtc$datetime >= as.POSIXct('2022-10-10 20:45', tz = 'Etc/GMT+6') & 
#                        irtc$datetime <= as.POSIXct('2022-10-11 04:15', tz = 'Etc/GMT+6')] <- NA
# irtc$obs_canopy_temp[irtc$datetime >= as.POSIXct('2022-12-26 16:00', tz = 'Etc/GMT+6') & 
#                        irtc$datetime <= as.POSIXct('2022-12-28 01:00', tz = 'Etc/GMT+6')] <- NA
# irtc$obs_canopy_temp[irtc$datetime >= as.POSIXct('2023-04-10 16:30', tz = 'Etc/GMT+6') & 
#                        irtc$datetime <= as.POSIXct('2023-04-13 14:45', tz = 'Etc/GMT+6')] <- NA
# irtc$obs_canopy_temp[irtc$datetime >= as.POSIXct('2022-06-13 16:45', tz = 'Etc/GMT+6') & 
#                        irtc$datetime <= as.POSIXct('2022-06-24 19:00', tz = 'Etc/GMT+6')] <- NA
# 
# irtc <- irtc |> 
#   mutate(
#     obs_canopy_temp_flag = ifelse(abs(t-obs_canopy_temp) > 10, T,F),
#     obs_canopy_temp = ifelse(obs_canopy_temp_flag, NA, obs_canopy_temp),
#     obs_canopy_temp = ifelse(is.na(obs_canopy_temp), t, obs_canopy_temp)) |> 
#   select(-obs_canopy_temp_flag)
# 
# # ffr_qc_fill_out_irtc|>
# #       pivot_longer(c(t, obs_canopy_temp)) |> 
# #   as.data.frame() |> 
# #   ggplot(aes(datetime, value, colour = name)) +
# #   geom_line()
# # plotly::ggplotly()
# 
# ffr_qc_fill_out_irtc_tree <- left_join(ffr_qc_fill_out_irtc,
#                                        combined_tree_events) |> # change this if want cold events too
#   mutate(obs_snow_load = ifelse(is.na(obs_snow_load), 9999, obs_snow_load))

