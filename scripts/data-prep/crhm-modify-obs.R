# Mod CRHM obs files to include some assimilation data relevant to this project

ft_obs <- readRDS('../../analysis/met-data-processing/data/ffr_crhm_modelling_obs.rds')

# Add in IRTC to model observation file for CRHM canopy snowmelt testing
irtc <- readRDS('../../analysis/met-data-processing/data/1000x_wnd_irtc.rds') |>
  select(datetime, obs_canopy_temp = IRTC_trunk)

# need to qaqc this first
# TODO move this to qc script
irtc$obs_canopy_temp[irtc$datetime >= as.POSIXct('2022-07-18 16:30', tz = 'Etc/GMT+6') &
                       irtc$datetime <= as.POSIXct('2022-08-06 08:00', tz = 'Etc/GMT+6')] <- NA
irtc$obs_canopy_temp[irtc$datetime >= as.POSIXct('2022-07-22 16:45', tz = 'Etc/GMT+6') &
                       irtc$datetime <= as.POSIXct('2022-07-23 07:00', tz = 'Etc/GMT+6')] <- NA
irtc$obs_canopy_temp[irtc$datetime >= as.POSIXct('2022-08-04 10:00', tz = 'Etc/GMT+6') &
                       irtc$datetime <= as.POSIXct('2022-08-06 08:00', tz = 'Etc/GMT+6')] <- NA
irtc$obs_canopy_temp[irtc$datetime >= as.POSIXct('2022-08-12 16:45', tz = 'Etc/GMT+6') &
                       irtc$datetime <= as.POSIXct('2022-08-13 09:00', tz = 'Etc/GMT+6')] <- NA
irtc$obs_canopy_temp[irtc$datetime >= as.POSIXct('2022-08-21 17:15', tz = 'Etc/GMT+6') &
                       irtc$datetime <= as.POSIXct('2022-08-23 12:00', tz = 'Etc/GMT+6')] <- NA
irtc$obs_canopy_temp[irtc$datetime >= as.POSIXct('2022-09-13 00:30', tz = 'Etc/GMT+6') &
                       irtc$datetime <= as.POSIXct('2022-09-17 10:30', tz = 'Etc/GMT+6')] <- NA
irtc$obs_canopy_temp[irtc$datetime >= as.POSIXct('2022-09-29 11:15', tz = 'Etc/GMT+6') &
                       irtc$datetime <= as.POSIXct('2022-09-30 05:30', tz = 'Etc/GMT+6')] <- NA
irtc$obs_canopy_temp[irtc$datetime >= as.POSIXct('2022-10-10 20:45', tz = 'Etc/GMT+6') &
                       irtc$datetime <= as.POSIXct('2022-10-11 04:15', tz = 'Etc/GMT+6')] <- NA
irtc$obs_canopy_temp[irtc$datetime >= as.POSIXct('2022-12-26 16:00', tz = 'Etc/GMT+6') &
                       irtc$datetime <= as.POSIXct('2022-12-28 01:00', tz = 'Etc/GMT+6')] <- NA
irtc$obs_canopy_temp[irtc$datetime >= as.POSIXct('2023-04-10 16:30', tz = 'Etc/GMT+6') &
                       irtc$datetime <= as.POSIXct('2023-04-13 14:45', tz = 'Etc/GMT+6')] <- NA
irtc$obs_canopy_temp[irtc$datetime >= as.POSIXct('2022-06-13 16:45', tz = 'Etc/GMT+6') &
                       irtc$datetime <= as.POSIXct('2022-06-24 19:00', tz = 'Etc/GMT+6')] <- NA

ft_obs_irtc <- ft_obs |>
  left_join(irtc) |> 
  mutate(
    obs_canopy_temp_flag = ifelse(abs(t-obs_canopy_temp) > 15, T,F),
    obs_canopy_temp = ifelse(obs_canopy_temp_flag, NA, obs_canopy_temp),
    obs_canopy_temp = ifelse(is.na(obs_canopy_temp), 9999, obs_canopy_temp)) |>
  select(-obs_canopy_temp_flag)

ft_obs_irtc |>
      pivot_longer(c(t, obs_canopy_temp)) |>
  as.data.frame() |>
  ggplot(aes(datetime, value, colour = name)) +
  geom_line()
plotly::ggplotly()

ffr_qc_fill_out_irtc_tree <- left_join(ft_obs_irtc,
                                       combined_tree_events) |> # change this if want cold events too
  mutate(obs_snow_load = ifelse(is.na(obs_snow_load), 9999, obs_snow_load))


ft_obs_irtc_tree <- ft_obs_irtc |> 
  left_join(combined_tree_events) |> # change this if want cold events too
  mutate(obs_snow_load = ifelse(is.na(obs_snow_load), 9999, obs_snow_load))

CRHMr::writeObsFile(ft_obs_irtc_tree,
                    '../../analysis/crhm-analysis/obs/ffr_crhm_modelling_obs_w_asm_data.obs',
                    comment = 'This file consists of air temp, rh, and wind speed from the Waterloo Forest Tower (aka Fortress Forest Ride) see the R proj met-data-processing for qaqc and gap fill procedures. The radiation data is from Fortress Ridge and has been gap filled with Fortress Ridge South.')

obs_event <- ft_obs_irtc_tree |> filter(datetime >= as.POSIXct('2023-05-08 00:15:00', tz = 'Etc/GMT+6'),
                                       datetime <= as.POSIXct('2023-05-10 00:00:00', tz = 'Etc/GMT+6'))

CRHMr::writeObsFile(obs_event,
                    '../../analysis/crhm-analysis/obs/ffr_crhm_modelling_obs_w_asm_data_fltr_event.obs',
                    comment = 'This file consists of air temp, rh, and wind speed from the Waterloo Forest Tower (aka Fortress Forest Ride) see the R proj met-data-processing for qaqc and gap fill procedures. The radiation data is from Fortress Ridge and has been gap filled with Fortress Ridge South. Filtered to snowfall event from March 13, 2023 to March 20, 2023')

# powerline input data found the timing of the wind at this station to be quite
# off, ocassionally some better performance due to temp/RH but overall not a
# huge difference. Decided to keep FFR forcing for validation. 
pwl_obs <- readRDS('../../analysis/met-data-processing/data/pwl_crhm_modelling_obs.rds') |> 
  # left_join(irtc) |> 
  left_join(combined_tree_events) |> # change this if want cold events too
  mutate(obs_snow_load = ifelse(is.na(obs_snow_load), 9999, obs_snow_load))
CRHMr::writeObsFile(pwl_obs, '../../analysis/crhm-analysis/obs/pwl_crhm_modelling_obs_w_asm_data.obs')
