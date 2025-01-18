# Mod CRHM obs files to include some assimilation data relevant to this project

ft_obs <- readRDS('../../analysis/met-data-processing/data/ffr_crhm_modelling_obs.rds') |> 
  # left_join(irtc) |> 
  left_join(combined_tree_events) |> # change this if want cold events too
  mutate(obs_snow_load = ifelse(is.na(obs_snow_load), 9999, obs_snow_load))

CRHMr::writeObsFile(ft_obs,
                    '../../analysis/crhm-analysis/obs/ffr_crhm_modelling_obs_w_asm_data.obs',
                    comment = 'This file consists of air temp, rh, and wind speed from the Waterloo Forest Tower (aka Fortress Forest Ride) see the R proj met-data-processing for qaqc and gap fill procedures. The radiation data is from Fortress Ridge and has been gap filled with Fortress Ridge South. Filtered to snowfall event from March 13, 2023 to March 20, 2023')

event_select <- '2023-01-28'
event_select_datetime <- obs_tree |> 
  filter(event_id == event_select) |> 
  pull(datetime)

CRHMr::writeObsFile(ft_obs |> filter(datetime %in% event_select_datetime),
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
