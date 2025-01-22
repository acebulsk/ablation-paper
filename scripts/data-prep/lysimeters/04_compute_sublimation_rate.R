# this script computes the sublimation rate using the state of canopy snow load
# in the weighed tree lysimter and the dimensionless sublimation rate from CRHM

# Compute the sublimation rate ---- 

# calculate the sublimation rate using the modelled dimensionless sublimation
# rate from CRHM (per second) multiplied by the observed weighed tree canopy
# snow load

select_met <- c('t', 'u', 'rh')

mod_subl_select <-
  mod_df |> select(
    datetime,
    mod_canopy_load = Snow_load.1,
    diml_subl = pot_subl_cpy.1,
    mod_subl_cpy = Subl_Cpy.1
  )

S_bar <- 8.7 # species correction for fir and spruce marmot by JP macdonald thesis cite Pomeroy2012 (kg / m2)
lai <- 1.66 # same as CRHM obs used 
ts_seconds <- 15*60 # running at 15 min intervals

# weighed_tree_zeroed |>
#   filter(event_id == '2023-03-13') |>
#   ggplot(aes(datetime, tree_mm, colour = tree_cal_trough_name)) +
#   geom_line()

weighed_tree_zeroed_met <- weighed_tree_zeroed |>
  select(datetime, event_id, tree_cal_trough_name, tree_cal_cc, obs_weighed_tree_mm = tree_mm) |>
  left_join(ffr_met |> 
              select(datetime, all_of(select_met))) |>
  left_join(mod_subl_select) |>
  mutate(
    fsd = rho_fresh_snow(t),
    spc_cap = species_capacity(S_bar, fsd),
    # W_max = W_max(spc_cap, lai),
    W_max = Lmax, # crhm modelling analysis shows that CRHM subl is too low with existing canopy snow load capacity
    Ce = snow_exposure_coef(W_max, obs_weighed_tree_mm),
    obs_tree_mod_subl_cpy_mm_15min = subl_cpy(diml_subl, 
                                     Ce, 
                                     obs_weighed_tree_mm, 
                                     timestep_seconds = ts_seconds)
  )

# sublimating more snow than there is in the canopy should not use for partitioning
weighed_tree_zeroed_met |>
  filter(event_id == '2023-03-13') |>
  group_by(tree_cal_trough_name) |> 
  summarise(subl_cpy = sum(obs_tree_mod_subl_cpy_mm_15min)) 

weighed_tree_zeroed_met |>
  filter(event_id == '2023-03-13') |>
  group_by(tree_cal_trough_name) |> 
  mutate(cml_sbl_cpy = cumsum(obs_tree_mod_subl_cpy_mm_15min)) |> 
  ggplot(aes(datetime, cml_sbl_cpy, colour = tree_cal_trough_name)) +
  geom_line()

# qc sublimation rate ----

mean_subl <- mean(weighed_tree_zeroed_met$obs_tree_mod_subl_cpy_mm_15min, na.rm = T)
sd_subl <- sd(weighed_tree_zeroed_met$obs_tree_mod_subl_cpy_mm_15min, na.rm = T)

stdep <- (weighed_tree_zeroed_met$obs_tree_mod_subl_cpy_mm_15min - mean_subl)/sd_subl
# met_unld$stdep <- stdep

flag <- stdep > stdep_subl_fltr

weighed_tree_zeroed_met$obs_tree_mod_subl_cpy_mm_15min <- ifelse(flag, NA, weighed_tree_zeroed_met$obs_tree_mod_subl_cpy_mm_15min)

paste0('Number of sublimation vals flagged: ', sum(flag, na.rm = T))

# output sublimation dateframe

obs_mod_subl_cpy_df_out <- weighed_tree_zeroed_met |> 
  select(datetime,
         event_id,
         tree_cal_trough_name, tree_cal_cc,
         obs_canopy_load = obs_weighed_tree_mm,
         obs_tree_mod_subl_cpy_mm_15min = obs_tree_mod_subl_cpy_mm_15min,
         mod_canopy_load,
         mod_subl_cpy_mm_15min = mod_subl_cpy) |> 
  mutate(obs_tree_mod_subl_cpy_mm_hr = obs_tree_mod_subl_cpy_mm_15min * 4, # mm/15 min to mm/hour for unloading comparison which is at hourly 
         mod_subl_cpy_mm_hr = mod_subl_cpy_mm_15min * 4
         )

saveRDS(obs_mod_subl_cpy_df_out, 'data/clean-data/obs_mod_canopy_load_and_sublimation_cnpy_snow.rds')
