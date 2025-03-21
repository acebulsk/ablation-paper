

# PLOT INDIVIDUALLY ----

obs_mod_tree_long <- obs_mod_tree |>
  # left_join(baseline_mod_tree) |>
  pivot_longer(!c(datetime, event_id)) |>
  mutate(group = 'Canopy Snow Load (mm)')

mod_d_drip <- crhm_output_newsim |> 
  mutate(delsub_veg_int.1 = -delsub_veg_int.1,
         total_ablation = delmelt_veg_int.1 + delunld_int.1 + delsub_veg_int.1
         ) |> 
  select(datetime,
         snowmelt = delmelt_veg_int.1,
         unloading = delunld_int.1,
         sublimation = delsub_veg_int.1,
         unload_melt = delunld_melt_int.1,
         unload_wind = delunld_wind_int.1,
         unload_subl = delunld_subl_int.1,
         total_ablation)


mod_cml_dU <- mod_d_drip |> 
  inner_join(select_events_long) |> 
  pivot_longer(c(snowmelt,
                 # unload_melt,
                 # unload_wind,
                 # unload_subl,
                 unloading,
                 sublimation),
               values_to = 'dU') |> 
  group_by(event_id, name) |> 
  mutate(
        cml_dU = cumsum(dU),
         cml_dU = cml_dU - first(cml_dU)) |>  # so each event starts at 0
  select(datetime, event_id, name = name, value = cml_dU) |> 
  mutate(group = 'Simulated Ablation (mm)')

mod_temps <- crhm_output_newsim |> 
  select(datetime, 
         # CRHM_Tsnow = TCanSnow.1,
         # CRHM_Tbiomass = Tbiomass.1,
         CRHM_hru_t = hru_t.1,
         CRHM_Tsnow_new = T_s_veg.1
  ) |> 
  mutate(CRHM_Tsnow_new = CRHM_Tsnow_new - 2.7316e2)

mod_rh_u <- crhm_output_newsim |> 
  select(datetime, 
         wind_speed = hru_u.1,
         RH = hru_rh.1
  ) |> 
  left_join(select_events_long) |> 
  pivot_longer(c(wind_speed, RH)) |> 
  mutate(group = ifelse(name == 'RH', 'Relative Humidity (%)', 'Wind Speed (m/s)'))

obs_mod_temps <- left_join(mod_temps, irtc_temps, by = 'datetime') |> 
  left_join(select_events_long) |> 
  pivot_longer(c(CRHM_hru_t:IRTC_trunk)) |> 
  filter(abs(value) < 100) |> 
  mutate(group = 'Temperature (°C)') |> 
  rbind(mod_rh_u)

event <- unique(select_events_long$event_id)[1]
event <- '2023-03-25'
for (event in unique(select_events_long$event_id)) {

  # calculate stats on obs vs mod tree
  obs_mod_tree_err_tbl <- obs_mod_tree |>
    filter(event_id == event) |>
    mutate(diff = observed - simulated_new) |>
    # group_by(name) |>
    summarise(
      MB = mean(diff, na.rm = T),
      MAE = mean(abs(diff), na.rm = T),
      RMSE = sqrt(mean(diff ^ 2, na.rm = T)),
      # NRMSE = RMSE / (max(observed, na.rm = TRUE) - min(observed, na.rm = TRUE)),
      NRMSE = RMSE / mean(observed, na.rm = T),
      R = cor(observed, simulated_new),
      `r^2` = R^2) |>
    mutate(across(MB:`r^2`, round, digits = 3))

  stats_text <- obs_mod_tree_err_tbl |>
    summarise(
      text = sprintf(
        "Mean Bias (mm): %.2f\nRMS Error (mm): %.2f\nNRMSE: %.2f\nR²: %.2f",
        MB, RMSE, NRMSE, `r^2`
      )
    ) |>
    pull(text)

  df_event <- obs_mod_tree_long |>
    rbind(mod_cml_dU) |>
    filter(event_id == event)
  
  # plot obs and mod tree and idiv mod tree processes

  df_event |>
    ggplot(aes(datetime, value,
               colour = ifelse(group == 'Simulated Ablation (mm)', name, "canopy_load"),
               linetype = ifelse(group == "Canopy Snow Load (mm)",
                                 ifelse(name == "simulated_new", "solid", "dashed"),
                                 "solid"))) +
    geom_line() +
    facet_grid(rows = vars(group), scales = 'free') +
    ggtitle(as.Date(event)) +
    ylab(element_blank()) +
    xlab(element_blank()) +
    scale_colour_manual(values = c("canopy_load" = "black",
                                   "sublimation" = "#E69F00",
                                   "unloading" = "#009E73",
                                   "snowmelt" = "#56B4E9"),
                        labels = c("canopy_load" = "Canopy Load",
                                   "sublimation" = "Sublimation",
                                   "unloading" = "Unloading",
                                   "snowmelt" = "Snowmelt")) +
    scale_linetype_manual(
      values = c("dashed" = "dashed",
                 "solid" = "solid"),
      labels = c("dashed" = "Observed",
                 "solid" = "Simulated")
    ) +
    labs(colour = "State/Process", linetype = "Data Type") +
    geom_text(
      data = df_event |> filter(group == "Canopy Snow Load (mm)"),
      aes(x = max(datetime), y = Inf, label = stats_text),
      inherit.aes = FALSE,
      hjust = 1, vjust = 1.1,
      size = 3.5, colour = "black"
    )

  ggsave(
    paste0(
      'figs/crhm-analysis/ablation-event-indiv/',
      prj_updt,
      '/canopy-snow/crhm_vs_tree_subl_drip_unld_',
      as.Date(event),
      '_',
      run_tag_updt,
      '.png'
    ),
    width = 8,
    height = 6
  )
  
  ggplot(obs_mod_temps |> 
           filter(event_id == event), aes(datetime, value, colour = name, group = name )) +
    geom_line() +
    ggtitle(as.Date(event)) +
    facet_grid(rows = vars(group), scales = 'free') +
    labs(x = element_blank(),
         y = element_blank())
  
  ggsave(
    paste0(
      'figs/crhm-analysis/ablation-event-indiv/',
      prj_updt,
      '/met/event_met_',
      as.Date(event),
      '_',
      run_tag_updt,
      '.png'
    ),
    width = 8,
    height = 6
  )
  

}


mod_d_drip_smry_frac <- mod_d_drip |> 
  inner_join(select_events_long) |> 
  group_by(event_id) |> 
  summarise(across(snowmelt:total_ablation, sum),
            melt = (snowmelt+unload_melt)/total_ablation,
            wind = unload_wind/total_ablation,
            sublimation = (sublimation+unload_subl)/total_ablation) |> 
  mutate(across(c(melt, wind, sublimation), round, 2)) |> 
  select(event_id, melt, wind, sublimation)

saveRDS(mod_d_drip_smry_frac, 'data/ablation_event_fraction_ablation_processes.rds')
