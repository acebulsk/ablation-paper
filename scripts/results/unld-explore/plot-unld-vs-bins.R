# Plot exploratory relationship between unloading and binned independent vars

## Exploratory plots ---- 

### wind vs unloading rate ----

xlabs_dict <- data.frame(
  name = c('wind_labs',
           'tau_labs',
           'temp_labs',
           'canopy_snowmelt_labs',
           'ti_dep_labs',
           'subl_labs'),
  name_pretty = c('Wind Speed (m/s)',
                  'Shear Stress (N/m^2)',
                  'Air Temperature (°C)',
                  'Snowmelt (mm/hr)',
                  'Ice Bulb Temp. Depression (°C)',
                  'Sublimation (mm/hr)')
)

met_unld_w_bins_smry_wind <- summarise_met_data(met_unld_w_bins,
                                                wind_labs,
                                                tree_labs,
                                                3,
                                                0.1)

met_unld_w_bins_smry_tau <- summarise_met_data(met_unld_w_bins,
                                                tau_labs,
                                                tree_labs,
                                                3,
                                                0.1)

met_unld_w_bins_smry_temp <- summarise_met_data(met_unld_w_bins,
                                                temp_labs,
                                                tree_labs,
                                                3,
                                                0.1)

met_unld_w_bins_smry_melt <- summarise_met_data(met_unld_w_bins,
                                                canopy_snowmelt_labs,
                                                tree_labs,
                                                3,
                                                0.1)

met_unld_w_bins_smry_subl <- summarise_met_data(met_unld_w_bins,
                                                subl_labs,
                                                tree_labs,
                                                3,
                                                0.1)

met_unld_w_bins_smry_ti_dep <- summarise_met_data(met_unld_w_bins,
                                                ti_dep_labs,
                                                tree_labs,
                                                3,
                                                0.1)


bins_df <- rbind(met_unld_w_bins_smry_wind, met_unld_w_bins_smry_temp) |> 
  rbind(met_unld_w_bins_smry_melt) |> 
  rbind(met_unld_w_bins_smry_subl) |> 
  rbind(met_unld_w_bins_smry_tau) |> 
  rbind(met_unld_w_bins_smry_ti_dep) |> 
  left_join(xlabs_dict)

# all plots together
ggplot(bins_df, 
       aes(x = value, y = q_unl_avg, colour = as.factor(round(tree_labs)))) + 
  # geom_point(data = met_unld_w_bins, aes(u, q_unl), alpha = 0.1, colour = 'black') +
  geom_errorbar(aes(
    x = value, 
    ymax = sd_hi,
    ymin = sd_low
  ), width = 0)  +
  geom_point(size = 3) +
  ylab(bin_unl_ax_lab) +
  xlab(element_blank()) +
  theme_bw() +
  # theme_bw(base_size = 14) +
  theme(legend.position = 'bottom') +
  # ylim(NA, 3.1) +
  # xlim(NA, 3.5) +
  scale_color_viridis_d(begin = 0, end = 0.8) +
  # scale_color_manual(values = c("#f89540", "#0072B2","#f89540", "#0072B2")) +
  labs(color = 'Mean Canopy Load (mm)') + facet_wrap(~name_pretty, scales = 'free_x')

ggsave('figs/results/explore/scl_q_unld_vs_bins.png', width = 6, height = 5)
