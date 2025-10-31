# Plot exploratory relationship between unloading and binned independent vars

## Exploratory plots ---- 

### wind vs unloading rate ----

xlabs_dict <- data.frame(
  name = c('temp_labs',
           'wind_labs',
           'tau_labs',
           'ti_dep_labs',
           'canopy_snowmelt_labs',
           'subl_labs'),
  name_pretty = c('Air Temperature (°C)',
                  'Wind Speed (m s⁻¹)',
                  'Shear Stress (N m⁻²)',
                  'Ice Bulb Temp. Depression (°C)',
                  'Snowmelt (mm hr⁻¹)',
                  'Sublimation (mm hr⁻¹)')
)

# Create a small data frame with manual labels for the specific facets
manual_labels <- data.frame(
  name_pretty = c("Snowmelt (mm hr⁻¹)", "Sublimation (mm hr⁻¹)"),  # facets to label
  x = c(0.65, 0.05),   # x-position for the label in each facet
  y = c(2.7, 2.7),   # y-position for the label in each facet (adjust as needed)
  label = c("*Simulated", "*Simulated")
)
manual_labels$name_pretty <- factor(
  manual_labels$name_pretty,
  levels = levels(bins_df$name_pretty)
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

# Convert name_pretty to a factor with desired order
bins_df$name_pretty <- factor(
  bins_df$name_pretty,
  levels = c('Air Temperature (°C)',
             'Wind Speed (m s⁻¹)',
             'Shear Stress (N m⁻²)',
             'Ice Bulb Temp. Depression (°C)',
             'Snowmelt (mm hr⁻¹)',
             'Sublimation (mm hr⁻¹)')
)

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
  ylab(expression("Unloading Rate (mm hr"^"-1"*")")) +
  xlab(element_blank()) +
  theme_bw() +
  theme(
    legend.position = 'bottom',
    strip.background = element_blank(),
    strip.placement = 'outside',
    panel.spacing = unit(0.5, "lines")  # <-- increase spacing between facets
  ) +
  # theme_bw(base_size = 14) +
  # ylim(NA, 3.1) +
  # xlim(NA, 3.5) +
  scale_color_viridis_d(begin = 0, end = 0.8) +
  # scale_color_manual(values = c("#f89540", "#0072B2","#f89540", "#0072B2")) +
  labs(color = 'Canopy Load (mm)') + 
  geom_text(
    data = manual_labels,
    aes(x = x, y = y, label = label),
    inherit.aes = FALSE,
    # fontface = "bold",
    vjust = -0.5,
    size = 3   # smaller text
  ) +
  facet_wrap(~name_pretty, scales = 'free_x', strip.position = 'bottom') 


# ggsave('figs/results/explore/scl_q_unld_vs_bins.png', width = 6.1, height = 5)
ggsave('figs/final/figure4.png', width = 6.5, height = 5)
