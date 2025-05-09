# Script to combine the data and plot outputs from the wind and tau scripts in
# this folder

# combine stats ---- 
wind_stats <- readRDS('data/modelled_wind_unloading_error_table.rds') |> rename(Wind = Value) 
tau_stats <- readRDS('data/modelled_tau_unloading_error_table.rds') |> rename(`Shear Stress` = Value)
stats_out <- left_join(wind_stats, tau_stats)

saveRDS(stats_out, 'data/modelled_combined_wind_tau_unloading_error_table.rds')

# combine plots ---- 
plot_wind_tau_df <- rbind(
  wind_plot_df |> mutate(group = 'Wind Speed (m s⁻¹)'),
  tau_plot_df |> mutate(group = 'Shear Stress (N m⁻²)')
)

ggplot(plot_wind_tau_df, aes(x=x_var_value)) +
  # geom_line(aes(tau_labs, new_predicted_y_nls, colour = factor(tree_labs)), linetype = 'dashed') +
  geom_line(aes(y = new_predicted_y, colour = factor(tree_labs))) +
  geom_errorbar(
    aes(
      ymax = sd_hi,
      ymin = sd_low,
      width = 0,
      colour = as.character(round(tree_labs))
    )) +
  geom_point(aes(y = q_unl_avg, colour = as.character(round(tree_labs))),
             size = 2) +
  ylab(expression("Unloading Rate (mm"~ hr^-1 * ")")) +
  xlab(element_blank()) +
  scale_color_viridis_d(begin = 0, end = 0.8) +
  labs(colour = 'Canopy Snow\nLoad (mm)') + 
  facet_wrap(~group, scales = 'free') 
  
ggsave(
  'figs/results/modelled_tau_wind_unloading_w_obs.png',
  width = 7,
  height = 3,
  device = png
)
