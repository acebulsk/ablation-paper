# Script to reproduce figures for the results section on partitioning

## tidy data ----
fltr <- met_unld |> 
  left_join(q_unld_tree) |> 
  mutate(residual = q_tree_ablation - q_unl) |> 
  filter(
    name %in% scl_names,
    # q_unl < 7,
    q_unl > min_qunld,
    tree_mm > min_canopy_snow) 

labs <- data.frame(partition_type = c('q_unl', 'obs_tree_mod_subl_cpy', 'residual'),
                   plot_name = c('Unloading + Drip', 'Simulated Sublimation', 'Residual + error'))

measured_df <- fltr |> 
  pivot_longer(c(q_unl, residual), names_to = 'partition_type', values_to = 'partition_value') |> 
  # group_by(tau_labs = tau_top_labs) |>
  group_by(partition_type) |> 
  summarise(mean_partition_value = mean(partition_value, na.rm = T),
            sd = sd(partition_value, na.rm = T),
            ci_low = quantile(partition_value, 0.1, na.rm = T),
            ci_hi = quantile(partition_value, 0.90, na.rm = T),
            n = n(), 
            group = 'Measured')

sim_df <- fltr |> 
  pivot_longer(c(q_unl, obs_tree_mod_subl_cpy), names_to = 'partition_type', values_to = 'partition_value') |> 
  # group_by(tau_labs = tau_top_labs) |>
  group_by(partition_type) |> 
  summarise(mean_partition_value = mean(partition_value, na.rm = T),
            sd = sd(partition_value, na.rm = T),
            ci_low = quantile(partition_value, 0.1, na.rm = T),
            ci_hi = quantile(partition_value, 0.90, na.rm = T),
            n = n(), 
            group = 'Measured + Simulated')

# Data Summary ---- 

# could add on here the portion of snow redistributed to the pluvio during high winds (i.e. a rectangle around part of the top wind speed bar)

low_wind_events <- c('2023-02-28', '2023-03-14', '2022-12-21')
entrainment_events <- c('2021-12-19', '2022-12-01', '2023-02-24')
events <- c(entrainment_events, low_wind_events)

ffr_met |> 
  pivot_longer(!datetime) |> 
  left_join(canopy_snow_long) |>
  filter(storm_id %in% low_wind_events) |> 
  ggplot(aes(datetime, value, colour = name)) + 
  geom_line() +
  facet_grid(name~storm_id, scales = 'free')

ffr_met |> 
  pivot_longer(!datetime) |> 
  left_join(canopy_snow_long) |>
  filter(storm_id %in% entrainment_events) |> 
  ggplot(aes(datetime, value, colour = name)) + 
  geom_line() +
  facet_grid(name~storm_id, scales = 'free')

pcp_events <- ffr_met |> 
  left_join(canopy_snow_long) |> 
  filter(storm_id %in% events,
         p > 0,
         p < 0.2) 

ggplot(pcp_events, aes(u, p*4)) + 
  geom_point() +
  ylim(c(0, NA)) +
  xlim(c(0, NA)) +
  ylab('Wind Redistribution (mm/hr)') +
  xlab('Wind Speed (m/s)') +
  geom_smooth(method = 'lm', se = F)

lm_mod <- lm(p*4~u, data = pcp_events)
summary(lm_mod)

# sig. relationship but its not physically realistic ... we are getting too high
# wind redistribution at low wind speeds also looking at the partitioning bar
# graph we can see that wind redistribution does not make up a significant
# portion of ablation in a wind exposed forest

# PLOTS ---- 

unld_partition_lims <- c(-0.05, 1.1)

p <- rbind(measured_df, sim_df) |> 
  left_join(labs, by = 'partition_type') |> 
  ggplot(aes(group, mean_partition_value, fill = plot_name)) + 
  geom_bar(stat = 'identity', position = 'stack', width = 0.5, colour = 'black')  +
  ylab('Ablation Rate (mm/hr)') +
  xlab(element_blank()) +
  theme_bw(base_size = 13) +
  theme(legend.title = element_blank()) +
  scale_fill_manual(values = c("lightgray", "#f89540", "#0072B2"))

p 

ggsave('figs/results/canopy_snow_ablation_partition_global.png', device = png, width = int_fig_width+4, height = int_fig_height, units = "in")

wind_p <- fltr |> 
  group_by(wind_labs) |>
  # group_by(tau_labs = tau_top_labs) |>
  summarise(`Unloading + Drip` = mean(q_unl, na.rm = T),
            `Sublimation` = mean(obs_tree_mod_subl_cpy, na.rm = T),
            `Residual + error` = mean(residual, na.rm =T),
            q_unl_sd = sd(`Unloading + Drip`, na.rm = T),
            ci_low = quantile(`Unloading + Drip`,0.05),
            ci_hi = quantile(`Unloading + Drip`, 0.95),
            n = n()) |> 
  filter(n > 3) |> 
  pivot_longer(c(#`Unloading + Drip`,
                 `Sublimation`,
                 `Residual + error`)) |> 
  ggplot(aes(wind_labs, value, fill = name)) + 
  geom_bar(stat = 'identity', position = 'dodge') +
  # geom_point(data = fltr, aes(tau_top_labs, q_unl)) +
  # geom_errorbar(aes(ymax = sd_hi, ymin = sd_low))  +
  # geom_errorbar(aes(ymax = ci_hi, ymin = ci_low))  +
  ylab('Ablation Rate (mm/hr)') +
  xlab(bin_wnd_ax_lab) +
  theme_bw() +
  # theme_bw(base_size = 13) +
  theme(legend.title = element_blank(),
        legend.position = 'none') +
  ylim(unld_partition_lims)  +
  scale_fill_manual(values = c("lightgray", "#f89540"))

ggsave(
  'figs/results/canopy_snow_ablation_partition_vs_mid_canopy_wind.png',
  device = png,
  width = 6,
  height = 4,
  units = "in"
)

temp_p <- fltr |> 
  group_by(temp_labs) |>
  # group_by(tau_labs = tau_top_labs) |>
  summarise(`Unloading + Drip` = mean(q_unl, na.rm = T),
            `Sublimation` = mean(obs_tree_mod_subl_cpy, na.rm = T),
            `Residual + error` = mean(residual, na.rm =T),
            q_unl_sd = sd(`Unloading + Drip`, na.rm = T),
            ci_low = quantile(`Unloading + Drip`,0.05),
            ci_hi = quantile(`Unloading + Drip`, 0.95),
            n = n()) |> 
  filter(n > 4) |> 
  pivot_longer(c(#`Unloading + Drip`,
                 `Residual + error`,
                 `Sublimation`)) |>
  ggplot(aes(temp_labs, value, fill = name)) + 
  geom_bar(stat = 'identity', position = 'dodge') +
  # geom_point(data = fltr, aes(tau_top_labs, q_unl)) +
  # geom_errorbar(aes(ymax = sd_hi, ymin = sd_low))  +
  # geom_errorbar(aes(ymax = ci_hi, ymin = ci_low))  +
  ylab('Ablation Rate (mm/hr)') +
  xlab(temp_bin_ax_lab) +
  theme_bw() +
  # theme_bw(base_size = 13) +
  theme(legend.title = element_blank(),
        legend.position = 'bottom') +
  ylim(unld_partition_lims) +
  scale_fill_manual(values = c("lightgray", "#f89540"))

ggsave(
  'figs/results/canopy_snow_ablation_partition_vs_air_temp.png',
  device = png,
  width = 6,
  height = 4.5,
  units = "in"
)

cowplot::plot_grid(wind_p, temp_p, nrow = 2, rel_heights = c(0.45, 0.55))

ggsave(
  'figs/results/canopy_snow_ablation_partition_cowplot_temp_wind.png',
  width = 8,
  height = 5
)
