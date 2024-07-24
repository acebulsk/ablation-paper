# Compute unloading as a binary 

## Tidy Data ---- 

met_unld$binary_unl <- ifelse(met_unld$q_unl > min_qunld, 1, 0)

prob_unl_df <- met_unld |> 
  filter(name %in% scl_names,
         inst_type == 'subcanopy trough') |> # this one is redundant but pedantic!  
  group_by(temp_labs, wind_labs) |> 
  summarise(n_unld_events = sum(binary_unl),
            N_condition = n(),
            prob_unl_df = n_unld_events/N_condition) |> 
  filter(N_condition > 0,
         n_unld_events > 0)

ggplot(prob_unl_df, aes(x = wind_labs, y = temp_labs, fill = prob_unl_df)) +
  geom_raster() +
  labs(fill='Probability\nof Unloading + Drip (-)') +
  scale_fill_viridis_c() +
  ylab(temp_bin_ax_lab) +
  xlab(bin_wnd_ax_lab) +
  theme_bw(base_size = 13) +
  theme(legend.position = 'bottom') 

ggsave(
  'figs/results/probability_of_unloading_wind_air_temp.png',
  device = png,
  width = 4,
  height = 4,
  units = "in"
)

ggplot(prob_unl_df, aes(x = wind_labs, y = temp_labs, fill = n_unld_events)) +
  geom_raster() +
  labs(fill=' Frequency\n of Unloading + Drip') +
  scale_fill_viridis_c(option = 'magma') +
  ylab(temp_bin_ax_lab) +
  xlab(bin_wnd_ax_lab)+
  theme_bw(base_size = 13) +
  theme(legend.position = 'bottom') 

ggsave(
  'figs/results/frequency_of_unloading_wind_air_temp.png',
  device = png,
  width = 4,
  height = 4,
  units = "in"
)