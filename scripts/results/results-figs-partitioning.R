# Script to reproduce figures for the results section on partitioning
# So far showing occasionally large residuals, potentially due to poor modelled sublimation / wind redistriubtion and misaslignment between troughs and tree

# NOTES:
# getting better aggreement with w tree and troughs for 2021/2022 yr vs 2022/2023 possibly due to different tree cals
total_abl_fltr <- 1 # minimum snow ablated to include the event

## tidy data ----

labs <- data.frame(partition_type = c('q_unl', 'obs_tree_mod_subl_cpy', 'residual'),
                   plot_name = c('Unloading + Drip', 'Simulated Sublimation', 'Residual + error'))

# PLOTS ---- 

# less unloading measured by troughs than expected for these events, these were classified based on the figs under figs/supplement/load_plots

# might expect moore loss from tree compared to troughs anyway due to diff in inst. location so these might not be valid...
possible_wind_redistribution <- c('2021-12-27',
                                  '2022-02-04', # large increase in wind coincides with loss of snow in weighed tree but large increase not seen in troughs
                                  # '2023-02-24', # tree only higher by 2.5 mm so 10% wind redistribution
                                  '2023-03-27', # on the edge between wind redistribution or just mismatch between tree and troughs
                                  '2023-04-03' # this one more likely to be both wind redist and sublimation underestimation, peak winds up to 4 m/s
                                  
                                  ) 

events_bad_sublimation <- c('2022-03-04',
                            '2022-03-09', # 100% humidity!! NOT WINDY modelled sublimation may be underestimating, due to high humidity observed at mid canopy which may be a result of canopy sublimation...
                            '2022-03-29', # peak avg wind 1.5 m/s - likely underestimate mod sub, little unloading to troughs here
                            '2022-04-21', # NOT WINDY modelled sublimation may be underestimating, low humidity herre!! 
                            '2022-04-23', # NOT WINDY modelled sublimation may be underestimating, due to high humidity observed at mid canopy which may be a result of canopy sublimation...
                            '2022-06-23', # based on the data looks like wind redistribution but timelapse imagery shows clear sublimation, maybe poor handling of liquid water sublimation/ 0-4 deg. C temps.... peak avg wind speed 3.5 m/s coincides with canopy snow ablation thats not explained by sublimation or unloading to troughs
                            '2022-06-24', # very warm up to 5 deg. C could be bad sublimation or just diff between tree and troughs
                            '2022-12-01', # NOT WINDY modelled sublimation may be underestimating, due to high humidity observed at mid canopy which may be a result of canopy sublimation...
                            '2023-03-25', # sublimation overestimated here possibly tree = troughs
                            '2023-04-17' # possibly over estimate mod subl or tree undercaled
                            ) 

events_good_sublimation <- c('2022-03-24',
                            '2023-04-03')


events_fltr <- canopy_snow_events |> 
  filter(quality < 3, # these have all around bad ablation data
         weighed_tree_quality < 3) |>  # these have bad weighed tree observations and thus do not have sublimation measurements for the following paritioning equations, useful to look at these events later on when just looking at the rates
  pull(event_id)

fltr_for_events <- q_unld_scl |> 
  left_join(q_unld_tree) |> 
  left_join(q_subl) |> 
  left_join(ft_met) |> 
  mutate(
    obs_tree_mod_subl_cpy = ifelse(
      obs_tree_mod_subl_cpy > q_tree_ablation,
      q_tree_ablation,
      obs_tree_mod_subl_cpy
    )
  ) |> # limit modelled sublimation to what was actually removed from the weighed tree, this still may over estimate as does not consider the concurrent unloading/drip but we dont have 100% confidence in the timining to do that subtraction
  filter(
    event_id %in% events_fltr)

# plot time series 
fltr_for_events |> 
  # filter(event_id == events_fltr[16]) |> 
  filter(event_id == '2022-03-16') |> 
  select(datetime,
         event_id,
         trough_name = name,
         q_unl,
         q_tree_ablation,
         obs_tree_mod_subl_cpy) |>
  mutate(q_unl_plus_subl = q_unl + obs_tree_mod_subl_cpy) |> 
  pivot_longer(c(
    q_unl,
    q_tree_ablation,
    obs_tree_mod_subl_cpy,
    q_unl_plus_subl
  )) |>
  group_by(name, trough_name) |> 
  mutate(value = cumsum(value/4)) |>  # mm/hr to mm/interval
  ggplot(aes(datetime, value, colour = name)) +
  geom_line() +
  facet_wrap(~event_id+trough_name, scales = 'free')

fltr_by_event <- fltr_for_events |> 
  select(datetime,
         event_id,
         value,
         name,
         obs_canopy_load,
         q_unl,
         q_tree_ablation,
         obs_tree_mod_subl_cpy,
         t,
         u) |>
  group_by(event_id, name) |> # group by name since we dont want to sum over different troughs
  mutate(
    n = n(),
    cuml_unld_by_event = sum(q_unl/4),
    cuml_cpy_abl_by_event = sum(q_tree_ablation/4),
    cuml_subl_by_event = sum(obs_tree_mod_subl_cpy/4),
    residual_by_event = cuml_cpy_abl_by_event - (cuml_unld_by_event + cuml_subl_by_event)) |> 
  filter(n > 4) |> 
  group_by(event_id) |>  # now avg over the troughs
  summarise(
    event_t = mean(t),
            event_u = mean(u),
            cuml_unld_by_event = mean(cuml_unld_by_event),
            cuml_subl_by_event = mean(cuml_subl_by_event),
            cuml_cpy_abl_by_event = mean(cuml_cpy_abl_by_event),
            residual_by_event = mean(residual_by_event),
            cuml_unld_plus_subl = cuml_unld_by_event + cuml_subl_by_event,
            frac_unld = cuml_unld_by_event / cuml_cpy_abl_by_event,
            frac_resid = residual_by_event/cuml_cpy_abl_by_event,
            frac_subl =  cuml_subl_by_event/cuml_cpy_abl_by_event,
            check_sum = frac_unld + frac_resid + frac_subl) |> 
  ungroup() |> 
  as.data.frame()

# remove outlier events 
fltr_by_event <- fltr_by_event |> 
  filter(!event_id %in% possible_wind_redistribution,
         !event_id %in% events_bad_sublimation,
         cuml_cpy_abl_by_event > 2
         )


y_axis_fracs <- 'Fraction of Event Weighed Tree Ablation (-)'
wind_axis <- 'Mean Event Wind Speed (m/s)'
temp_axis <- 'Mean Event Air Temperature (°C)'

ggplot(fltr_by_event |> 
         filter(cuml_cpy_abl_by_event > total_abl_fltr) |> 
         # filter(abs(frac_resid) < .5) |> # negative residuals we know our weighed tree and lysimeters are not lining up
         pivot_longer(frac_unld:frac_subl), aes(event_u, value, fill = name)) +
  geom_bar(stat = 'identity', position = 'stack', width = .025, colour = 'black')

ggplot(fltr_by_event |> 
         filter(cuml_cpy_abl_by_event > total_abl_fltr) |> 
         # filter(abs(frac_resid) < .5) |> # negative residuals we know our weighed tree and lysimeters are not lining up
         pivot_longer(frac_unld:frac_subl),
       aes(event_t, value, fill = name)) +
  geom_bar(stat = 'identity', position = 'stack', width = 0.25, colour = 'black')

ggplot(fltr_by_event |> 
         filter(cuml_cpy_abl_by_event > total_abl_fltr) |> 
         pivot_longer(c(cuml_unld_plus_subl, cuml_cpy_abl_by_event)),
       aes(event_u, value, fill = name)) +
  geom_bar(stat = 'identity', position = 'dodge', width = .1, colour = 'black')

fltr_by_event_long <- fltr_by_event |> 
  filter(cuml_cpy_abl_by_event > total_abl_fltr) |> 
  pivot_longer(cuml_unld_by_event:residual_by_event) 

ggplot(fltr_by_event_long, aes(event_t, value, fill = name)) +
  geom_bar(stat = 'identity', position = 'stack', width = .2, colour = 'black')

ggplot(fltr_by_event_long, aes(event_u, value, fill = name)) +
  geom_bar(stat = 'identity', position = 'stack', width = .015, colour = 'black')

# or by point

fltr_by_event_long <- fltr_by_event |> 
  filter(cuml_cpy_abl_by_event > total_abl_fltr) |> 
  pivot_longer(
    c(
      cuml_unld_plus_subl,
      cuml_cpy_abl_by_event,
      cuml_subl_by_event,
      cuml_unld_by_event,
      residual_by_event
    )
  )

ggplot(fltr_by_event_long, aes(event_t, value, colour = name)) +
  geom_point() +
  facet_grid(vars(name), scales = 'free')

ggplot(fltr_by_event_long, aes(event_u, value, colour = name)) +
  geom_point() +
  facet_grid(vars(name), scales = 'free')

## avg 15-min rates for the entire 2 years ---- 

fltr <- fltr_for_events |> 
  left_join(ft_met_binned, by = 'datetime') |> 
  mutate(residual = q_tree_ablation - q_unl) |> 
  filter(
    residual >= -5, # removes a couple large residuals just due to mismatched timing 
    residual <= 5, 
    name %in% scl_names,
    !event_id %in% possible_wind_redistribution,
    !event_id %in% events_bad_sublimation,
    # name %in% 'mixed',
    q_unl < 7,
    q_tree_ablation > min_qunld,
    q_unl > min_qunld) 


unld_partition_lims <- c(-0.05, 1.1)

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


p <- rbind(measured_df, sim_df) |> 
  left_join(labs, by = 'partition_type') |> 
  ggplot(aes(group, mean_partition_value, fill = plot_name)) + 
  geom_bar(stat = 'identity', position = 'stack', width = 0.5, colour = 'black')  +
  ylab('Ablation Rate (mm/hr)') +
  xlab(element_blank()) +
  theme_bw(base_size = 13) +
  theme(legend.title = element_blank(),
        legend.position = 'bottom') +
  scale_fill_manual(values = c("lightgray", "#f89540", "#0072B2"))

p 

ggsave('figs/results/canopy_snow_ablation_partition_global.png', device = png, width = int_fig_width, height = int_fig_height, units = "in")

wind_p <- fltr |> 
  filter(t < -6) |>
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

wind_p

ggsave(
  'figs/results/canopy_snow_ablation_partition_vs_mid_canopy_wind.png',
  device = png,
  width = 6,
  height = 4,
  units = "in"
)

temp_p <- fltr |> 
  filter(u < 2) |>
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
temp_p

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

