# Plot accumulation before serious ablation

select_events <- c(
  '2021-12-23',
  '2021-12-11', # filter
  # '2022-01-17',
  # '2022-02-19',
  # '2022-03-07',
  '2022-03-19',
  # '2022-03-24',
  '2022-05-09',
  # '2022-05-18',
  # '2022-06-12', # can add back just had bad time display
  '2022-11-25', # can add with filter below but is cut off
  # '2023-02-19', # removed for space can add back
  '2023-03-13'
  # '2023-03-24', # filter
  # '2023-04-09'
)

# this one is without weighed tree assimilation
crhm_output_tree <- CRHMr::readOutputFile(
  "../../analysis/crhm-analysis/output/weighed_tree_vector_based_new_ablation_psp/2024-12-03-15-46-45_weighed_tree_vector_based_new_ablation_psp_output.txt",
  timezone = 'Etc/GMT+6'
)

# periods to remove that are ablation
filter_times <- c(
  seq(as.POSIXct('2021-12-11 22:00:00', tz = 'Etc/GMT+6'),
               as.POSIXct('2021-12-22 00:00:00', tz = 'Etc/GMT+6'),
               by = 60*15),
  seq(as.POSIXct('2021-12-23 14:15:00', tz = 'Etc/GMT+6'),
      as.POSIXct('2022-01-02 00:00:00', tz = 'Etc/GMT+6'),
      by = 60*15),
  seq(as.POSIXct('2022-01-18 07:00:00', tz = 'Etc/GMT+6'),
      as.POSIXct('2022-02-17 22:30:00', tz = 'Etc/GMT+6'),
      by = 60*15),
  seq(as.POSIXct('2022-02-19 18:00:00', tz = 'Etc/GMT+6'),
      as.POSIXct('2022-03-06 22:30:00', tz = 'Etc/GMT+6'),
      by = 60*15),
  seq(as.POSIXct('2022-03-19 23:45:00', tz = 'Etc/GMT+6'),
      as.POSIXct('2022-03-23 22:30:00', tz = 'Etc/GMT+6'),
      by = 60*15),
  seq(as.POSIXct('2022-05-09 04:45:00', tz = 'Etc/GMT+6'),
      as.POSIXct('2022-05-12 22:30:00', tz = 'Etc/GMT+6'),
      by = 60*15),
  seq(as.POSIXct('2022-06-14 03:00:00', tz = 'Etc/GMT+6'),
      as.POSIXct('2022-06-18 22:30:00', tz = 'Etc/GMT+6'),
      by = 60*15),
  seq(as.POSIXct('2022-11-25 20:15:00', tz = 'Etc/GMT+6'),
      as.POSIXct('2023-02-18 22:30:00', tz = 'Etc/GMT+6'),
      by = 60*15),
  seq(as.POSIXct('2023-02-20 09:15:00', tz = 'Etc/GMT+6'),
      as.POSIXct('2023-03-12 22:30:00', tz = 'Etc/GMT+6'),
      by = 60*15),
  seq(as.POSIXct('2023-03-14 00:15:00', tz = 'Etc/GMT+6'),
      as.POSIXct('2023-03-19 22:30:00', tz = 'Etc/GMT+6'),
      by = 60*15),
  seq(as.POSIXct('2023-03-14 00:15:00', tz = 'Etc/GMT+6'),
      as.POSIXct('2023-03-19 22:30:00', tz = 'Etc/GMT+6'),
      by = 60*15))

load_suffix <- 'fsd_closed_0.88'

# weighed tree zeroed prior to snowfall events
obs_tree <-
  readRDS(
    paste0(
      '../../analysis/ablation/data/unloading_events_zero_weighed_tree_kg_m2_pre_post_cnpy_snow_',
      load_suffix,
      '.rds')) |> 
  select(datetime, event_id, Observed = value) |> 
  filter(event_id %in% select_events,
         !datetime %in% filter_times)

mod_tree <- crhm_output_tree |> 
  select(datetime, Simulated = Snow_load.1)
# baseline_mod_tree <- baseline_crhm_output_tree |> 
#   select(datetime, simulated_baseline = Snow_load.1)
obs_mod_tree <- left_join(obs_tree, mod_tree) |>
  # left_join(baseline_mod_tree) |> 
  pivot_longer(!c(datetime, event_id)) |> 
  mutate(group = 'Canopy Snow Load (mm)') |> 
  group_by(event_id,  name) |> 
  mutate(value = value - first(value))

ggplot(obs_mod_tree, aes(datetime, value, colour = name)) +
  geom_line() +
  # geom_line(data = mod_baseline, aes(datetime, Snow_load.1, color = "Baseline")) +
  # geom_line(data = mod_new, aes(datetime, Snow_load.1, color = "New Model")) +
  # geom_point(data = load_intercept_df, aes(datetime, tree_mm, color = "Observation"), size = 0.5) +
  # geom_point(data = tree_accum_ablate_df, aes(datetime, obs, color = "Observation"), size = 0.5) +
  # scale_color_manual(values = c("Baseline" = "#E69F00", "New Model" = "dodgerblue", "Observation" = "#009E73")) +
  labs(color = "Data Source") +
  theme_minimal() +
  ylab('Canopy Snow Load (mm)')

ggplot(obs_mod_tree, aes(datetime, value, linetype = name)) +
  geom_line() +
  # geom_line(data = mod_baseline, aes(datetime, Snow_load.1, color = "Baseline")) +
  # geom_line(data = mod_new, aes(datetime, Snow_load.1, color = "New Model")) +
  # geom_point(data = load_intercept_df, aes(datetime, tree_mm, color = "Observation"), size = 0.5) +
  # geom_point(data = tree_accum_ablate_df, aes(datetime, obs, color = "Observation"), size = 0.5) +
  # scale_color_manual(values = c("Baseline" = "#E69F00", "New Model" = "dodgerblue", "Observation" = "#009E73")) +
  labs(color = "Data Source") +
  # theme_minimal() +
  ylab('Canopy Snow Load (mm)') +
  xlab(element_blank()) +
  facet_wrap(~event_id, scales = 'free', nrow =3) +
  theme(legend.title = element_blank(),
        legend.position = 'bottom')

ggsave(
  paste0(
    'figs/crhm-analysis/initial_intercept/crhm_vs_tree_initial_',
    format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
    '.png'
  ),
  width = 8,
  height = 6
)

# scatter ----
obs_mod_tree <- left_join(obs_tree, mod_tree)

ggplot(obs_mod_tree, aes(Observed, Simulated)) + 
  geom_point() + 
  geom_abline()

obs_mod_tree_err_tbl <- obs_mod_tree |> 
  mutate(diff = Observed - Simulated,
         rel_error = ifelse(Observed > 0, abs(diff / Observed), 0)) |> 
  # group_by(name) |> 
  summarise(
    # `Mean Bias` = mean(diff, na.rm = T), # not useful for time series
    `Mean Relative Error` = mean(abs(diff / Observed)[Observed != 0], na.rm = T),
    MAE = mean(abs(diff), na.rm = T),
    `RMS Error` = sqrt(mean(diff ^ 2, na.rm = T)),
    # NRMSE = `RMS Error` / (max(Observed, na.rm = TRUE) - min(Observed, na.rm = TRUE)), # harder to understand... 
    NRMSE = `RMS Error` / mean(Observed, na.rm = T),
    R = cor(Observed, Simulated),
    `r^2` = R^2) |> 
  mutate(across(`Mean Bias`:`r^2`, round, digits = 3))

write_csv(obs_mod_tree_err_tbl,
          paste0(
            'tbls/crhm-snow-load-vs-wtree-errortbl',
            '_',
            format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
            '.csv'
          ))
