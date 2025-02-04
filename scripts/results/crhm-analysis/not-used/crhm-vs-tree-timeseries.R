# Plot mod vs obs tree timeseries

select_events <- c(
  # '2021-12-23',
  '2021-12-11', # filter
  # '2022-01-17',
  # '2022-02-19',
  # '2022-03-07',
  '2022-03-19',
  # '2022-03-24',
  '2022-05-09',
  # '2022-05-18',
  '2022-06-12',
  '2022-11-25', # can add with filter below but is cut off
  # '2023-02-19',
  '2023-03-13'
  # '2023-03-24', # filter
  # '2023-04-09'
)

obs_tree <-
  readRDS('../../analysis/ablation/data/unloading_events_zero_weighed_tree_mm_pre_post_cnpy_snow.rds') |> 
  select(datetime, event_id, Observed = value)

obs_mod_ts <- obs_tree |> 
  filter(event_id %in% select_events) |> 
  left_join(crhm_output_tree |>  select(datetime, Simulated = Snow_load.1)) |> 
  pivot_longer(!datetime:event_id) |> 
  mutate(year = year(datetime))

ggplot(obs_mod_ts, aes(datetime, value, colour = name)) +
  geom_line() +
  facet_wrap(~event_id, scale = 'free') + 
  ylab('Canopy Snow Load (mm)') +
  xlab(element_blank()) +
  theme(legend.title = element_blank())
