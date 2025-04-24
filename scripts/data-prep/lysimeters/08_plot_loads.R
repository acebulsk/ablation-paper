# Plot lysimeter data over each event

obs_mod_sub_cml <- obs_mod_subl_cpy_df_out |> 
  select(-event_id) |> 
  inner_join(canopy_snow_long |> select(datetime, event_id)) |>
  group_by(tree_cal_trough_name, event_id) |>
  mutate(value = cumsum(obs_tree_mod_subl_cpy_mm_15min),
         name = 'obs_mod_subl') |>
  ungroup() |> 
  select(datetime,
         name,
         tree_cal_trough_name,
         value, event_id) 

name_dict <- data.frame(
  name = c('closed',
               'mixed',
               'sparse'),
  new_name = c('closed',
               'mixed',
               'sparse')
)
scl_df_long <- q_unld_met_scl |> 
  select(-tree_cal_cc) |> 
  inner_join(canopy_snow_long |> select(datetime, event_id)) |>
  # inner_join(canopy_snow_long_pre_post |> select(datetime, event_id)) |> 
  group_by(event_id, name) |> 
  mutate(value = value - min(value),) |> 
  left_join(name_dict) |> 
  mutate(name = 'scl') |> 
  select(datetime, name, tree_cal_trough_name = name, value, event_id)

weighed_tree_zeroed_fltr <- weighed_tree_zeroed |> 
  mutate(name = 'w_tree') |>
  select(-event_id) |> 
  inner_join(canopy_snow_long |> select(datetime, event_id), by = 'datetime') |>
  select(datetime, name, tree_cal_trough_name, value = tree_mm, event_id)

weighed_tree_zeroed_fltr |>
  filter(event_id == "2022-04-05") |>
  ggplot(aes(datetime, value, colour = name)) +
  geom_line() +
  geom_point() +
  facet_grid(rows = vars(name), cols = vars(tree_cal_trough_name), scales = 'free')

plot_df <- rbind(obs_mod_sub_cml,
                 scl_df_long) |> 
  rbind(weighed_tree_zeroed_fltr) 

# look at all periods snow in the canopy to find where events have minimal unloading with some sublimation ----

# plot one
plot_df |>
  filter(event_id == "2021-12-14") |>
  ggplot(aes(datetime, value, colour = name)) +
  geom_line() +
  geom_point() +
  facet_grid(rows = vars(name), cols = vars(tree_cal_trough_name), scales = 'free')
ggplotly()

# plot all and save to figs
lapply(
  canopy_snow_events$event_id |> unique(),
  plot_facet_two_vars,
  cur_load_met = plot_df,
  cur_var_names = plot_df$name |> unique(),
  filename_prefix = 'figs/supplement/load_plots/scl_mod_subl_w_tree/timeseries_lysimeters_mod_obs_subl_'
)
