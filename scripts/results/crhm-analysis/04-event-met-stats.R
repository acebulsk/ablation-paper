# This script creates a table with average met information for each select
# ablation event where we test the canopy snow model

# script to met stats over the lysimeter snow interception periods

wind_threshold <- 999 # m/s

temp_ax_lab <- 'Air Temperature (°C)'
wind_ax_lab <- 'Wind Speed (m/s)'
pretty_names_vect <- c(
  temp_ax_lab,
  "Relative Humidity (%)",
  wind_ax_lab
)

var_name_dict <-
  data.frame(
    name = c('t', 'rh', 'u'),
    pretty_name = pretty_names_vect
  )

event_met <- select_events_long |>
  left_join(ft_met) |> 
  left_join(mod_d_drip_smry_frac)

event_df_long <- event_met |>
  pivot_longer(c(t:u)) |>
  left_join(var_name_dict, by = 'name') |>
  mutate(pretty_name = factor(pretty_name, levels = c(pretty_names_vect))) |>
  group_by(pretty_name)

# histogram of 15-min met data ----
event_df_long |>
  mutate(mean_value = mean(value, na.rm = TRUE)) |>  # Calculate the mean per group
  ggplot(aes(x = value)) +
  geom_histogram(color = 'black', fill = 'darkgray') +
  geom_vline(aes(xintercept = mean_value), color = 'red', linetype = 'dashed') +  # Add vertical line at mean
  facet_wrap(~pretty_name, scales = 'free') +
  xlab(element_blank()) +
  ylab('Count (-)')

ggsave('figs/crhm-analysis/met-figs/histogram_met_select_ablation_events.png', width = 6, height = 4, device = png)

# table of event met stats ----

event_avgs <- event_met |>
  group_by() |>
  summarise(
    # duration = difftime(max(datetime), min(datetime), units = 'hours'),
    melt = first(melt),
    wind = first(wind),
    sublimation = first(sublimation),
    t = mean(t, na.rm = TRUE),
    rh = mean(rh, na.rm = TRUE),
    u = mean(u, na.rm = TRUE)
  )

# saveRDS(event_avgs, 'data/event_met/event_avgs.rds')

event_avgs_maxmin <- event_met |>
  group_by(event_id) |>
  summarise(
    min_t = min(t, na.rm = TRUE),
    mean_t = mean(t, na.rm = TRUE),
    max_t = max(t, na.rm = TRUE),
    
    min_u = min(u, na.rm = TRUE),
    mean_u = mean(u, na.rm = TRUE),
    max_u = max(u, na.rm = TRUE),
    
    min_rh = min(rh, na.rm = TRUE),
    mean_rh = mean(rh, na.rm = TRUE),
    max_rh = max(rh, na.rm = TRUE),
    
    melt = first(melt),
    wind = first(wind),
    sublimation = first(sublimation)
    
  )  |>
  mutate(event_type = case_when(
    melt >= 0.6 ~ 'melt',
    sublimation >= 0.6 ~ 'sublimation',
    wind >= 0.6 ~ 'wind',
    TRUE ~ 'mixed'
  )
  )



saveRDS(event_avgs_maxmin, 'data/event_met/event_avgs_maxmin.rds')

# pretty table of event met stats ----
library(gt)
pretty_table <- event_avgs_maxmin |>
  arrange(event_type) |>
  select(
    event_id,
    event_type,
    starts_with("min_"),
    starts_with("mean_"),
    starts_with("max_"),
    melt, wind, sublimation
  ) |>
  gt() |>
  tab_spanner(
    label = "Air Temperature (°C)",
    columns = ends_with("_t")
  ) |>
  tab_spanner(
    label = "Wind Speed (m/s)",
    columns = ends_with("_u")
  ) |>
  tab_spanner(
    label = "Relative Humidity (%)",
    columns = ends_with("_rh")
  ) |>
  tab_spanner(
    label = "Process Fraction (-)",
    columns = c(melt, wind, sublimation)
  ) |>
  cols_label(
    event_id = "Start Date",
    event_type = "Event Type", 
    min_t = "Min",
    mean_t = "Mean",
    max_t = "Max",
    min_u = "Min",
    mean_u = "Mean",
    max_u = "Max",
    min_rh = "Min",
    mean_rh = "Mean",
    max_rh = "Max",
    melt = "Melt",
    wind = "Wind",
    sublimation = "Subl."
  ) |>
  cols_align(align = "center")  |>
  cols_width(
    event_id ~ px(90),
    event_type ~ px(100),  
    ends_with("_t") ~ px(50),
    ends_with("_u") ~ px(50),
    ends_with("_rh") ~ px(50),
    melt ~ px(60),
    wind ~ px(60),
    sublimation ~ px(60)
  ) |>
  fmt_number(
    columns = c(starts_with("min_"), starts_with("mean_"), starts_with("max_"), melt, wind, sublimation),
    decimals = 1
  )

pretty_table

saveRDS(pretty_table, 'tbls/select_event_met_stats_maxmin_pretty_gt_table.rds')
