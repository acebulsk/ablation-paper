# Script to plot met data and unloading events

# warm canopy snow melt events ---- 

obs_tree_events <- obs_tree |>
  left_join(ft_met |> 
              select(datetime, t:p)) |> 
  # filter(weighed_tree_quality < 2) |> 
  select(datetime, event_id, t:p, weighed_tree = observed)

stats_tbl <- obs_tree_events |> 
  group_by(event_id) |> 
  summarise(across(t:weighed_tree,
                   list(mean = mean, min = min, max = max),
                   .names = "{col}_{fn}"),
            p_sum = sum(p)) |> 
  select(event_id:Qsi_max, p_sum, weighed_tree_min, weighed_tree_max)

saveRDS(stats_tbl, 'data/ablation_event_met_summary.rds')

obs_tree_events_long <- pivot_longer(obs_tree_events, t:weighed_tree)

p <- obs_tree_events |> 
  pivot_longer(!c(datetime, event_id)) |> 
  ggplot(aes(datetime, value, colour = name)) +
  geom_line() +
  facet_grid(rows = vars(name), scales = 'free') + 
  ylab(element_blank()) + xlab(element_blank()) +
  theme(legend.position = 'none')

plotly::ggplotly(p)

for (event in obs_tree_events_long$event_id |> unique()) {
  
  df_fltr <- obs_tree_events_long |> filter(event_id == event)
  
  if (nrow(df_fltr) > 0) {
    df_fltr |> 
      ggplot(aes(datetime, value, colour = name)) +
      geom_line() +
      facet_grid(rows = vars(name), scales = 'free') + 
      ylab(element_blank()) + xlab(element_blank()) +
      ggtitle(as.Date(event)) +
      theme(legend.position = 'none')
    
    ggsave(
      paste0(
        'figs/supplement/w_tree_plots_met/mod_forcing/weighed_tree_w_met_',
        as.Date(event),
        '.png'
      ),
      width = 8,
      height = 6
    )
  }
}

# Unloading events with minimal precip ------ 

canopy_snow_events <- 
  read.csv('../../analysis/ablation/data/snow_in_canopy_post_snowfall.csv') |> 
  mutate(from =  as.POSIXct(from, tz = 'Etc/GMT+6'),
         to = as.POSIXct(to, tz = 'Etc/GMT+6'),
          event_id = format(from, "%Y-%m-%d %H")) 
events_fltr_long <-
  purrr::pmap_dfr(canopy_snow_events, to_long) |> 
  select(-quality)

obs_tree_events <- obs_tree |>
  left_join(ft_met |> 
              select(datetime, t:p)) |> 
  inner_join(events_fltr_long) |> 
  filter(weighed_tree_quality < 2) |> 
  select(datetime, event_id, t:p, obs_tree)

obs_tree_events_long <- pivot_longer(obs_tree_events, t:obs_tree)

p <- obs_tree |>
  left_join(ft_met |> 
              select(datetime, t:p)) |> 
  pivot_longer(!datetime) |> 
  ggplot(aes(datetime, value, colour = name)) +
  geom_line() +
  facet_grid(rows = vars(name), scales = 'free') + 
  ylab(element_blank()) + xlab(element_blank()) +
  theme(legend.position = 'none')

plotly::ggplotly(p)

for (event in canopy_snow_events$event_id) {
  
  df_fltr <- obs_tree_events_long |> filter(event_id == event)

  if (nrow(df_fltr) > 0) {
  df_fltr |> 
  ggplot(aes(datetime, value, colour = name)) +
    geom_line() +
    facet_grid(rows = vars(name), scales = 'free') + 
    ylab(element_blank()) + xlab(element_blank()) +
    ggtitle(as.Date(event)) +
      theme(legend.position = 'none')
  
  ggsave(
    paste0(
      'figs/supplement/w_tree_plots/weighed_tree_w_met_',
      as.Date(event),
      '.png'
    ),
    width = 8,
    height = 6
  )
  }
}

