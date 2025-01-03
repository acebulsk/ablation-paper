# Script to plot met data and unloading events

# warm canopy snow melt events ---- 

obs_tree_events <- obs_tree |>
  left_join(ft_met |> 
              select(datetime, t:p)) |> 
  inner_join(obs_tree_warm_events) |> 
  # filter(weighed_tree_quality < 2) |> 
  select(datetime, event_id, t:p, weighed_tree)

obs_tree_events_long <- pivot_longer(obs_tree_events, t:weighed_tree)

p <- obs_tree |>
  left_join(ft_met |> 
              select(datetime, t:p)) |> 
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
        'figs/supplement/w_tree_plots_met/warm_melt_events/weighed_tree_w_met_',
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
         event_id = as.Date(from, tz = 'Etc/GMT+6')) 
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

