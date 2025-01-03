# Script to plot weighed tree accumulation and ablation events w met

# weighed tree zeroed prior to snowfall events
weighed_tree_df <-
  readRDS('../../analysis/ablation/data/unloading_events_zero_weighed_tree_mm_pre_post_cnpy_snow.rds') |> 
  select(datetime, event_id, weighed_tree = value)

obs_tree_events <- weighed_tree_df |>
  left_join(ft_met |> 
              select(datetime, t:p)) |> 
  select(datetime, event_id, t:p, weighed_tree)

obs_tree_events_long <- pivot_longer(obs_tree_events, t:weighed_tree)

p <- obs_tree_events |>
  pivot_longer(!c(datetime, event_id)) |> 
  ggplot(aes(datetime, value, colour = name)) +
  geom_line() +
  facet_grid(rows = vars(name), scales = 'free') + 
  ylab(element_blank()) + xlab(element_blank()) +
  theme(legend.position = 'none')

plotly::ggplotly(p)
event <- unique(obs_tree_events$event_id)[1]
for (event in obs_tree_events$event_id |> unique()) {
  
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
        'figs/supplement/w_tree_plots_met/accum_ablate_events/weighed_tree_w_met_',
        as.Date(event),
        '.png'
      ),
      width = 8,
      height = 6
    )
  }
}
