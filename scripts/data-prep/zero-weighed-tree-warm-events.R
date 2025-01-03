# This script zeros the weighed tree data already converted to kg/m2 
to_long <- function(from,
                    to, event_id) {
  datetime <- seq(from, to, 900)
  
  out <- data.frame(datetime,
                    event_id)
  
  return(out)
}

# weighed tree zeroed prior to snowfall events
weighed_tree_df <-
  readRDS('../../analysis/ablation/data/unloading_events_zero_weighed_tree_mm_pre_post_cnpy_snow.rds')

warm_tree_events <- 
  read.csv('../../analysis/ablation/data/warm_ablation_events_weighed_tree_post.csv',
           skip = 1) |>
  filter(quality < 3) |> 
  mutate(
    across(from:to, ~as.POSIXct(.x, tz = 'Etc/GMT+6')),
    event_id = as.Date(from, tz = 'Etc/GMT+6'))
warm_tree_events_long <-
  purrr::pmap_dfr(warm_tree_events |> select(from, to, event_id), to_long) 

saveRDS(
  warm_tree_events_long,
  'data/clean-data/warm_tree_events_long.rds'
)

# check we have weighed tree obs for our new events
# warm_tree_events_long$new_event_flag <- warm_tree_events_long$datetime %in% weighed_tree_df$datetime

# filter pre/post weighed tree dataset to just the warm events
weighed_tree_df_fltr <- weighed_tree_df |>
  select(datetime, value) |> 
  left_join(warm_tree_events_long) |> # this is the df I created to select periods of time where snow is in the canopy and no above canopy precip is occuring
  filter(is.na(event_id) == F)

# ggplot(weighed_tree_df_fltr, aes(datetime, value)) + 
#   geom_line()
# plotly::ggplotly()

saveRDS(
  weighed_tree_df_fltr,
  'data/clean-data/warm_tree_events_zero_weighed_tree_mm_post_cnpy_snow.rds'
)
