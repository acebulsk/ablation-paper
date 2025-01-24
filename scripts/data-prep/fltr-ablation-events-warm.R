# This script zeros the weighed tree data already converted to kg/m2 
to_long <- function(from,
                    to, event_id) {
  datetime <- seq(from, to, 900)
  
  out <- data.frame(datetime,
                    event_id)
  
  return(out)
}

# weighed tree zeroed prior to snowfall events
load_suffix <- 'fsd_closed_0.88'
# load_suffix <- 'fsd_cal_for_each_trough'


# weighed tree zeroed prior to snowfall events
weighed_tree_df <-
  readRDS(
    paste0(
      'data/clean-data/unloading_events_zero_weighed_tree_kg_m2_pre_post_cnpy_snow_',
      load_suffix,
      '.rds'))

# file.copy(from = '../../analysis/ablation/data/warm_ablation_events_weighed_tree_post.csv',
#           to = 'data/raw-data',
#           overwrite = F,
#           copy.date = T)
warm_tree_events <- 
  read.csv('data/raw-data/warm_ablation_events_weighed_tree_post.csv',
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
  select(datetime:tree_mm, -name) |> 
  left_join(warm_tree_events_long) |> # this is the df I created to select periods of time where snow is in the canopy and no above canopy precip is occuring
  filter(is.na(event_id) == F)

ggplot(weighed_tree_df_fltr, aes(datetime, tree_mm)) +
  geom_line() +facet_grid(rows=vars(tree_cal_cc))
plotly::ggplotly()

saveRDS(
  weighed_tree_df_fltr,
  paste0(
    'data/clean-data/warm_tree_events_zero_weighed_tree_',
    load_suffix,
    '_kg_m2_post_cnpy_snow.rds'
  )
)
