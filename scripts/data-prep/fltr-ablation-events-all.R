# This script zeros the weighed tree data already converted to kg/m2 

library(tidyverse)

to_long <- function(from,
                    to,
                    class,
                    quality,
                    bad_troughs,
                    weighed_tree_quality,
                    is_melt_event,
                    notes,
                    event_id) {
  datetime <- seq(from, to, 900)
  
  out <- data.frame(datetime,
                    class,
                    quality,
                    bad_troughs,
                    weighed_tree_quality,
                    is_melt_event,
                    notes,
                    event_id)
  
  return(out)
}

load_suffix <- 'fsd_closed_0.88'
# load_suffix <- 'fsd_cal_for_each_trough'

# weighed tree zeroed prior to snowfall events
# file from scripts/data-prep/zero-weighed-tree.R
weighed_tree_df <-
  readRDS(
    paste0(
      'data/clean-data/unloading_events_zero_weighed_tree_kg_m2_pre_post_cnpy_snow_',
      load_suffix,
      '.rds'))

canopy_snow_events <- 
  read.csv('data/raw-data/snow_in_canopy_post_snowfall.csv') |> 
  mutate(from =  as.POSIXct(from, tz = 'Etc/GMT+6'),
         to = as.POSIXct(to, tz = 'Etc/GMT+6'),
         event_id = as.Date(from, tz = 'Etc/GMT+6')) 

events_fltr <- canopy_snow_events 

events_fltr_long <-
  purrr::pmap_dfr(events_fltr, to_long) |> 
  select(-quality)

# check we have weighed tree obs for our new events
# warm_tree_events_long$new_event_flag <- warm_tree_events_long$datetime %in% weighed_tree_df$datetime

# filter pre/post weighed tree dataset to just the warm events
weighed_tree_df_fltr <- weighed_tree_df |>
  select(datetime:tree_mm, -name) |> 
  left_join(events_fltr_long) |> # this is the df I created to select periods of time where snow is in the canopy and no above canopy precip is occuring
  filter(is.na(event_id) == F)

ggplot(weighed_tree_df_fltr, aes(datetime, tree_mm)) +
  geom_line() +facet_grid(rows=vars(tree_cal_cc))
plotly::ggplotly()

saveRDS(
  weighed_tree_df_fltr,
  paste0(
    'data/clean-data/all_tree_events_zero_weighed_tree_',
    load_suffix,
    '_kg_m2_post_cnpy_snow.rds'
  )
)
