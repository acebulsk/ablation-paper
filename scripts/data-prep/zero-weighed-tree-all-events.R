# This script zeros the weighed tree data already converted to kg/m2 
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

# weighed tree zeroed prior to snowfall events
weighed_tree_df <-
  readRDS('../../analysis/ablation/data/unloading_events_zero_weighed_tree_mm_pre_post_cnpy_snow.rds')

canopy_snow_events <- 
  read.csv('../../analysis/ablation/data/snow_in_canopy_post_snowfall.csv') |> 
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
  select(datetime, value) |> 
  left_join(events_fltr_long) |> # this is the df I created to select periods of time where snow is in the canopy and no above canopy precip is occuring
  filter(is.na(event_id) == F)

ggplot(weighed_tree_df_fltr, aes(datetime, value)) +
  geom_line()
plotly::ggplotly()

saveRDS(
  weighed_tree_df_fltr,
  'data/clean-data/all_tree_events_zero_weighed_tree_mm_post_cnpy_snow.rds'
)
