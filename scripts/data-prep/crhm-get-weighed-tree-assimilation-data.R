# Add in weighed tree obs for CRHM snowmelt initialization
# Filter this to just include data during the snowmelt events

# need to keep the below two datasets separate if including all events as there are some overlaps between events otherwise keep filtering
warm_events_select_agu24 <- c(
  '2022-04-21',
  '2022-04-23',
  '2022-06-14',
  '2022-06-24',
  '2023-03-14',
  '2023-03-25',
  '2023-03-26',
  '2023-03-28',
  '2023-05-08',
  '2023-06-15',
  '2023-06-21'
)
warm_tree_events_df <-
  readRDS('data/clean-data/warm_tree_events_zero_weighed_tree_fsd_closed_0.88_kg_m2_post_cnpy_snow.rds') |> 
  select(datetime, event_id, obs_snow_load = tree_mm) |> 
  group_by(event_id) |> 
  summarise(
    datetime = first(datetime),
    obs_snow_load = first(obs_snow_load)
  ) |> 
  filter(event_id %in% warm_events_select_agu24) |> 
  select(-event_id)

cold_events_select_agu24 <- c(
  '2022-03-02', 
  '2022-03-09',
  '2022-03-20', 
  '2022-03-24',  
  '2022-03-29',  
  '2022-12-01',
  '2023-01-28',
  '2023-02-24',
  '2023-02-26',
  '2023-04-12'
)
cold_tree_events_df <- 
  readRDS('data/clean-data/all_tree_events_zero_weighed_tree_fsd_closed_0.88_kg_m2_post_cnpy_snow.rds') |> 
  select(datetime, event_id, obs_snow_load = tree_mm) |> 
  group_by(event_id) |> 
  summarise(
    datetime = first(datetime),
    obs_snow_load = first(obs_snow_load)
  ) |> 
  filter(event_id %in% cold_events_select_agu24) |> 
  select(-event_id)

combined_tree_events <- rbind(
  warm_tree_events_df,
  cold_tree_events_df
)

