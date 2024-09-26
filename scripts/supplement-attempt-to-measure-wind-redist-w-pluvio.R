# script to show inability to measure wind redistribution using pluvio 

# could add on here the portion of snow redistributed to the pluvio during high winds (i.e. a rectangle around part of the top wind speed bar)
# UPDATE: not confident in wind redistribution to pluvio 

low_wind_events <- c('2023-02-28', '2023-03-14', '2022-12-21')
entrainment_events <- c('2021-12-19', '2021-12-27')

# entrainment_events <- c('2021-12-19', '2021-12-27', '2022-12-01', '2023-02-24')
events <- c(entrainment_events, low_wind_events)

canopy_snow_events <- 
  read.csv('../../analysis/ablation/data/snow_in_canopy_post_snowfall.csv') |> 
  mutate(from =  as.POSIXct(from, tz = 'Etc/GMT+6'),
         to = as.POSIXct(to, tz = 'Etc/GMT+6'),
         storm_id = as.Date(from, tz = 'Etc/GMT+6')) |> filter(quality < 3)

to_long <- function(from, to, class, quality, weighed_tree_quality, notes, storm_id){
  datetime <- seq(from, to, 900)
  
  out <- data.frame(datetime, class, quality, weighed_tree_quality, notes, storm_id)
  
  return(out)
}

# these are periods where we know snow is in the canopy and unloading is possible
canopy_snow_long <- purrr::pmap_dfr(canopy_snow_events, to_long)
canopy_snow_long$storm_id <- as.Date(canopy_snow_long$storm_id)

ft_met |> 
  pivot_longer(!datetime) |> 
  left_join(canopy_snow_long) |>
  filter(storm_id %in% low_wind_events) |> 
  ggplot(aes(datetime, value, colour = name)) + 
  geom_line() +
  facet_grid(name~storm_id, scales = 'free')

ft_met |> 
  pivot_longer(!datetime) |> 
  left_join(canopy_snow_long) |>
  filter(storm_id %in% entrainment_events) |> 
  ggplot(aes(datetime, value, colour = name)) + 
  geom_line() +
  facet_grid(name~storm_id, scales = 'free')

pcp_events <- ft_met |> 
  left_join(canopy_snow_long) |> 
  filter(storm_id %in% events,
         p > 0,
         p < 0.2) 

ggplot(pcp_events, aes(u, p*4)) + 
  geom_point() +
  ylim(c(0, NA)) +
  xlim(c(0, NA)) +
  ylab('Wind Redistribution (mm/hr)') +
  xlab('Wind Speed (m/s)') +
  geom_smooth(method = 'lm', se = F)

ggplot(pcp_events, aes(u_max, p*4)) + 
  geom_point() +
  ylim(c(0, NA)) +
  xlim(c(0, NA)) +
  ylab('Wind Redistribution (mm/hr)') +
  xlab('Peak 15-min Wind Speed (m/s)') +
  geom_smooth(method = 'lm', se = F)

ggplot(pcp_events, aes(u_std, p*4)) + 
  geom_point() +
  ylim(c(0, NA)) +
  xlim(c(0, NA)) +
  ylab('Wind Redistribution (mm/hr)') +
  xlab('15-min Std. Dev. Wind Speed (m/s)') +
  geom_smooth(method = 'lm', se = F)

lm_mod <- lm(p*4~u, data = pcp_events)
summary(lm_mod)

# sig. relationship but its not physically realistic ... we are getting too high
# wind redistribution at low wind speeds also looking at the partitioning bar
# graph we can see that wind redistribution does not make up a significant
# portion of ablation in a wind exposed forest
