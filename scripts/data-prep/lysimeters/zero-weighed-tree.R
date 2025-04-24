# Script to zero weighed tree at start of specified events 

# Ran this twice as we
# need one for the simulated closed canopy of 0.88 and again for the canopies
# that match the SCLs since we need the weighed tree snow load above the SCLs to
# match their corresponding canopy closures. Used a zenith angle of 15 deg for
# the SCL canopy coverage calc and then resampled the FSD snow surveys to match
# this canopy closure which were used to scale the weighed tree.

# setup ----
source('scripts/data-prep/lysimeters/01_define_functions.R')
library(tidyverse)
library(plotly)
library(viridis)
library(ggpubr)
options(scipen = 999)

quality_th <- 3 

load_suffix <- 'fsd_closed_0.88'
load_suffix <- 'fsd_cal_for_each_trough_vza_15'

to_long_short <- function(from, to, class, quality, notes, event_id){
  datetime <- seq(from, to, 900)
  
  out <- data.frame(datetime, class, quality, notes, event_id)
  
  return(out)
}

# load data ---- 
canopy_snow_events_pre_post <- read.csv('data/raw-data/snow_in_canopy_pre_and_post_snowfall.csv') |> 
  mutate(from =  as.POSIXct(from, tz = 'Etc/GMT+6'),
         to = as.POSIXct(to, tz = 'Etc/GMT+6'), 
          event_id = format(from, "%Y-%m-%d %H")) 

canopy_snow_events_pre_post$event_id <- as.Date(canopy_snow_events_pre_post$event_id)

canopy_snow_long_pre_post <- purrr::pmap_dfr(canopy_snow_events_pre_post, to_long_short)

# check we are including all of the unloading events within the above events

canopy_snow_events <- read.csv('data/raw-data/snow_in_canopy_post_snowfall.csv') |> 
  mutate(from =  as.POSIXct(from, tz = 'Etc/GMT+6'),
         to = as.POSIXct(to, tz = 'Etc/GMT+6'),
          event_id = format(from, "%Y-%m-%d %H")) |> filter(quality < quality_th)

for(row in 1:nrow(canopy_snow_events)){
  from_flag <- canopy_snow_events$from[row] %in% canopy_snow_long_pre_post$datetime
  canopy_snow_events$from_flag[row] <- from_flag
  
  to_flag <- canopy_snow_events$to[row] %in% canopy_snow_long_pre_post$datetime
  canopy_snow_events$to_flag[row] <- to_flag
}

# file.copy(from = paste0(
#   '../../analysis/interception/data/loadcell/treefort_weighed_tree_cal_kg_m2_plv_',
#   load_suffix,
#   '.rds'
# ),
# to = raw_data_path,
# overwrite = F,
# copy.date = T)
weighed_tree_df <-
  readRDS(
    paste0('data/raw-data/treefort_weighed_tree_cal_kg_m2_plv_',
           load_suffix,
           '.rds'))

# went through the plots of the weighed tree load on each of the snowy events
# and removed any events where the tree had irreparible data.
# bad_events <- c(
#   "2022-04-02",
#   "2022-04-22"
# )

bad_events <- c(NA)

canopy_snow_long_pre_post <- canopy_snow_long_pre_post |> filter(!event_id %in% bad_events)
cnpy_snow_events <- canopy_snow_long_pre_post$event_id |> unique()

cnpy_snow_events <- cnpy_snow_events[!cnpy_snow_events %in% bad_events]

weighed_tree_df_fltr <- weighed_tree_df |>
  left_join(canopy_snow_long_pre_post) |> # this is the df I created to select periods of time where snow is in the canopy and no above canopy precip is occuring
  filter(is.na(event_id) == F,
         event_id %in% cnpy_snow_events)

# zero the tree at the start of each event 

event_start_vals <- weighed_tree_df_fltr |> 
  group_by(event_id, name, tree_cal_cc) |> 
  summarise(start_time = min(datetime, na.rm = T),
            start_val = nth(value, which.min(datetime))) # grab instrument value at begining of event |> 

weighed_tree_zeroed_pre_post_cnpy_snow <- weighed_tree_df_fltr |> 
  left_join(event_start_vals, by = c('event_id', 'name', 'tree_cal_cc')) |> 
  mutate(value = value - start_val,
         hours = as.numeric(difftime(datetime, start_time, 'hours'))/(60*60),
         value = case_when(
           value < 0 ~ 0,
           T ~ value
         )) |> 
  select(-start_val) |> 
  rename(tree_mm = value) |> 
  # rbind(inc_snow) |> 
  filter(is.na(event_id) == F) 

# fix some noisey points 

good1 <- as.POSIXct('2023-06-15 11:15:00', tz = 'Etc/GMT+6')
good2 <- as.POSIXct('2023-06-15 11:45:00', tz = 'Etc/GMT+6')

bad1 <- as.POSIXct('2023-06-15 11:30:00', tz = 'Etc/GMT+6')
bad2 <- as.POSIXct('2023-06-15 12:00:00', tz = 'Etc/GMT+6')

fill_val1 <- weighed_tree_zeroed_pre_post_cnpy_snow[weighed_tree_zeroed_pre_post_cnpy_snow$datetime == good1,]$tree_mm
fill_val2 <- weighed_tree_zeroed_pre_post_cnpy_snow[weighed_tree_zeroed_pre_post_cnpy_snow$datetime == good2,]$tree_mm

weighed_tree_zeroed_pre_post_cnpy_snow[weighed_tree_zeroed_pre_post_cnpy_snow$datetime == bad1,]$tree_mm <- fill_val1
weighed_tree_zeroed_pre_post_cnpy_snow[weighed_tree_zeroed_pre_post_cnpy_snow$datetime == bad2,]$tree_mm <- fill_val2

ggplot(weighed_tree_zeroed_pre_post_cnpy_snow, aes(datetime, tree_mm, colour = name, group = name)) +
  geom_line() +
  facet_grid(rows = vars(tree_cal_cc))
plotly::ggplotly()



saveRDS(
  weighed_tree_zeroed_pre_post_cnpy_snow,
  paste0(
    'data/clean-data/unloading_events_zero_weighed_tree_kg_m2_pre_post_cnpy_snow_',
    load_suffix,
    '.rds'
  )
)

# plot all and save to figs
w_tree_plot <- weighed_tree_zeroed_pre_post_cnpy_snow |> 
  select(datetime, name = tree_cal_cc, value = tree_mm, event_id)
  
lapply(
  w_tree_plot$event_id |> unique(),
  plot_facet_one_vars,
  cur_load_met = w_tree_plot,
  cur_var_names = w_tree_plot$name |> unique(),
  filename_prefix = paste0('figs/supplement/w_tree_plots_pre_post/',
                           load_suffix,
                           '_timeseries_w_tree_')
)

# event_max_load <- weighed_tree_zeroed_pre_post_cnpy_snow |> 
#   group_by(event_id, name) |> 
#   summarise(max_load = max(value, na.rm =T)) # grab instrument value at begining of event |> 
# 
# # switch over from the pre - post storm ids above to the ablation only storm ids
# # used in most analysis
# 
# event_max_load_out <- canopy_snow_long_pre_post |> 
#   select(datetime, event_id) |> 
#   left_join(event_max_load) |> 
#   left_join(canopy_snow_long |> select(datetime, event_id) , by = 'datetime') |> 
#   group_by(event_id = event_id.y) |> 
#   summarise(max_load = first(max_load))
# 
# saveRDS(event_max_load_out, 'data/select_unloading_events_max_weighed_tree_load.rds')

# plot the weighed tree for each event 

# p <- lapply(
#   cnpy_snow_events,
#   plot_facet_two_vars,
#   cur_load_met = weighed_tree_zeroed_pre_post_cnpy_snow,
#   cur_var_names = 'obs',
#   filename_prefix = 'figs/model-assessment/weighed_tree/simulated_canopy_load_pre_post_event_'
# )

# p
