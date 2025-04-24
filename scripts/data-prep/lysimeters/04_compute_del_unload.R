# This script calculates the change in lysimeters over the given time interval

# Compute the unloading rate ----
scl_df_kg_m2 <- readRDS('data/clean-data/treefort_scl_qaqc.rds')

ggplot(scl_df_kg_m2, aes(datetime, value, colour = name)) + 
  geom_line() +
  facet_grid(rows = vars(name), scales = 'free_y')
plotly::ggplotly()

scl_q_unld_15 <- scl_df_kg_m2 |>
  filter(name %in% good_loads)  |>
  group_by(name, event_id) |>
  mutate(
    dU = value - lag(value),
    dU = ifelse(dU < 0, 0, dU),
    q_unl = (dU / 15) * 60
  ) |>
  ungroup() |>
  filter(is.na(q_unl) == F) 

w_tree_q_unld_15 <- weighed_tree_zeroed |>
  filter(name == 'weighed tree')  |>
  group_by(tree_cal_trough_name) |>
  mutate(
    dL = lag(tree_mm) - tree_mm,
    dL = ifelse(dL < 0, 0, dL),
    q_unl = (dL / 15) * 60
  ) |>
  ungroup() |>
  filter(is.na(q_unl) == F) 

if(hourly == T){
  
  stop("need to adjust to handle both dfs above")
  # start to hourly
  q_unld <- q_unld_15 |>
    # to hourly
    mutate(datetime = format(datetime, '%Y-%m-%d %H:00:00')) |>
    group_by(datetime, name, inst_type) |>
    summarise(q_unl = sum(dU, na.rm = T)) |>
    mutate(datetime = as.POSIXct(datetime),
           dU = q_unl)
} else {
  # keep at 15 min
  scl_q_unld <- scl_q_unld_15
  w_tree_q_unld <- w_tree_q_unld_15
  
}

saveRDS(scl_q_unld, 'data/clean-data/ft_scl_data_del_15_min.rds')
saveRDS(w_tree_q_unld, 'data/clean-data/ft_w_tree_data_del_15_min.rds')

scl_q_unld_plot <- scl_q_unld |> 
  rename(group = name) |> 
  mutate(name = 'scl') |> 
  select(datetime, event_id, name, value, group)
  

ffr_met_long_plot <- left_join(scl_q_unld_plot |> select(datetime, event_id) |> distinct(), ffr_met_long) |> 
  select(datetime, event_id, name, value) |> 
  mutate(group = 'met')

w_tree_zeroed <- 
  readRDS('data/clean-data/unloading_events_zero_weighed_tree_kg_m2_pre_post_cnpy_snow_fsd_cal_for_each_trough_vza_15.rds') |> 
  select(datetime, name = tree_cal_trough_name, tree_mm) |> 
  left_join(events_fltr_long |> select(datetime, event_id, weighed_tree_quality, notes), by = 'datetime') |>
  filter(weighed_tree_quality < 3) |> 
  mutate(group = 'w_tree') |> 
  select(datetime, event_id, name = group, value = tree_mm, group = name) 

plot_df <- rbind(scl_q_unld_plot, ffr_met_long_plot) |> rbind(w_tree_zeroed)

plot_facet_scl_met <- function(cur_event_id, cur_load_met, cur_var_names, filename_prefix) {
  if(is.na(filename_prefix) == T){
    filename <- paste0(cur_event_id, '.png')
  } else {
    filename <- paste0(filename_prefix, cur_event_id, '.png')
  }
  
  p <-  cur_load_met |> 
    filter(name %in% cur_var_names,
           event_id == cur_event_id,
           is.na(value) == F) |>
    ggplot(aes(datetime, value, colour = group)) +
    geom_line() +
    geom_point()+
    facet_grid(rows = vars(name),
               scales = 'free',
               switch = "y", # flip the facet labels along the y axis from the right side to the left
               # labeller = as_labeller( # redefine the text that shows up for the facets
               #   c(cur_var_names))
    ) +
    ylab(NULL) + # remove the word "values"
    xlab(NULL) +
    theme(strip.background = element_blank(), # remove the background
          strip.placement = "outside") +
    ggtitle(cur_event_id)
  
  ggsave(plot = p, filename = paste0(filename), width = 8, height = 8)
  
  return(p)
}

# plot all and save to figs
lapply(
  scl_q_unld_plot$event_id |> unique(),
  plot_facet_scl_met,
  cur_load_met = plot_df,
  cur_var_names = c('w_tree', 'scl', 't', 'u', 'p'),
  filename_prefix = 'figs/supplement/load_plots/scl_met/timeseries_lysimeters_met_'
)
