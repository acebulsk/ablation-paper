# This script calculates the change in lysimeters over the given time interval

# Compute the unloading rate ----

scl_q_unld_15 <- scl_df_kg_m2 |>
  filter(name %in% good_loads)  |>
  group_by(name) |>
  mutate(
    dU = value - lag(value),
    dU = ifelse(dU < 0, 0, dU),
    q_unl = (dU / 15) * 60
  ) |>
  ungroup() |>
  filter(is.na(q_unl) == F) 

w_tree_q_unld_15 <- w_tree_kg_m2 |>
  filter(name %in% good_loads)  |>
  group_by(name) |>
  mutate(
    dU = lag(value) - value,
    dU = ifelse(dU < 0, 0, dU),
    q_unl = (dU / 15) * 60
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


