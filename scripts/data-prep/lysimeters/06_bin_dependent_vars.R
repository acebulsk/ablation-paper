# This script bins the 15 minute met and snow load obs. This was done to attempt
# to handle some of the noise in these data.

if(hourly == T){
  # start to hourly
  met_binned_cpy_snow <- ffr_met |> 
    left_join(mod_subl_cpy) |>
    left_join(ec_df) |> 
    left_join(weighed_tree_zeroed) |> 
    # start to hourly
    mutate(datetime = format(datetime, '%Y-%m-%d %H:00:00')) |>
    group_by(datetime) |>
    summarise(
      t = mean(t, na.rm = T),
      t_ice_bulb = mean(t_ice_bulb, na.rm = T),
      u = mean(u, na.rm = T),
      diml_subl_rate = mean(diml_subl_rate, na.rm = T),
      tau_canopy_low = mean(tau_canopy_low, na.rm = T),
      tau_canopy_hi = mean(tau_canopy_hi, na.rm = T),
      tree_mm = mean(tree_mm, na.rm =T)
    ) |>
    mutate(datetime = as.POSIXct(datetime, tz = 'Etc/GMT+6')) |>
    # end to hourly
    inner_join(canopy_snow_long, by = 'datetime') |>  # limit to only periods where snow is in the canopy
    mutate(diml_subl_rate = abs(diml_subl_rate)) |> 
    select(datetime, event_id, t, t_ice_bulb, u, diml_subl_rate, tau = Tau, tree_mm) 
} else {
  met_binned_cpy_snow <- ffr_met |> 
    left_join(mod_subl_cpy) |>
    left_join(ec_df) |> 
    left_join(weighed_tree_zeroed |> select(-event_id)) |> 
    # start to hourly
    # mutate(datetime = format(datetime, '%Y-%m-%d %H:00:00')) |>
    # group_by(datetime) |>
    # summarise(
    #   t = mean(t, na.rm = T),
    #   t_ice_bulb = mean(t_ice_bulb, na.rm = T),
    #   u = mean(u, na.rm = T),
    #   diml_subl_rate = mean(diml_subl_rate, na.rm = T),
    #   Tau = mean(Tau, na.rm = T),
    #   tree_mm = mean(tree_mm, na.rm =T)
    # ) |>
    # mutate(datetime = as.POSIXct(datetime, tz = 'Etc/GMT+6')) |>
    # end to hourly
    inner_join(canopy_snow_long, by = 'datetime') |>  # limit to only periods where snow is in the canopy
    # filter(p == ppt_fltr) |> # limit to periods where there is no above canopy precip so our troughs are measuring unloading + drip and not throughfall
    mutate(diml_subl_rate = -diml_subl_rate) |> 
    select(datetime, event_id, name = tree_cal_trough_name, tree_cal_cc, t, t_ice_bulb, u, diml_subl_rate, tau_canopy_top, tau_canopy_low, tree_mm) 
}

# met_binned_cpy_snow[,good_wind][met_binned_cpy_snow[,good_wind]==0] <- 0.001 # need to remove 0 wind speeds that breaks regression, safe to assume 0 wind no possible so assign a very low value  

# Bin the met_binned_cpy_snow vars ----

label_bin_fn <- function(bins){
  (bins[-1] + bins[-length(bins)]) / 2
}

## bin temp ----

min_temp <- round(
  min(met_binned_cpy_snow[,good_temp], na.rm = T))

max_temp <- round(
  max(met_binned_cpy_snow[,good_temp], na.rm = T))

temp_step <- 1.5

temp_breaks <- seq(
  min_temp-0.5,
  max_temp+1,
  temp_step)

temp_labs_seq <- label_bin_fn(bins = temp_breaks)

stopifnot(tail(temp_breaks, 1) > max(met_binned_cpy_snow[,good_temp], na.rm = T))
stopifnot(head(temp_breaks, 1) < min(met_binned_cpy_snow[,good_temp], na.rm = T))

stopifnot(length(temp_labs_seq) + 1 == length(temp_breaks))

met_binned_cpy_snow$temp_binned <- cut(met_binned_cpy_snow[,good_temp, drop = TRUE], temp_breaks)

met_binned_cpy_snow$temp_labs <- cut(met_binned_cpy_snow[,good_temp, drop = TRUE], 
                     temp_breaks, 
                     labels = temp_labs_seq)

## bin ice-bulb temp ----

min_temp <- round(
  min(met_binned_cpy_snow[,'t_ice_bulb'], na.rm = T))

max_temp <- round(
  max(met_binned_cpy_snow[,'t_ice_bulb'], na.rm = T))

temp_step <- 1.5

temp_breaks <- seq(
  min_temp-0.5,
  max_temp+1,
  temp_step)

temp_labs_seq <- label_bin_fn(bins = temp_breaks)

# stopifnot(tail(temp_breaks, 1) > max(met_binned_cpy_snow[,'t_ice_bulb'], na.rm = T))
stopifnot(head(temp_breaks, 1) < min(met_binned_cpy_snow[,'t_ice_bulb'], na.rm = T))

stopifnot(length(temp_labs_seq) + 1 == length(temp_breaks))

met_binned_cpy_snow$t_ice_binned <- cut(met_binned_cpy_snow[,'t_ice_bulb', drop = TRUE], temp_breaks)

met_binned_cpy_snow$t_ice_labs <- cut(met_binned_cpy_snow[,'t_ice_bulb', drop = TRUE], 
                      temp_breaks, 
                      labels = temp_labs_seq)

## bin wind ---- 

min_wind <- 0
max_wind <- round(
  max(met_binned_cpy_snow[,good_wind], na.rm = T))

wind_step <- 0.5

wind_breaks <- seq(
  min_wind,
  max_wind+wind_step,
  wind_step)

wind_labs_seq <- label_bin_fn(bins = wind_breaks)

stopifnot(tail(wind_breaks, 1) > max(met_binned_cpy_snow[,good_wind], na.rm = T))

stopifnot(length(wind_labs_seq) + 1 == length(wind_breaks))

met_binned_cpy_snow$wind_binned <- cut(met_binned_cpy_snow[,good_wind, drop = TRUE], wind_breaks)

met_binned_cpy_snow$wind_labs <- cut(met_binned_cpy_snow[,good_wind, drop = TRUE], 
                     wind_breaks, 
                     labels = wind_labs_seq
)

## bin mid canopy wind as shear stress ---- 

# since we forced the above through the origin we just need to multiply by the slope 

met_binned_cpy_snow$mid_can_est_tau <- (met_binned_cpy_snow[,good_wind]^2)*lm_mid_wnd_sqrd_low_tau$slope

min_mid_can_tau <- 0
max_mid_can_tau <- round(
  max(met_binned_cpy_snow$mid_can_est_tau, na.rm = T))

mid_can_tau_step <- 0.05

mid_can_tau_breaks <- seq(
  min_mid_can_tau,
  max_mid_can_tau,
  mid_can_tau_step)

mid_can_tau_labs_seq <- label_bin_fn(bins = mid_can_tau_breaks)

# stopifnot(tail(mid_can_tau_breaks, 1) > max(met_binned_cpy_snow[,'mid_can_est_tau'], na.rm = T))

stopifnot(length(mid_can_tau_labs_seq) + 1 == length(mid_can_tau_breaks))

met_binned_cpy_snow$tau_mid_binned <- cut(met_binned_cpy_snow[,'mid_can_est_tau', drop = TRUE], mid_can_tau_breaks)

met_binned_cpy_snow$tau_mid_labs <- cut(met_binned_cpy_snow[,'mid_can_est_tau', drop = TRUE], 
                        mid_can_tau_breaks, 
                        labels = mid_can_tau_labs_seq
)

## bin dimensionless sublimation rate (s-1) ----

min_vs <- 0
max_vs <- round(
  max(met_binned_cpy_snow$diml_subl_rate, na.rm = T),3)
vs_step <- 0.0002

vs_breaks <- seq(
  min_vs,
  max_vs+0.0005,
  vs_step)

vs_labs_seq <- label_bin_fn(bins = vs_breaks)

stopifnot(tail(vs_breaks, 1) > max(met_binned_cpy_snow$diml_subl_rate, na.rm = T))
stopifnot(length(vs_labs_seq) + 1 == length(vs_breaks))

met_binned_cpy_snow$vs_binned <- cut(met_binned_cpy_snow[,'diml_subl_rate', drop = TRUE], vs_breaks)

met_binned_cpy_snow$vs_labs <- cut(met_binned_cpy_snow[,'diml_subl_rate', drop = TRUE], 
                   vs_breaks, 
                   labels = vs_labs_seq
)

## bin shear stress (N m-2) at canopy top ----

min_tau_top <- 0
max_tau_top <- round(
  max(met_binned_cpy_snow$tau_canopy_top, na.rm = T),3)
tau_step <- 0.1

tau_breaks <- seq(
  min_tau_top,
  max_tau_top + tau_step,
  tau_step)

tau_labs_seq <- label_bin_fn(bins = tau_breaks)

stopifnot(tail(tau_breaks, 1) > max(met_binned_cpy_snow$tau_canopy_top, na.rm = T))
stopifnot(length(tau_labs_seq) + 1 == length(tau_breaks))

met_binned_cpy_snow$tau_top_binned <- cut(met_binned_cpy_snow[,'tau_canopy_top', drop = TRUE], tau_breaks)

met_binned_cpy_snow$tau_top_labs <- cut(met_binned_cpy_snow[,'tau_canopy_top', drop = TRUE], 
                        tau_breaks, 
                        labels = tau_labs_seq
)

## bin shear stress (N m-2) at canopy bottom ----

min_tau_low <- 0
max_tau_low <- round(
  max(met_binned_cpy_snow$tau_canopy_low, na.rm = T),3)
tau_step <- 0.05

tau_breaks <- seq(
  min_tau_low,
  max_tau_low + tau_step,
  tau_step)

tau_labs_seq <- label_bin_fn(bins = tau_breaks)

stopifnot(tail(tau_breaks, 1) > max(met_binned_cpy_snow$tau_canopy_low, na.rm = T))
stopifnot(length(tau_labs_seq) + 1 == length(tau_breaks))

met_binned_cpy_snow$tau_low_binned <- cut(met_binned_cpy_snow[,'tau_canopy_low', drop = TRUE], tau_breaks)

met_binned_cpy_snow$tau_low_labs <- cut(met_binned_cpy_snow[,'tau_canopy_low', drop = TRUE], 
                        tau_breaks, 
                        labels = tau_labs_seq
)

## bin tree load (mm) ----

min_tree <- 0
max_tree <- round(
  max(met_binned_cpy_snow$tree_mm, na.rm = T),3)
tree_step <- 3

tree_breaks <- seq(
  min_tree,
  max_tree + tree_step,
  tree_step)

tree_labs_seq <- label_bin_fn(bins = tree_breaks)

stopifnot(tail(tree_breaks, 1) > max(met_binned_cpy_snow$tree_mm, na.rm = T))
stopifnot(length(tree_labs_seq) + 1 == length(tree_breaks))

met_binned_cpy_snow$tree_binned <- cut(met_binned_cpy_snow[,'tree_mm', drop = TRUE], tree_breaks)

met_binned_cpy_snow$tree_labs <- cut(met_binned_cpy_snow[,'tree_mm', drop = TRUE], 
                     tree_breaks, 
                     labels = tree_labs_seq
)

## bin by duration (hours) ----

met_binned_cpy_snow <- met_binned_cpy_snow |> 
  group_by(event_id) |> 
  mutate(time_elapsed_event = as.numeric(difftime(datetime, min(datetime), units = 'hours'))+0.25) # add 0.25 to account match up to the end of the timestep instead of corresponding the the beginning of the timestep

min_bin <- 0
max_bin <- round(
  max(met_binned_cpy_snow$time_elapsed_event, na.rm = T),3)
step <- 6

breaks <- seq(
  min_bin,
  max_bin + step,
  step)

labs_seq <- seq(from = step, by = step, length.out = length(breaks)-1)

stopifnot(tail(breaks, 1) > max(met_binned_cpy_snow$time_elapsed_event, na.rm = T))
stopifnot(length(labs_seq) + 1 == length(breaks))

met_binned_cpy_snow$time_elapsed_binned <- cut(met_binned_cpy_snow[, 'time_elapsed_event',
                                   drop = TRUE], breaks)

met_binned_cpy_snow$time_elapsed_bin_labs <- cut(met_binned_cpy_snow[, 'time_elapsed_event',
                                     drop = TRUE],
                                 breaks, labels = labs_seq)

met_binned_cpy_snow$time_elapsed_bin_labs <- as.numeric(as.character(met_binned_cpy_snow$time_elapsed_bin_labs))

met_binned_cpy_snow <- met_binned_cpy_snow |> 
  mutate(
    temp_labs = as.numeric(as.character(temp_labs)),
    t_ice_labs = as.numeric(as.character(t_ice_labs)),
    wind_labs = as.numeric(as.character(wind_labs)),
    tau_mid_labs = as.numeric(as.character(tau_mid_labs)),
    tau_low_labs = as.numeric(as.character(tau_low_labs)),
    tau_top_labs = as.numeric(as.character(tau_top_labs)),
    vs_labs = as.numeric(as.character(vs_labs)),
    tree_labs = as.numeric(as.character(tree_labs)),
    duration_labs = as.numeric(as.character(time_elapsed_bin_labs))
  ) 

#saveRDS(met_binned_cpy_snow, 'data/clean-data/met_binned_for_unloading_analysis.rds')
