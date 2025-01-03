# See if there is an effect of time induced unloading based on filtered
# unloading data set to remove periods above the temperature and wind thresholds
# found in the results figs. Also calculated the time-elapsed over each event to
# act as the independent variable.

met_unld_time <- met_unld |> 
  filter(name %in% scl_names,
         inst_type == 'subcanopy trough') |> # this one is redundant but pedantic!  
  mutate(d_time = 15,
         binary_unl = ifelse(q_unl > min_qunld, 1, 0)) |> # timesteps are 15 minutes
  group_by(event_id) |> 
  mutate(time_elapsed_event_mins = cumsum(d_time)) |> 
  select(event_id, datetime, time_elapsed_event_mins, binary_unl, q_unl)

min_bin <- 0
max_bin <- round(
  max(met_unld_time$time_elapsed_event_mins, na.rm = T),3)
step <- 180

breaks <- seq(
  min_bin,
  max_bin + step,
  step)

label_bin_fn <- function(bins){
  (bins[-1] + bins[-length(bins)]) / 2
}

labs_seq <- label_bin_fn(bins = breaks)

stopifnot(tail(breaks, 1) > max(met_unld_time$time_elapsed_event_mins, na.rm = T))
stopifnot(length(labs_seq) + 1 == length(breaks))

met_unld_time$time_elapsed_binned <- cut(met_unld_time[, 'time_elapsed_event_mins',
                                             drop = TRUE], breaks)

met_unld_time$time_elapsed_bin_labs <- cut(met_unld_time[, 'time_elapsed_event_mins',
                                               drop = TRUE],
                                      breaks, labels = labs_seq)

met_unld_time$time_elapsed_bin_labs <- as.numeric(as.character(met_unld_time$time_elapsed_bin_labs))
# plot rates ---- 

met_unld_time_smry <- met_unld_time |> 
  group_by(time_elapsed_bin_labs) |> 
  summarise(q_unl_mean = mean(q_unl, na.rm = T),
            q_unl_sd = sd(q_unl, na.rm = T))

met_unld_time |> 
  ggplot() +
  geom_point(
    aes(x = time_elapsed_event_mins, y = q_unl),
    colour = 'dodgerblue',
    alpha = 0.5,
    size = 0.5
  ) +
  geom_errorbar(
    data = met_unld_time_smry,
    aes(
      x = time_elapsed_bin_labs,
      ymax = q_unl_mean + q_unl_sd,
      ymin = q_unl_mean - q_unl_sd
    ),
    width = .5
  )  +
  geom_point(
    data = met_unld_time_smry,
    aes(x = time_elapsed_bin_labs, y = q_unl_mean),
    colour = 'black',
    shape = 1,
    size = 4
  )

# plot probability ----

prob_unl_df <- met_unld_time |> 
  group_by(time_elapsed_bin_labs) |> 
  summarise(n_unld_events = sum(binary_unl),
            N_condition = n(),
            prob_unl = n_unld_events/N_condition) |> 
  filter(N_condition > 0,
         n_unld_events > 0)

# add 15min obs and error bars
ggplot(prob_unl_df, aes(time_elapsed_bin_labs/60, prob_unl)) + 
  geom_point()
