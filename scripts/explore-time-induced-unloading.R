# See if there is an effect of time induced unloading based on filtered
# unloading data set to remove periods above the temperature and wind thresholds
# found in the results figs. Also calculated the time-elapsed over each event to
# act as the independent variable.
break_point_low <- 22
break_point_high <- 17
y_lab <- 'Probability of Unloading (-)'
x_lab <- 'Duration Snow Intercepted in Canopy (Hours)'

met_unld_time <- met_unld |> 
  filter(name %in% scl_names,
         # inst_type == 'subcanopy trough', # this one is redundant but pedantic!  
         q_unl < 7,
         # q_unl > min_qunld,
         t < -6,
         u < 2,
         !(time_elapsed_bin_labs == 54 & q_unl > 2)
         obs_canopy_load >= min_canopy_snow) |> 
  mutate(binary_unl = ifelse(q_unl > min_qunld, 1, 0)) |> # timesteps are 15 minutes
  select(datetime, t, time_elapsed_event, binary_unl, q_unl)

min_bin <- 0
max_bin <- round(
  max(met_unld_time$time_elapsed_event, na.rm = T),3)
step <- 0.25

breaks <- seq(
  min_bin,
  max_bin + step,
  step)

labs_seq <- seq(from = step, by = step, length.out = length(breaks)-1)

stopifnot(tail(breaks, 1) > max(met_unld_time$time_elapsed_event, na.rm = T))
stopifnot(length(labs_seq) + 1 == length(breaks))

met_unld_time$time_elapsed_binned <- cut(met_unld_time[, 'time_elapsed_event',
                                             drop = TRUE], breaks)

met_unld_time$time_elapsed_bin_labs <- cut(met_unld_time[, 'time_elapsed_event',
                                               drop = TRUE],
                                      breaks, labels = labs_seq)

met_unld_time$time_elapsed_bin_labs <- as.numeric(as.character(met_unld_time$time_elapsed_bin_labs))

# plot probability ----

prob_unl_df <- met_unld_time |>
  group_by(time_elapsed_bin_labs) |>
  summarise(n_unld_events = sum(binary_unl),
            N_condition = n(),
            prob_unl = n_unld_events/N_condition,
            t = mean(t)) |>
  filter(N_condition > 0,
         n_unld_events > 0)

# add 15min obs and error bars
ggplot(prob_unl_df, aes(time_elapsed_bin_labs, prob_unl)) +
  geom_point() +
  labs(y = y_lab,
       x = x_lab) #+
  # scale_color_viridis_c(option = 'magma') +
  # labs(colour = 'Air Temperature (°C)')

ggsave('figs/results/probability_of_unloading_w_time_rm_warm_windy_periods.png', width = 4.5, height = 3)

# fit model to probability ----

### fit a linear model ----
# plot(prob_unl_df$time_elapsed_bin_labs, prob_unl_df$log_q_unl)
prob_unl_df$log_prob_unl <- 
  log(prob_unl_df$prob_unl)

model_lm <- lm(log_prob_unl ~ time_elapsed_bin_labs, data = prob_unl_df)
coefs <- coef(model_lm) |> as.numeric()
summary(model_lm)
a_lm <- exp(coefs[1])
b_lm <- coefs[2]

### Fit a non linear least squares model ----

# use starting values from the linear model 
model_nls <- nls(prob_unl ~ a * exp(b * time_elapsed_bin_labs), 
                 data = prob_unl_df, 
                 start = list(a = a_lm, b = b_lm))
# summary(model_nls)

RSS.p <- sum(residuals(model_nls)^2)  # Residual sum of squares
TSS <- sum((prob_unl_df$prob_unl - mean(prob_unl_df$prob_unl))^2)  # Total sum of squares
rsq_nls <- 1 - (RSS.p/TSS) |> round(2)  # R-squared measure

modelr::rsquare(model_nls, prob_unl_df) # check is the same as our manually defined method

# explore resids ---- 

lm <- data.frame(
  mod_name = 'lm',
  preds = exp(predict(model_lm)),
  resids = residuals(model_lm, type = "pearson")
)

nls <- data.frame(
  mod_name = 'nls',
  preds = predict(model_nls),
  resids = residuals(model_nls, type = "pearson")
)

resids_df <- rbind(
  lm,
  nls
)

# Look at the Q-Q plot and residuals for the warm events 

ggplot(resids_df, aes(sample = preds, colour = mod_name)) +
  stat_qq() +
  stat_qq_line()

ggplot(resids_df, aes(x = preds, y = resids, colour = mod_name)) + 
  geom_point() +
  geom_hline(yintercept = 0)

## plot model and observed data ----

ex_time <- seq(0,150,0.25)
ex_df <- data.frame(time_elapsed_bin_labs = ex_time)
ex_df$new_predicted_y <- predict(model_nls, newdata = ex_df)

ggplot(prob_unl_df) +
  geom_line(data = ex_df, aes(time_elapsed_bin_labs, new_predicted_y)) +
  # geom_errorbar(data = met_unld_cold_windy_smry,
  #               aes(
  #                 x = wind_labs,
  #                 ymax = sd_hi,
  #                 ymin = sd_low,
  #                 width = 0.25,
  #                 colour = as.character(round(avg_w_tree))
  #               )) +
  geom_point(aes(time_elapsed_bin_labs, prob_unl)) +
  ylab(y_lab) +
  xlab(x_lab) +
  # ylim(c(0,2)) +
  theme_bw()

ggsave(
  'figs/results/modelled_wind_unloading_w_obs.png',
  width = 6,
  height = 4
)

# fit model to probability using breakpoint regression ----

### fit a linear model to first 15 hrs ----
# plot(prob_unl_df$time_elapsed_bin_labs, prob_unl_df$log_q_unl)

model_lm <- lm(log_prob_unl ~ time_elapsed_bin_labs,
               data = prob_unl_df |> filter(time_elapsed_bin_labs <= break_point_low))
coefs <- coef(model_lm) |> as.numeric()
summary(model_lm)
a_lm <- exp(coefs[1])
b_lm <- coefs[2]

### Fit a non linear least squares model ----

# use starting values from the linear model 
model_nls_first15 <- nls(prob_unl ~ a * exp(b * time_elapsed_bin_labs), 
                 data = prob_unl_df |> filter(time_elapsed_bin_labs <= break_point_low), 
                 start = list(a = a_lm, b = b_lm))
# summary(model_nls)

RSS.p <- sum(residuals(model_nls)^2)  # Residual sum of squares
TSS <- sum((prob_unl_df$prob_unl - mean(prob_unl_df$prob_unl))^2)  # Total sum of squares
rsq_nls <- 1 - (RSS.p/TSS) |> round(2)  # R-squared measure

modelr::rsquare(model_nls, prob_unl_df) # check is the same as our manually defined method

### fit a linear model to after 15 hrs ----
# plot(prob_unl_df$time_elapsed_bin_labs, prob_unl_df$log_q_unl)

model_lm <- lm(log_prob_unl ~ time_elapsed_bin_labs,
               data = prob_unl_df |> filter(time_elapsed_bin_labs > break_point_high))
coefs <- coef(model_lm) |> as.numeric()
summary(model_lm)
a_lm <- exp(coefs[1])
b_lm <- coefs[2]

### Fit a non linear least squares model ----

# use starting values from the linear model 
model_nls_after15 <- nls(prob_unl ~ a * exp(b * time_elapsed_bin_labs), 
                         data = prob_unl_df |> filter(time_elapsed_bin_labs > break_point_high), 
                         start = list(a = a_lm, b = b_lm))
# summary(model_nls)

RSS.p <- sum(residuals(model_nls)^2)  # Residual sum of squares
TSS <- sum((prob_unl_df$prob_unl - mean(prob_unl_df$prob_unl))^2)  # Total sum of squares
rsq_nls <- 1 - (RSS.p/TSS) |> round(2)  # R-squared measure

modelr::rsquare(model_nls, prob_unl_df) # check is the same as our manually defined method


## plot model and observed data ----

ex_time <- seq(0,150,0.25)
ex_df <- data.frame(time_elapsed_bin_labs = ex_time)
ex_df$new_predicted_y_f15 <- predict(model_nls_first15, newdata = ex_df)
ex_df$new_predicted_y_a15 <- predict(model_nls_after15, newdata = ex_df)

ex_df$new_predicted_y <- ifelse(ex_df <= 15,
                                ex_df$new_predicted_y_f15,
                                ex_df$new_predicted_y_a15)

ggplot(prob_unl_df) +
  geom_line(data = ex_df, aes(time_elapsed_bin_labs, new_predicted_y_f15)) +
  geom_line(data = ex_df, aes(time_elapsed_bin_labs, new_predicted_y_a15)) +
  # geom_errorbar(data = met_unld_cold_windy_smry,
  #               aes(
  #                 x = wind_labs,
  #                 ymax = sd_hi,
  #                 ymin = sd_low,
  #                 width = 0.25,
  #                 colour = as.character(round(avg_w_tree))
  #               )) +
  geom_point(aes(time_elapsed_bin_labs, prob_unl)) +
  ylab(y_lab) +
  xlab(x_lab) +
  ylim(c(0,1)) +
  theme_bw() 

ggsave(
  'figs/results/modelled_wind_unloading_w_obs_breakpoint.png',
  width = 6,
  height = 4
)


