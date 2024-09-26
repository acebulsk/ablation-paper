# Final results section showing the unloading rate with increasing air
# temperature and wind speed. The data are filtered to produced these in an
# attempt to isolate temperature induced unloading and wind induced unloading.

temp_bin_ax_lab <- 'Air Temperature Bins (°C)'

## PLOTS ---- 

### duration vs unloading rate ---- 
met_unld_time_fltr <- met_unld |> 
  filter(is.na(duration_labs) == F,
         name %in% scl_names,
         u < 2,
         t < -6,
         q_unl < 7) |> 
  mutate(`Canopy Load (mm)` = case_when(
    obs_canopy_load >= 6.5 ~ '>= 6.5',
    TRUE  ~ '< 6.5'
  )) |> group_by(`Canopy Load (mm)`) |> 
  mutate(avg_w_tree = mean(obs_canopy_load, na.rm = T))

met_unld_time_smry <- met_unld_time_fltr |> 
  group_by(duration_labs, avg_w_tree) |> 
  summarise(q_unl_avg = mean(q_unl, na.rm = T),
            q_unl_sd = sd(q_unl, na.rm = T),
            sd_low = ifelse((q_unl_avg - q_unl_sd)<0,0, q_unl_avg - q_unl_sd),
            sd_hi = q_unl_avg + q_unl_sd,
            ci_low = quantile(q_unl,0.05),
            ci_hi = quantile(q_unl, 0.95),
            n = n()) |> 
  filter(n > 10)

ggplot(met_unld_time_smry, 
       aes(x = duration_labs, y = q_unl_avg, colour = as.character(round(avg_w_tree)))) + 
  # geom_point(data = met_unld_fltr, aes(time_elapsed_event, q_unl), alpha = 0.1, colour = 'black') +
  geom_errorbar(aes(
    x = duration_labs, 
    ymax = sd_hi,
    ymin = sd_low
    
  ), width = 0.2) +
  geom_point(size = 3) +
  ylab(bin_unl_ax_lab) +
  xlab('Duration Snow Intercepted in the Canopy (hours)') +
  theme_bw()  +
  scale_color_manual(values = c("#f89540", "#0072B2","#f89540", "#0072B2")) +
  labs(color = 'Mean Canopy Load (mm)')

### canopy snow load vs unloading rate ---- 

met_unld_tree_fltr <- met_unld |> 
  filter(name %in% scl_names,
         u < 2,
         t < -6,
         q_unl < 7) |> 
  mutate(time_group = case_when(
    time_elapsed_bin_labs > 15 ~ 'After 15 Hours',
    TRUE  ~ 'First 15 Hours'
  )) |> group_by(time_group) |> 
  mutate(avg_time_group = mean(time_elapsed_bin_labs))
  
met_unld_tree_smry <- met_unld_tree_fltr |> 
  filter(is.na(tree_labs) == F) |> 
  group_by(tree_labs) |> 
  summarise(q_unl_avg = mean(q_unl, na.rm = T),
            q_unl_sd = sd(q_unl, na.rm = T),
            sd_low = ifelse((q_unl_avg - q_unl_sd)<0,0, q_unl_avg - q_unl_sd),
            sd_hi = q_unl_avg + q_unl_sd,
            ci_low = quantile(q_unl,0.05),
            ci_hi = quantile(q_unl, 0.95),
            n = n()) |> 
  filter(n > 10)

ggplot(met_unld_tree_smry, 
       aes(x = tree_labs, y = q_unl_avg)) + 
  # geom_point(data = met_unld_tree_fltr, aes(tree_labs, q_unl), alpha = 0.1, colour = 'black') +
  # ylim(c(0, 1.5)) +
  geom_errorbar(aes(
    x = tree_labs, 
    ymax = sd_hi,
    ymin = sd_low
    
  ), width = 0.2) +
  geom_point(size = 3) +
  ylab(bin_unl_ax_lab) +
  xlab('Canopy Snow Load (mm)') +
  theme_bw() 

### wind vs unloading rate ----

met_unld_all_winds_cold <- met_unld |> 
  filter(
    name %in% scl_names,
    q_unl < 7,
    q_unl > min_qunld,
    t < -6,
    # obs_canopy_load >= min_canopy_snow # this reduces our data by like 25%, and removes some good obs of temp unloading near 5 deg. C
  ) |> 
  mutate(`Canopy Load (mm)` = case_when(
    obs_canopy_load >= 6.5 ~ '>= 6.5',
    is.na(obs_canopy_load) ~ 'NA',
    TRUE  ~ '< 6.5'
  )) |> group_by(`Canopy Load (mm)`) |> 
  mutate(avg_w_tree = mean(obs_canopy_load))

met_unld_all_winds_cold |> 
  group_by(`Canopy Load (mm)`) |> 
  summarise(avg_w_tree = mean(obs_canopy_load))

met_unld_all_winds_cold_smry <- met_unld_all_winds_cold |> 
  group_by(wind_labs, `Canopy Load (mm)`, avg_w_tree) |> 
  summarise(q_unl_avg = mean(q_unl, na.rm = T),
            q_unl_sd = sd(q_unl, na.rm = T),
            sd_low = ifelse((q_unl_avg - q_unl_sd)<0,0, q_unl_avg - q_unl_sd),
            sd_hi = q_unl_avg + q_unl_sd,
            ci_low = quantile(q_unl,0.05),
            ci_hi = quantile(q_unl, 0.95),
            n = n()) |> 
  filter(n >= 3)

ggplot(met_unld_all_winds_cold_smry, 
       aes(x = wind_labs, y = q_unl_avg, colour = as.character(round(avg_w_tree)))) + 
  geom_point(data = met_unld_all_winds_cold, aes(u, q_unl), alpha = 0.1, colour = 'black') +
  geom_errorbar(aes(
    x = wind_labs, 
    ymax = sd_hi,
    ymin = sd_low
   
  ), width = 0.2)  +
  geom_point(size = 3) +
  ylab(bin_unl_ax_lab) +
  xlab(bin_wnd_ax_lab) +
  theme_bw() +
  # theme_bw(base_size = 14) +
  theme(legend.position = 'bottom') +
  # ylim(NA, 3.1) +
  # xlim(NA, 3.5) +
  scale_color_manual(values = c("#f89540", "#0072B2","#f89540", "#0072B2")) +
  labs(color = 'Mean Canopy Load (mm)')

ggsave(
  'figs/results/binned_unloading_rate_and_wind_mid_class_tree_load.png',
  device = png,
  width = 4,
  height = 4,
  units = "in"
)

### temp vs unloading rate ----

met_unld_all_temps_limit_wind <- met_unld |> 
  filter(
    name %in% scl_names,
    q_unl < 7,
    q_unl > min_qunld,
    u <= 2,
    #obs_canopy_load > min_canopy_snow # this reduces our data by like 25%
  ) |> 
  mutate(`Canopy Load (mm)` = case_when(
    obs_canopy_load >= 6.5 ~ '>= 6.5',
    is.na(obs_canopy_load) ~ 'NA',
    TRUE  ~ '< 6.5'
  )) |> group_by(`Canopy Load (mm)`) |> 
  mutate(avg_w_tree = mean(obs_canopy_load))

met_unld_all_temps_limit_wind_smry <- met_unld_all_temps_limit_wind |> 
  group_by(temp_labs, `Canopy Load (mm)`, avg_w_tree) |> 
  summarise(q_unl_avg = mean(q_unl, na.rm = T),
            q_unl_sd = sd(q_unl, na.rm = T),
            sd_low = ifelse((q_unl_avg - q_unl_sd)<0,0, q_unl_avg - q_unl_sd),
            sd_hi = q_unl_avg + q_unl_sd,
            ci_low = quantile(q_unl,0.05),
            ci_hi = quantile(q_unl, 0.95),
            n = n()) |>
  filter(n > 10)

ggplot(met_unld_all_temps_limit_wind_smry) + 
  geom_point(data = met_unld_all_temps_limit_wind,
             aes(t, q_unl),
             alpha = 0.1) +
  geom_errorbar(aes(
    x = temp_labs,
    ymax = sd_hi,
    ymin = sd_low,
    width = 1,
    colour = as.character(round(avg_w_tree))
  ))  +
  geom_point(aes(temp_labs, q_unl_avg, colour = as.character(round(avg_w_tree))), size = 2) +
  ylab(bin_unl_ax_lab) +
  xlab(temp_bin_ax_lab)+
  # xlim(c(-15, 3))+
  theme_bw() +
  # theme_bw(base_size = 14) +
  theme(legend.position = 'bottom')+
  # ylim(NA, 3.1)+
  scale_color_manual(values = c("#f89540", "#0072B2","#f89540", "#0072B2")) +
  labs(color = 'Mean Canopy Load (mm)')

ggsave(
  'figs/results/binned_unloading_rate_and_t_air_class_canopy_load.png',
  device = png,
  width = 4,
  height = 4,
  units = "in"
)

# air temperature model ----

# fit a linear model
# plot(met_unld_all_temps_limit_wind_smry$temp_labs, met_unld_all_temps_limit_wind_smry$log_q_unl)
met_unld_all_temps_limit_wind_smry$log_q_unl_avg <- 
  log(met_unld_all_temps_limit_wind_smry$q_unl_avg)

model_lm <- lm(log_q_unl_avg ~ temp_labs, data = met_unld_all_temps_limit_wind_smry)
coefs <- coef(model_lm) |> as.numeric()
summary(model_lm)
a_lm <- exp(coefs[1])
b_lm <- coefs[2]

## Fit a non linear least squares model
# use starting values from the linear model 
model_nls <- nls(q_unl_avg ~ a * avg_w_tree * exp(b * temp_labs), 
                 data = met_unld_all_temps_limit_wind_smry, 
                 start = list(a = a_lm, b = b_lm))
# summary(model_nls)

RSS.p <- sum(residuals(model_nls)^2)  # Residual sum of squares
TSS <- sum((met_unld_all_temps_limit_wind_smry$q_unl_avg - mean(met_unld_all_temps_limit_wind_smry$q_unl_avg))^2)  # Total sum of squares
rsq_nls <- 1 - (RSS.p/TSS) |> round(2)  # R-squared measure

modelr::rsquare(model_nls, met_unld_all_temps_limit_wind_smry) # check is the same as our manually defined method

## Fit a non linear least squares model with weights iteratively 
# give small values a higher weight

max_iter <- 1000
tol <- 1e-6

coefs_old <- as.numeric(c(a_lm, b_lm))
wts <- met_unld_all_temps_limit_wind_smry$q_unl_avg^2

# # apply weights iteratively
# for (i in 1:max_iter) {
#   model_nlswi <- nls(q_unl_avg ~ a * avg_w_tree * exp(b * temp_labs),
#                      data = met_unld_all_temps_limit_wind_smry,
#                      weights = wts,
#                      start = c(a = coefs_old[1],
#                                b = coefs_old[2]),
#                      control = nls.control(maxiter = 1000))
#   coefs <- as.numeric(coef(model_nlswi))
#   max_change <- max(abs((coefs - coefs_old)/coefs_old))
#   if (max_change < tol) break
#   coefs_old <- coefs
#   yp <- predict(model_nlswi)
#   wts <- yp^2
# }
# 
# #summary(model_nls)
# 
# RSS.p <- sum(residuals(model_nlswi)^2)  # Residual sum of squares
# TSS <- sum((met_unld_all_temps_limit_wind_smry$q_unl_avg - mean(met_unld_all_temps_limit_wind_smry$q_unl_avg))^2)  # Total sum of squares
# rsq_nlswi <- 1 - (RSS.p/TSS) |> round(2)  # R-squared measure
# 
# modelr::rsquare(model_nlswi, met_unld_all_temps_limit_wind_smry) # check is the same as our manually defined method

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
# 
# nlswi <- data.frame(
#   mod_name = 'nlswi',
#   preds = predict(model_nlswi),
#   resids = residuals(model_nlswi, type = "pearson")
# )

resids_df <- rbind(
  lm,
  nls
  # nlswi
)

# Look at the Q-Q plot and residuals for the warm events 

ggplot(resids_df, aes(sample = preds, colour = mod_name)) +
  stat_qq() +
  stat_qq_line()

ggplot(resids_df, aes(x = preds, y = resids, colour = mod_name)) + 
  geom_point() +
  geom_hline(yintercept = 0)


# Look at the different models for the warm events 
ex_temp_labs <- seq(-35,5,0.25)
ex_avg_w_tree <- c(3, 5, 10, 11, seq(15, 30, by = 5))
temp_ex_df <- expand.grid(temp_labs = ex_temp_labs, avg_w_tree = ex_avg_w_tree)
new_predicted_y <- predict(model_nls, newdata = temp_ex_df)

ggplot(temp_ex_df) +
  geom_line(aes(temp_labs, new_predicted_y, colour = factor(avg_w_tree))) +
  geom_errorbar(data = met_unld_all_temps_limit_wind_smry,
                aes(
                  x = temp_labs,
                  ymax = sd_hi,
                  ymin = sd_low,
                  width = 1,
                  colour = as.character(round(avg_w_tree))
                )) +
  geom_point(data = met_unld_all_temps_limit_wind_smry,
             aes(temp_labs, q_unl_avg, colour = as.character(round(avg_w_tree))),
             size = 2) +
  ylab('Unloading + Drip Rate (mm/hr)') +
  xlab('Air Temperature (°C)') +
  # ylim(c(0,2)) +
  theme_bw() +
  scale_color_viridis_d() +
  labs(colour = 'Canopy Snow\nLoad (mm)')

ggsave(
  'figs/results/modelled_temp_unloading_w_obs.png',
  width = 6,
  height = 4
)

met_unld_all_temps_limit_wind_smry$pred_q_unl <- 
  predict(model_nls, met_unld_all_temps_limit_wind_smry)

met_unld_all_temps_limit_wind_smry |> 
  ggplot(aes(temp_labs, colour = factor(round(avg_w_tree)), group = factor(avg_w_tree))) + 
  geom_point(aes(y = q_unl_avg)) +
  geom_line(aes(y = pred_q_unl))

## temperature based unloading error table ----

q_unl_temp_model_err_tbl <- met_unld_all_temps_limit_wind_smry |> 
mutate(diff = q_unl_avg - pred_q_unl) |> 
  group_by(avg_w_tree) |> 
  summarise(
    `Mean Bias` = mean(diff, na.rm = T),
    # `Max Error` = diff[which.max(abs(diff))],
    MAE = mean(abs(diff), na.rm = T),
    `RMS Error` = sqrt(mean(diff ^ 2, na.rm = T))) |> 
  # left_join(coefs_df, by = c('plot_name', 'name')) |> 
  # left_join(df_r2_adj, by = c('plot_name', 'name')) |> 
  select(
    `Mean Canopy Load (mm)` = avg_w_tree,
    `Mean Bias`,
      MAE,
    `RMS Error`
  ) |> 
  mutate(across(`Mean Bias`:`RMS Error`, round, digits = 3),
         R2 = rsq_nls)

saveRDS(q_unl_temp_model_err_tbl,
        'data/modelled_temp_unloading_error_table.rds')

# wind induced unloading model ----

### fit a linear model ----
# plot(met_unld_all_winds_cold_smry$wind_labs, met_unld_all_winds_cold_smry$log_q_unl)
met_unld_all_winds_cold_smry$log_q_unl_avg <- 
  log(met_unld_all_winds_cold_smry$q_unl_avg)

model_lm <- lm(log_q_unl_avg ~ wind_labs, data = met_unld_all_winds_cold_smry)
coefs <- coef(model_lm) |> as.numeric()
summary(model_lm)
a_lm <- exp(coefs[1])
b_lm <- coefs[2]

### Fit a non linear least squares model ----

# use starting values from the linear model 
model_nls <- nls(q_unl_avg ~ a * avg_w_tree * exp(b * wind_labs), 
                 data = met_unld_all_winds_cold_smry, 
                 start = list(a = a_lm, b = b_lm))
# summary(model_nls)

RSS.p <- sum(residuals(model_nls)^2)  # Residual sum of squares
TSS <- sum((met_unld_all_winds_cold_smry$q_unl_avg - mean(met_unld_all_winds_cold_smry$q_unl_avg))^2)  # Total sum of squares
rsq_nls <- 1 - (RSS.p/TSS) |> round(2)  # R-squared measure

modelr::rsquare(model_nls, met_unld_all_winds_cold_smry) # check is the same as our manually defined method

### Fit a non linear least squares model with weights iteratively ----
# give large values a higher weight

max_iter <- 1000
tol <- 1e-6

coefs_old <- as.numeric(coef(model_nls))
wts <- met_unld_all_winds_cold_smry$q_unl_avg^2

# apply weights iteratively
# for (i in 1:max_iter) {
#   model_nlswi <- nls(q_unl_avg ~ a  * avg_w_tree * exp(b * wind_labs),
#                      data = met_unld_all_winds_cold_smry,
#                      weights = wts,
#                      start = c(a = coefs_old[1],
#                                b = coefs_old[2]),
#                      control = nls.control(maxiter = 1000))
#   coefs <- as.numeric(coef(model_nlswi))
#   max_change <- max(abs((coefs - coefs_old)/coefs_old))
#   if (max_change < tol) break
#   coefs_old <- coefs
#   yp <- predict(model_nlswi)
#   wts <- yp^2
# }
# 
# RSS.p <- sum(residuals(model_nlswi)^2)  # Residual sum of squares
# TSS <- sum((met_unld_all_winds_cold_smry$q_unl_avg - mean(met_unld_all_winds_cold_smry$q_unl_avg))^2)  # Total sum of squares
# rsq_nlswi <- 1 - (RSS.p/TSS) |> round(2)  # R-squared measure
# 
# modelr::rsquare(model_nlswi, met_unld_all_winds_cold_smry) # check is the same as our manually defined method

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

# nlswi <- data.frame(
#   mod_name = 'nlswi',
#   preds = predict(model_nlswi),
#   resids = residuals(model_nlswi, type = "pearson")
# )

resids_df <- rbind(
  lm,
  nls
 # nlswi
)

# Look at the Q-Q plot and residuals for the warm events 

ggplot(resids_df, aes(sample = preds, colour = mod_name)) +
  stat_qq() +
  stat_qq_line()

ggplot(resids_df, aes(x = preds, y = resids, colour = mod_name)) + 
  geom_point() +
  geom_hline(yintercept = 0)


# Look at the different models for the warm events 
ex_wind_labs <- seq(0,5,0.1)
ex_avg_w_tree <- c(3, 5, 10, 11, seq(15, 30, by = 5))
wind_ex_df <- expand.grid(wind_labs = ex_wind_labs, avg_w_tree = ex_avg_w_tree)
new_predicted_y <- predict(model_nls, newdata = wind_ex_df)

ggplot(wind_ex_df) +
  geom_line(aes(wind_labs, new_predicted_y, colour = factor(avg_w_tree))) +
  geom_errorbar(data = met_unld_all_winds_cold_smry,
                aes(
                  x = wind_labs,
                  ymax = sd_hi,
                  ymin = sd_low,
                  width = 0.15,
                  colour = as.character(round(avg_w_tree))
                )) +
  geom_point(data = met_unld_all_winds_cold_smry,
             aes(wind_labs, q_unl_avg, colour = as.character(round(avg_w_tree))),
             size = 2) +
  ylab('Unloading + Drip Rate (mm/hr)') +
  xlab('Wind Speed (m/s)') +
  # xlim(c(0,3.5)) +
  theme_bw() +
  scale_color_viridis_d() +
  labs(colour = 'Canopy Snow\nLoad (mm)')

ggsave(
  'figs/results/modelled_wind_unloading_w_obs.png',
  width = 6,
  height = 4
)

met_unld_all_winds_cold_smry$pred_q_unl <- 
  predict(model_nls, met_unld_all_winds_cold_smry)

met_unld_all_winds_cold_smry |> 
  ggplot(aes(wind_labs, colour = factor(round(avg_w_tree)), group = factor(avg_w_tree))) + 
  geom_point(aes(y = q_unl_avg)) +
  geom_line(aes(y = pred_q_unl))

## wind induced unloading error table ----

q_unl_temp_model_err_tbl <- met_unld_all_winds_cold_smry |> 
  mutate(diff = q_unl_avg - pred_q_unl) |> 
  group_by(avg_w_tree) |> 
  summarise(
    `Mean Bias` = mean(diff, na.rm = T),
    # `Max Error` = diff[which.max(abs(diff))],
    MAE = mean(abs(diff), na.rm = T),
    `RMS Error` = sqrt(mean(diff ^ 2, na.rm = T))) |> 
  # left_join(coefs_df, by = c('plot_name', 'name')) |> 
  # left_join(df_r2_adj, by = c('plot_name', 'name')) |> 
  select(
    `Mean Canopy Load (mm)` = avg_w_tree,
    `Mean Bias`,
    MAE,
    `RMS Error`
  ) |> 
  mutate(across(`Mean Bias`:`RMS Error`, round, digits = 3),
         R2 = rsq_nls)

saveRDS(q_unl_temp_model_err_tbl,
        'data/modelled_wind_unloading_error_table.rds')

# duration based unloading model ----

# adding canopy load failed to improve duration model so leaving it out 

### fit a linear model ----
# plot(met_unld_all_winds_cold_smry$wind_labs, met_unld_all_winds_cold_smry$log_q_unl)
met_unld_time_smry$log_q_unl_avg <- 
  log(met_unld_time_smry$q_unl_avg)

model_lm <- lm(log_q_unl_avg ~ duration_labs, data = met_unld_time_smry)
coefs <- coef(model_lm) |> as.numeric()
summary(model_lm)
a_lm <- exp(coefs[1])
b_lm <- coefs[2]

### Fit a non linear least squares model ----

# use starting values from the linear model 
model_nls <- nls(q_unl_avg ~ a * exp(b * duration_labs), 
                 data = met_unld_time_smry, 
                 start = list(a = a_lm, b = b_lm))
# summary(model_nls)

RSS.p <- sum(residuals(model_nls)^2)  # Residual sum of squares
TSS <- sum((met_unld_time_smry$q_unl_avg - mean(met_unld_time_smry$q_unl_avg))^2)  # Total sum of squares
rsq_nls <- 1 - (RSS.p/TSS) |> round(2)  # R-squared measure

modelr::rsquare(model_nls, met_unld_time_smry) # check is the same as our manually defined method

### Fit a non linear least squares model with weights iteratively ----
# give large values a higher weight

max_iter <- 1000
tol <- 1e-6

coefs_old <- as.numeric(coef(model_nls))
wts <- met_unld_time_smry$q_unl_avg^2

# apply weights iteratively
for (i in 1:max_iter) {
  model_nlswi <- nls(q_unl_avg ~ a * exp(b * duration_labs),
                     data = met_unld_time_smry,
                     weights = wts,
                     start = c(a = coefs_old[1],
                               b = coefs_old[2]),
                     control = nls.control(maxiter = 1000))
  coefs <- as.numeric(coef(model_nlswi))
  max_change <- max(abs((coefs - coefs_old)/coefs_old))
  if (max_change < tol) break
  coefs_old <- coefs
  yp <- predict(model_nlswi)
  wts <- yp^2
}

RSS.p <- sum(residuals(model_nlswi)^2)  # Residual sum of squares
TSS <- sum((met_unld_time_smry$q_unl_avg - mean(met_unld_time_smry$q_unl_avg))^2)  # Total sum of squares
rsq_nlswi <- 1 - (RSS.p/TSS) |> round(2)  # R-squared measure

modelr::rsquare(model_nlswi, met_unld_time_smry) # check is the same as our manually defined method

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

nlswi <- data.frame(
  mod_name = 'nlswi',
  preds = predict(model_nlswi),
  resids = residuals(model_nlswi, type = "pearson")
)

resids_df <- rbind(
  lm,
  nls,
  nlswi
)

# Look at the Q-Q plot and residuals for the warm events 

ggplot(resids_df, aes(sample = preds, colour = mod_name)) +
  stat_qq() +
  stat_qq_line()

ggplot(resids_df, aes(x = preds, y = resids, colour = mod_name)) + 
  geom_point() +
  geom_hline(yintercept = 0)


# Look at the different models for the warm events 
ex_duration_labs <- seq(0,225,0.25)
ex_avg_w_tree <- c(4, 5, 10, 11, seq(15, 30, by = 5))
ex_df <- expand.grid(duration_labs = ex_duration_labs, avg_w_tree = ex_avg_w_tree)
ex_df$new_predicted_y <- predict(model_nls, newdata = ex_df)

ggplot(ex_df) +
  geom_line(aes(duration_labs, new_predicted_y)) +
  geom_errorbar(data = met_unld_time_smry,
                aes(
                  x = duration_labs,
                  ymax = sd_hi,
                  ymin = sd_low,
                  width = 0.25,
                  colour = as.character(round(avg_w_tree))
                )) +
  geom_point(data = met_unld_time_smry,
             aes(duration_labs, q_unl_avg, colour = as.character(round(avg_w_tree))),
             size = 2) +
  ylab('Unloading + Drip Rate (mm/hr)') +
  xlab('Duration Snow Intercepted in Canopy (Hours)') +
  # ylim(c(0,2)) +
  theme_bw() +
  scale_color_viridis_d() +
  labs(colour = 'Canopy Snow\nLoad (mm)')

ggsave(
  'figs/results/modelled_duration_unloading_w_obs.png',
  width = 6,
  height = 4
)

met_unld_time_smry$pred_q_unl <- 
  predict(model_nls, met_unld_time_smry)

met_unld_time_smry |> 
  ggplot(aes(duration_labs, colour = factor(round(avg_w_tree)), group = factor(avg_w_tree))) + 
  geom_point(aes(y = q_unl_avg)) +
  geom_line(aes(y = pred_q_unl))

## duration unloading error table ----

q_unl_time_model_err_tbl <- met_unld_time_smry |> 
  mutate(diff = q_unl_avg - pred_q_unl) |> 
  group_by(avg_w_tree) |> 
  summarise(
    `Mean Bias` = mean(diff, na.rm = T),
    # `Max Error` = diff[which.max(abs(diff))],
    MAE = mean(abs(diff), na.rm = T),
    `RMS Error` = sqrt(mean(diff ^ 2, na.rm = T))) |> 
  # left_join(coefs_df, by = c('plot_name', 'name')) |> 
  # left_join(df_r2_adj, by = c('plot_name', 'name')) |> 
  select(
    `Mean Canopy Load (mm)` = avg_w_tree,
    `Mean Bias`,
    MAE,
    `RMS Error`
  ) |> 
  mutate(across(`Mean Bias`:`RMS Error`, round, digits = 3),
         R2 = rsq_nls)

saveRDS(q_unl_time_model_err_tbl,
        'data/modelled_duration_unloading_error_table.rds')

# snow snow load based unloading model ----

### fit a linear model ----
# plot(met_unld_all_winds_cold_smry$wind_labs, met_unld_all_winds_cold_smry$log_q_unl)
met_unld_tree_smry$log_q_unl_avg <- 
  log(met_unld_tree_smry$q_unl_avg)

model_lm <- lm(log_q_unl_avg ~ tree_labs, data = met_unld_tree_smry)
coefs <- coef(model_lm) |> as.numeric()
summary(model_lm)
a_lm <- exp(coefs[1])
b_lm <- coefs[2]

### Fit a non linear least squares model ----

# use starting values from the linear model 
model_nls <- nls(q_unl_avg ~ a * exp(b * tree_labs), 
                 data = met_unld_tree_smry, 
                 start = list(a = a_lm, b = b_lm))
# summary(model_nls)

RSS.p <- sum(residuals(model_nls)^2)  # Residual sum of squares
TSS <- sum((met_unld_tree_smry$q_unl_avg - mean(met_unld_tree_smry$q_unl_avg))^2)  # Total sum of squares
rsq_nls <- 1 - (RSS.p/TSS) |> round(2)  # R-squared measure

modelr::rsquare(model_nls, met_unld_tree_smry) # check is the same as our manually defined method

### Fit a non linear least squares model with weights iteratively ----
# give large values a higher weight

max_iter <- 1000
tol <- 1e-6

coefs_old <- as.numeric(coef(model_nls))
wts <- met_unld_tree_smry$q_unl_avg^2

# # apply weights iteratively
# for (i in 1:max_iter) {
#   model_nlswi <- nls(q_unl_avg ~ a * exp(b * tree_labs),
#                      data = met_unld_tree_smry,
#                      weights = wts,
#                      start = c(a = coefs_old[1],
#                                b = coefs_old[2]),
#                      control = nls.control(maxiter = 1000))
#   coefs <- as.numeric(coef(model_nlswi))
#   max_change <- max(abs((coefs - coefs_old)/coefs_old))
#   if (max_change < tol) break
#   coefs_old <- coefs
#   yp <- predict(model_nlswi)
#   wts <- yp^2
# }

# RSS.p <- sum(residuals(model_nlswi)^2)  # Residual sum of squares
# TSS <- sum((met_unld_tree_smry$q_unl_avg - mean(met_unld_tree_smry$q_unl_avg))^2)  # Total sum of squares
# rsq_nlswi <- 1 - (RSS.p/TSS) |> round(2)  # R-squared measure

# modelr::rsquare(model_nlswi, met_unld_tree_smry) # check is the same as our manually defined method

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

# nlswi <- data.frame(
#   mod_name = 'nlswi',
#   preds = predict(model_nlswi),
#   resids = residuals(model_nlswi, type = "pearson")
# )

resids_df <- rbind(
  lm,
  nls
  # nlswi
)

# Look at the Q-Q plot and residuals for the warm events 

ggplot(resids_df, aes(sample = preds, colour = mod_name)) +
  stat_qq() +
  stat_qq_line()

ggplot(resids_df, aes(x = preds, y = resids, colour = mod_name)) + 
  geom_point() +
  geom_hline(yintercept = 0)


# Look at the different models for the warm events 
ex_tree_labs <- seq(0,20,0.25)
ex_df <- data.frame(tree_labs = ex_tree_labs)
ex_df$new_predicted_y <- predict(model_nls, newdata = ex_df)

ggplot(ex_df) +
  geom_line(aes(tree_labs, new_predicted_y)) +
  geom_errorbar(data = met_unld_tree_smry,
                aes(
                  x = tree_labs,
                  ymax = sd_hi,
                  ymin = sd_low,
                  width = 0.25)
                ) +
  geom_point(data = met_unld_tree_smry,
             aes(tree_labs, q_unl_avg),
             size = 2) +
  ylab('Unloading + Drip Rate (mm/hr)') +
  xlab('Canopy Snow Load (mm)') +
  # ylim(c(0,2)) +
  theme_bw() +
  scale_color_viridis_d() +
  labs(colour = 'Canopy Snow\nLoad (mm)')

ggsave(
  'figs/results/modelled_canopy_snowload_unloading_w_obs.png',
  width = 6,
  height = 4
)

met_unld_tree_smry$pred_q_unl <- 
  predict(model_nls, met_unld_tree_smry)

met_unld_tree_smry |> 
  ggplot(aes(tree_labs)) + 
  geom_point(aes(y = q_unl_avg)) +
  geom_line(aes(y = pred_q_unl))

## snow load unloading error table ----

q_unl_time_model_err_tbl <- met_unld_tree_smry |> 
  mutate(diff = q_unl_avg - pred_q_unl) |> 
  summarise(
    `Mean Bias` = mean(diff, na.rm = T),
    # `Max Error` = diff[which.max(abs(diff))],
    MAE = mean(abs(diff), na.rm = T),
    `RMS Error` = sqrt(mean(diff ^ 2, na.rm = T))) |> 
  # left_join(coefs_df, by = c('plot_name', 'name')) |> 
  # left_join(df_r2_adj, by = c('plot_name', 'name')) |> 
  select(
    `Mean Bias`,
    MAE,
    `RMS Error`
  ) |> 
  mutate(across(`Mean Bias`:`RMS Error`, round, digits = 3),
         R2 = rsq_nls)

saveRDS(q_unl_time_model_err_tbl,
        'data/modelled_canopyload_unloading_error_table.rds')

