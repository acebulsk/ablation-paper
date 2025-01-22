# script to generate model of wind induced unloading

## COMPUTE AVERAGES OVER BINS ---- 

met_unld_all_temps_limit_wind <- q_unld_met_scl |> 
  filter(
    name %in% scl_names,
    q_unl < 7,
    q_unl > min_qunld,
    u <= 1,
    #tree_mm > min_canopy_snow # this reduces our data by like 25%
  ) |> 
  mutate(`Canopy Load (mm)` = case_when(
    tree_mm < snow_load_th ~ 'Light',
    is.na(tree_mm) ~ 'NA',
    TRUE  ~ 'Heavy'
  )) |> group_by(`Canopy Load (mm)`) |> 
  mutate(avg_w_tree = mean(tree_mm))

met_unld_all_temps_limit_wind_smry <- met_unld_all_temps_limit_wind |> 
  group_by(temp_labs, `Canopy Load (mm)`, avg_w_tree) |> 
  summarise(q_unl_avg = mean(q_unl, na.rm = T),
            q_unl_sd = sd(q_unl, na.rm = T),
            sd_low = ifelse((q_unl_avg - q_unl_sd)<0,0, q_unl_avg - q_unl_sd),
            sd_hi = q_unl_avg + q_unl_sd,
            ci_low = quantile(q_unl,0.05),
            ci_hi = quantile(q_unl, 0.95),
            n = n()) |>
  filter(n > 3,
         !is.na(`Canopy Load (mm)`),
         !is.na(avg_w_tree))

## PLOT BINS ----



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

## MODEL ----

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

# per second for model
model_nls <- nls(q_unl_avg/(60*60) ~ a * avg_w_tree * exp(b * temp_labs), 
                 data = met_unld_all_temps_limit_wind_smry, 
                 start = list(a = a_lm, b = b_lm))
saveRDS(coef(model_nls), 'data/model_coef_temp_unld_per_second.rds')

# per hour for plotting
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

## PLOT MODEL ----

ex_temp_labs <- seq(-25,5,0.25)
ex_avg_w_tree <- c(3, 11)
temp_ex_df <- expand.grid(temp_labs = ex_temp_labs, avg_w_tree = ex_avg_w_tree)
temp_ex_df$new_predicted_y <- predict(model_nls, newdata = temp_ex_df)

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
  # theme_bw() +
  # scale_color_manual(values = c("#E69F00", "#009E73", "#56B4E9")) +
  scale_color_viridis_d(begin = 0, end = 0.8) +
  labs(colour = 'Canopy Snow\nLoad (mm)') +
  theme(legend.position = 'none')

ggsave(
  'figs/results/modelled_temp_unloading_w_obs.png',
  width = 4.5,
  height = 4
)

met_unld_all_temps_limit_wind_smry$pred_q_unl <- 
  predict(model_nls, met_unld_all_temps_limit_wind_smry)
# test what predict is doing 
met_unld_all_temps_limit_wind_smry$pred_q_unl_test <- 
  coef(model_nls)[['a']] * met_unld_all_temps_limit_wind_smry$avg_w_tree * exp(coef(model_nls)[['b']] * met_unld_all_temps_limit_wind_smry$temp_labs)

met_unld_all_temps_limit_wind_smry |> 
  ggplot(aes(temp_labs, colour = factor(round(avg_w_tree)), group = factor(avg_w_tree))) + 
  geom_point(aes(y = q_unl_avg)) +
  # geom_line(aes(y = pred_q_unl)) +
  geom_line(aes(y = pred_q_unl_test), linetype = 'dashed')

## ERROR TABLE ----

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