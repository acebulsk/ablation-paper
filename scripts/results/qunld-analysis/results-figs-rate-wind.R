# script to generate model of wind induced unloading

min_snow_bin <- 4

## COMPUTE AVERAGES OVER BINS ---- 

met_unld_all_winds_cold <- 
  # q_unld_met_scl |> 
  # met_binned has duplicated data for each scaled weighed tree
  # q_unld_scl has been filtered to remove troughs for erroneous periods
  left_join(q_unld_scl, met_binned, by = c('datetime', 'name')) |>
  filter(
    # name %in% scl_names,
    q_unl < 7,
    q_unl > 0,
    t < -6,
    wind_labs < 5, # obs at 6 m/s is outlier
  ) |> 
  mutate(`Canopy Load (mm)` = case_when(
    tree_mm < snow_load_th ~ 'Light',
    # tree_mm >= 5 & tree_mm <= 10 ~ 'Med',
    is.na(tree_mm) ~ 'NA',
    TRUE  ~ 'Heavy'
  )) |> group_by(`Canopy Load (mm)`) |> 
  mutate(avg_w_tree = mean(tree_mm))

met_unld_all_winds_cold |> 
  group_by(`Canopy Load (mm)`) |> 
  summarise(avg_w_tree = mean(tree_mm))

met_unld_all_winds_cold_smry <- met_unld_all_winds_cold |> 
  group_by(wind_labs, `Canopy Load (mm)`, avg_w_tree) |> 
  summarise(q_unl_avg = mean(q_unl, na.rm = T),
            q_unl_sd = sd(q_unl, na.rm = T),
            sd_low = ifelse((q_unl_avg - q_unl_sd)<0,0, q_unl_avg - q_unl_sd),
            sd_hi = q_unl_avg + q_unl_sd,
            ci_low = quantile(q_unl,0.05),
            ci_hi = quantile(q_unl, 0.95),
            sum_snow = sum(dU),
            n = n()) |> 
  filter(n >= 3,
         sum_snow > min_snow_bin,
         !is.na(`Canopy Load (mm)`),
         !is.na(avg_w_tree))

## PLOT BINS ----

### wind vs unloading rate ----

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
  ylim(NA, 3.1) +
  xlim(NA, 3.5) +
  scale_color_manual(values = c("#f89540", "#0072B2","#f89540", "#0072B2")) +
  labs(color = 'Mean Canopy Load (mm)')# + facet_grid(cols = vars(name))

ggsave(
  'figs/results/binned_unloading_rate_and_wind_mid_class_tree_load.png',
  device = png,
  width = 4,
  height = 4,
  units = "in"
)

## MODEL ----

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
# per second for model
# wind labs in here twice sets to 0 when wind is 0, dont need this for other params
model_nls <- nls(q_unl_avg/(60*60) ~ wind_labs * a * avg_w_tree * exp(b * wind_labs), 
                 data = met_unld_all_winds_cold_smry, 
                 start = list(a = a_lm, b = b_lm))
nls_coefs <- coef(model_nls)
saveRDS(nls_coefs, 'data/model_coef_wind_unld_per_second.rds')

met_unld_all_winds_cold_smry <- met_unld_all_winds_cold_smry |> 
  mutate(
    model_nls_pred = wind_labs * nls_coefs[[1]] * avg_w_tree * exp(nls_coefs[[2]]* wind_labs) # Linear model prediction
  )
# Create ggplot
ggplot(met_unld_all_winds_cold_smry, aes(x = wind_labs, y = model_nls_pred)) +
  # Observed vs NLS model
  geom_point(aes(color = "Observed vs NLS"), size = 2) 
# summary(model_nls)

# per hour for plotting
model_nls <- nls(q_unl_avg ~ wind_labs * a * avg_w_tree * exp(b * wind_labs), 
                 data = met_unld_all_winds_cold_smry, 
                 start = list(a = a_lm, b = b_lm))

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
ex_avg_w_tree <- c(3, 11)
wind_ex_df <- expand.grid(wind_labs = ex_wind_labs, avg_w_tree = ex_avg_w_tree)
new_predicted_y <- predict(model_nls, newdata = wind_ex_df)

## PLOT MODEL ----

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
  ylab(expression("Unloading Rate (kg" ~ m^-2 ~ hr^-1 * ")")) +
  xlab(expression("Wind Speed (m"~ s^-1 * ")")) +
  ylim(c(0, 2)) +
  xlim(c(0,3.5)) + # removes outlier at 6 m/s
  # theme_bw() +
  scale_color_viridis_d(begin = 0, end = 0.8) +
  labs(colour = expression(atop("Canopy Snow", "Load (kg m"^-2*")"))) # avoids large space using regular way

ggsave(
  'figs/results/modelled_wind_unloading_w_obs.png',
  width = 6,
  height = 4,
  device = png
)

met_unld_all_winds_cold_smry$pred_q_unl <- 
  predict(model_nls, met_unld_all_winds_cold_smry)

met_unld_all_winds_cold_smry |> 
  ggplot(aes(wind_labs, colour = factor(round(avg_w_tree)), group = factor(avg_w_tree))) + 
  geom_point(aes(y = q_unl_avg)) +
  geom_line(aes(y = pred_q_unl))

## ERROR TABLE ----

q_unl_temp_model_err_tbl <- met_unld_all_winds_cold_smry |> 
  ungroup() |> 
  mutate(diff = q_unl_avg - pred_q_unl) |> 
  # group_by(avg_w_tree) |> 
  summarise(
    `Mean Bias` = mean(diff, na.rm = T),
    # `Max Error` = diff[which.max(abs(diff))],
    MAE = mean(abs(diff), na.rm = T),
    `RMS Error` = sqrt(mean(diff ^ 2, na.rm = T))) |> 
  # left_join(coefs_df, by = c('plot_name', 'name')) |> 
  # left_join(df_r2_adj, by = c('plot_name', 'name')) |> 
  select(
    # `Mean Canopy Load (mm)` = avg_w_tree,
    `Mean Bias`,
    MAE,
    `RMS Error`
  ) |> 
  mutate(across(`Mean Bias`:`RMS Error`, round, digits = 3),
         R2 = rsq_nls)

saveRDS(q_unl_temp_model_err_tbl,
        'data/modelled_wind_unloading_error_table.rds')
