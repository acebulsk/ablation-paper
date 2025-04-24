# Since the scripts results-figs-rate-subl and ...-wind both show assocaitions
# with unloading we need to test a relationship with both wind and subl as
# predictors of unloading

## BIN TREE DATA ----

# note zeros are not included in binning, to add set inlcude.lowest = T
min_tree <- round(
  min(met_unld_no_melt$tree_mm, na.rm = T),3)
max_tree <- round(
  max(met_unld_no_melt$tree_mm, na.rm = T),3)
tree_step <- 5

tree_breaks <- seq(
  min_tree,
  max_tree+tree_step,
  tree_step)

# tree_breaks <- c(0, 4,20)
# tree_breaks <- c(0, 3, 6,  20) # works well for wind but breaks sublimation fn

tree_labs_seq <- label_bin_fn(bins = tree_breaks)

stopifnot(tail(tree_breaks, 1) > max(met_unld_no_melt$tree_mm, na.rm = T))
stopifnot(length(tree_labs_seq) + 1 == length(tree_breaks))

met_unld_no_melt$tree_binned <- cut(met_unld_no_melt[,'tree_mm', drop = TRUE], tree_breaks, include.lowest = T)

met_unld_no_melt$tree_labs <- cut(met_unld_no_melt[,'tree_mm', drop = TRUE], 
                                  tree_breaks, include.lowest = T, 
                                  labels = tree_labs_seq)

met_unld_no_melt$tree_labs <- as.numeric(as.character(met_unld_no_melt$tree_labs))

met_unld_no_melt |> 
  group_by(tree_labs) |> 
  summarise(tree_mean = mean(tree_mm))

met_unld_no_melt |> 
  group_by(tree_labs) |> 
  tally()

# average unloading rate over bins of sublimation, wind, and tree load
met_unld_no_melt_smry <- met_unld_no_melt |> 
  group_by(name, subl_labs, wind_labs, tree_labs) |> 
  summarise(q_unl_avg = mean(q_unl, na.rm = T),
            q_unl_sd = sd(q_unl, na.rm = T),
            sd_low = ifelse((q_unl_avg - q_unl_sd)<0,0, q_unl_avg - q_unl_sd),
            sd_hi = q_unl_avg + q_unl_sd,
            ci_low = quantile(q_unl,0.05),
            ci_hi = quantile(q_unl, 0.95),
            sum_snow = sum(dU),
            n = n()) |> 
  mutate(q_unl_subl_ratio = q_unl_avg / subl_labs) |> 
  filter(n >= 3,
         sum_snow > 0.1,
         subl_labs > 0)

ggplot(
  met_unld_no_melt_smry |> filter(q_unl_avg < 1),
  aes(subl_labs, wind_labs, fill = q_unl_avg)
) +
  geom_tile() +
  scale_fill_viridis_c()

## PLOT BINS ----

### check wind subl interaction ----

met_unld_no_melt |> 
  ggplot(aes(u, q_subl_veg)) + 
  geom_point()

# binned data 

met_unld_no_melt_smry |> 
  ggplot(aes(wind_labs, q_unl_avg, colour = subl_labs)) + 
  geom_point()
met_unld_no_melt_smry |> 
  ggplot(aes(wind_labs, q_unl_avg, colour = tree_labs)) + 
  geom_point()

met_unld_no_melt_smry |> 
  ggplot(aes(subl_labs, q_unl_avg)) + 
  geom_point()

met_unld_no_melt_smry |> 
  ggplot(aes(tree_labs, q_unl_avg)) + 
  geom_point()

met_unld_no_melt_smry |> 
  ggplot(aes(wind_labs, q_unl_subl_ratio)) + 
  geom_point()

met_unld_no_melt_smry |> 
  ggplot(aes(tree_labs, q_unl_subl_ratio)) + 
  geom_point()

met_unld_no_melt_smry |> 
  ggplot(aes(wind_labs, subl_labs)) + 
  geom_point()

met_unld_no_melt_smry |> 
  ggplot(aes(wind_labs, tree_labs)) + 
  geom_point()

met_unld_no_melt_smry |> 
  ggplot(aes(tree_labs, subl_labs)) + 
  geom_point()

met_unld_no_melt_smry |> 
  ungroup() |> 
  select(q_unl_avg, subl_labs, wind_labs, tree_labs) |> cor()


## linear model ----

model_lm <- lm(q_unl_avg ~ tree_labs + wind_labs + subl_labs, data = met_unld_no_melt_smry)
summary(model_lm)
model_lm <- lm(q_unl_avg ~ tree_labs + wind_labs, data = met_unld_no_melt_smry)
summary(model_lm)
model_lm <- lm(q_unl_avg ~ tree_labs +subl_labs, data = met_unld_no_melt_smry)
summary(model_lm)

## non linear relationship ----

### predict unloading ----

model_nls <- nls(q_unl_avg ~ a * tree_labs * exp(b * wind_labs), 
                 data = met_unld_no_melt_smry, 
                 start = list(a = 0.1, b = 0.1))

summary(model_nls)
nls_coefs <- coef(model_nls)


RSS.p <- sum(residuals(model_nls)^2)  # Residual sum of squares
TSS <- sum((met_unld_no_melt_smry$q_unl_avg - mean(met_unld_no_melt_smry$q_unl_avg))^2)  # Total sum of squares
rsq_nls <- 1 - (RSS.p/TSS) |> round(2)  # R-squared measure
rsq_nls
modelr::rsquare(model_nls, met_unld_no_melt_smry) # check is the same as our manually defined method

met_unld_no_melt_smry$q_unl_avg_pred <- predict(model_nls, met_unld_no_melt_smry)

# Create ggplot

ggplot(met_unld_no_melt_smry, aes(x = wind_labs, y = q_unl_avg_pred, colour = tree_labs)) +
  # Observed vs NLS model
  geom_point() 

ggplot(met_unld_no_melt_smry, aes(x = wind_labs, y = q_unl_avg_pred, colour = subl_labs)) +
  # Observed vs NLS model
  geom_point() 

ggplot(met_unld_no_melt_smry, aes(x = subl_labs, y = q_unl_avg_pred, colour = tree_labs)) +
  # Observed vs NLS model
  geom_point() 

#### plot mod sensitivity ----

ex_u <- seq(0,5,0.1)
ex_tree <- seq(2, 20, 5)
wind_ex_df <- expand.grid(wind_labs = ex_u, subl_labs = 0.05, tree_labs = ex_tree)

wind_ex_df$pred_q_unld <- predict(model_nls, newdata = wind_ex_df)

ggplot(wind_ex_df) +
  geom_line(aes(wind_labs, pred_q_unld, colour = factor(tree_labs))) +
  # geom_errorbar(data = met_unld_all_winds_cold_smry,
  #               aes(
  #                 x = wind_labs,
  #                 ymax = sd_hi,
  #                 ymin = sd_low,
  #                 width = 0.15,
  #                 colour = as.character(round(tree_labs))
  #               )) +
  # geom_point(data = met_unld_all_winds_cold_smry,
  #            aes(wind_labs, q_unl_avg, colour = as.character(round(tree_labs))),
  #            size = 2) +
  ylab(expression("Unloading Rate (kg" ~ m^-2 ~ hr^-1 * ")")) +
  xlab(expression("Wind Speed (m"~ s^-1 * ")")) +
  # ylim(c(0, 2)) +
  # xlim(c(0,3.5)) + # removes outlier at 6 m/s
  # theme_bw() +
  scale_color_viridis_d(begin = 0, end = 0.8) +
  labs(colour = expression(atop("Canopy Snow", "Load (kg m"^-2*")"))) # avoids large space using regular way

wind_ex_df <- expand.grid(wind_labs = 1, subl_labs = ex_q_subl_veg, tree_labs = ex_tree)
wind_ex_df$pred_q_unld <- predict(model_nls, newdata = wind_ex_df)

ggplot(wind_ex_df) +
  geom_line(aes(subl_labs, pred_q_unld, colour = factor(tree_labs))) +
  # geom_errorbar(data = met_unld_all_winds_cold_smry,
  #               aes(
  #                 x = wind_labs,
  #                 ymax = sd_hi,
  #                 ymin = sd_low,
  #                 width = 0.15,
  #                 colour = as.character(round(tree_labs))
  #               )) +
  # geom_point(data = met_unld_all_winds_cold_smry,
  #            aes(wind_labs, q_unl_avg, colour = as.character(round(tree_labs))),
  #            size = 2) +
  ylab(expression("Unloading Rate (kg" ~ m^-2 ~ hr^-1 * ")")) +
  xlab('Sublimation Rate (mm/hr)') +
  # ylim(c(0, 2)) +
  # xlim(c(0,3.5)) + # removes outlier at 6 m/s
  # theme_bw() +
  scale_color_viridis_d(begin = 0, end = 0.8) +
  labs(colour = expression(atop("Canopy Snow", "Load (kg m"^-2*")"))) # avoids large space using regular way

### predict ratio ----

model_nls <- nls(q_unl_subl_ratio ~ a * tree_labs * exp(b * wind_labs), 
                 data = met_unld_no_melt_smry, 
                 start = list(a = 0.1, b = 0.1))

summary(model_nls)
nls_coefs <- coef(model_nls)


RSS.p <- sum(residuals(model_nls)^2)  # Residual sum of squares
TSS <- sum((met_unld_no_melt_smry$q_unl_subl_ratio - mean(met_unld_no_melt_smry$q_unl_subl_ratio))^2)  # Total sum of squares
rsq_nls <- 1 - (RSS.p/TSS) |> round(2)  # R-squared measure
rsq_nls

met_unld_no_melt_smry$q_unl_subl_ratio_pred <- predict(model_nls, met_unld_no_melt_smry)
met_unld_no_melt_smry$q_unl_avg_pred <- met_unld_no_melt_smry$q_unl_subl_ratio_pred * met_unld_no_melt_smry$subl_labs

# Create ggplot
ggplot(met_unld_no_melt_smry, aes(x = wind_labs, y = q_unl_avg_pred, colour = tree_labs)) +
  # Observed vs NLS model
  geom_point() 

ggplot(met_unld_no_melt_smry, aes(x = subl_labs, y = q_unl_avg_pred, colour = tree_labs)) +
  # Observed vs NLS model
  geom_point() 

### PLOT MODEL SENSITIVITY ----

ex_u <- seq(0,3,0.1)
ex_tree <- seq(2, 20, 5)
wind_ex_df <- expand.grid(wind_labs = ex_u, subl_labs = 0.1, tree_labs = ex_tree)

wind_ex_df$pred_ratio <- predict(model_nls, newdata = wind_ex_df)
wind_ex_df$pred_q_unld <- wind_ex_df$pred_ratio * wind_ex_df$subl_labs

ggplot(wind_ex_df) +
  geom_line(aes(wind_labs, pred_q_unld, colour = factor(tree_labs))) +
  # geom_errorbar(data = met_unld_all_winds_cold_smry,
  #               aes(
  #                 x = wind_labs,
  #                 ymax = sd_hi,
  #                 ymin = sd_low,
  #                 width = 0.15,
  #                 colour = as.character(round(tree_labs))
  #               )) +
  # geom_point(data = met_unld_all_winds_cold_smry,
  #            aes(wind_labs, q_unl_avg, colour = as.character(round(tree_labs))),
  #            size = 2) +
  ylab(expression("Unloading Rate (kg" ~ m^-2 ~ hr^-1 * ")")) +
  xlab(expression("Wind Speed (m"~ s^-1 * ")")) +
  # ylim(c(0, 2)) +
  # xlim(c(0,3.5)) + # removes outlier at 6 m/s
  # theme_bw() +
  scale_color_viridis_d(begin = 0, end = 0.8) +
  labs(colour = expression(atop("Canopy Snow", "Load (kg m"^-2*")"))) # avoids large space using regular way

ex_q_subl_veg <- seq(0, 0.7, by = 0.1)
wind_ex_df <- expand.grid(wind_labs = 1, subl_labs = ex_q_subl_veg, tree_labs = ex_tree)
wind_ex_df$pred_ratio <- predict(model_nls, newdata = wind_ex_df)
wind_ex_df$pred_q_unld <- wind_ex_df$pred_ratio * wind_ex_df$subl_labs

ggplot(wind_ex_df) +
  geom_line(aes(subl_labs, pred_q_unld, colour = factor(tree_labs))) +
  # geom_errorbar(data = met_unld_all_winds_cold_smry,
  #               aes(
  #                 x = wind_labs,
  #                 ymax = sd_hi,
  #                 ymin = sd_low,
  #                 width = 0.15,
  #                 colour = as.character(round(tree_labs))
  #               )) +
  # geom_point(data = met_unld_all_winds_cold_smry,
  #            aes(wind_labs, q_unl_avg, colour = as.character(round(tree_labs))),
  #            size = 2) +
  ylab(expression("Unloading Rate (kg" ~ m^-2 ~ hr^-1 * ")")) +
  xlab('Sublimation Rate (mm/hr)') +
  # ylim(c(0, 2)) +
  # xlim(c(0,3.5)) + # removes outlier at 6 m/s
  # theme_bw() +
  scale_color_viridis_d(begin = 0, end = 0.8) +
  labs(colour = expression(atop("Canopy Snow", "Load (kg m"^-2*")"))) # avoids large space using regular way

## BINNED DATA ----

# all vars
model_nls_wind_subl <- nls(q_unl_avg ~ wind_labs * a * tree_labs * exp(b * subl_labs + c * wind_labs), 
                 data = met_unld_no_melt_smry, 
                 start = list(a = 0.02, b = 0.6, c = 0.01))
summary(model_nls_wind_subl)

# Check model assumptions


RSS.p <- sum(residuals(model_nls_wind_subl)^2)  # Residual sum of squares
TSS <- sum((met_unld_no_melt_smry$q_unl_avg - mean(met_unld_no_melt_smry$q_unl_avg))^2)  # Total sum of squares
rsq_nls <- 1 - (RSS.p/TSS) |> round(2)  # R-squared measure
rsq_nls
modelr::rsquare(model_nls_wind_subl, met_unld_no_melt_smry) # check is the same as our manually defined method

# just subl
model_nls <- nls(q_unl_avg ~ a * tree_labs * exp(b * subl_labs),
                 data = met_unld_no_melt_smry,
                 start = list(a = 0.02, b = 0.6))
summary(model_nls)
RSS.p <- sum(residuals(model_nls)^2)  # Residual sum of squares
TSS <- sum((met_unld_no_melt_smry$q_unl_avg - mean(met_unld_no_melt_smry$q_unl_avg))^2)  # Total sum of squares
rsq_nls <- 1 - (RSS.p/TSS) |> round(2)  # R-squared measure
rsq_nls
modelr::rsquare(model_nls, met_unld_no_melt_smry) # check is the same as our manually defined method

# all var model explains ~ 13% more variability

# just wind
model_nls <- nls(q_unl_avg ~ wind_labs * a * tree_labs * exp(b * wind_labs),
                 data = met_unld_no_melt_smry,
                 start = list(a = 0.02, b = 0.6))
summary(model_nls)
RSS.p <- sum(residuals(model_nls)^2)  # Residual sum of squares
TSS <- sum((met_unld_no_melt_smry$q_unl_avg - mean(met_unld_no_melt_smry$q_unl_avg))^2)  # Total sum of squares
rsq_nls <- 1 - (RSS.p/TSS) |> round(2)  # R-squared measure
rsq_nls
modelr::rsquare(model_nls, met_unld_no_melt_smry) # check is the same as our manually defined method

### subl vs unloading rate ----

ggplot(met_unld_no_melt_smry, 
       aes(x = subl_labs, y = q_unl_avg, colour = as.character(round(tree_labs)))) + 
  geom_point(size = 3) +
  ylab(bin_unl_ax_lab) +
  xlab('Sublimation Rate (mm/hr)') +
  theme_bw() +
  # theme_bw(base_size = 14) +
  theme(legend.position = 'bottom') +
  # ylim(NA, 3.1) +
  # xlim(NA, 3.5) +
  # scale_color_manual(values = c("#f89540", "#0072B2","#f89540", "#0072B2")) +
  labs(color = 'Mean Canopy Load (mm)')# + facet_grid(cols = vars(name))

ggplot(met_unld_no_melt_smry, 
       aes(x = wind_labs, y = q_unl_avg, colour = as.character(round(tree_labs)))) + 
  geom_point(size = 3) +
  ylab(bin_unl_ax_lab) +
  xlab('Wind Speed (m/s)') +
  theme_bw() +
  # theme_bw(base_size = 14) +
  theme(legend.position = 'bottom') +
  # ylim(NA, 3.1) +
  # xlim(NA, 3.5) +
  # scale_color_manual(values = c("#f89540", "#0072B2","#f89540", "#0072B2")) +
  labs(color = 'Mean Canopy Load (mm)')# + facet_grid(cols = vars(name))

# Look at the different models for the warm events 

ex_wind_labs <- seq(0,5,0.1)
ex_subl_labs <- seq(0, 0.5, 0.05)
ex_tree_labs <- c(2, 13)
wind_ex_df <- expand.grid(wind_labs = ex_wind_labs, subl_labs = ex_subl_labs, tree_labs = ex_tree_labs)

## PLOT MODEL ----

wind_ex_df <- expand.grid(wind_labs = ex_wind_labs, subl_labs = 0.01, tree_labs = ex_tree_labs)
wind_ex_pred <- predict(model_nls_wind_subl, newdata = wind_ex_df)

ggplot(wind_ex_df) +
  geom_line(aes(wind_labs, wind_ex_pred, colour = factor(tree_labs))) +
  # geom_errorbar(data = met_unld_all_winds_cold_smry,
  #               aes(
  #                 x = wind_labs,
  #                 ymax = sd_hi,
  #                 ymin = sd_low,
  #                 width = 0.15,
  #                 colour = as.character(round(tree_labs))
  #               )) +
  # geom_point(data = met_unld_all_winds_cold_smry,
  #            aes(wind_labs, q_unl_avg, colour = as.character(round(tree_labs))),
  #            size = 2) +
  ylab(expression("Unloading Rate (kg" ~ m^-2 ~ hr^-1 * ")")) +
  xlab(expression("Wind Speed (m"~ s^-1 * ")")) +
  # ylim(c(0, 2)) +
  # xlim(c(0,3.5)) + # removes outlier at 6 m/s
  # theme_bw() +
  scale_color_viridis_d(begin = 0, end = 0.8) +
  labs(colour = expression(atop("Canopy Snow", "Load (kg m"^-2*")"))) # avoids large space using regular way

wind_ex_df <- expand.grid(wind_labs = 0.1, subl_labs = ex_subl_labs, tree_labs = ex_tree_labs)
wind_ex_pred <- predict(model_nls_wind_subl, newdata = wind_ex_df)

ggplot(wind_ex_df) +
  geom_line(aes(subl_labs, wind_ex_pred, colour = factor(tree_labs))) +
  # geom_errorbar(data = met_unld_all_winds_cold_smry,
  #               aes(
  #                 x = wind_labs,
  #                 ymax = sd_hi,
  #                 ymin = sd_low,
  #                 width = 0.15,
  #                 colour = as.character(round(tree_labs))
  #               )) +
  # geom_point(data = met_unld_all_winds_cold_smry,
  #            aes(wind_labs, q_unl_avg, colour = as.character(round(tree_labs))),
  #            size = 2) +
  ylab(expression("Unloading Rate (kg" ~ m^-2 ~ hr^-1 * ")")) +
  xlab('Sublimation Rate (mm/hr)') +
  # ylim(c(0, 2)) +
  # xlim(c(0,3.5)) + # removes outlier at 6 m/s
  # theme_bw() +
  scale_color_viridis_d(begin = 0, end = 0.8) +
  labs(colour = expression(atop("Canopy Snow", "Load (kg m"^-2*")"))) # avoids large space using regular way

# ggsave(
#   'figs/results/modelled_wind_unloading_w_obs.png',
#   width = 6,
#   height = 4,
#   device = png
# )

met_unld_all_winds_cold_smry$pred_q_unl <- 
  predict(model_nls, met_unld_all_winds_cold_smry)

met_unld_all_winds_cold_smry |> 
  ggplot(aes(wind_labs, colour = factor(round(tree_labs)), group = factor(tree_labs))) + 
  geom_point(aes(y = q_unl_avg)) +
  geom_line(aes(y = pred_q_unl))

## ERROR TABLE ----

q_unl_temp_model_err_tbl <- met_unld_all_winds_cold_smry |> 
  ungroup() |> 
  mutate(diff = q_unl_avg - pred_q_unl) |> 
  # group_by(tree_labs) |> 
  summarise(
    `Mean Bias` = mean(diff, na.rm = T),
    # `Max Error` = diff[which.max(abs(diff))],
    MAE = mean(abs(diff), na.rm = T),
    `RMS Error` = sqrt(mean(diff ^ 2, na.rm = T))) |> 
  # left_join(coefs_df, by = c('plot_name', 'name')) |> 
  # left_join(df_r2_adj, by = c('plot_name', 'name')) |> 
  select(
    # `Mean Canopy Load (mm)` = tree_labs,
    `Mean Bias`,
    MAE,
    `RMS Error`
  ) |> 
  mutate(across(`Mean Bias`:`RMS Error`, round, digits = 3),
         R2 = rsq_nls)

saveRDS(q_unl_temp_model_err_tbl,
        'data/modelled_subl_wind_unloading_error_table.rds')
