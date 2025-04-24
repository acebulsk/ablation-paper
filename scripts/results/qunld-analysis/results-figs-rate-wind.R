# script to generate model of wind induced unloading

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

# tree_breaks <- c(0, 5,20)
tree_breaks <- c(0, 2, 6,  20) # works well for wind but breaks sublimation fn

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


## COMPUTE AVERAGES OVER BINS ---- 

met_unld_no_melt_cold <- met_unld_no_melt |> 
  filter(
    #t < -6, 
         # q_subl_veg < 0.1
         )

met_unld_no_melt_wind_smry <- met_unld_no_melt_cold |> 
  filter(is.na(tree_mm) == F) |> 
  group_by(wind_labs, tree_labs) |> 
  summarise(q_unl_avg = mean(q_unl, na.rm = T),
            q_unl_sd = sd(q_unl, na.rm = T),
            sd_low = ifelse((q_unl_avg - q_unl_sd)<0,0, q_unl_avg - q_unl_sd),
            sd_hi = q_unl_avg + q_unl_sd,
            ci_low = quantile(q_unl,0.05),
            ci_hi = quantile(q_unl, 0.95),
            sum_snow = sum(dU),
            n = n()) |> 
  filter(n >= 3,
         # wind_labs < 3, # wind transport potential above this threshold
         sum_snow > 0.1)

## PLOT BINS ----

### wind vs unloading rate ----

ggplot(met_unld_no_melt_wind_smry, 
       aes(x = wind_labs, y = q_unl_avg, colour = as.character(round(tree_labs)))) + 
  geom_point(data = met_unld_no_melt_cold, aes(u, q_unl), alpha = 0.1, colour = 'black') +
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
  # scale_color_manual(values = c("#f89540", "#0072B2","#f89540", "#0072B2")) +
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
# plot(met_unld_no_melt_wind_smry$wind_labs, met_unld_no_melt_wind_smry$log_q_unl)
met_unld_no_melt_wind_smry$log_q_unl_avg <- 
  log(met_unld_no_melt_wind_smry$q_unl_avg)

model_lm <- lm(log_q_unl_avg ~ wind_labs, data = met_unld_no_melt_wind_smry)
coefs <- coef(model_lm) |> as.numeric()
summary(model_lm)
a_lm <- exp(coefs[1])
b_lm <- coefs[2]

### Fit a non linear least squares model ----

# use starting values from the linear model 
# per second for model
# wind labs in here twice sets to 0 when wind is 0, dont need this for other params
model_nls <- nls(q_unl_avg/(60*60) ~ wind_labs * a * tree_labs * exp(b * wind_labs), 
                 data = met_unld_no_melt_wind_smry, 
                 start = list(a = a_lm, b = b_lm))
summary(model_nls)
nls_coefs <- coef(model_nls)
saveRDS(nls_coefs, 'data/model_coef_wind_unld_per_second.rds')
nls_smry <- summary(model_nls)
coefs_df <- as.data.frame(coef(nls_smry))
coefs_df <- coefs_df |> 
  rownames_to_column(var = "term") |> 
  select(term, Estimate, p_value = `Pr(>|t|)`) |> 
  pivot_wider(names_from = term, values_from = c(Estimate, p_value), names_glue = "{term}_{.value}") |> 
  mutate(across(contains("Estimate"), ~ formatC(.x, format = "e", digits = 2))) |> 
  mutate(across(contains("p_value"), ~ ifelse(.x < 0.05, 'p < 0.05', 'n.s.')))

met_unld_no_melt_wind_smry <- met_unld_no_melt_wind_smry |> 
  mutate(
    model_nls_pred = wind_labs * nls_coefs[[1]] * tree_labs * exp(nls_coefs[[2]]* wind_labs) # Linear model prediction
  )
# Create ggplot
ggplot(met_unld_no_melt_wind_smry, aes(x = wind_labs, y = model_nls_pred)) +
  # Observed vs NLS model
  geom_point(aes(color = "Observed vs NLS"), size = 2) 
# summary(model_nls)

# per hour for plotting
model_nls <- nls(q_unl_avg ~ wind_labs * a * tree_labs * exp(b * wind_labs), 
                 data = met_unld_no_melt_wind_smry, 
                 start = list(a = a_lm, b = b_lm))
summary(model_nls)
RSS.p <- sum(residuals(model_nls)^2)  # Residual sum of squares
TSS <- sum((met_unld_no_melt_wind_smry$q_unl_avg - mean(met_unld_no_melt_wind_smry$q_unl_avg))^2)  # Total sum of squares
rsq_nls <- 1 - (RSS.p/TSS) |> round(2)  # R-squared measure

modelr::rsquare(model_nls, met_unld_no_melt_wind_smry) # check is the same as our manually defined method

### Fit a non linear least squares model on the RAW data ----
# model_nls <- nls(q_unl ~ u * a * tree_mm * exp(b * u), 
#                  data = met_unld_no_melt_cold, 
#                  start = list(a = a_lm, b = b_lm))
# summary(model_nls)
# nls_coefs <- coef(model_nls)
# 
# RSS.p <- sum(residuals(model_nls)^2)  # Residual sum of squares
# TSS <- sum((met_unld_no_melt_cold$q_unl - mean(met_unld_no_melt_cold$q_unl))^2)  # Total sum of squares
# rsq_nls <- 1 - (RSS.p/TSS) |> round(2)  # R-squared measure
# rsq_nls
# modelr::rsquare(model_nls, met_unld_no_melt_cold) # check is the same as our manually defined method
# 
# met_unld_no_melt_cold <- met_unld_no_melt_cold |> 
#   mutate(
#     model_nls_pred = u * nls_coefs[[1]] * tree_mm * exp(nls_coefs[[2]]* u) # Linear model prediction
#   )
# # Create ggplot
# ggplot(met_unld_no_melt_cold, aes(x = u, y = model_nls_pred, colour = tree_mm)) +
#   # Observed vs NLS model
#   geom_point() 
# # summary(model_nls)
# 
# 
# lm <- data.frame(
#   mod_name = 'lm',
#   preds = exp(predict(model_lm)),
#   resids = residuals(model_lm, type = "pearson")
# )
# 
# nls <- data.frame(
#   mod_name = 'nls',
#   preds = predict(model_nls),
#   resids = residuals(model_nls, type = "pearson")
# )
# 
# # nlswi <- data.frame(
# #   mod_name = 'nlswi',
# #   preds = predict(model_nlswi),
# #   resids = residuals(model_nlswi, type = "pearson")
# # )
# 
# resids_df <- rbind(
#   lm,
#   nls
#   # nlswi
# )
# 
# # Look at the Q-Q plot and residuals for the warm events 
# 
# ggplot(resids_df, aes(sample = preds, colour = mod_name)) +
#   stat_qq() +
#   stat_qq_line()
# 
# ggplot(resids_df, aes(x = preds, y = resids, colour = mod_name)) + 
#   geom_point() +
#   geom_hline(yintercept = 0)


# Look at the different models for the warm events 
ex_wind_labs <- seq(0,6,0.1)
ex_tree_labs <- c(1, 4, 13)
wind_ex_df <- expand.grid(wind_labs = ex_wind_labs, tree_labs = ex_tree_labs)
wind_ex_df$new_predicted_y <- predict(model_nls, newdata = wind_ex_df)

## PLOT MODEL ----

ggplot(wind_ex_df |> filter(!(tree_labs == 13 & wind_labs > 3.5),
                            !(tree_labs == 4 & wind_labs > 5)
                            )) +
  geom_line(aes(wind_labs, new_predicted_y, colour = factor(tree_labs))) +
  geom_errorbar(data = met_unld_no_melt_wind_smry,
                aes(
                  x = wind_labs,
                  ymax = sd_hi,
                  ymin = sd_low,
                  width = 0.05,
                  colour = as.character(round(tree_labs))
                )) +
  geom_point(data = met_unld_no_melt_wind_smry,
             aes(wind_labs, q_unl_avg, colour = as.character(round(tree_labs))),
             size = 2) +
  ylab('Unloading Rate (mm/hr)') +
  xlab('Wind Speed (m/s)') +
  # ylab(expression("Unloading Rate (kg" ~ m^-2 ~ hr^-1 * ")")) +
  # xlab(expression("Wind Speed (m"~ s^-1 * ")")) +
  # ylim(c(0, 1.25)) +
  # xlim(c(0,2.5)) + # removes outlier at 6 m/s
  # theme_bw() +
  scale_color_viridis_d(begin = 0, end = 0.8) +
  labs(colour = 'Canopy Snow\nLoad (mm)') # avoids large space using regular way

ggsave(
  'figs/results/modelled_wind_unloading_w_obs.png',
  width = 6,
  height = 4,
  device = png
)

met_unld_no_melt_wind_smry$pred_q_unl <- 
  predict(model_nls, met_unld_no_melt_wind_smry)

met_unld_no_melt_wind_smry |> 
  ggplot(aes(wind_labs, colour = factor(round(tree_labs)), group = factor(tree_labs))) + 
  geom_point(aes(y = q_unl_avg)) +
  geom_line(aes(y = pred_q_unl))

## ERROR TABLE ----

q_unl_temp_model_err_tbl <- met_unld_no_melt_wind_smry |> 
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

# Performance metrics reshaped to long format (convert values to character)
perf_tbl <- q_unl_temp_model_err_tbl |> 
  select(`Mean Bias (mm/hr)` = `Mean Bias`, `Mean Absolute Error (mm/hr)` = MAE, `Root Mean Square Error (mm/hr)` = `RMS Error`, `Coefficient of Determination ($R^2$)` = R2) |> 
  pivot_longer(everything(), names_to = "Metric", values_to = "Value") |> 
  mutate(Value = as.character(Value))

# Coefficient table in long format
coef_tbl <- tibble(
  Metric = c("Coefficient a", "Significance of a", "Coefficient b", "Significance of b"),
  Value = c(
    coefs_df$a_Estimate,
    coefs_df$a_p_value,
    coefs_df$b_Estimate,
    coefs_df$b_p_value
  )
)

# Combine into final long format table
long_tbl <- bind_rows(perf_tbl, coef_tbl)

saveRDS(long_tbl,
        'data/modelled_wind_unloading_error_table.rds')
