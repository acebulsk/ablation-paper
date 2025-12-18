# script to generate model of wind induced unloading based on shear stress (subl)
# although wind appears a better predictor of unloading considering all ablation
# periods (incl. melt) subl is a better predicter of unloading for the non-melt
# periods

# linear and non linear models below are quite similar linear with no intercept
# is (R2 = 0.6) exponential is (R2 = 0.66)

# for heavier loads, the relationship looks more exponential (visually) and the
# R2 supports this with no intercept adjusted R2 of 0.67 for the linear model
# and 0.74 for the exponential model

## COMPUTE AVERAGES OVER BINS ---- 

met_unld_no_melt_cold <- met_unld_no_melt |> 
  filter(
    #t < -6, 
  # q_subl_veg < 0.3, # most canopy snow starts partitioning into sublimation after this threshold
  )

met_unld_no_melt_subl_smry <- met_unld_no_melt_cold |> 
  filter(is.na(tree_mm) == F) |> 
  group_by(subl_labs, tree_labs) |> 
  summarise(q_unl_avg = mean(q_unl, na.rm = T),
            q_unl_sd = sd(q_unl, na.rm = T),
            sd_low = ifelse((q_unl_avg - q_unl_sd)<0,0, q_unl_avg - q_unl_sd),
            sd_hi = q_unl_avg + q_unl_sd,
            ci_low = quantile(q_unl,0.05),
            ci_hi = quantile(q_unl, 0.95),
            sum_snow = sum(dU),
            n = n()) |> 
  filter(n >= 3,
         # tree_labs > 1,
         # subl_labs < 3, # subl transport potential above this threshold
         sum_snow > 0.1) |> 
  ungroup()

         

## PLOT BINS ----

### subl vs unloading rate ----

ggplot(met_unld_no_melt_subl_smry, 
       aes(x = subl_labs, y = q_unl_avg, colour = as.character(round(tree_labs)))) + 
  # geom_point(data = met_unld_no_melt_cold, aes(u, q_unl), alpha = 0.1, colour = 'black') +
  # geom_errorbar(aes(
  #   x = subl_labs, 
  #   ymax = sd_hi,
  #   ymin = sd_low
  # ), width = 0.2)  +
  geom_point(size = 3) +
  # ylab(bin_unl_ax_lab) +
  # xlab('Shear Stress Bins (N/m^2)') +
  theme_bw() +
  # theme_bw(base_size = 14) +
  theme(legend.position = 'bottom') +
  # ylim(NA, 3.1) +
  # xlim(NA, 3.5) +
  # scale_color_manual(values = c("#f89540", "#0072B2","#f89540", "#0072B2")) +
  labs(color = 'Mean Canopy Load (mm)')# + facet_grid(cols = vars(name))

ggsave(
  'figs/results/binned_unloading_rate_and_subl_mid_class_tree_load.png',
  device = png,
  width = 4,
  height = 4,
  units = "in"
)

## MODEL ----

### fit a linear model ----

# to recreate the interaction its just (coef * tree_labs * subl_labs)
model_lm <- lm(q_unl_avg ~ tree_labs:subl_labs - 1, data = met_unld_no_melt_subl_smry)

summary(model_lm)
coefs_df <- broom::tidy(model_lm)  # Using broom to extract coefficients nicely
coefs_df <- coefs_df |> 
  # rownames_to_column(var = "term") |> 
  select(term, Estimate = estimate, p_value = p.value) |> 
  pivot_wider(names_from = term, values_from = c(Estimate, p_value), names_glue = "{term}_{.value}") |> 
  mutate(across(contains("Estimate"), ~ formatC(.x, format = "e", digits = 2))) |> 
  mutate(across(contains("p_value"), ~ ifelse(.x < 0.05, 'p < 0.05', 'n.s.')))

# check adjusted R square, since we forced through the origin

# source('../../../Documents/code/stats/lm-through-the-origin/example-r2-from-lm-through-the-origin.R')

model_lm_rsq_adj <- compute_r2(model_lm)
d_lm <- hydroGOF::dr(fitted(model_lm), met_unld_no_melt_subl_smry$q_unl_avg) |> round(2)

aic <- AIC(model_lm) |> round(2) # cannot use AIC bc different n of binned response

# r_squared_no_intercept_bad(model_lm)

### fit linear model on persecond unloading for CRHM ----
model_lm_ps <- lm(q_unl_avg/(60*60) ~ tree_labs:subl_labs - 1, data = met_unld_no_melt_subl_smry)
coefs_ps <- coef(model_lm_ps) |> as.numeric()

### test assumptions
subl_lm_checks <- check_lm_assumptions(model_lm)
car::ncvTest(model_lm)
coeftest(model_lm, vcov = vcovHC(model_lm, type = "HC1")) # checks model significance considering homoscedasticity is violated
plot(model_lm, which = 1)

plot(fitted(model_lm), resid(model_lm))

### Fit a non linear least squares model ----

# use starting values from the linear model 
# per second for model
# subl labs in here twice sets to 0 when subl is 0, dont need this for other params
# model_nls <- nls(q_unl_avg/(60*60) ~ subl_labs * a * tree_labs * exp(b * subl_labs), 
#                  data = met_unld_no_melt_subl_smry, 
#                  start = list(a = 1, b = 1))
# summary(model_nls)
# nls_coefs <- coef(model_nls)
# saveRDS(nls_coefs, 'data/model_coef_subl_unld_per_second.rds')
# nls_smry <- summary(model_nls)
# coefs_df <- as.data.frame(coef(nls_smry))
# coefs_df <- coefs_df |> 
#   rownames_to_column(var = "term") |> 
#   select(term, Estimate, p_value = `Pr(>|t|)`) |> 
#   pivot_wider(names_from = term, values_from = c(Estimate, p_value), names_glue = "{term}_{.value}") |> 
#   mutate(across(contains("Estimate"), ~ formatC(.x, format = "e", digits = 2))) |> 
#   mutate(across(contains("p_value"), ~ ifelse(.x < 0.05, 'p < 0.05', 'n.s.')))
# 
# met_unld_no_melt_subl_smry <- met_unld_no_melt_subl_smry |> 
#   mutate(
#     model_nls_pred = subl_labs * nls_coefs[[1]] * tree_labs * exp(nls_coefs[[2]]* subl_labs) # Linear model prediction
#   )
# # Create ggplot
# ggplot(met_unld_no_melt_subl_smry, aes(x = subl_labs, y = model_nls_pred)) +
#   # Observed vs NLS model
#   geom_point(aes(color = "Observed vs NLS"), size = 2) 
# # summary(model_nls)
# 
# # per hour for plotting
# model_nls <- nls(q_unl_avg ~ subl_labs * a * tree_labs * exp(b * subl_labs), 
#                  data = met_unld_no_melt_subl_smry, 
#                  start = list(a = 1, b = 1))
# summary(model_nls)
# RSS.p <- sum(residuals(model_nls)^2)  # Residual sum of squares
# TSS <- sum((met_unld_no_melt_subl_smry$q_unl_avg - mean(met_unld_no_melt_subl_smry$q_unl_avg))^2)  # Total sum of squares
# rsq_nls <- 1 - (RSS.p/TSS) |> round(2)  # R-squared measure
# rsq_nls
# modelr::rsquare(model_nls, met_unld_no_melt_subl_smry) # check is the same as our manually defined method

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
ex_subl_labs <- seq(-0.1,1,0.001) |> round(3)
ex_tree_labs <- c(1, 4, 13)
subl_ex_df <- expand.grid(subl_labs = ex_subl_labs, tree_labs = ex_tree_labs)
# subl_ex_df$new_predicted_y_nls <- predict(model_nls, newdata = subl_ex_df)
subl_ex_df$new_predicted_y <- predict(model_lm, newdata = subl_ex_df)

stopifnot(all(met_unld_no_melt_subl_smry$subl_labs %in% ex_subl_labs))

## PLOT MODEL ----
subl_plot_df <- subl_ex_df |> left_join(met_unld_no_melt_subl_smry) |>
  filter(
    !(tree_labs == 13 & subl_labs > 0.25),
    !(tree_labs == 4 & subl_labs > 0.5),
    !(tree_labs == 1 & subl_labs > 0.75)) |> 
  pivot_longer(subl_labs, names_to = 'x_var_name', values_to = 'x_var_value')

ggplot(subl_plot_df, aes(x=x_var_value)) +
  # geom_line(aes(subl_labs, new_predicted_y_nls, colour = factor(tree_labs)), linetype = 'dashed') +
  geom_line(aes(y = new_predicted_y, colour = factor(tree_labs))) +
  geom_errorbar(
                aes(
                  ymax = sd_hi,
                  ymin = sd_low,
                  width = 0.05,
                  colour = as.character(round(tree_labs))
                )) +
  geom_point(aes(y = q_unl_avg, colour = as.character(round(tree_labs))),
             size = 2) +
  ylab(expression("Unloading Rate (mm"~ hr^-1 * ")")) +
  xlab(expression("Shear Stress (N"~ m^-2 * ")")) +
  scale_color_viridis_d(begin = 0, end = 0.8) +
  labs(colour = 'Canopy Snow\nLoad (mm)') # avoids large space using regular way

# ggsave(
#   'figure5.png',
#   width = 6,
#   height = 4,
#   device = png
# )

ggsave(
  'figs/results/modelled_subl_unloading_w_obs.png',
  width = 6,
  height = 4,
  device = png
)

# OLS with heteroscedasticity -----

## compute model error ----

met_unld_no_melt_subl_smry$pred_q_unl <- 
  predict(model_lm, met_unld_no_melt_subl_smry)

met_unld_no_melt_subl_smry |> 
  ggplot(aes(subl_labs, colour = factor(round(tree_labs)), group = factor(tree_labs))) + 
  geom_point(aes(y = q_unl_avg)) +
  geom_line(aes(y = pred_q_unl))

## ERROR TABLE ----

q_unl_temp_model_err_tbl <- met_unld_no_melt_subl_smry |> 
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
  mutate(
    across(`Mean Bias`:`RMS Error`, round, digits = 3),
    R2 = model_lm_rsq_adj |> round(2),
    # AIC = aic,
    d = d_lm) 

# Performance metrics reshaped to long format (convert values to character)
perf_tbl <- q_unl_temp_model_err_tbl |> 
  select(
    `Mean Bias (mm/hr)` = `Mean Bias`,
    `Mean Absolute Error (mm/hr)` = MAE,
    `Root Mean Square Error (mm/hr)` = `RMS Error`,
    # `Akaike Information Criterion` = AIC,
    `Coefficient of Determination` = R2,
    `Coefficient of Agreement` = d
  ) |> 
  pivot_longer(everything(), names_to = "Metric", values_to = "Value") |> 
  mutate(Value = as.character(Value))

# Coefficient table in long format
coef_tbl <- tibble(
  Metric = c("Coefficient a", "Significance of a", "Coefficient b", "Significance of b"),
  Value = c(
    coefs_df$`tree_labs:subl_labs_Estimate`,
    coefs_df$`tree_labs:subl_labs_p_value`,
    'NA',
    'NA'
  )
)

# Combine into final long format table
man_corr_test <- tibble(Metric = "Linear/Non-linear Correlation", Value = "NA")
model_type <- tibble(Metric = 'Model', Value = 'OLS')
eqn <- tibble(
  Metric = 'Equation',
  Value  = "$q_{unld}^{dry} = L \\cdot q_{subl}^{veg}  \\cdot a$"
)

long_tbl <- bind_rows(model_type, eqn) |>
  bind_rows(perf_tbl) |> 
  bind_rows(coef_tbl) |>
  bind_rows(subl_lm_checks$table |> filter(Metric != 'Independence')) |>
  rbind(man_corr_test)
saveRDS(long_tbl,
        'data/results/modelled_subl_unloading_error_table_lm.rds')
