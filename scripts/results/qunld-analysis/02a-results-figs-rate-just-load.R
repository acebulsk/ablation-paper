# script to generate model of dry snow unloading based on load alone

## COMPUTE AVERAGES OVER BINS ---- 

met_unld_no_melt_cold <- met_unld_no_melt |> 
  filter(
    #t < -6, 
  # q_subl_veg < 0.3, # most canopy snow starts partitioning into sublimation after this threshold
  )

met_unld_no_melt_load_fine_smry <- met_unld_no_melt_cold |> 
  filter(is.na(tree_mm) == F) |> 
  group_by(tree_labs_fine) |> 
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
         # tree_labs_fine < 3, # tau transport potential above this threshold
         sum_snow > 0.1) |> 
  ungroup()

         

## PLOT BINS ----

### tau vs unloading rate ----

ggplot(met_unld_no_melt_load_fine_smry, 
       aes(x = tree_labs_fine, y = q_unl_avg)) + 
  # geom_point(data = met_unld_no_melt_cold, aes(u, q_unl), alpha = 0.1, colour = 'black') +
  # geom_errorbar(aes(
  #   x = tree_labs_fine, 
  #   ymax = sd_hi,
  #   ymin = sd_low
  # ), width = 0.2)  +
  geom_point(size = 3) +
  # ylab(bin_unl_ax_lab) +
  # xlab('Shear Stress Bins (N/m^2)') +
  theme_bw() 

ggsave(
  'figs/results/binned_unloading_rate_and_just_load.png',
  device = png,
  width = 4,
  height = 4,
  units = "in"
)

## MODEL ----

### fit a linear model ----

# to recreate the interaction its just (coef * tree_labs * tree_labs_fine)
model_lm <- lm(q_unl_avg ~ tree_labs_fine - 1, data = met_unld_no_melt_load_fine_smry)

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
d_lm <- hydroGOF::dr(fitted(model_lm), met_unld_no_melt_load_fine_smry$q_unl_avg) |> round(2)

aic <- AIC(model_lm) |> round(2) # cannot use AIC bc different n of binned response

### test assumptions
tau_lm_checks <- check_lm_assumptions(model_lm)

# Handle heteroscadeticity first using weighted OLS .. this is not as good as the GLS

# Step 2: estimate residual variance
m_var <- lm(abs(resid(model_lm)) ~ fitted(model_lm))
sigma_hat <- fitted(m_var)           # predicted residual magnitude
w <- 1 / sigma_hat^2                  # weights = inverse of variance

# Step 3: weighted regression
model_wls <- lm(q_unl_avg ~ tree_labs_fine - 1,
                data = met_unld_no_melt_load_fine_smry,
                weights = w)

summary(model_wls)

# check hetero
resid_gls <- resid(model_wls)

# Fitted values
fitted_gls <- fitted(model_wls)

plot(fitted_gls, resid_gls,
     xlab = "Fitted values",
     ylab = "Normalized residuals",
     main = "GLS: Residuals vs Fitted")
abline(h = 0, lty = 2)

library(nlme)

# varPower: Var(e) ∝ |fitted|^(2*delta)  (common for mean-dependent variance)
gls_power <- gls(q_unl_avg ~ tree_labs_fine - 1,
                 data = met_unld_no_melt_load_fine_smry,
                 method = "REML",
                 weights = varPower(form = ~ fitted(.)))

# varExp: exponential relationship Var(e) ∝ exp(2*delta*fitted)
gls_exp <- gls(q_unl_avg ~ tree_labs_fine - 1,
               data = met_unld_no_melt_load_fine_smry,
               method = "REML",
               weights = varExp(form = ~ fitted(.)))

# varIdent: different variances for groups (if variance differs by tree_labs_fine bin)
gls_ident <- gls(q_unl_avg ~ tree_labs_fine - 1,
                 data = met_unld_no_melt_load_fine_smry,
                 method = "REML",
                 weights = varIdent(form = ~1 | tree_labs_fine))

AIC(model_lm, gls_power, gls_exp, gls_ident)     # lower AIC from gls power ... 
anova(gls_power, gls_exp)                 # compare nested models if appropriate

summary(gls_exp)
intervals(gls_exp)   # CIs for coefficients and variance parameters

tau_gls_checks <- check_gls_assumptions(gls_exp)

# Normalized residuals (account for variance structure)
resid_gls <- resid(gls_exp)

# Fitted values
fitted_gls <- fitted(gls_exp)

plot(fitted_gls, resid_gls,
     xlab = "Fitted values",
     ylab = "Normalized residuals",
     main = "GLS: Residuals vs Fitted")
abline(h = 0, lty = 2)

hist(resid_gls, breaks = 15, main = "GLS normalized residuals", xlab = "Residuals")
qqnorm(resid_gls)
qqline(resid_gls, col = "red")
shapiro.test(resid_gls)

coefs <- coef(gls_exp)

# Create tibble
coefs_df_glm <- tibble(
  `tree_labs:tau_labs_Estimate` = formatC(coefs, format = "e", digits = 2),
  `tree_labs:tau_labs_p_value` = 'NA for GLS'
)

model_gls_rsq_adj <- compute_r2(gls_exp)
d_gls <- hydroGOF::dr(fitted(gls_exp), met_unld_no_melt_load_fine_smry$q_unl_avg) |> round(2)


### Fit a non linear least squares model ----

# use starting values from the linear model 
# per second for model
# tau labs in here twice sets to 0 when tau is 0, dont need this for other params
# model_nls <- nls(q_unl_avg/(60*60) ~ tree_labs_fine * a * tree_labs * exp(b * tree_labs_fine), 
#                  data = met_unld_no_melt_load_fine_smry, 
#                  start = list(a = 1, b = 1))
# summary(model_nls)
# nls_coefs <- coef(model_nls)
# saveRDS(nls_coefs, 'data/model_coef_tau_unld_per_second.rds')
# nls_smry <- summary(model_nls)
# coefs_df <- as.data.frame(coef(nls_smry))
# coefs_df <- coefs_df |> 
#   rownames_to_column(var = "term") |> 
#   select(term, Estimate, p_value = `Pr(>|t|)`) |> 
#   pivot_wider(names_from = term, values_from = c(Estimate, p_value), names_glue = "{term}_{.value}") |> 
#   mutate(across(contains("Estimate"), ~ formatC(.x, format = "e", digits = 2))) |> 
#   mutate(across(contains("p_value"), ~ ifelse(.x < 0.05, 'p < 0.05', 'n.s.')))
# 
# met_unld_no_melt_load_fine_smry <- met_unld_no_melt_load_fine_smry |> 
#   mutate(
#     model_nls_pred = tree_labs_fine * nls_coefs[[1]] * tree_labs * exp(nls_coefs[[2]]* tree_labs_fine) # Linear model prediction
#   )
# # Create ggplot
# ggplot(met_unld_no_melt_load_fine_smry, aes(x = tree_labs_fine, y = model_nls_pred)) +
#   # Observed vs NLS model
#   geom_point(aes(color = "Observed vs NLS"), size = 2) 
# # summary(model_nls)
# 
# # per hour for plotting
# model_nls <- nls(q_unl_avg ~ tree_labs_fine * a * tree_labs * exp(b * tree_labs_fine), 
#                  data = met_unld_no_melt_load_fine_smry, 
#                  start = list(a = 1, b = 1))
# summary(model_nls)
# RSS.p <- sum(residuals(model_nls)^2)  # Residual sum of squares
# TSS <- sum((met_unld_no_melt_load_fine_smry$q_unl_avg - mean(met_unld_no_melt_load_fine_smry$q_unl_avg))^2)  # Total sum of squares
# rsq_nls <- 1 - (RSS.p/TSS) |> round(2)  # R-squared measure
# rsq_nls
# modelr::rsquare(model_nls, met_unld_no_melt_load_fine_smry) # check is the same as our manually defined method

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
ex_df <- data.frame(tree_labs_fine = seq(0.25, 19.75, 0.25))
ex_df$new_predicted_y <- predict(gls_exp, newdata = ex_df)

stopifnot(all(met_unld_no_melt_load_fine_smry$tree_labs_fine %in% ex_df$tree_labs_fine))

## PLOT MODEL ----
plot_df <- ex_df |> left_join(met_unld_no_melt_load_fine_smry) |>
  pivot_longer(tree_labs_fine, names_to = 'x_var_name', values_to = 'x_var_value')

ggplot(plot_df, aes(x=x_var_value)) +
  # geom_line(aes(tree_labs_fine, new_predicted_y_nls, colour = factor(tree_labs)), linetype = 'dashed') +
  geom_line(aes(y = new_predicted_y)) +
  geom_errorbar(
                aes(
                  ymax = sd_hi,
                  ymin = sd_low,
                  width = 0.05
                )) +
  geom_point(aes(y = q_unl_avg),
             size = 2) +
  ylab(expression("Unloading Rate (mm"~ hr^-1 * ")")) +
  xlab('Canopy Snow Load (mm)') +
  scale_color_viridis_d(begin = 0, end = 0.8) 


ggsave(
  'figs/results/modelled_snow_load_unloading_w_obs.png',
  width = 6,
  height = 4,
  device = png
)

# OLS with heteroscedasticity -----

## compute model error ----

met_unld_no_melt_load_fine_smry$pred_q_unl <- 
  predict(model_lm, met_unld_no_melt_load_fine_smry)

met_unld_no_melt_load_fine_smry |> 
  ggplot(aes(tree_labs_fine)) + 
  geom_point(aes(y = q_unl_avg)) +
  geom_line(aes(y = pred_q_unl))

## ERROR TABLE ----

q_unl_temp_model_err_tbl <- met_unld_no_melt_load_fine_smry |> 
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
    coefs_df$`tree_labs_fine_Estimate`,
    coefs_df$tree_labs_fine_p_value,
    'NA',
    'NA'
  )
)

# Combine into final long format table
man_corr_test <- tibble(Metric = "Linear/Non-linear Correlation", Value = "NA")
model_type <- tibble(Metric = 'Model', Value = 'OLS')
eqn <- tibble(
  Metric = 'Equation',
  Value  = "$q_{unld}^{dry} = L \\cdot a$"
)
long_tbl <- bind_rows(model_type, eqn) |> bind_rows(perf_tbl) |> bind_rows(coef_tbl) |> bind_rows(tau_lm_checks$table) |> rbind(man_corr_test)

saveRDS(long_tbl,
        'data/results/modelled_load_unloading_error_table_hetero.rds')

# GLS with variance modelled  -----

## compute model error ----

met_unld_no_melt_load_fine_smry$pred_q_unl <- 
  predict(gls_exp, met_unld_no_melt_load_fine_smry)

met_unld_no_melt_load_fine_smry |> 
  ggplot(aes(tree_labs_fine)) + 
  geom_point(aes(y = q_unl_avg)) +
  geom_line(aes(y = pred_q_unl))

## ERROR TABLE ----

q_unl_temp_model_err_tbl <- met_unld_no_melt_load_fine_smry |> 
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
    R2 = model_gls_rsq_adj |> round(2),
    # AIC = aic,
    d = d_gls) 

# Performance metrics reshaped to long format (convert values to character)
perf_tbl <- 
  q_unl_temp_model_err_tbl |> 
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
    coefs_df_glm$`tree_labs:tau_labs_Estimate`,
    coefs_df_glm$`tree_labs:tau_labs_p_value`,
    'NA',
    'NA'
  )
)

# Combine into final long format table
man_corr_test <- tibble(Metric = "Linear/Non-linear Correlation", Value = "NA")
model_type <- tibble(Metric = 'Model', Value = 'GLS')
eqn <- tibble(
  Metric = 'Equation',
  Value  = "$q_{unld}^{dry} = L \\cdot a$"
)

long_tbl <- bind_rows(model_type, eqn) |>
  bind_rows(perf_tbl) |> 
  bind_rows(coef_tbl) |>
  bind_rows(tau_gls_checks$table |> filter(Metric != 'Independence')) |>
  rbind(man_corr_test)

saveRDS(long_tbl,
        'data/results/modelled_load_unloading_error_table_gls.rds')
