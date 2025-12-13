# script to generate model of wind induced unloading based on shear stress (tau)
# although wind appears a better predictor of unloading considering all ablation
# periods (incl. melt) tau is a better predicter of unloading for the non-melt
# periods

# linear and non linear models below are quite similar linear with no intercept
# is (R2 = 0.6) exponential is (R2 = 0.66)

# for heavier loads, the relationship looks more exponential (visually) and the
# R2 supports this with no intercept adjusted R2 of 0.67 for the linear model
# and 0.74 for the exponential model

met_unld_no_melt_cold <- met_unld_no_melt |> 
  filter(
    #t < -6, 
    # q_subl_veg < 0.1
  ) |> 
  mutate(
    ti_dep = t - t_ice_bulb
  )

## Check correlations 

ggplot(met_unld_no_melt_cold, aes(tau, ti_dep)) + # looks non-linear!
  geom_point()

ggplot(met_unld_no_melt_cold, aes(tau_labs, ti_dep_labs)) + 
  geom_point()

# check collinerarity 

predictors <- met_unld_no_melt_cold[, c("tree_mm", "tau", "ti_dep")] # low linear correlation between tau and temp but is non-linear relationship...

cor_mat <- cor(predictors, use = "complete.obs")
cor_mat

corrplot(cor_mat, method = "color", tl.cex = 0.8, number.cex = 0.7)

lm_col <- lm(q_unl ~ tree_mm + tau + ti_dep, data = met_unld_no_melt_cold)
vif(lm_col)

# test nonlinear correlation
x <- met_unld_no_melt_cold$tau
y <- met_unld_no_melt_cold$ti_dep
cor(x, y, method = "spearman") # 0.2 non linear correlation 

# The MIC (Maximal Information Coefficient) measures any type of dependence—linear or nonlinear—on a scale of:

# 0 → no detectable relationship

# 1 → perfectly predictable relationship

# 0.3–0.5 → moderate dependence

# >0.5 → strong dependence
mine(x, y)$MIC # moderate correlation between temp and tau

warning("Correlation between shear stress and temperature is pretty low overall at linear and non linear (0.2-0.34). MIC is close to 0.3 which flags moderate correlation and ice bulb temp is a significant predictor in the linear regression below but fails 3/4 assumptions on linear regression and has low R2 so not going to include.")

## COMPUTE AVERAGES OVER BINS ---- 

met_unld_no_melt_tau_smry <- met_unld_no_melt_cold |> 
  filter(is.na(tree_mm) == F) |> 
  group_by(tau_labs, tree_labs, ti_dep_labs) |> 
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
         # tau_labs < 3, # tau transport potential above this threshold
         sum_snow > 0.1)

## PLOT BINS ----

### tau vs unloading rate ----

ggplot(met_unld_no_melt_tau_smry, 
       aes(x = tau_labs, y = q_unl_avg, colour = as.character(round(tree_labs)))) + 
  # geom_point(data = met_unld_no_melt_cold, aes(u, q_unl), alpha = 0.1, colour = 'black') +
  # geom_errorbar(aes(
  #   x = tau_labs, 
  #   ymax = sd_hi,
  #   ymin = sd_low
  # ), width = 0.2)  +
  geom_point(size = 3) +
  # ylab(bin_unl_ax_lab) +
  xlab('Shear Stress Bins (N/m^2)') +
  theme_bw() +
  # theme_bw(base_size = 14) +
  theme(legend.position = 'bottom') +
  # ylim(NA, 3.1) +
  # xlim(NA, 3.5) +
  # scale_color_manual(values = c("#f89540", "#0072B2","#f89540", "#0072B2")) +
  labs(color = 'Mean Canopy Load (mm)')# + facet_grid(cols = vars(name))

# ggsave(
#   'figs/results/binned_unloading_rate_and_tau_mid_class_tree_load.png',
#   device = png,
#   width = 4,
#   height = 4,
#   units = "in"
# )

## MODEL ----

### fit a linear model ----

# to recreate the interaction its just (coef * tree_labs * tau_labs)
model_lm <- lm(q_unl_avg ~ tree_labs:tau_labs:ti_dep_labs - 1, data = met_unld_no_melt_tau_smry)
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
model_lm_rsq_adj
d_lm <- hydroGOF::dr(fitted(model_lm), met_unld_no_melt_tau_smry$q_unl_avg) |> round(2)

# aic <- AIC(model_lm) |> round(2) # cannot use AIC bc different n of binned response

# compare to no air temp on some binning structure
model_lm_no_ibd <- update(model_lm, . ~ . - ti_dep_labs) # temp is insignificant 

AIC(model_lm, model_lm_no_ibd)
anova(model_lm, model_lm_no_ibd, test = "LRT") 

### test assumptions
tau_lm_checks <- check_lm_assumptions(model_lm)

car::ncvTest(model_lm)
coeftest(model_lm, vcov = vcovHC(model_lm, type = "HC1")) # checks model significance considering homoscedasticity is violated
plot(model_lm, which = 1)

# Handle heteroscadeticity first using weighted OLS .. this is not as good as the GLS

# Step 2: estimate residual variance
m_var <- lm(abs(resid(model_lm)) ~ fitted(model_lm))
sigma_hat <- fitted(m_var)           # predicted residual magnitude
w <- 1 / sigma_hat^2                  # weights = inverse of variance

# Step 3: weighted regression
model_wls <- lm(q_unl_avg ~ tree_labs:tau_labs + ti_dep_labs - 1,
                data = met_unld_no_melt_tau_smry,
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
gls_power <- gls(q_unl_avg ~ tree_labs:tau_labs:ti_dep_labs - 1,
                 data = met_unld_no_melt_tau_smry,
                 method = "REML",
                 weights = varPower(form = ~ fitted(.)))

# varExp: exponential relationship Var(e) ∝ exp(2*delta*fitted)
gls_exp <- gls(q_unl_avg ~ tree_labs:tau_labs:ti_dep_labs - 1,
               data = met_unld_no_melt_tau_smry,
               method = "REML",
               weights = varExp(form = ~ fitted(.)))

# varIdent: different variances for groups (if variance differs by tau_labs bin)
gls_ident <- gls(q_unl_avg ~ tree_labs:tau_labs:ti_dep_labs - 1,
                 data = met_unld_no_melt_tau_smry,
                 method = "REML",
                 weights = varIdent(form = ~1 | tree_labs))

# create combined group factor
met_unld_no_melt_tau_smry$group <- interaction(met_unld_no_melt_tau_smry$tree_labs,
                                               met_unld_no_melt_tau_smry$tau_labs,
                                               drop = TRUE)
gls_group <- gls(q_unl_avg ~ tree_labs:tau_labs:ti_dep_labs - 1,
                 data = met_unld_no_melt_tau_smry,
                 weights = varIdent(form = ~1 | group),
                 method = "REML")
resid_gls <- resid(gls_group, type = "normalized")
fitted_gls <- fitted(gls_group)

AIC(model_lm, gls_power, gls_exp, gls_group)     # lower AIC from gls power ... 
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
model_gls_rsq_adj <- compute_r2(gls_exp)
d_gls <- hydroGOF::dr(fitted(gls_exp), met_unld_no_melt_tau_smry$q_unl_avg) |> round(2)

# Look at the different models for the warm events 
ex_tau_labs <- seq(0,1,0.001) |> round(3)
ex_tree_labs <- c(1, 4, 13)
ex_ibd_labs <- seq(-0.25, 5, 0.5)
tau_ex_df <- expand.grid(tau_labs = ex_tau_labs, tree_labs = ex_tree_labs, ti_dep_labs = ex_ibd_labs)
# tau_ex_df$new_predicted_y_nls <- predict(model_nls, newdata = tau_ex_df)
tau_ex_df$new_predicted_y <- predict(gls_exp, newdata = tau_ex_df)

stopifnot(all(met_unld_no_melt_tau_smry$tau_labs %in% ex_tau_labs))

## PLOT MODEL ----
tau_plot_df <- tau_ex_df |> left_join(met_unld_no_melt_tau_smry) |>
  filter(
    !(tree_labs == 13 & tau_labs > 0.25),
    !(tree_labs == 4 & tau_labs > 0.5),
    !(tree_labs == 1 & tau_labs > 0.75)) |> 
  pivot_longer(tau_labs, names_to = 'x_var_name', values_to = 'x_var_value')

ggplot(tau_plot_df, aes(x=x_var_value)) +
  # geom_line(aes(tau_labs, new_predicted_y_nls, colour = factor(tree_labs)), linetype = 'dashed') +
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

ggsave(
  'figs/results/modelled_tau_ibd_unloading_w_obs.png',
  width = 6,
  height = 4,
  device = png
)

met_unld_no_melt_tau_smry$pred_q_unl <- 
  predict(gls_exp, met_unld_no_melt_tau_smry)

met_unld_no_melt_tau_smry |> 
  ggplot(aes(tau_labs, colour = factor(round(tree_labs)), group = factor(tree_labs))) + 
  geom_point(aes(y = q_unl_avg)) +
  geom_line(aes(y = pred_q_unl))

## ERROR TABLE ----

q_unl_temp_model_err_tbl <- met_unld_no_melt_tau_smry |> 
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
    formatC(coefs[1], format = "e", digits = 2),
    "NA for GLS",
    "NA",
    "NA"
  )
)

# Combine into final long format table
man_corr_test <- tibble(Metric = "Linear/Non-linear Correlation", Value = "Moderate")

model_type <- tibble(Metric = 'Model', Value = 'GLS')
eqn <- tibble(
  Metric = 'Equation',
  Value  = "$q_{unld}^{dry} = L \\cdot \\tau_{mid} \\cdot (T_a - T_i) \\cdot a$"
)

long_tbl <- bind_rows(model_type, eqn) |>
  bind_rows(perf_tbl) |> 
  bind_rows(coef_tbl) |>
  bind_rows(tau_gls_checks$table |> filter(Metric != 'Independence')) |>
  rbind(man_corr_test)

saveRDS(long_tbl,
        'data/results/modelled_tau_ibd_unloading_error_table.rds')
