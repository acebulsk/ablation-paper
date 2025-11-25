options(ggplot2.discrete.colour= c("#DF536B", "#000000"))

# melt rate is correlated with snow load so need to convert to dimensionless melt (kg m-2 s-1 / snow load) similar to dimensionless subl rate

## COMPUTE AVERAGES OVER BINS ---- 

unld_met_smry <- obs_mod_met_melt |> 
  filter(is.na(tree_mm) == F) |> 
  group_by(tree_labs, canopy_snowmelt_dml_labs) |> 
  summarise(q_unl_avg = mean(q_unl, na.rm = T),
            q_unl_sd = sd(q_unl, na.rm = T),
            sd_low = ifelse((q_unl_avg - q_unl_sd)<0,0, q_unl_avg - q_unl_sd),
            sd_hi = q_unl_avg + q_unl_sd,
            ci_low = quantile(q_unl,0.05),
            ci_hi = quantile(q_unl, 0.95),
            sum_snow = sum(dU),
            n = n()) |> 
  filter(#n >= 3,
         # tree_labs > 1,
         # tau_labs < 3, # tau transport potential above this threshold
         canopy_snowmelt_dml_labs < 5,
         sum_snow > 0.1)

ggplot(unld_met_smry, 
       aes(x = canopy_snowmelt_dml_labs, y = q_unl_avg, colour = as.character(round(tree_labs)))) + 
  # geom_point(data = met_unld_no_melt_cold, aes(u, q_unl), alpha = 0.1, colour = 'black') +
  # geom_errorbar(aes(
  #   x = tau_labs, 
  #   ymax = sd_hi,
  #   ymin = sd_low
  # ), width = 0.2)  +
  geom_point(size = 3) +
  # ylab(bin_unl_ax_lab) +
  # xlab('Shear Stress Bins (N/m^2)') +
  theme_bw() +
  # theme_bw(base_size = 14) +
  theme(legend.position = 'bottom') +
  ylim(0, NA) +
  xlim(0, NA) +
  # scale_color_manual(values = c("#f89540", "#0072B2","#f89540", "#0072B2")) +
  labs(color = 'Mean Canopy Load (mm)')# + facet_grid(cols = vars(name))

## MODEL ----

### fit a linear model ----

# to recreate the interaction its just (coef * tree_labs * tau_labs)
model_lm <- lm(q_unl_avg ~ tree_labs:canopy_snowmelt_dml_labs - 1, data = unld_met_smry)
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
d_lm <- hydroGOF::dr(fitted(model_lm), unld_met_smry$q_unl_avg) |> round(2)

# aic <- AIC(model_lm) |> round(2) # cannot use AIC bc different n of binned response

### test assumptions
lm_checks <- check_lm_assumptions(model_lm)

# Look at the different models for the warm events 
ex_tree_labs <- c(1, 3, 6)
ex_df <- expand.grid(canopy_snowmelt_dml_labs = sm_dml_labs_seq[sm_dml_labs_seq<5], tree_labs = ex_tree_labs)
# tau_ex_df$new_predicted_y_nls <- predict(model_nls, newdata = tau_ex_df)
ex_df$new_predicted_y <- predict(model_lm, newdata = ex_df)

# stopifnot(all(unld_met_smry$canopy_snowmelt_dml_labs %in% ex_sm_labs))

### fit a GLS model ---- 

# check normality 

hist(unld_met_smry$q_unl_avg)
library(car)
qqPlot(unld_met_smry$q_unl_avg) # not terrible could try both

glmm_model_log <- glmmTMB(q_unl_avg ~ tree_labs + canopy_snowmelt_dml_labs,
  data = unld_met_smry,
  family = Gamma(link = "log")
)

summary(glmm_model_log)

glmm_model_gauss <- glmmTMB(q_unl_avg ~ tree_labs + canopy_snowmelt_dml_labs, data = unld_met_smry)

summary(glmm_model_gauss)

## PLOT MODEL ----
plot_df <- ex_df |> left_join(unld_met_smry) |>
  pivot_longer(canopy_snowmelt_dml_labs, names_to = 'x_var_name', values_to = 'x_var_value')

ggplot(plot_df, aes(x=x_var_value)) +
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
  xlab(expression("Dimensionless Snowmelt Rate (-)")) +
  scale_color_viridis_d(begin = 0, end = 0.8) +
  labs(colour = 'Canopy Snow\nLoad (mm)') # avoids large space using regular way

# ggsave(
#   'figs/results/modelled_tau_temp_unloading_w_obs.png',
#   width = 6,
#   height = 4,
#   device = png
# )

unld_met_smry$pred_q_unl <- 
  predict(model_lm, unld_met_smry)

unld_met_smry |> 
  ggplot(aes(canopy_snowmelt_dml_labs, colour = factor(round(tree_labs)), group = factor(tree_labs))) + 
  geom_point(aes(y = q_unl_avg)) +
  geom_line(aes(y = pred_q_unl))

## ERROR TABLE ----

q_unl_temp_model_err_tbl <- unld_met_smry |> 
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
    `Coefficient of Determination` = R2,
    # `Akaike Information Criterion` = AIC,
    `Coefficient of Agreement` = d
  ) |> 
  pivot_longer(everything(), names_to = "Metric", values_to = "Value") |> 
  mutate(Value = as.character(Value))

# Coefficient table in long format
coef_tbl <- tibble(
  Metric = c("Coefficient a", "Significance of a", "Coefficient b", "Significance of b"),
  Value = c(
    coefs_df$`tree_labs:canopy_snowmelt_dml_labs_Estimate`,
    coefs_df$`tree_labs:canopy_snowmelt_dml_labs_p_value`,
    NA,
    NA
  )
)

# Combine into final long format table
man_corr_test <- tibble(Metric = "Linear/Non-linear Correlation", Value = "NA")

model_type <- tibble(Metric = 'Model', Value = 'OLS')
eqn <- tibble(
  Metric = 'Equation',
  Value  = "$q_{unld}^{melt} = L \\cdot \\frac{q_{melt}}{L} \\cdot a$"
)

long_tbl <- bind_rows(model_type, eqn) |>
  bind_rows(perf_tbl) |> 
  bind_rows(coef_tbl) |>
  bind_rows(lm_checks$table |> filter(Metric != 'Independence')) |>
  rbind(man_corr_test)

saveRDS(long_tbl,
        'data/results/modelled_melt_unloading_melt_only_error_table.rds')


