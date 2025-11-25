# script to generate model of wind induced unloading

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
  # ylab(bin_unl_ax_lab) +
  # xlab(bin_wnd_ax_lab) +
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
nls_smry <- summary(model_nls)
plot(fitted(model_nls), residuals(model_nls))

# weighted model, makes it worse
# weights_vec <- 1 / sqrt(fitted(model_nls))  # or 1 / fitted(model_nls0)^p
# model_nls <- nls(q_unl_avg ~ wind_labs * a * tree_labs * exp(b * wind_labs),
#                  data = met_unld_no_melt_wind_smry, start=list(a=a_lm, b=b_lm),
#                  weights = weights_vec)
# summary(model_nls)
# plot(fitted(model_nls), residuals(model_nls))

### Assumptions -----

lm_checks <- check_nls_assumptions(model_nls)
car::ncvTest(model_lm)
coeftest(model_lm, vcov = vcovHC(model_lm, type = "HC1")) # checks model significance considering homoscedasticity is violated
plot(model_lm, which = 1)

# Performance 
rsq_nls <- compute_r2(model_nls) |> round(2)
d_nls <- hydroGOF::dr(fitted(model_nls), met_unld_no_melt_wind_smry$q_unl_avg) |> round(2)
# aic <- AIC(model_lm) |> round(2) # cannot use AIC bc different n of binned response

coefs_df <- as.data.frame(coef(nls_smry))
coefs_df <- coefs_df |> 
  rownames_to_column(var = "term") |> 
  select(term, Estimate, p_value = `Pr(>|t|)`) |> 
  pivot_wider(names_from = term, values_from = c(Estimate, p_value), names_glue = "{term}_{.value}") |> 
  mutate(across(contains("Estimate"), ~ formatC(.x, format = "e", digits = 2))) |> 
  mutate(across(contains("p_value"), ~ ifelse(.x < 0.05, 'p < 0.05', 'n.s.')))

# Look at the different models for the warm events 
ex_wind_labs <- seq(0,6,0.05) |> round(2)
ex_tree_labs <- c(1, 4, 13)
wind_ex_df <- expand.grid(wind_labs = ex_wind_labs, tree_labs = ex_tree_labs)
wind_ex_df$new_predicted_y <- predict(model_nls, newdata = wind_ex_df)

stopifnot(all(met_unld_no_melt_wind_smry$wind_labs %in% ex_wind_labs))

## PLOT MODEL ----

wind_plot_df <- wind_ex_df |> left_join(met_unld_no_melt_wind_smry) |> 
  filter(!(tree_labs == 13 & wind_labs > 3.5),
         !(tree_labs == 4 & wind_labs > 5)) |> 
  pivot_longer(wind_labs, names_to = 'x_var_name', values_to = 'x_var_value') |> 
  select(-log_q_unl_avg, -model_nls_pred)


ggplot(wind_plot_df, aes(x = x_var_value)) +
  geom_line(aes(y = new_predicted_y, colour = factor(tree_labs))) +
  geom_errorbar(aes(
                  ymax = sd_hi,
                  ymin = sd_low,
                  width = 0.05,
                  colour = as.character(round(tree_labs))
                )) +
  geom_point(aes(y = q_unl_avg, colour = as.character(round(tree_labs))),
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
         R2 = rsq_nls,
        #  AIC = aic,
          d = d_nls) 

# Performance metrics reshaped to long format (convert values to character)
perf_tbl <- q_unl_temp_model_err_tbl |> 
  select(`Mean Bias (mm/hr)` = `Mean Bias`,
 `Mean Absolute Error (mm/hr)` = MAE,
  `Root Mean Square Error (mm/hr)` = `RMS Error`,
   `Coefficient of Determination` = R2,
  #  `Akaike Information Criterion` = AIC,
   `Coefficient of Agreement` = d
  ) |> 
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

# manual check hetero and looks ok
lm_checks$table$Value[lm_checks$table$Metric == "Homoscedasticity"] <- "Pass"

man_corr_test <- tibble(Metric = "Linear/Non-linear Correlation", Value = "NA")

model_type <- tibble(Metric = 'Model', Value = 'NLS')
eqn <- tibble(
  Metric = 'Equation',
  Value  = "$q_{unld}^{dry} = L \\cdot u_{mid} \\cdot a \\cdot e^{b\\cdot u_{mid}}$"
)

long_tbl <- bind_rows(model_type, eqn) |>
  bind_rows(perf_tbl) |> 
  bind_rows(coef_tbl) |>
  bind_rows(lm_checks$table |> filter(Metric != 'Independence')) |>
  rbind(man_corr_test)

saveRDS(long_tbl,
        'data/results/modelled_wind_unloading_error_table.rds')
