# script to generate model of unloading w canopy snow load

## COMPUTE AVERAGES OVER BINS ---- 

met_unld_tree_fltr <- q_unld_met_scl |> 
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

## PLOT BINS ----

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

## MODEL ----

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

## PLOT MODEL ----

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
  # theme_bw() +
  scale_color_viridis_d(begin = 0, end = 0.8) +
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

## ERROR TABLE ----

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