# script to generate model of duration based unloading

## COMPUTE AVERAGES OVER BINS ---- 

met_unld_time_fltr <- q_unld_met_scl |> 
  filter(is.na(duration_labs) == F,
         name %in% scl_names,
         u <= 1,
         t < -6,
         q_unl < 7,
         !(time_elapsed_bin_labs == 54 & q_unl > 2)
  ) |> # removes outlier at 54 hours to some extent
  mutate(`Canopy Load (mm)` = case_when(
    tree_mm >= snow_load_th ~ 'Heavy',
    TRUE  ~ 'Low'
  )) |> group_by(`Canopy Load (mm)`) |> 
  mutate(avg_w_tree = mean(tree_mm, na.rm = T))

met_unld_time_smry <- met_unld_time_fltr |> 
  group_by(duration_labs, avg_w_tree) |> 
  summarise(q_unl_avg = mean(q_unl, na.rm = T),
            q_unl_sd = sd(q_unl, na.rm = T),
            sd_low = ifelse((q_unl_avg - q_unl_sd)<0,0, q_unl_avg - q_unl_sd),
            sd_hi = q_unl_avg + q_unl_sd,
            ci_low = quantile(q_unl,0.05),
            ci_hi = quantile(q_unl, 0.95),
            n = n()) |> 
  filter(n > 10)

## PLOT BINS ----

ggplot(met_unld_time_smry, 
       aes(x = duration_labs, y = q_unl_avg, colour = as.character(round(avg_w_tree)))) + 
  # geom_point(data = met_unld_fltr, aes(time_elapsed_event, q_unl), alpha = 0.1, colour = 'black') +
  geom_errorbar(aes(
    x = duration_labs, 
    ymax = sd_hi,
    ymin = sd_low
    
  ), width = 0.2) +
  geom_point(size = 3) +
  ylab(bin_unl_ax_lab) +
  xlab('Duration Snow Intercepted in the Canopy (hours)') +
  theme_bw()  +
  scale_color_manual(values = c("#f89540", "#0072B2","#f89540", "#0072B2")) +
  labs(color = 'Mean Canopy Load (mm)')

## MODEL ----

# adding canopy load failed to improve duration model so leaving it out 

### fit a linear model ----
# plot(met_unld_all_winds_cold_smry$wind_labs, met_unld_all_winds_cold_smry$log_q_unl)
met_unld_time_smry$log_q_unl_avg <- 
  log(met_unld_time_smry$q_unl_avg)

### Fit a non linear least squares model ----

# use starting values from the linear model 

# per second for model
met_unld_time_smry$log_q_unl_avg_sec <- 
  log(met_unld_time_smry$q_unl_avg/(60*60))
met_unld_time_smry$q_unl_avg_sec <- 
  met_unld_time_smry$q_unl_avg/(60*60)
met_unld_time_smry$duration_labs_sec <- 
  met_unld_time_smry$duration_labs*(60*60)

model_lm <- lm(log_q_unl_avg_sec ~ duration_labs_sec, data = met_unld_time_smry)
coefs <- coef(model_lm) |> as.numeric()
summary(model_lm)
a_lm <- exp(coefs[1])
b_lm <- coefs[2]
model_nls <- nls(q_unl_avg_sec ~ avg_w_tree* a * exp(b *  duration_labs_sec), # adding w_tree here adds 10% to the R^2 here
                 data = met_unld_time_smry |> filter(avg_w_tree < 5, duration_labs < 75), 
                 start = list(a = a_lm, b = b_lm))
nls_coefs <- coef(model_nls)
saveRDS(nls_coefs, 'data/model_coef_duration_unld_per_second.rds')

# Data preparation for ggplot
met_unld_time_smry <- met_unld_time_smry |> 
  mutate(
    model_nls_pred = avg_w_tree*nls_coefs[[1]] * exp(nls_coefs[[2]] * duration_labs_sec) # Linear model prediction
  )

# Create ggplot
ggplot(met_unld_time_smry, aes(x = duration_labs_sec, y = model_nls_pred, colour = avg_w_tree)) +
  # Observed vs NLS model
  geom_point() 

# per hour for plotting 
model_lm <- lm(log_q_unl_avg ~ duration_labs, data = met_unld_time_smry)
coefs <- coef(model_lm) |> as.numeric()
summary(model_lm)
a_lm <- exp(coefs[1])
b_lm <- coefs[2]

model_nls <- nls(q_unl_avg ~ a * exp(b * duration_labs), # adding w_tree here adds 10% to the R^2 here
                 data = met_unld_time_smry |> filter(avg_w_tree < 5, duration_labs < 75), 
                 start = list(a = a_lm, b = b_lm))
# summary(model_nls)

RSS.p <- sum(residuals(model_nls)^2)  # Residual sum of squares
TSS <- sum((met_unld_time_smry$q_unl_avg - mean(met_unld_time_smry$q_unl_avg))^2)  # Total sum of squares
rsq_nls <- 1 - (RSS.p/TSS) |> round(2)  # R-squared measure

modelr::rsquare(model_nls, met_unld_time_smry) # check is the same as our manually defined method

### Fit a non linear least squares model with weights iteratively ----
# give large values a higher weight

max_iter <- 1000
tol <- 1e-6

coefs_old <- as.numeric(coef(model_nls))
wts <- met_unld_time_smry$q_unl_avg^2

# apply weights iteratively
# for (i in 1:max_iter) {
#   model_nlswi <- nls(q_unl_avg ~ a * exp(b * duration_labs),
#                      data = met_unld_time_smry,
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
# TSS <- sum((met_unld_time_smry$q_unl_avg - mean(met_unld_time_smry$q_unl_avg))^2)  # Total sum of squares
# rsq_nlswi <- 1 - (RSS.p/TSS) |> round(2)  # R-squared measure

# modelr::rsquare(model_nlswi, met_unld_time_smry) # check is the same as our manually defined method

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

ex_duration_labs <- seq(0,225,0.25)
ex_avg_w_tree <- c(4, 5, 10, 11, seq(15, 30, by = 5))
ex_df <- expand.grid(duration_labs = ex_duration_labs, avg_w_tree = ex_avg_w_tree)
ex_df$new_predicted_y <- predict(model_nls, newdata = ex_df)

ggplot(ex_df) +
  geom_line(aes(duration_labs, new_predicted_y)) +
  geom_errorbar(data = met_unld_time_smry,
                aes(
                  x = duration_labs,
                  ymax = sd_hi,
                  ymin = sd_low,
                  width = 0.25,
                  colour = as.character(round(avg_w_tree))
                )) +
  geom_point(data = met_unld_time_smry,
             aes(duration_labs, q_unl_avg, colour = as.character(round(avg_w_tree))),
             size = 2) +
  ylab('Unloading + Drip Rate (mm/hr)') +
  xlab('Duration Snow Intercepted in Canopy (Hours)') +
  # ylim(c(0,2)) +
  # theme_bw() +
  scale_color_viridis_d(begin = 0, end = 0.8) +
  labs(colour = 'Canopy Snow\nLoad (mm)')

ggsave(
  'figs/results/modelled_duration_unloading_w_obs.png',
  width = 6,
  height = 4
)

met_unld_time_smry$pred_q_unl <- 
  predict(model_nls, met_unld_time_smry)

met_unld_time_smry |> 
  ggplot(aes(duration_labs, colour = factor(round(avg_w_tree)), group = factor(avg_w_tree))) + 
  geom_point(aes(y = q_unl_avg)) +
  geom_line(aes(y = pred_q_unl))

## ERROR TABLE ----

q_unl_time_model_err_tbl <- met_unld_time_smry |> 
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

saveRDS(q_unl_time_model_err_tbl,
        'data/modelled_duration_unloading_error_table.rds')