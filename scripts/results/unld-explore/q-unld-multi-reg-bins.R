# script to run multi regression on unloading, wind, subl, canopy load BINS this
# doesnt work on raw data due to instrument error OR with time aggregated data
# due to non-stationarity

# avg unloading rate for each combination of bins ----

# try OLS regression across binned data, some assumptions are invalid

predictors <- c(
  L = "tree_labs",
  u = "wind_labs",
  q_melt = "canopy_snowmelt_labs",
  q_subl = "subl_labs", # consider removing, similar to ice bulb dep...
  T_a = "temp_labs",
  tau = "tau_labs",
  T_ib_dep = "ti_dep_labs"
)

lm_table <- generate_lm_model_table(met_unld_w_bins, predictors) |>
  arrange(desc(Adj_R2)) |>
  mutate(`Model Name` = paste0('M', row_number()), .before = intercept)

print(lm_table)
saveRDS(lm_table, 'data/stats/lm_multi_reg_q_unld_bins.rds')

# select models 

select_models <- c(
  'M1', # best one with L, u and melt
  'M10', # as above but adds temp
  'M24', # as w M1 but adds subl
  'M4', # as w M1 but tau instead of u
  'M7', # just sublimation and melt
  'M40', # L , wind, air temp
  'M63' # L, wind, subl
)

select_mods <- lm_table |> filter(`Model Name` %in% select_models)
write.csv(select_mods, 'data/stats/lm_multi_reg_q_unld_bins_select.csv')

# try generalized linear mixed effects model

library(glmmTMB)
library(DHARMa)
library(broom)
library(dplyr)
library(purrr)
library(tibble)

predictors <- c(
  L = "tree_labs",
  u = "wind_labs",
  q_melt = "canopy_snowmelt_labs",
  q_subl = "subl_labs", # consider removing, similar to ice bulb dep...
  T_a = "temp_labs",
  tau = "tau_labs",
  T_ib_dep = "ti_dep_labs"
)

glmm_table <- generate_glmm_model_table(met_unld_w_bins, predictors)

glmm_table_fltr <- glmm_table |> 
  filter(!(u == 'x' & tau == 'x'),
          !(T_a == 'x' & q_melt == 'x'),
          !(q_subl == 'x' & T_ib_dep == 'x')) |> arrange(AIC)

write.csv(glmm_table_fltr, 'data/stats/glmm_multi_reg_q_unld_bins_select.csv', row.names = F)


# MANUAL EXPLORE MODELS ----

# WARNING: adding too many grouping variables here lowers the amount of snow unloading into each bin
met_unld_w_bins_smry <- met_unld_w_bins |> 
  filter(is.na(tree_mm) == F) |> 
  group_by(
    name,
    # wind_labs,
    tree_labs,
    canopy_snowmelt_labs,
    # subl_labs,
    temp_labs,
    tau_labs,
    # ti_dep_labs
  ) |>
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

ggplot(met_unld_w_bins_smry, 
       aes(x = tree_labs, y = q_unl_avg, colour = tau_labs)) + 
  geom_point(size = 3)

# check normality 

hist(met_unld_w_bins_smry$q_unl_avg)
library(car)
qqPlot(met_unld_w_bins_smry$q_unl_avg) # v skewed.. should use gaussian or log transform

# check collinerarity 

predictors <- met_unld_w_bins_smry[, c("canopy_snowmelt_labs",
 "tau_labs",
  # "subl_labs",
   "temp_labs")]

library(corrplot)
cor_mat <- cor(predictors, use = "complete.obs")
corrplot(cor_mat, method = "color", tl.cex = 0.8, number.cex = 0.7)

library(car)
lm_col <- lm(q_unl_avg ~ canopy_snowmelt_labs + 
  tau_labs + 
  # subl_labs + 
  temp_labs, data = met_unld_w_bins_smry)
vif(lm_col)

## linear regression  ----
  
lm_all_vars <- lm(q_unl_avg ~
                    tree_labs +
                    tau_labs + 
                    canopy_snowmelt_labs, 
                  data = met_unld_w_bins_smry)

summary(lm_all_vars)
par(mfrow = c(2,3))
plot(lm_all_vars, which = 1)  # Residuals vs Fitted
durbinWatsonTest(lm_all_vars)
plot(lm_all_vars, which = 3)  # Scale-Location plot
plot(lm_all_vars, which = 2)  # QQ plot
hist(resid(lm_all_vars))
shapiro.test(resid(lm_all_vars))
vif(lm_all_vars)

## Generalized Linear Mixed Effects Model  ----

# transforms predictor variable to account for normality assumption since unloading is highly right skewed

library(glmmTMB)
library(performance)
library(DHARMa)

glmm_model <- glmmTMB(
  q_unl_avg ~ tree_labs +
    tau_labs +
    # subl_labs +
    canopy_snowmelt_labs +
    temp_labs + (1 | name), # SCL sensor is random effect
  data = met_unld_w_bins_smry,
  family = Gamma(link = "log")
)

summary(glmm_model)

# linear model ----

# check interaction
# it makes the melt model worse..
lm_model <- lm(q_unl_avg ~
                 # tree_labs*wind_labs +
                  tree_labs*canopy_snowmelt_labs,
                 # canopy_snowmelt_labs,
                 # subl_labs,
                 # temp_labs,
               data = met_unld_w_bins_smry)

summary(lm_model)


# Get partial R² for each predictor
library(rsq)

rsq.partial(lm_model)

# 
# # w interaction is 2% better
# lm_model <- lm(q_unl_avg ~ 
#                  # tree_labs +
#                  # wind_labs + 
#                  # canopy_snowmelt_labs + 
#                  # subl_labs:tree_labs + 
#                  # temp_labs:tree_labs +
#                  wind_labs*tree_labs +
#                  canopy_snowmelt_labs*tree_labs,
#                data = met_unld_w_bins_smry)
# 
# summary(lm_model)
# 
# # w interaction test one by one
# lm_model <- lm(q_unl_avg ~ 
#                  tree_labs +
#                  # subl_labs +
#                  # wind_labs +
#                  canopy_snowmelt_labs +
#                  # subl_labs:tree_labs + 
#                  # temp_labs:tree_labs +
#                  wind_labs:tree_labs, 
#                  # canopy_snowmelt_labs:tree_labs,
#                data = met_unld_w_bins_smry)
# 
# summary(lm_model)
# 
# lm_model <- lm(q_unl_avg ~ 
#                  tree_labs +
#                  # subl_labs +
#                  # wind_labs +
#                  # canopy_snowmelt_labs +
#                  # subl_labs:tree_labs + 
#                  # temp_labs:tree_labs +
#                  wind_labs:tree_labs +
#                canopy_snowmelt_labs:tree_labs,
#                data = met_unld_w_bins_smry)
# 
# summary(lm_model)
# 
# # w interaction and main effects is 3% better
# lm_model <- lm(q_unl_avg ~ 
#                  # tree_labs + 
#                  subl_labs*tree_labs + 
#                  temp_labs*tree_labs +
#                  wind_labs*tree_labs +
#                  canopy_snowmelt_labs*tree_labs,
#                data = met_unld_w_bins_smry)
# 
# summary(lm_model)
# 
# GAM model ---- 

# this is supposed to handle non-linear and thresholding relationships better
# than nls and lms

library(mgcv)

gam_model <- gam(
  q_unl ~
    ti(tree_mm) +                   # main effect: snow load
    ti(q_melt) + ti(tree_mm, q_melt) +   # melt effect + interaction
    ti(t) + ti(tree_mm, t) +             # temperature effect + interaction
    ti(q_subl) + ti(tree_mm, q_subl) +   # sublimation effect + interaction
    ti(u) + ti(tree_mm, u),              # wind effect + interaction
  data = scl_met_agg_fltr,
  method = "REML"
)
summary(gam_model)

vis.gam(gam_model, view = c("tree_mm", "q_melt"), plot.type = "contour")
vis.gam(gam_model, view = c("tree_mm", "u"), plot.type = "contour")


# remove insig terms
gam_model <- gam(q_unl ~ tree_mm + s(q_melt) + s(u),
                 data = scl_met_agg_fltr)
summary(gam_model)

plot(gam_model)
# 
# # EXP Model ----
# 
# ## predict unloading to sublimation ratio ----
# 
# # wind and canopy snow load
# nls_model <- nls(
#   q_unl ~ tree_mm * a * exp(b * u),
#   data = scl_met_agg_fltr,
#   start = list(a = 0.1, b = 0.1)
# )
# 
# summary(nls_model)
# 
# RSS.p <- sum(residuals(nls_model)^2)  # Residual sum of squares
# TSS <- sum((scl_met_agg_fltr$q_unl_subl_ratio - mean(scl_met_agg_fltr$q_unl_subl_ratio))^2)  # Total sum of squares
# rsq_nls <- 1 - (RSS.p/TSS) |> round(2)  # R-squared measure
# rsq_nls
# modelr::rsquare(nls_model, scl_met_agg_fltr) # check is the same as our manually defined method
# 
# # tree only 
# nls_model <- nls(
#   q_unl_subl_ratio ~ a * tree_mm * exp(b * tree_mm),
#   data = scl_met_agg_fltr,
#   start = list(a = 0.1, b = 0.1)
# )
# 
# summary(nls_model)
# 
# RSS.p <- sum(residuals(nls_model)^2)  # Residual sum of squares
# TSS <- sum((scl_met_agg_fltr$q_unl_subl_ratio - mean(scl_met_agg_fltr$q_unl_subl_ratio))^2)  # Total sum of squares
# rsq_nls <- 1 - (RSS.p/TSS) |> round(2)  # R-squared measure
# rsq_nls
# modelr::rsquare(nls_model, scl_met_agg_fltr) # check is the same as our manually defined method
# 
# ## predict unloading ----
# 
# # wind and canopy snow load
# nls_model <- nls(
#   q_unl ~ tree_mm * a * exp(b * u + c * tree_mm + d * q_subl),
#   data = scl_met_agg_fltr,
#   start = list(a = 0.1, b = 0.1, c = 0.1, d = 0.1)
# )
# 
# summary(nls_model)
# 
# RSS.p <- sum(residuals(nls_model)^2)  # Residual sum of squares
# TSS <- sum((scl_met_agg_fltr$q_unl - mean(scl_met_agg_fltr$q_unl))^2)  # Total sum of squares
# rsq_nls <- 1 - (RSS.p/TSS) |> round(2)  # R-squared measure
# rsq_nls
# modelr::rsquare(nls_model, scl_met_agg_fltr) # check is the same as our manually defined method
# 
# # tree only 
# nls_model <- nls(
#   q_unl ~ a * tree_mm * exp(b * tree_mm),
#   data = scl_met_agg_fltr,
#   start = list(a = 0.1, b = 0.1)
# )
# 
# summary(nls_model)
# 
# RSS.p <- sum(residuals(nls_model)^2)  # Residual sum of squares
# TSS <- sum((scl_met_agg_fltr$q_unl - mean(scl_met_agg_fltr$q_unl))^2)  # Total sum of squares
# rsq_nls <- 1 - (RSS.p/TSS) |> round(2)  # R-squared measure
# rsq_nls
# modelr::rsquare(nls_model, scl_met_agg_fltr) # check is the same as our manually defined method
# 
