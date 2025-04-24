# script to run multi regression on unloading, wind, subl, canopy load BINS this
# doesnt work on raw data due to instrument error OR with time aggregated data
# due to non-stationarity

library(dplyr)
library(purrr)
library(broom)
library(tidyr)

# avg unloading rate for each combination of bins ----

met_unld_w_bins_smry <- met_unld_w_bins |> 
  filter(is.na(tree_mm) == F) |> 
  group_by(wind_labs, tree_labs, canopy_snowmelt_labs, subl_labs, temp_labs) |> 
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

# print the indiv bins
met_unld_w_bins_smry$wind_labs |> unique()
met_unld_w_bins_smry$temp_labs |> unique()
met_unld_w_bins_smry$subl_labs |> unique()
met_unld_w_bins_smry$canopy_snowmelt_labs |> unique()

# MAIN EFFECTS ----

# fn to test each individual combination of predictors
# TODO add in partial R2 for each term, could reduce number of models shown
generate_lm_model_table <- function(df) {
  library(dplyr)
  library(purrr)
  library(broom)
  library(tidyr)
  
  predictors <- c(
    T_a = "temp_labs",
    L = "tree_labs",
    u = "wind_labs",
    q_melt = "canopy_snowmelt_labs",
    q_subl = "subl_labs"
  )
  
  combos <- unlist(lapply(1:length(predictors), function(i) combn(predictors, i, simplify = FALSE)), recursive = FALSE)
  
  format_estimate <- function(estimate, pval) {
    sig <- case_when(
      # pval < 0.001 ~ "***",
      # pval < 0.01  ~ "**",
      pval < 0.05  ~ "*",
      TRUE ~ "ns"
    )
    sprintf("%.2f (%s)", estimate, sig)
  }
  
  results <- map_dfr(combos, function(vars) {
    formula <- as.formula(paste("q_unl_avg ~", paste(vars, collapse = " + ")))
    model <- lm(formula, data = df)
    tidy_model <- tidy(model)
    glance_model <- glance(model)
    
    # Format terms
    terms <- setNames(rep("—", length(predictors)), names(predictors))
    for (term in tidy_model$term) {
      if (term == "(Intercept)") next
      name <- names(predictors)[which(predictors == term)]
      estimate <- tidy_model$estimate[tidy_model$term == term]
      pval <- tidy_model$p.value[tidy_model$term == term]
      terms[[name]] <- format_estimate(estimate, pval)
    }
    
    intercept_row <- tidy_model |> filter(term == "(Intercept)")
    intercept_val <- format_estimate(intercept_row$estimate, intercept_row$p.value)
    
    tibble(
      intercept = intercept_val,
      T_a = terms["T_a"],
      L = terms["L"],
      u = terms["u"],
      q_melt = terms["q_melt"],
      q_subl = terms["q_subl"],
      R2 = round(glance_model$r.squared, 2),
      AIC = round(glance_model$AIC, 1)
    )
  })
  
  return(results)
}

lm_table <- generate_lm_model_table(met_unld_w_bins_smry) |> 
  arrange(AIC) |> 
  mutate(`Model Name` = paste0('M', row_number()), .before = intercept)
print(lm_table)
saveRDS(lm_table, 'data/stats/lm_multi_reg_q_unld_bins.rds')

# # MANUAL EXPLORE MODELS ----
# linear model ----

# no interaction
lm_model <- lm(q_unl_avg ~
                 tree_labs +
                 wind_labs +
                 canopy_snowmelt_labs,
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
# # GAM model ---- 
# 
# # this is supposed to handle non-linear and thresholding relationships better
# # than nls and lms
# 
# library(mgcv)
# 
# gam_model <- gam(
#   q_unl ~
#     ti(tree_mm) +                   # main effect: snow load
#     ti(q_melt) + ti(tree_mm, q_melt) +   # melt effect + interaction
#     ti(t) + ti(tree_mm, t) +             # temperature effect + interaction
#     ti(q_subl) + ti(tree_mm, q_subl) +   # sublimation effect + interaction
#     ti(u) + ti(tree_mm, u),              # wind effect + interaction
#   data = scl_met_agg_fltr,
#   method = "REML"
# )
# summary(gam_model)
# 
# vis.gam(gam_model, view = c("tree_mm", "q_melt"), plot.type = "contour")
# vis.gam(gam_model, view = c("tree_mm", "u"), plot.type = "contour")
# 
# 
# # remove insig terms
# gam_model <- gam(q_unl ~ tree_mm + s(q_melt) + s(u),
#                  data = scl_met_agg_fltr)
# summary(gam_model)
# 
# plot(gam_model)
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
