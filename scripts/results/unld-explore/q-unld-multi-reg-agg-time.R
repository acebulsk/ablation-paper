# script to run multi regression on unloading, wind, subl, canopy load

scl_met_agg_fltr <- scl_met_agg |> 
  mutate(scl_flag = ifelse(scl_rel_perc_error > err_th, T, F),
         q_unl_subl_ratio = q_unl/-q_subl) |> 
  filter(scl_flag == F,
         # q_unl < 1, # rm outliers
         # dmelt_veg == 0,
         # dU > 0,
         # q_subl < 0) 
  )

scl_met_agg_fltr |> 
  ggplot(aes(q_melt, q_unl, colour = u)) + 
  geom_point()

scl_met_agg_fltr |> 
  ggplot(aes(q_melt, q_unl, colour = t)) + 
  geom_point()

scl_met_agg_fltr |> 
  ggplot(aes(t, q_unl)) + 
  geom_point()

scl_met_agg_fltr |> 
  ggplot(aes(u, q_unl)) + 
  geom_point()

scl_met_agg_fltr |> 
  ggplot(aes(q_subl, q_unl)) + 
  geom_point()

scl_met_agg_fltr |> 
  ggplot(aes(tree_mm, q_unl)) + 
  geom_point()

# linear model ----

# only snow load is significant, likely due to nonlinearity and thresholding
lm_model <- lm(q_unl ~ tree_mm + t + u + q_subl + q_melt, data = scl_met_agg_fltr)
summary(lm_model)

lm_model_interaction <- lm(q_unl ~ (tree_mm + t + u + q_subl + q_melt)^2, data = scl_met_agg_fltr)
summary(lm_model_interaction)

# GAM model ---- 

# this is supposed to handle non-linear and thresholding relationships better
# than nls and lms

library(mgcv)

# basic gam, wind speed is bad relationship as we have high unloading at low wind speeds, vise versa with melt
gam_model <- gam(q_unl ~ tree_mm + s(q_melt) + s(u),
                 data = scl_met_agg_fltr)
summary(gam_model)
vis.gam(gam_model, view = c("tree_mm", "q_melt"), plot.type = "contour")
vis.gam(gam_model, view = c("tree_mm", "u"), plot.type = "contour")

# add in interactions, start small
# Using ti() with separate main effects
gam_model_u_melt_interact <- gam(q_unl ~ s(u) + s(q_melt) + 
                      ti(u, q_melt) +
                      s(t) + s(q_subl) + s(tree_mm),
                    data = scl_met_agg_fltr, method = "REML")
summary(gam_model_u_melt_interact)
plot(gam_model_u_melt_interact)

# test interactions with snow load 
gam_model_tree_interact <- gam(
  q_unl ~ 
    # Main effect of canopy load (necessary control)
    s(tree_mm) +
    
    # Hypothesis 1: Melt effect and its interaction with load
    s(q_melt) + ti(tree_mm, q_melt) +
    
    # Hypothesis 2: Sublimation effect and its interaction with load
    s(q_subl) + ti(tree_mm, q_subl) +
    
    # Hypothesis 3: Wind effect and its interaction with load
    s(u) + ti(tree_mm, u) +
    
    # Hypothesis 4: Temperature effect and its interaction with load
    s(t) + ti(tree_mm, t),
  
  data = scl_met_agg_fltr,
  method = "REML"
)
summary(gam_model_u_melt_interact)
plot(gam_model_u_melt_interact)

# Compare models
AIC(gam_model_ti, gam_model_te)

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

# remove insig terms
gam_model <- gam(q_unl ~ tree_mm + s(q_melt) + s(u),
                 data = scl_met_agg_fltr)
summary(gam_model)

plot(gam_model)

# EXP Model ----

## predict unloading to sublimation ratio ----

# wind and canopy snow load
nls_model <- nls(
  q_unl ~ tree_mm * a * exp(b * u),
  data = scl_met_agg_fltr,
  start = list(a = 0.1, b = 0.1)
)

summary(nls_model)

RSS.p <- sum(residuals(nls_model)^2)  # Residual sum of squares
TSS <- sum((scl_met_agg_fltr$q_unl_subl_ratio - mean(scl_met_agg_fltr$q_unl_subl_ratio))^2)  # Total sum of squares
rsq_nls <- 1 - (RSS.p/TSS) |> round(2)  # R-squared measure
rsq_nls
modelr::rsquare(nls_model, scl_met_agg_fltr) # check is the same as our manually defined method

# tree only 
nls_model <- nls(
  q_unl_subl_ratio ~ a * tree_mm * exp(b * tree_mm),
  data = scl_met_agg_fltr,
  start = list(a = 0.1, b = 0.1)
)

summary(nls_model)

RSS.p <- sum(residuals(nls_model)^2)  # Residual sum of squares
TSS <- sum((scl_met_agg_fltr$q_unl_subl_ratio - mean(scl_met_agg_fltr$q_unl_subl_ratio))^2)  # Total sum of squares
rsq_nls <- 1 - (RSS.p/TSS) |> round(2)  # R-squared measure
rsq_nls
modelr::rsquare(nls_model, scl_met_agg_fltr) # check is the same as our manually defined method

## predict unloading ----

# wind and canopy snow load
nls_model <- nls(
  q_unl ~ tree_mm * a * exp(b * u + c * tree_mm + d * q_subl),
  data = scl_met_agg_fltr,
  start = list(a = 0.1, b = 0.1, c = 0.1, d = 0.1)
)

summary(nls_model)

RSS.p <- sum(residuals(nls_model)^2)  # Residual sum of squares
TSS <- sum((scl_met_agg_fltr$q_unl - mean(scl_met_agg_fltr$q_unl))^2)  # Total sum of squares
rsq_nls <- 1 - (RSS.p/TSS) |> round(2)  # R-squared measure
rsq_nls
modelr::rsquare(nls_model, scl_met_agg_fltr) # check is the same as our manually defined method

# tree only 
nls_model <- nls(
  q_unl ~ a * tree_mm * exp(b * tree_mm),
  data = scl_met_agg_fltr,
  start = list(a = 0.1, b = 0.1)
)

summary(nls_model)

RSS.p <- sum(residuals(nls_model)^2)  # Residual sum of squares
TSS <- sum((scl_met_agg_fltr$q_unl - mean(scl_met_agg_fltr$q_unl))^2)  # Total sum of squares
rsq_nls <- 1 - (RSS.p/TSS) |> round(2)  # R-squared measure
rsq_nls
modelr::rsquare(nls_model, scl_met_agg_fltr) # check is the same as our manually defined method

