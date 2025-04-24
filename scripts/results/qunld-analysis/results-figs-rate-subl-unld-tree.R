# This script analyzes the relationship between observed weighed tree ablation
# due to mass unloading of snow as itsublimates.
# Canopy snow unloading is determined as residual ablation after removing
# q_subl^veg, q_drip, q_unld^wind

# NOTE: looks like a relationship for binned unloading to subl ratio across bins
# for canopy snow load, however this ended up over estimating the unloading
# rate. for and example look at event 2022-03-24 which has almost no unloading
# with significant sublimation.

library(tidyverse)

load_suffix <- 'fsd_closed_0.88'

# LOAD DATA ----

## model ---- 

# Select model run with all unloading events weighed tree snow load assimilated
prj <- "ffr_closed_canopy_cc0.88_cansnobal"

# specify certain model run
run_tag <- "cansnobal_v_1_0_output"
run_tag <- "cansnobal_init_run_cansnobal_v_1_1_updt_obs_SW2"

path <- list.files(
  paste0(
    "crhm/output/",
    prj
  ),
  pattern = run_tag,
  full.names = T
)

crhm_output <- CRHMr::readOutputFile(
  path,
  timezone = 'Etc/GMT+6') |> 
  select(datetime, hru_t.1, m_s_veg.1, hru_u.1, hru_p.1,
         delsub_veg_int.1:delunld_subl_int.1) |> 
  mutate(delsub_veg_int.1 = -delsub_veg_int.1)

# Combine dfs and aggregate to hourly -----

obs_mod <- left_join(w_tree_q_unld_15, crhm_output) |>
  mutate(wtr_year = weatherdash::wtr_yr(datetime)) |> 
  left_join(tree_cal_val_88) |> 
  # left_join(tree_weight) |> 
  # mutate(datetime = ceiling_date(datetime, unit = '1 hour')) |> # ceiling ensures the timestamp corresponds to preeceeding records
  group_by(datetime, event_id) |>
  summarise(tree_mm = last(tree_mm),
            dL = sum(dL),
            delsub_veg_int.1 = sum(delsub_veg_int.1),
            delunld_wind_int.1 = sum(delunld_wind_int.1),
            delunld_melt_int.1 = sum(delunld_melt_int.1),
            delmelt_veg_int.1 = sum(delmelt_veg_int.1),
            delsub_veg_int.1 = sum(delsub_veg_int.1),
            est_q_unld_subl = dL - delsub_veg_int.1 - delunld_wind_int.1 - delmelt_veg_int.1,
            unld_melt_ratio = est_q_unld_subl/delmelt_veg_int.1,
            hru_t.1 = mean(hru_t.1),
            hru_u.1 = mean(hru_u.1)
  ) 

### bin temp  ----

# note zeros are not included in binning, to add set inlcude.lowest = T
min_temp <- round(
  min(obs_mod$hru_t.1, na.rm = T),3)
max_temp <- round(
  max(obs_mod$hru_t.1, na.rm = T),3)
temp_step <- 5

temp_breaks <- seq(
  min_temp,
  max_temp+4,
  temp_step)

temp_labs_seq <- label_bin_fn(bins = temp_breaks)

stopifnot(tail(temp_breaks, 1) > max(obs_mod$hru_t.1, na.rm = T))
stopifnot(length(temp_labs_seq) + 1 == length(temp_breaks))

obs_mod$temp_binned <- cut(obs_mod[,'hru_t.1', drop = TRUE], temp_breaks)

obs_mod$temp_labs <- cut(obs_mod[,'hru_t.1', drop = TRUE], 
                             temp_breaks, 
                             labels = temp_labs_seq)

obs_mod$temp_labs <- as.numeric(as.character(obs_mod$temp_labs))

## bin weighed tree ----

# note zeros are not included in binning, to add set inlcude.lowest = T
min_tree <- 0
max_tree <- round(
  max(obs_mod$tree_mm, na.rm = T),0)
tree_step <- 2

tree_breaks <- seq(
  min_tree,
  max_tree+2,
  tree_step)

tree_labs_seq <- label_bin_fn(bins = tree_breaks)

stopifnot(tail(tree_breaks, 1) > max(obs_mod$tree_mm, na.rm = T))
stopifnot(length(tree_labs_seq) + 1 == length(tree_breaks))

obs_mod$tree_binned <- cut(obs_mod[,'tree_mm', drop = TRUE], tree_breaks)

obs_mod$tree_labs <- cut(obs_mod[,'tree_mm', drop = TRUE], 
                          tree_breaks, 
                          labels = tree_labs_seq)

obs_mod$tree_labs <- as.numeric(as.character(obs_mod$tree_labs))

# PLOT -----

subl_event_filter <- obs_mod |> 
  group_by(event_id) |> 
  summarise(subl = sum(delsub_veg_int.1),
            wind = sum(delunld_wind_int.1),
            melt = sum(delmelt_veg_int.1),
            melt_unld = sum(delunld_melt_int.1),
            total = subl+wind+melt+melt_unld,
            frac_subl = subl/total,
            frac_wind = wind/total,
            frac_melt = (melt+melt_unld)/total) |> 
  filter(frac_subl > .5)

obs_mod_fltr <- obs_mod |> 
  filter(#delmelt_veg_int.1 == 0,
         est_q_unld_subl >= 0,
         # est_q_unld_subl < 1,
         delsub_veg_int.1 > 0,
         # Subl_Cpy.1 < 0.175,
         dL > 0.02,
         # tree_mm > 2,
         event_id %in% subl_event_filter$event_id,
         hru_u.1 < 1.25# important to remove some outliers due to wind unloading
         # hru_t.1 < 0
  ) |> 
  # convert mm/interval to mm/hour
  mutate(est_q_unld_subl = est_q_unld_subl*4,
         delsub_veg_int.1 = delsub_veg_int.1*4,
         unld_subl_ratio = est_q_unld_subl/delsub_veg_int.1) |> 
  filter(unld_subl_ratio < 10)

ggplot(obs_mod_fltr,
       aes(delsub_veg_int.1, est_q_unld_subl, colour = factor(event_id))) + geom_point()
ggplot(obs_mod_fltr,
       aes(tree_mm, est_q_unld_subl, colour = hru_t.1)) + geom_point()
ggplot(obs_mod_fltr,
       aes(tree_mm, unld_subl_ratio, colour = factor(event_id))) + geom_point()

ggplot(obs_mod_fltr,
       aes(dL, delsub_veg_int.1, colour = factor(event_id))) + geom_point()

# option 1 cumulative within bins, gives very low instrument error as we have a
# large mass measured over the bin
obs_mod_fltr_binned <- obs_mod_fltr |>
  group_by(tree_labs, temp_labs) |>
  summarise(subl = sum(delsub_veg_int.1),
            unld = sum(est_q_unld_subl),
            unld_subl_ratio = unld/subl) 

ggplot(obs_mod_fltr_binned,
       aes(tree_labs, unld_subl_ratio, colour = factor(temp_labs))) + geom_point()

# Binned analysis ---- 


bin_unld_subl_lm <- lm(unld_subl_ratio ~ tree_labs, data = obs_mod_fltr_binned)
summary(bin_unld_subl_lm)
saveRDS(bin_unld_subl_lm, 'data/lm_q_drip_vs_q_unld_subl.rds')
# Extract the coefficient (slope) from the model
slope <- coef(bin_unld_subl_lm)[1]
r2_adj_lm <- r_squared_no_intercept(bin_unld_subl_lm)
r2_adj_lm

ggplot(obs_mod_fltr_binned, aes(tree_labs, unld/subl)) + 
  geom_point(aes(colour = factor(temp_labs))) +
  geom_smooth(method = "lm", se = F) +  # Use method="lm" for linear model
  annotate(
    'label',
    x = 5,
    y = 4,
    label = paste("R² =", round(r2_adj_lm, 2))
  ) +
  labs(
    colour = 'Temp. bin (°C)',
    x = "Canopy Snow Load (mm)",
    y = "Unloading to Sublimation Ratio (-)"
  ) 

ggsave(
  'figs/results/modelled_subl_unloading_ratio_vs_snow_load_bin.png',
  width = 5,
  height = 4,
  device = png
)

# ## model linear ---- 
# 
# q_unld_subl_lm <- lm(est_q_unld_subl ~ 0 + delsub_veg_int.1, data = obs_mod_fltr)
# r2_adj_lm <- r_squared_no_intercept(q_unld_subl_lm)
# obs_mod_fltr$mod_q_unld_subl <- predict(q_unld_subl_lm)
# saveRDS(q_unld_subl_lm, 'data/lm_q_drip_vs_q_unld_subl.rds')
# # Extract the coefficient (slope) from the model
# slope <- coef(q_unld_subl_lm)[1]
# 
# ## model nls ---- 
# q_unld_subl_nls <-
#   nls(est_q_unld_subl ~ delsub_veg_int.1 * a * exp(b * delsub_veg_int.1),
#       data = obs_mod_fltr,
#       start = list(a = 5, b = 1)
#   )
# RSS.p <- sum(residuals(q_unld_subl_nls)^2)  # Residual sum of squares
# TSS <- sum((obs_mod_fltr$est_q_unld_subl - mean(obs_mod_fltr$est_q_unld_subl))^2)  # Total sum of squares
# r2_nls <- 1 - (RSS.p/TSS) |> round(2)  # R-squared measure
# saveRDS(q_unld_subl_nls, 'data/lm_q_drip_vs_q_unld_subl_bin.rds')
# new_data <- data.frame(
#   Subl_Cpy.1 = seq(
#     0,
#     max(obs_mod_fltr$Subl_Cpy.1),
#     length.out = 100
#   )
# )
# new_data$mod_q_unld_subl_nls <- predict(q_unld_subl_nls, newdata = new_data)
# 
# # Create the plot
# ggplot(obs_mod_fltr, aes(x = delsub_veg_int.1, y = est_q_unld_subl)) +
#   geom_point(aes(colour = hru_t.1), size = 2) +                      # Scatter plot of the data
#   geom_abline(intercept = 0, slope = slope, color = "red",    # Model line
#               linetype = "solid", size = 0.5) +
#   labs(
#     colour = 'temp_bin',
#     x = expression("Simulated Canopy Snow Sublimation Rate ("*kg ~ m^-2 ~ hr^-1*")"),
#     # y = expression(q[unld]^{melt} ~ "(" ~ kg ~ m^-2 ~ hr^-1 ~ ")"),
#     y = expression("Sublimation Unloading Rate ("*kg ~ m^-2 ~ hr^-1*")")
#   ) +
#   scale_color_viridis_c() #+
#  # geom_line(data = new_data, aes(Subl_Cpy.1, mod_q_unld_subl_nls))
# 
# ggsave(
#   'figs/results/modelled_subl_unloading_w_obs.png',
#   width = 4,
#   height = 4,
#   device = png
# )
# 
# ggplot(obs_mod_fltr, aes(x = tree_mm, y = est_q_unld_subl/delsub_veg_int.1)) +
#   geom_point(aes(colour = hru_u.1), size = 2) +                      # Scatter plot of the data
#   labs(
#     x = "Canopy Snow Load (mm)",
#     # y = expression(q[unld]^{melt} ~ "(" ~ kg ~ m^-2 ~ hr^-1 ~ ")"),
#     y = "Unloading to Sublimation Ratio (-)"
#   ) +
#   scale_color_viridis_c()
# 
# # bin model ----
# 
# obs_mod_fltr_avg <- obs_mod_fltr |> 
#   group_by(Subl_Cpy.1 = canopy_sub_labs) |> 
#   summarise(est_q_unld_subl_bin = mean(est_q_unld_subl, na.rm = T),
#             q_unl_sd = sd(est_q_unld_subl, na.rm = T),
#             sd_low = ifelse((est_q_unld_subl_bin - q_unl_sd)<0,0, est_q_unld_subl_bin - q_unl_sd),
#             sd_hi = est_q_unld_subl_bin + q_unl_sd,
#             n = n()) |> 
#   filter(n >= 3)
# 
# ## bin model linear ---- 
# 
# q_unld_subl_lm_bin <- lm(est_q_unld_subl_bin ~ 0 + Subl_Cpy.1, data = obs_mod_fltr_avg)
# r2_adj_lm <- r_squared_no_intercept(q_unld_subl_lm_bin)
# obs_mod_fltr_avg$mod_q_unld_subl_lm <- predict(q_unld_subl_lm_bin)
# saveRDS(q_unld_subl_lm_bin, 'data/lm_q_drip_vs_q_unld_subl_bin_lm.rds')
# # Extract the coefficient (slope) from the model
# slope <- coef(q_unld_subl_lm_bin)[1]
# 
# ## bin model nls ---- 
# q_unld_subl_nls_bin <-
#   nls(est_q_unld_subl_bin ~ Subl_Cpy.1 * a * exp(b * Subl_Cpy.1),
#       data = obs_mod_fltr_avg,
#       start = list(a = 5, b = -1)
#       )
# RSS.p <- sum(residuals(q_unld_subl_nls_bin)^2)  # Residual sum of squares
# TSS <- sum((obs_mod_fltr_avg$est_q_unld_subl_bin - mean(obs_mod_fltr_avg$est_q_unld_subl_bin))^2)  # Total sum of squares
# r2_nls <- 1 - (RSS.p/TSS) |> round(2)  # R-squared measure
# saveRDS(q_unld_subl_nls_bin, 'data/lm_q_drip_vs_q_unld_subl_bin_nls.rds')
# new_data <- data.frame(
#   Subl_Cpy.1 = seq(
#     0,
#     max(obs_mod_fltr$Subl_Cpy.1),
#     length.out = 100
#   )
# )
# new_data$mod_q_unld_subl_nls <- predict(q_unld_subl_nls_bin, newdata = new_data)
# 
# ## bin model sigmoid ---- 
# # q_unld_melt_sig_bin <-
# #   nls(est_q_unld_melt_bin ~ ((asym) / (1 + exp(-scal * (canopy_snowmelt.1 - xmid)))) - 
# #         ((asym) / (1 + exp(-scal * (0 - xmid)))),
# #       data = obs_mod_fltr_avg,
# #       start = list(asym = 2, scal = 13, xmid = 0.3))
# # 
# # RSS.p <- sum(residuals(q_unld_melt_sig_bin)^2)  # Residual sum of squares
# # TSS <- sum((obs_mod_fltr_avg$est_q_unld_melt_bin - mean(obs_mod_fltr_avg$est_q_unld_melt_bin))^2)  # Total sum of squares
# # r2_sig <- 1 - (RSS.p/TSS) |> round(2)  # R-squared measure
# # saveRDS(q_unld_melt_sig_bin, 'data/lm_q_drip_vs_q_unld_melt_bin_nls.rds')
# # new_data$mod_q_unld_melt_sigmoid <- predict(q_unld_melt_sig_bin, newdata = new_data)
# 
# # Create the plot
# ggplot(obs_mod_fltr_avg) + 
#   geom_point(aes(Subl_Cpy.1, est_q_unld_subl_bin)) +
#   geom_errorbar(aes(
#     x = Subl_Cpy.1,
#     ymax = sd_hi,
#     ymin = sd_low), width = 0.02) +
#   geom_point(
#     data = obs_mod_fltr,
#     aes(Subl_Cpy.1, est_q_unld_subl),
#     alpha = 0.3,
#     size = 1
#   ) +                      # show the linear model
#   geom_abline(intercept = 0, slope = slope, color = "red",    # Model line
#               linetype = "solid", size = 0.5) +
#   # geom_line(data = new_data, aes(Subl_Cpy.1, mod_q_unld_subl_nls)) +
#   # geom_line(data = new_data, aes(canopy_snowmelt.1, mod_q_unld_melt_sigmoid)) +
#   labs(
#     x = expression("Simulated Canopy Snow Sublimation Rate ("*kg ~ m^-2 ~ hr^-1*")"),
#     # y = expression(q[unld]^{melt} ~ "(" ~ kg ~ m^-2 ~ hr^-1 ~ ")"),
#     y = expression("Sublimation Unloading Rate ("*kg ~ m^-2 ~ hr^-1*")")
#   )
# 
# ggsave(
#   'figs/results/modelled_subl_unloading_w_obs_binned.png',
#   width = 5,
#   height = 4,
#   device = png
# )
# ## ERROR TABLE ----
# 
# obs_mod_fltr$pred_q_unld_subl_lm <- predict(q_unld_subl_lm_bin, newdata = obs_mod_fltr)
# obs_mod_fltr$pred_q_unld_subl_nls <- predict(q_unld_subl_nls_bin, newdata = obs_mod_fltr)
# # obs_mod_fltr$pred_q_unld_melt_sig <- predict(q_unld_melt_sig_bin, newdata = obs_mod_fltr)
# 
# obs_mod_fltr_long <- pivot_longer(obs_mod_fltr, starts_with('pred'))
# 
# ggplot(obs_mod_fltr_long) +
#   geom_point(
#     aes(est_q_unld_subl, value),
#     alpha = 0.1
#   ) +
#   geom_abline() + 
#   facet_wrap(~name)
# 
# lm_error_metrics <-   
#   obs_mod_fltr_long |> 
#   mutate(diff = est_q_unld_subl - value) |> 
#   group_by(name) |>
#   summarise(
#     # runtag = run_tag_updt,
#     MB = mean(diff, na.rm = T),
#     MAE = mean(abs(diff), na.rm = T),
#     RMSE = sqrt(mean(diff ^ 2, na.rm = T)),
#     # NRMSE = `RMS Error` / (max(observed, na.rm = TRUE) - min(observed, na.rm = TRUE)),
#     NRMSE = RMSE / mean(est_q_unld_subl, na.rm = T),
#     R = cor(est_q_unld_subl, value),
#     R2_gof = R^2,
#     R2_cd = 1 - sum(diff^2, na.rm = T) / sum((est_q_unld_subl - mean(est_q_unld_subl, na.rm = T))^2, na.rm = T)) |>
#   mutate(across(MB:R2_cd, round, digits = 3))
# 
# saveRDS(lm_error_metrics,
#         'data/modelled_subl_unloading_error_table.rds')
# 
# 
# # Create observed vs predicted plot
# ggplot(obs_mod_fltr, aes(x = est_q_unld_melt, y = mod_q_unld_melt, colour = tree_mm)) +
#   geom_point() +
#   geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
#   labs(
#     title = "Observed vs Predicted",
#     x = "Observed Values",
#     y = "Predicted Values"
#   ) +
#   theme_minimal()
