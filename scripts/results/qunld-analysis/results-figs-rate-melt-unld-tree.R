# This script analyzes the relationship between observed weighed tree ablation
# due to mass unloading of snow as it melts with simulated canopy snowmelt.
# Canopy snow unloading is determined as residual ablation after removing
# q_subl^veg, q_drip, q_unld^wind

library(tidyverse)

load_suffix <- 'fsd_closed_0.88'

bad_events <- c('2022-06-23',
                # '2023-06-21',
                '2022-04-21',
                '2022-04-23')

good_events <- mod_d_drip_smry_frac |> 
  filter(melt > 0.50,
         !event_id %in% bad_events)

# LOAD DATA ----

## model ---- 

# Select model run with all unloading events weighed tree snow load assimilated
prj <- "ffr_closed_canopy_cc0.88_cansnobal"

# specify certain model run
run_tag <- "updt_SW_to_obs_no_melt_unld"

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
            delmelt_veg_int.1 = sum(delmelt_veg_int.1),
            delsub_veg_int.1 = sum(delsub_veg_int.1),
            est_q_unld_melt = dL - delsub_veg_int.1 - delunld_wind_int.1 - delmelt_veg_int.1,
            unld_melt_ratio = est_q_unld_melt/delmelt_veg_int.1
            ) |> 
  filter(event_id %in% good_events$event_id)

# bin rates ---- 

### bin snowmelt rate (s-1) ----

# note zeros are not included in binning, to add set inlcude.lowest = T
min_sm <- 0
max_sm <- round(
  max(obs_mod$delmelt_veg_int.1, na.rm = T),3)
sm_step <- 0.5

sm_breaks <- seq(
  0,
  max_sm+0.5,
  sm_step)

sm_labs_seq <- label_bin_fn(bins = sm_breaks)

stopifnot(tail(sm_breaks, 1) > max(obs_mod$delmelt_veg_int.1, na.rm = T))
stopifnot(length(sm_labs_seq) + 1 == length(sm_breaks))

obs_mod$canopy_snowmelt_binned <- cut(obs_mod[,'delmelt_veg_int.1', drop = TRUE], sm_breaks)

obs_mod$canopy_snowmelt_labs <- cut(obs_mod[,'delmelt_veg_int.1', drop = TRUE], 
                                        sm_breaks, 
                                        labels = sm_labs_seq)

obs_mod$canopy_snowmelt_labs <- as.numeric(as.character(obs_mod$canopy_snowmelt_labs))

### bin weighed tree ----

# note zeros are not included in binning, to add set inlcude.lowest = T
min_tree <- 0
max_tree <- round(
  max(obs_mod$tree_mm, na.rm = T),0)
tree_step <- 4

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

# obs_mod |> pivot_longer(c(dL, tree_mm, m_s_veg.1, delsub_veg_int.1, delunld_wind_int.1, delmelt_veg_int.1)) |>
#   # filter(event_id %in% events_w_melt$event_id) |>
#   ggplot(aes(datetime, value)) +
#   geom_line()+
#   facet_grid(rows = vars(name), scales = 'free_y')
# plotly::ggplotly()

obs_mod_fltr <- obs_mod |> 
  filter(delmelt_veg_int.1 > 0,
         unld_melt_ratio < 20,
         # est_q_unld_melt >= 0,
         # event_id %in% events_w_melt,
         # !event_id %in% c('2022-06-14'),
         dL > 0.02,
         # tree_mm > 2,
         # hru_p.1 == 0,
         # hru_u.1 < 1
  )

ggplot(obs_mod_fltr, aes(delmelt_veg_int.1, est_q_unld_melt)) + 
  geom_point()
ggplot(obs_mod_fltr, aes(tree_mm, est_q_unld_melt)) + 
  geom_point()
ggplot(obs_mod_fltr, aes(tree_mm, unld_melt_ratio, colour = factor(event_id))) + 
  geom_point()
# option 1 cumulative within bins, gives very low instrument error as we have a
# large mass measured over the bin
obs_mod_fltr_binned <- obs_mod_fltr |>
  group_by(tree_labs, event_id) |>
  summarise(melt = sum(delmelt_veg_int.1),
            unld = sum(est_q_unld_melt),
            unld_melt_ratio = unld/melt) 

# option 1b avg over events
# obs_mod_fltr_binned <- obs_mod_fltr |>
#   group_by(tree_labs) |>
#   summarise(melt = sum(delmelt_veg_int.1),
#             unld = sum(est_q_unld_melt),
#             # strain_gauge_abs_error = mean(strain_gauge_abs_error),
#             # unld_melt_ratio_hi = (unld + strain_gauge_abs_error)/melt,
#             # unld_melt_ratio_lo = (unld - strain_gauge_abs_error)/melt,
# 
#             unld_melt_ratio = unld/melt) |>
#   filter(unld_melt_ratio < 5)

# option 2 calculate average within each bin at fifteen min intervals
# obs_mod_fltr_binned <- obs_mod_fltr |>
#   mutate(unld_melt_ratio = est_q_unld_melt/delmelt_veg_int.1) |>
#   group_by(tree_labs) |>
#   summarise(
#             sd = sd(unld_melt_ratio, na.rm = T),
#             unld_melt_ratio = mean(unld_melt_ratio),
#             unld_melt_ratio_hi = (unld_melt_ratio + sd),
#             unld_melt_ratio_lo = (unld_melt_ratio - sd),
# 
#             )

bin_unld_melt_lm <- lm(unld_melt_ratio ~ tree_labs, data = obs_mod_fltr_binned)
summary(bin_unld_subl_lm)
summary(bin_unld_melt_lm)
saveRDS(bin_unld_melt_lm, 'data/lm_q_drip_vs_q_unld_melt.rds')
# Extract the coefficient (slope) from the model
r2_adj_lm <- summary(bin_unld_melt_lm)$r.squared

ggplot(obs_mod_fltr_binned,
       aes(tree_labs, unld_melt_ratio)) +
  geom_point() +
  geom_smooth(method = "lm", se = F) +  # Use method="lm" for linear model
  annotate(
      'label',
      x = 4,
      y = 3,
      label = paste("R² =", round(r2_adj_lm, 2))
  ) +
  # geom_errorbar(aes(ymax = unld_melt_ratio_hi, ymin = unld_melt_ratio_lo), width = 1, alpha = 0.5)  +
  # lims(y = c(0, NA),
  #      x = c(0, NA)) +
  labs(
    x = "Canopy Snow Load (mm)",
    y = "Unloading to Melt Ratio (-)"
  ) 

ggsave(
  'figs/results/modelled_melt_unloading_ratio_vs_snow_load_bin.png',
  width = 5,
  height = 4,
  device = png
)

# obs_mod_fltr_binned_sm <- obs_mod_fltr |>
#   group_by(canopy_snowmelt_labs) |>
#   summarise(melt = sum(delmelt_veg_int.1),
#             unld = sum(est_q_unld_melt),
#             strain_gauge_abs_error = mean(strain_gauge_abs_error),
#             unld_melt_ratio_hi = (unld + strain_gauge_abs_error)/melt,
#             unld_melt_ratio_lo = (unld - strain_gauge_abs_error)/melt,
#             
#             unld_melt_ratio = unld/melt)
# 
# # ggplot(obs_mod_fltr_binned_sm, aes(melt, unld)) + 
# #   geom_point() +
# #   # annotate(
# #   #     'label',
# #   #     x = 3,
# #   #     y = 5,
# #   #     label = paste("R² =", round(r2_adj_lm, 2))
# #   # ) +
# #   # geom_errorbar(aes(ymax = unld_melt_ratio_hi, ymin = unld_melt_ratio_lo), width = 1, alpha = 0.5)  +
# #   lims(y = c(0, NA),
# #        x = c(0, NA)) +
# #   # geom_abline(intercept = 0, slope = slope, color = "red",    # Model line
# #   #             linetype = "solid", size = 0.5) +
# #   labs(
# #     x = expression("Simulated Canopy Snowmelt Rate ("*kg ~ m^-2 ~ hr^-1*")"),
# #     # y = expression(q[unld]^{melt} ~ "(" ~ kg ~ m^-2 ~ hr^-1 ~ ")"),
# #     y = expression("Melt Unloading Rate ("*kg ~ m^-2 ~ hr^-1*")")
# #   ) 
# # 
# # ggsave(
# #   'figs/results/modelled_melt_unloading_ratio_vs_snow_load_bin.png',
# #   width = 5,
# #   height = 4,
# #   device = png
# # )
# 
# obs_mod_fltr <- obs_mod |> 
#   filter(delmelt_veg_int.1 > 0,
#          est_q_unld_melt >= 0,
#          # event_id %in% events_w_melt,
#          dL > 0.1,
#          tree_mm > 2,
#          hru_p.1 == 0,
#          # hru_u.1 < 1
#   ) |> 
#   # convert mm/interval to mm/hour
#   mutate(est_q_unld_melt = est_q_unld_melt*4,
#          delmelt_veg_int.1 = delmelt_veg_int.1*4,
#          unld_melt_ratio = est_q_unld_melt/delmelt_veg_int.1) |> 
#   filter(unld_melt_ratio < 10)
# 
# # 15 min obs model ----
# 
# obs_mod_fltr_nomelt <- obs_mod |> 
#   filter(delmelt_veg_int.1 == 0,
#          dL > 0.1,
#          tree_mm > 2,
#          hru_p.1 == 0,
#          # hru_u.1 < 1
#          ) 
# # snow is observed to unload even when not melting see if this shows up in the data
# ggplot(obs_mod_fltr_nomelt, aes(x = hru_t.1, y = est_q_unld_melt, colour = factor(event_id))) +
#   geom_point(size = 2) +                      # Scatter plot of the data
#   # geom_abline(intercept = 0, slope = slope, color = "red",    # Model line
#   #             linetype = "solid", size = 0.5) +
#   labs(
#     x = "Air Temperature (°C)",
#     # y = expression(q[unld]^{melt} ~ "(" ~ kg ~ m^-2 ~ hr^-1 ~ ")"),
#     y = expression("Melt Unloading Rate ("*kg ~ m^-2 ~ hr^-1*")")
#   ) 
# 
# 
# q_unld_melt_lm <- lm(est_q_unld_melt ~ 0 + delmelt_veg_int.1, data = obs_mod_fltr)
# obs_mod_fltr$mod_q_unld_melt <- predict(q_unld_melt_lm)
# saveRDS(q_unld_melt_lm, 'data/lm_q_drip_vs_q_unld_melt.rds')
# # Extract the coefficient (slope) from the model
# slope <- coef(q_unld_melt_lm)[1]
# r2_adj_lm <- r_squared_no_intercept(q_unld_melt_lm)
# r2_adj_lm
# 
# # Create the plot
# ggplot(obs_mod_fltr, aes(x = delmelt_veg_int.1, y = est_q_unld_melt, colour = factor(event_id))) +
#   geom_point(size = 2) +                      # Scatter plot of the data
#   geom_abline(intercept = 0, slope = slope, color = "red",    # Model line
#               linetype = "solid", size = 0.5) +
#   labs(
#     x = expression("Simulated Canopy Snowmelt Rate ("*kg ~ m^-2 ~ hr^-1*")"),
#     # y = expression(q[unld]^{melt} ~ "(" ~ kg ~ m^-2 ~ hr^-1 ~ ")"),
#     y = expression("Melt Unloading Rate ("*kg ~ m^-2 ~ hr^-1*")")
#   ) 
# 
# ggsave(
#   'figs/results/modelled_melt_unloading_w_obs.png',
#   width = 5,
#   height = 4,
#   device = png
# )
# 
# ggplot(obs_mod_fltr, aes(x = tree_mm, y = est_q_unld_melt/delmelt_veg_int.1, colour = factor(event_id))) +
#   geom_point(size = 2) +                      # Scatter plot of the data
#   # geom_abline(intercept = 0, slope = slope, color = "red",    # Model line
#   #             linetype = "solid", size = 0.5) +
#   labs(
#     x = expression("Observed Snow Load ("*kg ~ m^-2 ~ hr^-1*")"),
#     # y = expression(q[unld]^{melt} ~ "(" ~ kg ~ m^-2 ~ hr^-1 ~ ")"),
#     y = "Unload to Melt Ratio"
#   ) 
# 
# # bin model ----
# 
# obs_mod_fltr_avg <- obs_mod_fltr |> 
#   group_by(delmelt_veg_int.1 = canopy_snowmelt_labs) |> 
#   summarise(est_q_unld_melt_bin = mean(est_q_unld_melt, na.rm = T),
#             q_unl_sd = sd(est_q_unld_melt, na.rm = T),
#             sd_low = ifelse((est_q_unld_melt_bin - q_unl_sd)<0,0, est_q_unld_melt_bin - q_unl_sd),
#             sd_hi = est_q_unld_melt_bin + q_unl_sd,
#             n = n()) |> 
#   filter(n >= 3)
# 
# ## bin model linear ---- 
# 
# # SELECTED THIS ONE BASED ON BEST ABLATION PERFORMANCE ON
# # ALL ABLATION EVENTS WITH WEIGHED TREE. EXP is a close second but only because
# # its larger positive bias balances out some of the negative bias from the cold
# # events. The linear model is better for the warm events.
# q_unld_melt_lm_bin <- lm(est_q_unld_melt_bin ~ 0 + delmelt_veg_int.1, data = obs_mod_fltr_avg)
# r2_adj_lm <- r_squared_no_intercept(q_unld_melt_lm_bin)
# obs_mod_fltr_avg$mod_q_unld_melt_lm <- predict(q_unld_melt_lm_bin)
# saveRDS(q_unld_melt_lm_bin, 'data/lm_q_drip_vs_q_unld_melt_bin_lm.rds')
# # Extract the coefficient (slope) from the model
# slope <- coef(q_unld_melt_lm_bin)[1]
# 
# ## bin model nls ---- 
# # q_unld_melt_nls_bin <-
# #   nls(est_q_unld_melt_bin ~ delmelt_veg_int.1 * a * exp(b * delmelt_veg_int.1),
# #       data = obs_mod_fltr_avg,
# #       start = list(a = 5, b = -1)
# #       )
# # RSS.p <- sum(residuals(q_unld_melt_nls_bin)^2)  # Residual sum of squares
# # TSS <- sum((obs_mod_fltr_avg$est_q_unld_melt_bin - mean(obs_mod_fltr_avg$est_q_unld_melt_bin))^2)  # Total sum of squares
# # r2_nls <- 1 - (RSS.p/TSS) |> round(2)  # R-squared measure
# # saveRDS(q_unld_melt_nls_bin, 'data/lm_q_drip_vs_q_unld_melt_bin_nls.rds')
# # new_data <- data.frame(
# #   delmelt_veg_int.1 = seq(
# #     0,
# #     max(obs_mod_fltr$delmelt_veg_int.1),
# #     length.out = 100
# #   )
# # )
# # new_data$mod_q_unld_melt_nls <- predict(q_unld_melt_nls_bin, newdata = new_data)
# 
# ## bin model sigmoid ---- 
# # q_unld_melt_sig_bin <-
# #   nls(est_q_unld_melt_bin ~ ((asym) / (1 + exp(-scal * (delmelt_veg_int.1 - xmid)))) - 
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
#   geom_point(aes(delmelt_veg_int.1, est_q_unld_melt_bin)) +
#   geom_errorbar(aes(
#     x = delmelt_veg_int.1, 
#     ymax = sd_hi,
#     ymin = sd_low), width = 0.02) +
#   geom_point(
#     data = obs_mod_fltr,
#     aes(delmelt_veg_int.1, est_q_unld_melt),
#     alpha = 0.3,
#     size = 1
#   ) +                      # show the linear model
#   geom_abline(intercept = 0, slope = slope, color = "red",    # Model line
#               linetype = "solid", size = 0.5) +
#   # geom_line(data = new_data, aes(delmelt_veg_int.1, mod_q_unld_melt_nls)) +
#   # geom_line(data = new_data, aes(delmelt_veg_int.1, mod_q_unld_melt_sigmoid)) +
#   labs(
#     x = expression("Simulated Canopy Snowmelt Rate ("*kg ~ m^-2 ~ hr^-1*")"),
#     # y = expression(q[unld]^{melt} ~ "(" ~ kg ~ m^-2 ~ hr^-1 ~ ")"),
#     y = expression("Melt Unloading Rate ("*kg ~ m^-2 ~ hr^-1*")")
#   ) 
# 
# ggsave(
#   'figs/results/modelled_melt_unloading_w_obs_binned.png',
#   width = 5,
#   height = 4,
#   device = png
# )
# ## ERROR TABLE ----
# 
# obs_mod_fltr$pred_q_unld_melt_lm <- predict(q_unld_melt_lm_bin, newdata = obs_mod_fltr)
# # obs_mod_fltr$pred_q_unld_melt_nls <- predict(q_unld_melt_nls_bin, newdata = obs_mod_fltr)
# # obs_mod_fltr$pred_q_unld_melt_sig <- predict(q_unld_melt_sig_bin, newdata = obs_mod_fltr)
# 
# obs_mod_fltr_long <- pivot_longer(obs_mod_fltr, starts_with('pred'))
# 
# ggplot(obs_mod_fltr_long) +
#   geom_point(
#     aes(est_q_unld_melt, value),
#     alpha = 0.1
#   ) +
#   geom_abline() + 
#   facet_wrap(~name)
# 
# lm_error_metrics <-   
#   obs_mod_fltr_long |> 
#   mutate(diff = est_q_unld_melt - value) |> 
#   group_by(name) |>
#   summarise(
#     # runtag = run_tag_updt,
#     MB = mean(diff, na.rm = T),
#     MAE = mean(abs(diff), na.rm = T),
#     RMSE = sqrt(mean(diff ^ 2, na.rm = T)),
#     # NRMSE = `RMS Error` / (max(tree_mm, na.rm = TRUE) - min(tree_mm, na.rm = TRUE)),
#     NRMSE = RMSE / mean(est_q_unld_melt, na.rm = T),
#     R = cor(est_q_unld_melt, value),
#     R2_gof = R^2,
#     R2_cd = 1 - sum(diff^2, na.rm = T) / sum((est_q_unld_melt - mean(est_q_unld_melt, na.rm = T))^2, na.rm = T)) |> 
#   mutate(across(MB:R2_cd, round, digits = 3))
# 
# saveRDS(lm_error_metrics,
#         'data/modelled_melt_unloading_error_table.rds')
# 
# 
# # Create tree_mm vs predicted plot
# ggplot(obs_mod_fltr, aes(x = est_q_unld_melt, y = mod_q_unld_melt, colour = tree_mm)) +
#   geom_point() +
#   geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
#   labs(
#     title = "Observed vs Predicted",
#     x = "Observed Values",
#     y = "Predicted Values"
#   ) +
#   theme_minimal()
