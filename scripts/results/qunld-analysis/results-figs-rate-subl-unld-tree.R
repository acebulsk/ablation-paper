# This script analyzes the relationship between observed weighed tree ablation
# due to mass unloading of snow as it melts with simulated canopy snowmelt.
# Canopy snow unloading is determined as residual ablation after removing
# q_subl^veg, q_drip, q_unld^wind

# NOTE: investigated this and could not find relationship

library(tidyverse)

load_suffix <- 'fsd_closed_0.88'

# LOAD DATA ----

## model ---- 

# Select model run with all unloading events weighed tree snow load assimilated
prj <- "ffr_closed_canopy_cc0.88_vector_based_new_ablation_psp"

# specify certain model run
run_tag <- "updated_q_unld_wind_pars"

path <- list.files(
  paste0(
    "../../analysis/crhm-analysis/output/",
    prj
  ),
  pattern = run_tag,
  full.names = T
)

crhm_output <- CRHMr::readOutputFile(
  path,
  timezone = 'Etc/GMT+6') |> 
  select(datetime, hru_t.1, hru_u.1, hru_p.1,
         Subl_Cpy.1:SUnloadWind.1)

### bin sublimation rate (s-1) ----

# note zeros are not included in binning, to add set inlcude.lowest = T
min_sub <- 0
max_sub <- round(
  max(crhm_output$Subl_Cpy.1*4, na.rm = T),3)
sub_step <- 0.2

sub_breaks <- seq(
  .2,
  max_sub+0.05,
  sub_step)

sub_breaks <- c(0, 0.05, 0.1, 0.15, sub_breaks)

sub_labs_seq <- label_bin_fn(bins = sub_breaks)

stopifnot(tail(sub_breaks, 1) > max(crhm_output$Subl_Cpy.1, na.rm = T))
stopifnot(length(sub_labs_seq) + 1 == length(sub_breaks))

crhm_output$canopy_sub_binned <- cut(crhm_output[,'Subl_Cpy.1', drop = TRUE]*4, sub_breaks, include.lowest = T)

crhm_output$canopy_sub_labs <- cut(crhm_output[,'Subl_Cpy.1', drop = TRUE]*4, 
                                   sub_breaks, 
                                   labels = sub_labs_seq,
                                   include.lowest = T
)

crhm_output$canopy_sub_labs <- as.numeric(as.character(crhm_output$canopy_sub_labs))

### bin temp  ----

# note zeros are not included in binning, to add set inlcude.lowest = T
min_temp <- round(
  min(crhm_output$hru_t.1, na.rm = T),3)
max_temp <- round(
  max(crhm_output$hru_t.1, na.rm = T),3)
temp_step <- 5

temp_breaks <- seq(
  min_temp,
  max_temp+2,
  temp_step)

temp_labs_seq <- label_bin_fn(bins = temp_breaks)

stopifnot(tail(temp_breaks, 1) > max(crhm_output$hru_t.1, na.rm = T))
stopifnot(length(temp_labs_seq) + 1 == length(temp_breaks))

crhm_output$temp_binned <- cut(crhm_output[,'hru_t.1', drop = TRUE], temp_breaks)

crhm_output$temp_labs <- cut(crhm_output[,'hru_t.1', drop = TRUE], 
                             temp_breaks, 
                             labels = temp_labs_seq)

crhm_output$temp_labs <- as.numeric(as.character(crhm_output$temp_labs))

## obs ----

### all events snow in canopy ----
# obs_tree <-
#   readRDS(paste0(
#     'data/clean-data/unloading_events_zero_weighed_tree_kg_m2_pre_post_cnpy_snow_',
#     load_suffix,
#     '.rds'
#   )) |>  filter(name == 'weighed tree')

#' # warm tree specific events
#' warm_events <- c(
#'   '2022-04-21',
#'   '2022-04-23',
#'   '2022-06-14',
#'   '2022-06-24',
#'   '2023-03-14',
#'   '2023-03-25',
#'   '2023-03-26',
#'   '2023-03-28',
#'   '2023-05-08',
#'   '2023-06-15',
#'   '2023-06-21'
#' )
#' 
#' obs_tree_warm <-
#'   readRDS(paste0(
#'     'data/clean-data/warm_tree_events_zero_weighed_tree_',
#'     load_suffix,
#'     '_kg_m2_post_cnpy_snow.rds'
#'   )) |> 
#'   filter(event_id %in% warm_events)
#' 
#' # cold tree events
#' cold_events <- c(
#'   '2022-03-02', 
#'   '2022-03-09',
#'   '2022-03-20', 
#'   '2022-03-24',  
#'   '2022-03-29',  
#'   '2022-12-01',
#'   '2023-01-28',
#'   '2023-02-24',
#'   '2023-02-26'
#'   #'2023-04-12' could add back if filter to start later.. also removed jjst so have clean 20 events
#' )
#' obs_tree_cold <-
#'   readRDS(paste0(
#'     'data/clean-data/all_tree_events_zero_weighed_tree_',
#'     load_suffix,
#'     '_kg_m2_post_cnpy_snow.rds'
#'   )) |> 
#'   filter(event_id %in% cold_events)
#' 
#' obs_tree <- rbind(obs_tree_cold  |> 
#'                     select(datetime, event_id, tree_mm),
#'                   obs_tree_warm |> 
#'                     select(datetime, event_id, tree_mm)) 

# stronger relationship using all the data
obs_tree <- q_unld_tree

# PLOT -----

w_tree_q_unld_15 <- obs_tree |>
  mutate(
    dL = lag(tree_mm) - tree_mm,
    dL = ifelse(dL < 0, 0, dL),
    q_unl = (dL / 15) * 60
  ) |>
  ungroup() |>
  filter(is.na(q_unl) == F,
         dL > 0) |> 
  select(datetime, event_id, tree_mm, dL)

obs_mod <- left_join(w_tree_q_unld_15, crhm_output) |> 
  mutate(est_q_unld_subl = dL - Subl_Cpy.1 - SUnloadWind.1 - canopy_snowmelt.1) 

subl_event_filter <- obs_mod |> 
  group_by(event_id) |> 
  summarise(subl = sum(Subl_Cpy.1),
            wind = sum(SUnloadWind.1),
            melt = sum(canopy_snowmelt.1),
            total = subl+wind+melt,
            frac_subl = subl/total,
            frac_wind = wind/total,
            frac_melt = melt/total) |> 
  filter(frac_subl > .6)

# obs_mod |> pivot_longer(c(dL, tree_mm, Subl_Cpy.1, SUnloadWind.1, canopy_snowmelt.1)) |> 
#   filter(event_id %in% subl_event_filter$event_id) |> 
#   ggplot(aes(datetime, value)) +
#   geom_line()+
#   facet_grid(rows = vars(name), cols = vars(event_id), scales = 'free')
# plotly::ggplotly()


# to hourly
# obs_mod <- obs_mod |> 
#   mutate(
#     datetime_hour = datetime - (15*60), # shift so datetime corresponds to start of measurement interval, this is to avoid measurements with the 00:00 timestamp which correspond to the average from 23:45 to 00:00 to be assigned incorrectly to the next day.
#     datetime_hour = format(datetime_hour, '%Y-%m-%d %H:00:00'),
#     datetime_hour = as.POSIXct(datetime_hour, tz = 'Etc/GMT+6'),
#     datetime_hour = datetime_hour + 60*60 
#     # datetime = lubridate::ceiling_date(datetime, unit = 'hour') # checked and this is the same as above
#   ) |> 
#   group_by(datetime_hour) |> 
#   mutate(n = n()) |> 
#   filter(n == 4) |> 
#   summarise(
#     tree_mm = nth(tree_mm, which.max(datetime)), # Get tree_mm with the largest datetime    t = mean(t),
#     dL = sum(dL),
#     hru_u.1 = mean(hru_u.1),
#     Subl_Cpy.1 = sum(Subl_Cpy.1),
#     SUnload.1 = sum(SUnload.1),
#     canopy_snowmelt.1 = sum(canopy_snowmelt.1),
#     SUnloadMelt.1 = sum(SUnloadMelt.1),
#     SUnloadWind.1 = sum(SUnloadWind.1)
#     ) |> 
#   as.data.frame() |> 
#   mutate(est_q_unld_subl = dL - Subl_Cpy.1 - SUnloadWind.1 - canopy_snowmelt.1) 

obs_mod_fltr <- obs_mod |> 
  filter(canopy_snowmelt.1 == 0,
         est_q_unld_subl >= 0,
         est_q_unld_subl < 1,
         dL > 0.01,
         tree_mm > 2,
         event_id %in% subl_event_filter$event_id,
         hru_u.1 < 1,
         hru_t.1 < 0
  ) |> 
  # convert mm/interval to mm/hour
  mutate(est_q_unld_subl = est_q_unld_subl*4,
         Subl_Cpy.1 = Subl_Cpy.1*4)  |> 
  mutate(`Canopy Load (mm)` = case_when(
    tree_mm < 4 ~ 'Light',
    # tree_mm >= 5 & tree_mm <= 10 ~ 'Med',
    is.na(tree_mm) ~ 'NA',
    TRUE  ~ 'Heavy'
  )) |> group_by(`Canopy Load (mm)`) |> 
  mutate(avg_w_tree = mean(tree_mm))

q_unld_subl_lm <- lm(est_q_unld_subl ~ 0 + Subl_Cpy.1:hru_t.1, data = obs_mod_fltr)
obs_mod_fltr$mod_q_unld_subl <- predict(q_unld_subl_lm)
saveRDS(q_unld_subl_lm, 'data/lm_q_drip_vs_q_unld_subl.rds')
# Extract the coefficient (slope) from the model
slope <- coef(q_unld_subl_lm)[1]

# Create the plot
ggplot(obs_mod_fltr, aes(x = Subl_Cpy.1, y = est_q_unld_subl)) +
  geom_point(aes(colour = hru_t.1), size = 2) +                      # Scatter plot of the data
  # geom_abline(intercept = 0, slope = slope, color = "red",    # Model line
  #             linetype = "solid", size = 0.5) +
  labs(
    x = expression("Simulated Canopy Snow Sublimation Rate ("*kg ~ m^-2 ~ hr^-1*")"),
    # y = expression(q[unld]^{melt} ~ "(" ~ kg ~ m^-2 ~ hr^-1 ~ ")"),
    y = expression("Sublimation Unloading Rate ("*kg ~ m^-2 ~ hr^-1*")")
  ) +
  scale_color_viridis_c()

ggsave(
  'figs/results/modelled_subl_unloading_w_obs.png',
  width = 4,
  height = 4,
  device = png
)

# bin model ----

obs_mod_fltr_avg <- obs_mod_fltr |> 
  group_by(Subl_Cpy.1 = canopy_sub_labs, avg_w_tree) |> 
  summarise(est_q_unld_subl_bin = mean(est_q_unld_subl, na.rm = T),
            q_unl_sd = sd(est_q_unld_subl, na.rm = T),
            sd_low = ifelse((est_q_unld_subl_bin - q_unl_sd)<0,0, est_q_unld_subl_bin - q_unl_sd),
            sd_hi = est_q_unld_subl_bin + q_unl_sd,
            n = n()) |> 
  filter(n >= 3)

## bin model linear ---- 

q_unld_subl_lm_bin <- lm(est_q_unld_subl_bin ~ 0 + Subl_Cpy.1, data = obs_mod_fltr_avg)
r2_adj_lm <- r_squared_no_intercept(q_unld_subl_lm_bin)
obs_mod_fltr_avg$mod_q_unld_subl_lm <- predict(q_unld_subl_lm_bin)
saveRDS(q_unld_subl_lm_bin, 'data/lm_q_drip_vs_q_unld_subl_bin_lm.rds')
# Extract the coefficient (slope) from the model
slope <- coef(q_unld_subl_lm_bin)[1]

## bin model nls ---- 
# q_unld_melt_nls_bin <-
#   nls(est_q_unld_melt_bin ~ canopy_snowmelt.1 * a * exp(b * canopy_snowmelt.1),
#       data = obs_mod_fltr_avg,
#       start = list(a = 5, b = -1)
#       )
# RSS.p <- sum(residuals(q_unld_melt_nls_bin)^2)  # Residual sum of squares
# TSS <- sum((obs_mod_fltr_avg$est_q_unld_melt_bin - mean(obs_mod_fltr_avg$est_q_unld_melt_bin))^2)  # Total sum of squares
# r2_nls <- 1 - (RSS.p/TSS) |> round(2)  # R-squared measure
# saveRDS(q_unld_melt_nls_bin, 'data/lm_q_drip_vs_q_unld_melt_bin_nls.rds')
# new_data <- data.frame(
#   canopy_snowmelt.1 = seq(
#     0,
#     max(obs_mod_fltr$canopy_snowmelt.1),
#     length.out = 100
#   )
# )
# new_data$mod_q_unld_melt_nls <- predict(q_unld_melt_nls_bin, newdata = new_data)

## bin model sigmoid ---- 
# q_unld_melt_sig_bin <-
#   nls(est_q_unld_melt_bin ~ ((asym) / (1 + exp(-scal * (canopy_snowmelt.1 - xmid)))) - 
#         ((asym) / (1 + exp(-scal * (0 - xmid)))),
#       data = obs_mod_fltr_avg,
#       start = list(asym = 2, scal = 13, xmid = 0.3))
# 
# RSS.p <- sum(residuals(q_unld_melt_sig_bin)^2)  # Residual sum of squares
# TSS <- sum((obs_mod_fltr_avg$est_q_unld_melt_bin - mean(obs_mod_fltr_avg$est_q_unld_melt_bin))^2)  # Total sum of squares
# r2_sig <- 1 - (RSS.p/TSS) |> round(2)  # R-squared measure
# saveRDS(q_unld_melt_sig_bin, 'data/lm_q_drip_vs_q_unld_melt_bin_nls.rds')
# new_data$mod_q_unld_melt_sigmoid <- predict(q_unld_melt_sig_bin, newdata = new_data)

# Create the plot
ggplot(obs_mod_fltr_avg) + 
  geom_point(aes(Subl_Cpy.1, est_q_unld_subl_bin, colour = factor(avg_w_tree))) +
  # geom_errorbar(aes(
  #   x = Subl_Cpy.1, 
  #   ymax = sd_hi,
  #   ymin = sd_low), width = 0.02) +
  geom_point(
    data = obs_mod_fltr,
    aes(Subl_Cpy.1, est_q_unld_subl),
    alpha = 0.3,
    size = 1
  ) +                      # show the linear model
  # geom_abline(intercept = 0, slope = slope, color = "red",    # Model line
  #             linetype = "solid", size = 0.5) +
  # geom_line(data = new_data, aes(canopy_snowmelt.1, mod_q_unld_melt_nls)) +
  # geom_line(data = new_data, aes(canopy_snowmelt.1, mod_q_unld_melt_sigmoid)) +
  labs(
    x = expression("Simulated Canopy Snowmelt Rate ("*kg ~ m^-2 ~ hr^-1*")"),
    # y = expression(q[unld]^{melt} ~ "(" ~ kg ~ m^-2 ~ hr^-1 ~ ")"),
    y = expression("Sublimation Unloading Rate ("*kg ~ m^-2 ~ hr^-1*")")
  )

ggsave(
  'figs/results/modelled_subl_unloading_w_obs_binned.png',
  width = 5,
  height = 4,
  device = png
)
## ERROR TABLE ----

obs_mod_fltr$pred_q_unld_melt_lm <- predict(q_unld_melt_lm_bin, newdata = obs_mod_fltr)
# obs_mod_fltr$pred_q_unld_melt_nls <- predict(q_unld_melt_nls_bin, newdata = obs_mod_fltr)
# obs_mod_fltr$pred_q_unld_melt_sig <- predict(q_unld_melt_sig_bin, newdata = obs_mod_fltr)

obs_mod_fltr_long <- pivot_longer(obs_mod_fltr, starts_with('pred'))

ggplot(obs_mod_fltr_long) +
  geom_point(
    aes(est_q_unld_melt, value),
    alpha = 0.1
  ) +
  geom_abline() + 
  facet_wrap(~name)

lm_error_metrics <-   
  obs_mod_fltr_long |> 
  mutate(diff = est_q_unld_melt - value) |> 
  group_by(name) |>
  summarise(
    # runtag = run_tag_updt,
    MB = mean(diff, na.rm = T),
    MAE = mean(abs(diff), na.rm = T),
    RMSE = sqrt(mean(diff ^ 2, na.rm = T)),
    # NRMSE = `RMS Error` / (max(observed, na.rm = TRUE) - min(observed, na.rm = TRUE)),
    NRMSE = RMSE / mean(est_q_unld_melt, na.rm = T),
    R = cor(est_q_unld_melt, value),
    R2_gof = R^2,
    R2_cd = 1 - sum(diff^2, na.rm = T) / sum((value - mean(value, na.rm = T))^2, na.rm = T)) |> 
  mutate(across(MB:R2_cd, round, digits = 3))

saveRDS(lm_error_metrics,
        'data/modelled_melt_unloading_error_table.rds')


# Create observed vs predicted plot
ggplot(obs_mod_fltr, aes(x = est_q_unld_melt, y = mod_q_unld_melt, colour = tree_mm)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(
    title = "Observed vs Predicted",
    x = "Observed Values",
    y = "Predicted Values"
  ) +
  theme_minimal()
