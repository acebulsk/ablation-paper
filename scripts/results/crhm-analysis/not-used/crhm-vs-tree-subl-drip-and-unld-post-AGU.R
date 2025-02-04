# This script plots the selected events used at AGU 2024 and now combined into one df
library(tidyverse)

base_path <- 'figs/crhm-analysis/ablation-events/'
fig_tbl_tag <- 'updated_model_only'

# LOAD DATA ----

## OBSERVED ----
obs_tree <- rbind(obs_tree_cold  |> 
                    select(datetime, event_id, observed = tree_mm),
                  obs_tree_warm |> 
                    select(datetime, event_id, observed = tree_mm)) 
select_events_long <- obs_tree |> select(datetime, event_id)

## MODELLED ----

### UPDATED ABLATION MODEL WITH PSP ----
# Select model run with all unloading events weighed tree snow load assimilated
prj_updt <- "ffr_closed_canopy_cc0.88_vector_based_new_ablation_psp"

# specify certain model run
#run_tag <- "turn_off_duration_based_ablation_output.txt" # baseline prior to ratio based unloading
run_tag_updt <- "try_wind_unld_fn_w_temp_parameter"
# run_tag <- "testing123"

path <- list.files(
  paste0(
    "../../analysis/crhm-analysis/output/",
    prj_updt
  ),
  pattern = run_tag_updt,
  full.names = T
)

stopifnot(length(path) == 1)

crhm_output_updated <- CRHMr::readOutputFile(
  path,
  timezone = 'Etc/GMT+6') |> filter(datetime %in% obs_tree$datetime)

mod_tree <- crhm_output_updated |> 
  select(datetime, simulated_updated = Snow_load.1)

obs_mod_tree <- left_join(obs_tree, mod_tree)

# plot weighed tree obs vs. sim facet by event ----
#TODO change to mark 00 at at least every midnight
obs_mod_tree |> 
  pivot_longer(!c(datetime, event_id)) |> 
  ggplot(aes(datetime, value, 
             colour = name)) +
  geom_line() +
  facet_wrap(~event_id, scales = 'free') +
  ylab(expression("Canopy Snow Load (kg m"^-2*")")) +
  xlab(element_blank()) +
  labs(colour = 'Data Type') +
  theme(legend.position = 'bottom') +
  scale_x_datetime(date_labels = "%H")

ggsave(
  paste0(
    base_path,
    'obs_mod_canopy_snow_load_',
    fig_tbl_tag,
    '.png'
  ),
  width = 8,
  height = 6,
  device = png
)

# generate error table avg all events
obs_mod_tree_err_tbl <- obs_mod_tree |> 
  pivot_longer(starts_with('simulated')) |> 
  group_by(name) |> 
  mutate(diff = observed - value) |> 
  # group_by(name) |> 
  summarise(
    runtag = run_tag_updt,
    MB = mean(diff, na.rm = T),
    MAE = mean(abs(diff), na.rm = T),
    RMSE = sqrt(mean(diff ^ 2, na.rm = T)),
    # NRMSE = `RMS Error` / (max(observed, na.rm = TRUE) - min(observed, na.rm = TRUE)),
    NRMSE = RMSE / mean(observed, na.rm = T),
    R = cor(observed, value),
    R2 = R^2) |> 
  mutate(across(MB:R2, round, digits = 3))

obs_mod_tree_err_tbl

all_err_tbl_avgs <- read.csv(
  paste0(
    'tbls/',
    'obs_mod_canopy_snow_load_err_tbl_avg_',
    fig_tbl_tag,
    '.csv'
  )
)

all_err_tbl_avgs_out <- rbind(obs_mod_tree_err_tbl,
                              all_err_tbl_avgs)

write.csv(
  all_err_tbl_avgs_out,
  paste0(
    'tbls/',
    'obs_mod_canopy_snow_load_err_tbl_avg_',
    fig_tbl_tag,
    '.csv'
  )
)

# obs vs mod weighed tree error table 

obs_mod_tree_err_tbl_events <- obs_mod_tree |> 
  pivot_longer(starts_with('simulated')) |> 
  group_by(event_id, name) |> 
  mutate(diff = observed - value) |> 
  # group_by(name) |> 
  summarise(
    `Mean Bias` = mean(diff, na.rm = T),
    MAE = mean(abs(diff), na.rm = T),
    `RMS Error` = sqrt(mean(diff ^ 2, na.rm = T)),
    # NRMSE = `RMS Error` / (max(observed, na.rm = TRUE) - min(observed, na.rm = TRUE)),
    NRMSE = `RMS Error` / mean(observed, na.rm = T),
    R = cor(observed, value),
    `r^2` = R^2) |> 
  mutate(across(`Mean Bias`:`r^2`, round, digits = 3))

write.csv(obs_mod_tree_err_tbl_events,
          paste0(
            'tbls/',
            'obs_mod_canopy_snow_load_err_tbl_by_event_',
            fig_tbl_tag,
            '.csv'
          ))

saveRDS(obs_mod_tree_err_tbl_events,
        paste0(
          'tbls/',
          'obs_mod_canopy_snow_load_err_tbl_by_event_',
          fig_tbl_tag,
          '.rds'
        ))

# compare error with met
event_met <- readRDS('data/ablation_event_met_summary.rds')

event_error_met <- left_join(obs_mod_tree_err_tbl_events, event_met)

# below shows updated is more consistent across temperaturs and wind speed

event_error_met |> 
  ggplot(aes(t_mean, `Mean Bias`, colour = rh_mean)) + 
  geom_point() + 
  facet_wrap(~name)

event_error_met |> 
  ggplot(aes(rh_mean, `Mean Bias`, colour = t_mean)) + 
  geom_point()+ 
  facet_wrap(~name)

event_error_met |> 
  ggplot(aes(u_mean, `Mean Bias`, colour = rh_mean)) + 
  geom_point()+ 
  facet_wrap(~name)

event_error_met |> 
  ggplot(aes(t_mean, `RMS Error`, colour = rh_mean)) + 
  geom_point() + 
  facet_wrap(~name)

event_error_met |> 
  ggplot(aes(rh_mean, `RMS Error`, colour = t_mean)) + 
  geom_point()+ 
  facet_wrap(~name)

event_error_met |> 
  ggplot(aes(u_mean, `RMS Error`, colour = rh_mean)) + 
  geom_point()+ 
  facet_wrap(~name)

# calculate ablation processes separately for each event ----- 

## updated crhm model ---- 

mod_d_drip <- crhm_output_updated |> 
  mutate(total_ablation = canopy_snowmelt.1 + SUnload.1 + Subl_Cpy.1) |> 
  select(datetime,
         snowmelt = canopy_snowmelt.1,
         unloading = SUnload.1,
         sublimation = Subl_Cpy.1,
         unload_melt = SUnloadMelt.1,
         unload_wind = SUnloadWind.1,
         total_ablation)

mod_d_drip_smry <- mod_d_drip |> 
  inner_join(select_events_long) |> 
  group_by(event_id) |> 
  summarise(across(snowmelt:total_ablation, sum))

mod_d_drip_smry_frac <- mod_d_drip |> 
  inner_join(select_events_long) |> 
  group_by(event_id) |> 
  summarise(across(snowmelt:total_ablation, sum),
            snowmelt = snowmelt/total_ablation,
            unloading = unloading/total_ablation,
            unload_melt = unload_melt/total_ablation,
            unload_wind = unload_wind/total_ablation,
            sublimation = sublimation/total_ablation) 

mod_d_drip_smry_frac <- mod_d_drip |> 
  inner_join(select_events_long) |> 
  group_by(event_id) |> 
  summarise(across(snowmelt:total_ablation, sum),
            melt = (snowmelt+unload_melt)/total_ablation,
            wind = unload_wind/total_ablation,
            sublimation = sublimation/total_ablation) |> 
  mutate(across(c(melt, wind, sublimation), round, 2)) |> 
  select(event_id, melt, wind, sublimation)

saveRDS(mod_d_drip_smry_frac, 'data/ablation_event_fraction_ablation_processes.rds')

melt_events <- mod_d_drip_smry_frac |> 
  filter(melt > 0.8) |> 
  pull(event_id)

subl_events <- mod_d_drip_smry_frac |> 
  filter(sublimation > 0.8) |> 
  pull(event_id)

wind_events <- mod_d_drip_smry_frac |> 
  filter(wind > 0.6) |> 
  pull(event_id)

mixed_events <- mod_d_drip_smry_frac |> 
  filter(wind < 0.6,
         melt < 0.8,
         sublimation < 0.8) |> 
  pull(event_id)

length(mod_d_drip_smry_frac$event_id |> unique()) == length(c(melt_events,
                                                              subl_events,
                                                              wind_events,
                                                              mixed_events))

# plot weighed tree vs crhm canopy snow load again but for characterized events ----  
obs_mod_tree |> 
  filter(event_id %in% melt_events) |>
  pivot_longer(!c(datetime, event_id)) |> 
  ggplot(aes(datetime, value, 
             linetype = name)) +
  geom_line() +
  facet_wrap(~event_id, scales = 'free') +
  ylab(expression("Canopy Snow Load (kg m"^-2*")")) +
  xlab(element_blank()) +
  labs(linetype = 'Data Type') +
  theme(legend.position = 'bottom') +
  scale_x_datetime(date_labels = "%H")


ggsave(paste0(
  'figs/crhm-analysis/subl_drip_unld/select_post_agu_events/',
  fig_tbl_tag,
  '/obs_mod_canopy_snow_load_baseline_vs_updated_melt_events.png'
),
width = 8, 
height = 6,
device = png
)

obs_mod_tree |> 
  filter(event_id %in% wind_events) |>
  pivot_longer(!c(datetime, event_id)) |> 
  ggplot(aes(datetime, value, 
             linetype = name)) +
  geom_line() +
  facet_wrap(~event_id, scales = 'free') +
  ylab(expression("Canopy Snow Load (kg m"^-2*")")) +
  xlab(element_blank()) +
  labs(linetype = 'Data Type') +
  theme(legend.position = 'bottom') +
  scale_x_datetime(date_labels = "%H")


ggsave(paste0(
  'figs/crhm-analysis/subl_drip_unld/select_post_agu_events/',
  fig_tbl_tag,
  '/obs_mod_canopy_snow_load_baseline_vs_updated_wind_events.png'
),
width = 8, 
height = 6,
device = png
)

obs_mod_tree |> 
  filter(event_id %in% subl_events) |>
  pivot_longer(!c(datetime, event_id)) |> 
  ggplot(aes(datetime, value, 
             linetype = name)) +
  geom_line() +
  facet_wrap(~event_id, scales = 'free') +
  ylab(expression("Canopy Snow Load (kg m"^-2*")")) +
  xlab(element_blank()) +
  labs(linetype = 'Data Type') +
  theme(legend.position = 'bottom') +
  scale_x_datetime(date_labels = "%H")


ggsave(paste0(
  'figs/crhm-analysis/subl_drip_unld/select_post_agu_events/',
  fig_tbl_tag,
  '/obs_mod_canopy_snow_load_baseline_vs_updated_subl_events.png'
),
width = 8, 
height = 6,
device = png
)

obs_mod_tree |> 
  filter(event_id %in% mixed_events) |>
  pivot_longer(!c(datetime, event_id)) |> 
  ggplot(aes(datetime, value, 
             linetype = name)) +
  geom_line() +
  facet_wrap(~event_id, scales = 'free') +
  ylab(expression("Canopy Snow Load (kg m"^-2*")")) +
  xlab(element_blank()) +
  labs(linetype = 'Data Type') +
  theme(legend.position = 'bottom') +
  scale_x_datetime(date_labels = "%H")


ggsave(paste0(
  'figs/crhm-analysis/subl_drip_unld/select_post_agu_events/',
  fig_tbl_tag,
  '/obs_mod_canopy_snow_load_baseline_vs_updated_mixed_events.png'
),
width = 8, 
height = 6,
device = png
)

# Plot ablation processes separately
mod_cml_dU <- mod_d_drip |> 
  inner_join(select_events_long) |> 
  pivot_longer(c(snowmelt,
                 # unload_melt,
                 # unload_wind,
                 unloading,
                 sublimation),
               values_to = 'dU') |> 
  group_by(event_id, name) |> 
  mutate(cml_dU = cumsum(dU),
         cml_dU = cml_dU - first(cml_dU)) |>  # so each event starts at 0
  select(datetime, event_id, name = name, value = cml_dU) |> 
  mutate(group = 'Simulated Ablation (mm)')

mod_cml_dU |> 
  ggplot(aes(datetime, value, 
             colour = name)) +
  geom_line() +
  facet_wrap(vars(event_id), scales = 'free') +
  scale_colour_manual(values = c("sublimation" = "#E69F00",
                                 "unloading" = "#009E73",
                                 "snowmelt" = "#56B4E9"),
                      labels = c("sublimation" = "Sublimation",
                                 "unloading" = "Unloading",
                                 "snowmelt" = "Snowmelt")) +
  labs(colour = "Process:") +
  ylab(expression("Canopy Snow Ablation (kg m"^-2*")")) +
  xlab(element_blank()) +
  scale_x_datetime(date_labels = "%H") +
  theme(legend.position = 'bottom')

ggsave(paste0(
  'figs/crhm-analysis/subl_drip_unld/select_post_agu_events/',
  prj_updt,
  '/',
  'mod_canopy_snow_ablation_',
  run_tag_updt,
  '_updated.png'
),
width = 8, 
height = 6,
device = png
)

mod_cml_dU |> 
  filter(event_id %in% melt_events) |> 
  ggplot(aes(datetime, value, 
             colour = name)) +
  geom_line() +
  facet_wrap(vars(event_id), scales = 'free') +
  scale_colour_manual(values = c("sublimation" = "#E69F00",
                                 "unloading" = "#009E73",
                                 "snowmelt" = "#56B4E9"),
                      labels = c("sublimation" = "Sublimation",
                                 "unloading" = "Unloading",
                                 "snowmelt" = "Snowmelt")) +
  labs(colour = "Process:") +
  ylab(expression("Canopy Snow Ablation (kg m"^-2*")")) +
  xlab(element_blank()) +
  scale_x_datetime(date_labels = "%H") +
  theme(legend.position = 'bottom')

ggsave(paste0(
  'figs/crhm-analysis/subl_drip_unld/select_post_agu_events/',
  prj_updt,
  '/',
  'mod_canopy_snow_ablation_',
  run_tag_updt,
  '_updated_melt_events.png'
),
width = 8, 
height = 6,
device = png
)

mod_cml_dU |> 
  filter(event_id %in% wind_events) |> 
  ggplot(aes(datetime, value, 
             colour = name)) +
  geom_line() +
  facet_wrap(vars(event_id), scales = 'free') +
  scale_colour_manual(values = c("sublimation" = "#E69F00",
                                 "unloading" = "#009E73",
                                 "snowmelt" = "#56B4E9"),
                      labels = c("sublimation" = "Sublimation",
                                 "unloading" = "Unloading",
                                 "snowmelt" = "Snowmelt")) +
  labs(colour = "Process:") +
  ylab(expression("Canopy Snow Ablation (kg m"^-2*")")) +
  xlab(element_blank()) +
  scale_x_datetime(date_labels = "%H") +
  theme(legend.position = 'bottom')

ggsave(paste0(
  'figs/crhm-analysis/subl_drip_unld/select_post_agu_events/',
  prj_updt,
  '/',
  'mod_canopy_snow_ablation_',
  run_tag_updt,
  '_updated_wind_events.png'
),
width = 8, 
height = 6,
device = png
)

mod_cml_dU |> 
  filter(event_id %in% subl_events) |> 
  ggplot(aes(datetime, value, 
             colour = name)) +
  geom_line() +
  facet_wrap(vars(event_id), scales = 'free') +
  scale_colour_manual(values = c("sublimation" = "#E69F00",
                                 "unloading" = "#009E73",
                                 "snowmelt" = "#56B4E9"),
                      labels = c("sublimation" = "Sublimation",
                                 "unloading" = "Unloading",
                                 "snowmelt" = "Snowmelt")) +
  labs(colour = "Process:") +
  ylab(expression("Canopy Snow Ablation (kg m"^-2*")")) +
  xlab(element_blank()) +
  scale_x_datetime(date_labels = "%H") +
  theme(legend.position = 'bottom')

ggsave(paste0(
  'figs/crhm-analysis/subl_drip_unld/select_post_agu_events/',
  prj_updt,
  '/',
  'mod_canopy_snow_ablation_',
  run_tag_updt,
  '_updated_subl_events.png'
),
width = 8, 
height = 6,
device = png
)

## Show baseline partitioning ---- 

# get the mod ablation partition

mod_d_drip <- crhm_output_baseline |> 
  mutate(crhm_canopy_snowmelt = canopy_snowmelt.1,
         crhm_drip = drip_cpy.1,
         crhm_unload = SUnload.1,
         # unload_test = canopy_snowmelt.1^2,
         crhm_sbl = Subl_Cpy.1) |> 
  select(datetime, snowmelt = crhm_canopy_snowmelt,
         crhm_drip, unloading = crhm_unload, sublimation = crhm_sbl)

mod_cml_dU <- mod_d_drip |> 
  inner_join(select_events_long) |> 
  pivot_longer(c(snowmelt,
                 # crhm_drip,
                 # unload_test,
                 unloading,
                 sublimation),
               values_to = 'dU') |> 
  group_by(event_id, name) |> 
  mutate(cml_dU = cumsum(dU),
         cml_dU = cml_dU - first(cml_dU)) |>  # so each event starts at 0
  select(datetime, event_id, name = name, value = cml_dU) |> 
  mutate(group = 'Simulated Ablation (mm)')

mod_cml_dU |> 
  ggplot(aes(datetime, value, 
             colour = name)) +
  geom_line() +
  facet_wrap(vars(event_id), scales = 'free') +
  scale_colour_manual(values = c("canopy_load" = "black", 
                                 "sublimation" = "#E69F00", 
                                 "unloading" = "#009E73",
                                 "snowmelt" = "#56B4E9"),
                      labels = c("canopy_load" = "Canopy Load",
                                 "sublimation" = "Sublimation",
                                 "unloading" = "Unloading",
                                 "snowmelt" = "Snowmelt")) +
  labs(colour = "Process") +
  ylab(expression("Canopy Snow Ablation (kg m"^-2*")")) +
  xlab(element_blank()) +
  scale_x_datetime(date_labels = "%H") +
  theme(legend.position = 'bottom')

ggsave(paste0(
  'figs/crhm-analysis/subl_drip_unld/select_post_agu_events/',
  prj_base,
  '/',
  'mod_canopy_snow_ablation_',
  run_tag_updt,
  '_baseline.png'
),
width = 8, 
height = 6,
device = png
)

# PLOT INDIVIDUALLY ---- 

obs_mod_tree_long <- obs_mod_tree |>
  # left_join(baseline_mod_tree) |> 
  pivot_longer(!c(datetime, event_id)) |> 
  mutate(group = 'Canopy Snow Load (mm)')

for (event in unique(select_events_long$event_id)) {
  
  obs_mod_tree_err_tbl <- obs_mod_tree |> 
    filter(event_id == event) |> 
    mutate(diff = observed - simulated) |> 
    # group_by(name) |> 
    summarise(
      `Mean Bias` = mean(diff, na.rm = T),
      MAE = mean(abs(diff), na.rm = T),
      `RMS Error` = sqrt(mean(diff ^ 2, na.rm = T)),
      # NRMSE = `RMS Error` / (max(observed, na.rm = TRUE) - min(observed, na.rm = TRUE)),
      NRMSE = `RMS Error` / mean(observed, na.rm = T),
      R = cor(observed, simulated),
      `r^2` = R^2) |> 
    mutate(across(`Mean Bias`:`r^2`, round, digits = 3))
  
  stats_text <- obs_mod_tree_err_tbl |> 
    summarise(
      text = sprintf(
        "Mean Bias (mm): %.2f\nRMS Error (mm): %.2f\nNRMSE: %.2f\nR²: %.2f", 
        `Mean Bias`, `RMS Error`, NRMSE, `r^2`
      )
    ) |> 
    pull(text)
    
  df_event <- obs_mod_tree_long |> 
    rbind(mod_cml_dU) |>
    filter(event_id == event)
  
  df_event |> 
    ggplot(aes(datetime, value, 
               colour = ifelse(group == 'Simulated Ablation (mm)', name, "canopy_load"), 
               linetype = ifelse(group == "Canopy Snow Load (mm)", 
                                 ifelse(name == "simulated", "solid", "dashed"), 
                                 "solid"))) +
    geom_line() +
    facet_grid(rows = vars(group), scales = 'free') +
    ggtitle(as.Date(event)) +
    ylab(element_blank()) +
    xlab(element_blank()) +
    scale_colour_manual(values = c("canopy_load" = "black", 
                                   "sublimation" = "#E69F00", 
                                   "unloading" = "#009E73",
                                   "snowmelt" = "#56B4E9"),
                        labels = c("canopy_load" = "Canopy Load",
                                   "sublimation" = "Sublimation",
                                   "unloading" = "Unloading",
                                   "snowmelt" = "Snowmelt")) +
    scale_linetype_manual(
      values = c("dashed" = "dashed", 
                 "solid" = "solid"),
      labels = c("dashed" = "Observed",
                 "solid" = "Simulated")
    ) +
    labs(colour = "State/Process", linetype = "Data Type") +
    geom_text(
      data = df_event |> filter(group == "Canopy Snow Load (mm)"), 
      aes(x = max(datetime), y = Inf, label = stats_text), 
      inherit.aes = FALSE, 
      hjust = 1, vjust = 1.1, 
      size = 3.5, colour = "black"
    )
  
  ggsave(
    paste0(
      'figs/crhm-analysis/subl_drip_unld/select_post_agu_events/',
      prj,
      '/crhm_vs_tree_subl_drip_unld_',
      as.Date(event),
      '_',
      run_tag,
      '.png'
    ),
    width = 8,
    height = 6
  )
  
}
