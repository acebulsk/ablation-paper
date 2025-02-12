# This script plots the performance of the updated canopy snow ablation model
# for select ablation events

library(tidyverse)

base_path <- 'figs/crhm-analysis/ablation-events/'
fig_tbl_tag <- 'updated_model_only_add_new_events'

# LOAD DATA ----

## OBSERVED ----
obs_tree <- rbind(obs_tree_cold  |> 
                    select(datetime, event_id, observed = tree_mm),
                  obs_tree_warm |> 
                    select(datetime, event_id, observed = tree_mm)) 
select_events_long <- obs_tree |> select(datetime, event_id)

# obs_tree <- obs_tree_post_sf
# select_events_long <- obs_tree |> select(datetime, event_id)

## MODELLED ----

### UPDATED ABLATION MODEL WITH PSP ----
# Select model run with all unloading events weighed tree snow load assimilated
prj_updt <- "ffr_closed_canopy_cc0.88_vector_based_new_ablation_psp"

# specify certain model run
# run_tag_updt <- "psp_temp_as_canopy_snow_temp"
# run_tag_updt <- "air_temp_as_canopy_snow_temp"
# run_tag_updt <- "icebulb_temp_as_canopy_snow_temp"
# run_tag_updt <- "obs_irtc_trunk_temp_as_canopy_snow_temp"
run_tag_updt <- "new_event_set_output.txt"
run_tag_updt <- "test_LW_in_eq_vf_4pir"


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

crhm_output_updated$Subl_Cpy.1 |> sum()

mod_tree <- crhm_output_updated |> 
  select(datetime, simulated_updated = Snow_load.1)

obs_mod_tree <- left_join(obs_tree, mod_tree)

# plot weighed tree obs vs. sim facet by event ----
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
    '_',
    run_tag_updt,
    '.png'
  ),
  width = 8,
  height = 6,
  device = png
)

# obs vs mod weighed tree error table 

obs_mod_tree_err_tbl_events <- obs_mod_tree |> 
  pivot_longer(starts_with('simulated')) |> 
  group_by(event_id) |> 
  mutate(diff = observed - value) |> 
  # group_by(name) |> 
  summarise(
    runtag = run_tag_updt,
    MB = mean(diff, na.rm = T),
    MAE = mean(abs(diff), na.rm = T),
    RMSE = sqrt(mean(diff ^ 2, na.rm = T)),
    # NRMSE = RMSE / (max(observed, na.rm = TRUE) - min(observed, na.rm = TRUE)),
    NRMSE = RMSE / mean(observed, na.rm = T),
    R = cor(observed, value),
    R2 = R^2) |> 
  mutate(across(MB:R2, round, digits = 3))

# saveRDS(obs_mod_tree_err_tbl_events, paste0(
#   'tbls/',
#   'obs_mod_canopy_snow_load_err_tbl_by_event_',
#   fig_tbl_tag,
#   '.rds'
# ))

obs_mod_tree_err_tbl_events_all <- readRDS(paste0(
  'tbls/',
  'obs_mod_canopy_snow_load_err_tbl_by_event_',
  fig_tbl_tag,
  '.rds'
))

obs_mod_tree_err_tbl_events_out <- rbind(obs_mod_tree_err_tbl_events_all,
                                         obs_mod_tree_err_tbl_events)

ggplot(obs_mod_tree_err_tbl_events_out, #|> 
         # filter(grepl('updated_q_unld_melt_load_new', runtag) | grepl('updated_q_unld_melt_load_lin_6', runtag)),
       aes(factor(event_id), MB, fill = runtag)) +
  geom_bar(stat = 'identity', position = 'dodge') 

ggplot(obs_mod_tree_err_tbl_events_out,
       aes(factor(event_id), RMSE, fill = runtag)) +
  geom_bar(stat = 'identity', position = 'dodge') 

saveRDS(obs_mod_tree_err_tbl_events_out,
        paste0(
          'tbls/',
          'obs_mod_canopy_snow_load_err_tbl_by_event_',
          fig_tbl_tag,
          '.rds'
        ))

obs_mod_tree_err_tbl_avgs <- obs_mod_tree_err_tbl_events |> 
  group_by(runtag) |> summarise(across(MB:R2, mean))

# write.csv(obs_mod_tree_err_tbl_avgs, paste0(
#   'tbls/',
#   'obs_mod_canopy_snow_load_err_tbl_avg_',
#   fig_tbl_tag,
#   '.csv'
# ), row.names = F)

obs_mod_tree_err_tbl_all <- read.csv(paste0(
  'tbls/',
  'obs_mod_canopy_snow_load_err_tbl_avg_',
  fig_tbl_tag,
  '.csv'
))

obs_mod_tree_err_tbl_out <- rbind(obs_mod_tree_err_tbl_all, obs_mod_tree_err_tbl_avgs)

write.csv(obs_mod_tree_err_tbl_out,
          paste0(
            'tbls/',
            'obs_mod_canopy_snow_load_err_tbl_avg_',
            fig_tbl_tag,
            '.csv'
          ), row.names = F)

# compare error with met
event_met <- readRDS('data/ablation_event_met_summary.rds')

event_error_met <- left_join(obs_mod_tree_err_tbl_events, event_met)

# below shows updated is more consistent across temperatures and wind speed

event_error_met |> 
  ggplot(aes(t_mean, MB, colour = rh_mean)) + 
  geom_point() 

event_error_met |> 
  ggplot(aes(Qsi_mean, MB, colour = u_mean)) + 
  geom_point() 

event_error_met |> 
  ggplot(aes(rh_mean, MB, colour = t_mean)) + 
  geom_point()

event_error_met |> 
  ggplot(aes(u_mean, MB, colour = rh_mean)) + 
  geom_point()

event_error_met |> 
  ggplot(aes(t_mean, RMSE, colour = rh_mean)) + 
  geom_point() 

event_error_met |> 
  ggplot(aes(rh_mean, RMSE, colour = t_mean)) + 
  geom_point()

event_error_met |> 
  ggplot(aes(u_mean, RMSE, colour = rh_mean)) + 
  geom_point()

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
         unload_subl = SUnloadSubl.1,
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
            sublimation = sublimation/total_ablation,
            unload_melt = unload_melt/total_ablation,
            unload_wind = unload_wind/total_ablation,
            unload_subl = unload_subl/total_ablation) 

mod_d_drip_smry_frac <- mod_d_drip |> 
  inner_join(select_events_long) |> 
  group_by(event_id) |> 
  summarise(across(snowmelt:total_ablation, sum),
            melt = (snowmelt+unload_melt)/total_ablation,
            wind = unload_wind/total_ablation,
            sublimation = (sublimation+unload_subl)/total_ablation) |> 
  mutate(across(c(melt, wind, sublimation), round, 2)) |> 
  select(event_id, melt, wind, sublimation)

saveRDS(mod_d_drip_smry_frac, 'data/ablation_event_fraction_ablation_processes.rds')

melt_events <- mod_d_drip_smry_frac |> 
  filter(melt > 0.8) |> 
  pull(event_id)

subl_events <- mod_d_drip_smry_frac |> 
  filter(sublimation > 0.5) |> 
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

# error table by ablation fractions ---- 

obs_mod_tree_err_tbl_events_ablt_frac <- obs_mod_tree_err_tbl_events |> 
  left_join(mod_d_drip_smry_frac) |> 
  mutate(event_type = case_when(
    event_id %in% melt_events ~ 'melt',
    event_id %in% subl_events ~ 'sublimation',
    event_id %in% wind_events ~ 'wind',
    TRUE ~ 'mixed'
  ))

write.csv(obs_mod_tree_err_tbl_events_ablt_frac,
          paste0(
            'tbls/',
            'obs_mod_canopy_snow_load_err_tbl_event_frac_ablation',
            run_tag_updt,
            '.csv'
          ), row.names = F)

obs_mod_tree_err_tbl_ablt_frac_avg <- obs_mod_tree_err_tbl_events_ablt_frac |> 
  group_by(event_type) |> 
  summarise(across(MB:sublimation, mean))

write.csv(obs_mod_tree_err_tbl_ablt_frac_avg,
          paste0(
            'tbls/',
            'obs_mod_canopy_snow_load_err_tbl_avg_frac_ablation',
            run_tag_updt,
            '.csv'
          ), row.names = F)

# plot weighed tree obs vs. sim facet by event type by ablation fractions ----
obs_mod_tree_typed <- obs_mod_tree |> 
  mutate(event_type = case_when(
    event_id %in% melt_events ~ 'melt',
    event_id %in% subl_events ~ 'sublimation',
    event_id %in% wind_events ~ 'wind',
    TRUE ~ 'mixed'
  ))

obs_mod_tree_typed |> 
  pivot_longer(!c(datetime, event_id, event_type)) |> 
  ggplot(aes(datetime, value, 
             colour = name)) +
  geom_line() +
  facet_wrap(event_type~event_id, scales = 'free') +
  ylab(expression("Canopy Snow Load (kg m"^-2*")")) +
  xlab(element_blank()) +
  labs(colour = 'Data Type') +
  theme(legend.position = 'bottom') +
  scale_x_datetime(date_labels = "%H") +
  ylim(c(0, NA))


ggsave(
  paste0(
    base_path,
    'obs_mod_canopy_snow_load_ablation_type',
    fig_tbl_tag,
    '_',
    run_tag_updt,
    '.png'
  ),
  width = 8,
  height = 6,
  device = png
)




# plot weighed tree vs crhm canopy snow load again but for characterized events ----  
obs_mod_tree |> 
  filter(event_id %in% melt_events) |>
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
    'obs_mod_canopy_snow_load_baseline_vs_updated_melt_events_',
    fig_tbl_tag,
    '_',
    run_tag_updt,
    '.png'
  ),
width = 8, 
height = 6,
device = png
)

obs_mod_tree |> 
  filter(event_id %in% wind_events) |>
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
    'obs_mod_canopy_snow_load_baseline_vs_updated_wind_events_',
    fig_tbl_tag,
    '_',
    run_tag_updt,
    '.png'
  ),
width = 8, 
height = 6,
device = png
)

obs_mod_tree |> 
  filter(event_id %in% subl_events) |>
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
    'obs_mod_canopy_snow_load_baseline_vs_updated_subl_events_',
    fig_tbl_tag,
    '_',
    run_tag_updt,
    '.png'
  ),
width = 8, 
height = 6,
device = png
)

# Plot ablation processes separately ----
mod_cml_dU <- mod_d_drip |> 
  inner_join(select_events_long) |> 
  pivot_longer(c(snowmelt,
                 unload_melt,
                 unload_wind,
                 unload_subl,
                 # unloading,
                 sublimation),
               values_to = 'dU') |> 
  group_by(event_id, name) |> 
  mutate(cml_dU = cumsum(dU),
         cml_dU = cml_dU - first(cml_dU)) |>  # so each event starts at 0
  select(datetime, event_id, name = name, value = cml_dU) |> 
  mutate(group = 'Simulated Ablation (mm)')

mod_cml_dU |> 
  mutate(event_type = case_when(
    event_id %in% melt_events ~ 'melt',
    event_id %in% subl_events ~ 'sublimation',
    event_id %in% wind_events ~ 'wind',
    TRUE ~ 'mixed'
  )) |> 
  ggplot(aes(datetime, value, 
             colour = name)) +
  geom_line() +
  facet_wrap(~event_id, scales = 'free') +
  # facet_wrap(event_type~event_id, scales = 'free') +
  # scale_colour_manual(values = c("sublimation" = "#E69F00",
  #                                "unloading" = "#009E73",
  #                                "snowmelt" = "#56B4E9"),
  #                     labels = c("sublimation" = "Sublimation",
  #                                "unloading" = "Unloading",
  #                                "snowmelt" = "Snowmelt")) +
  labs(colour = "Process:") +
  ylab(expression("Canopy Snow Ablation (kg m"^-2*")")) +
  xlab(element_blank()) +
  scale_x_datetime(date_labels = "%H") +
  theme(legend.position = 'bottom')

ggsave(
  paste0(
    base_path,
    'mod_canopy_snow_ablation_',
    fig_tbl_tag,
    '_',
    run_tag_updt,
    '.png'
  ),
width = 8, 
height = 6,
device = png
)

# # PLOT INDIVIDUALLY ---- 
# 
# obs_mod_tree_long <- obs_mod_tree |>
#   # left_join(baseline_mod_tree) |> 
#   pivot_longer(!c(datetime, event_id)) |> 
#   mutate(group = 'Canopy Snow Load (mm)')
# 
# for (event in unique(select_events_long$event_id)) {
#   
#   obs_mod_tree_err_tbl <- obs_mod_tree |> 
#     filter(event_id == event) |> 
#     mutate(diff = observed - simulated) |> 
#     # group_by(name) |> 
#     summarise(
#       MB = mean(diff, na.rm = T),
#       MAE = mean(abs(diff), na.rm = T),
#       RMSE = sqrt(mean(diff ^ 2, na.rm = T)),
#       # NRMSE = RMSE / (max(observed, na.rm = TRUE) - min(observed, na.rm = TRUE)),
#       NRMSE = RMSE / mean(observed, na.rm = T),
#       R = cor(observed, simulated),
#       `r^2` = R^2) |> 
#     mutate(across(MB:`r^2`, round, digits = 3))
#   
#   stats_text <- obs_mod_tree_err_tbl |> 
#     summarise(
#       text = sprintf(
#         "Mean Bias (mm): %.2f\nRMS Error (mm): %.2f\nNRMSE: %.2f\nR²: %.2f", 
#         MB, RMSE, NRMSE, `r^2`
#       )
#     ) |> 
#     pull(text)
#   
#   df_event <- obs_mod_tree_long |> 
#     rbind(mod_cml_dU) |>
#     filter(event_id == event)
#   
#   df_event |> 
#     ggplot(aes(datetime, value, 
#                colour = ifelse(group == 'Simulated Ablation (mm)', name, "canopy_load"), 
#                linetype = ifelse(group == "Canopy Snow Load (mm)", 
#                                  ifelse(name == "simulated", "solid", "dashed"), 
#                                  "solid"))) +
#     geom_line() +
#     facet_grid(rows = vars(group), scales = 'free') +
#     ggtitle(as.Date(event)) +
#     ylab(element_blank()) +
#     xlab(element_blank()) +
#     scale_colour_manual(values = c("canopy_load" = "black", 
#                                    "sublimation" = "#E69F00", 
#                                    "unloading" = "#009E73",
#                                    "snowmelt" = "#56B4E9"),
#                         labels = c("canopy_load" = "Canopy Load",
#                                    "sublimation" = "Sublimation",
#                                    "unloading" = "Unloading",
#                                    "snowmelt" = "Snowmelt")) +
#     scale_linetype_manual(
#       values = c("dashed" = "dashed", 
#                  "solid" = "solid"),
#       labels = c("dashed" = "Observed",
#                  "solid" = "Simulated")
#     ) +
#     labs(colour = "State/Process", linetype = "Data Type") +
#     geom_text(
#       data = df_event |> filter(group == "Canopy Snow Load (mm)"), 
#       aes(x = max(datetime), y = Inf, label = stats_text), 
#       inherit.aes = FALSE, 
#       hjust = 1, vjust = 1.1, 
#       size = 3.5, colour = "black"
#     )
#   
#   ggsave(
#     paste0(
#       'figs/crhm-analysis/subl_drip_unld/select_post_agu_events/',
#       prj,
#       '/crhm_vs_tree_subl_drip_unld_',
#       as.Date(event),
#       '_',
#       run_tag,
#       '.png'
#     ),
#     width = 8,
#     height = 6
#   )
#   
# }
