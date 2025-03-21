# This script plots the selected events used at AGU 2024 and now combined into one df
# It compares the observed weighed tree ablation against the following models:
# - CRHM baseline (i.e., HP98 and Ellis et al., 2010)
# - crhm_updated (working updates to ablation routine for 2025 paper)
# - Roesch 2001
# - Andreadis 2009 

base_path <- 'figs/crhm-analysis/model-comparison/'

# plot weighed tree obs vs. sim facet by event ----
options(ggplot2.discrete.colour= c("#000000", "#DF536B", "#61D04F", "#2297E6", "#CD0BBC"))
# "#000000" "#E69F00" "#56B4E9" "#009E73" "#F0E442" "#0072B2" "#D55E00" "#CC79A7" "#999999"
# "#000000" "#DF536B" "#61D04F" "#2297E6" "#CD0BBC" "#F5C710" "#9E9E9E"
obs_mod_tree_comp |> 
  pivot_longer(!c(datetime, event_id, event_type, melt, sublimation, wind)) |> 
  mutate(name = factor(name, c('observed', 'simulated_new', 'ellis2010', 'andreadis2009', 'roesch2001')),
         facet_title = paste(event_type, '-', event_id)) |> 
  ggplot(aes(datetime, value, 
             colour = name, 
             linetype = name)) +  
  geom_line() +
  facet_wrap(~facet_title, scales = 'free', ncol = 3) +
  # facet_wrap(~event_id, scales = 'free') +
  ylab("Canopy Snow Load (mm)") +
  xlab(element_blank()) +
  labs(colour = 'Data Type', linetype = 'Data Type') +  # Same label for both
  theme(legend.position = 'bottom') +
  scale_x_datetime(date_labels = "%H") +
  scale_linetype_manual(values = c(
    observed = "solid",
    simulated_new = "solid",
    roesch2001 = "dashed",
    andreadis2009 = "dashed",
    ellis2010 = "dashed"
  ))

ggsave(
  paste0(
    base_path,
    'obs_mod_canopy_snow_load_',
    run_tag_updt,
    '.png'
  ),
  width = 7,
  height = 10,
  device = png
)


# hourly analysis

dL_fifteen <- obs_mod_tree_comp |> 
  pivot_longer(!c(datetime, event_id, event_type, melt, sublimation, wind)) |> 
  group_by(event_id, name, event_type) |> 
  mutate(dL = lag(value) - value) |> 
  select(datetime, event_id, event_type, name, value = dL) 


dL_hourly <- dL_fifteen |> 
  mutate(datetime = ceiling_date(datetime, unit = '1 hour')) |> # ceiling ensures the timestamp corresponds to preeceeding records
  group_by(datetime, event_id, event_type, name) |>
  summarise(value = sum(value)) 
  

dL_hourly_err_summary <- dL_hourly |> 
    pivot_wider() |> 
    pivot_longer(!c(datetime, event_id, event_type, observed)) |> 
    filter(observed > 0) |> 
    mutate(diff = observed - value) |> 
    group_by(name, event_type, event_id) |>
    summarise(
      MB = mean(diff, na.rm = T),
      MAE = mean(abs(diff), na.rm = T),
      RMSE = sqrt(mean(diff ^ 2, na.rm = T)),
      # NRMSE = RMSE / (max(observed, na.rm = TRUE) - min(observed, na.rm = TRUE)),
      NRMSE = RMSE / mean(observed, na.rm = T),
      R = cor(observed, value),
      R2 = R^2) |> 
  group_by(name, event_type) |> 
  summarise(
    across(MB:R2, mean, digits = 3)) |> 
  mutate(across(MB:R2, round, digits = 3)) |> 
  arrange(event_type, MB) 

write.csv(dL_hourly_err_summary,
          paste0(
            'tbls/',
            'obs_mod_canopy_snow_load_err_hourly_event_type',
            run_tag_updt,
            '.csv'
          ))

dL_hourly_err_summary |> 
  mutate(name = factor(name, c('simulated_new', 'ellis2010', 'andreadis2009', 'roesch2001'))) |> 
  ggplot(aes(name, MB, fill = name)) + 
  geom_bar(stat = 'identity') +
  facet_wrap(~event_type, scales = 'free') +
  ylab('Mean Bias (mm)') +
  xlab(element_blank())

ggsave(
  paste0(
    base_path,
    'obs_mod_canopy_snow_load_event_type_mean_bias_',
    run_tag_updt,
    '.png'
  ),
  width = 10,
  height = 4,
  device = png
)


# generate error table by event

dL_hourly_err_summary <- dL_hourly |> 
  pivot_wider() |> 
  pivot_longer(!c(datetime, event_id, event_type, observed)) |> 
  filter(observed > 0) |> 
  mutate(diff = observed - value) |> 
  group_by(name, event_id) |>
  summarise(
    MB = mean(diff, na.rm = T),
    MAE = mean(abs(diff), na.rm = T),
    RMSE = sqrt(mean(diff ^ 2, na.rm = T)),
    # NRMSE = RMSE / (max(observed, na.rm = TRUE) - min(observed, na.rm = TRUE)),
    NRMSE = RMSE / mean(observed, na.rm = T),
    R = cor(observed, value),
    R2 = R^2) |> 
  mutate(across(MB:R2, round, digits = 3)) |> 
  arrange(event_id, RMSE) |> 
  left_join(mod_d_drip_smry_frac)

write.csv(dL_hourly_err_summary,
          paste0(
            'tbls/',
            'obs_mod_canopy_snow_load_err_tbl_hourly_by_event_',
            run_tag_updt,
            '.csv'
          ))

dL_hourly_err_summary |> 
  rename(mod_name = name) |> 
  mutate(mod_name = factor(mod_name, c('simulated_new', 'ellis2010', 'andreadis2009', 'roesch2001'))) |> 
  pivot_longer(melt:sublimation) |> 
  ggplot(aes(value, MB)) + 
  geom_point() + facet_grid(mod_name~name) + 
  xlab('Fraction of Event Ablation (-)') +
  ylab('Mean Bias (mm)')

ggsave(
  paste0(
    base_path,
    'obs_mod_canopy_snow_load_event_type_mean_bias_vs_frac_ablation_process_',
    run_tag_updt,
    '.png'
  ),
  width = 8,
  height = 8,
  device = png
)


# obs vs mod weighed tree error table avg by model

obs_mod_tree_err_tbl_avgs <- dL_hourly |> 
  pivot_wider() |> 
  pivot_longer(!c(datetime, event_id, event_type, observed)) |> 
  filter(observed > 0) |> 
  mutate(diff = observed - value) |> 
  group_by(name) |>
  summarise(
    MB = mean(diff, na.rm = T),
    MAE = mean(abs(diff), na.rm = T),
    RMSE = sqrt(mean(diff ^ 2, na.rm = T)),
    # NRMSE = RMSE / (max(observed, na.rm = TRUE) - min(observed, na.rm = TRUE)),
    NRMSE = RMSE / mean(observed, na.rm = T),
    R = cor(observed, value),
    R2 = R^2) |> 
  mutate(across(MB:R2, round, digits = 3)) |> 
  arrange(RMSE)

write.csv(obs_mod_tree_err_tbl_avgs,
          paste0(
            'tbls/',
            'obs_mod_canopy_snow_load_err_tbl_avg_',
            run_tag_updt,
            '.csv'
          ))

saveRDS(obs_mod_tree_err_tbl_avgs,
        paste0(
          'tbls/',
          'obs_mod_canopy_snow_load_err_tbl_avg_',
          run_tag_updt,
          '.rds'
        ))

# cumulative ablation 
cml_err <- obs_mod_tree_comp |> 
  pivot_longer(!c(datetime, event_id, event_type, melt, sublimation, wind)) |> 
  group_by(event_id, name) |> 
  summarise(cumulative_ablation = first(value) - last(value)) |> 
  pivot_wider(values_from = cumulative_ablation) |> 
  pivot_longer(!c(event_id, observed)) |> 
  mutate(cumulative_ablation_error = observed - value) 

cml_err |> 
  ggplot(aes(name, cumulative_ablation_error, fill = name)) + 
  geom_bar(stat = 'identity') +
  facet_wrap(~event_id)

cml_err |> 
  group_by(name) |> 
  summarise(mean_bias = mean(cumulative_ablation_error),
            RMSE = sqrt(mean(cumulative_ablation_error ^ 2, na.rm = T))
  ) |> 
  ggplot(aes(name, mean_bias, fill = name)) + 
  geom_bar(stat = 'identity') 

ggsave(
  paste0(
    base_path,
    'obs_mod_mean_bias_',
    run_tag_updt,
    '.png'
  ),
  width = 8,
  height = 6,
  device = png
)

# compare error with met
event_met <- readRDS('data/ablation_event_met_summary.rds')

event_error_met <- left_join(dL_hourly_err_summary, event_met)

# below shows crhm_updated is more consistent across temperaturs and wind speed

event_error_met |> 
  ggplot(aes(t_mean, MB, colour = rh_mean)) + 
  geom_point() + 
  facet_wrap(~name)

event_error_met |> 
  ggplot(aes(rh_mean, MB, colour = t_mean)) + 
  geom_point()+ 
  facet_wrap(~name)

event_error_met |> 
  ggplot(aes(u_mean, MB, colour = rh_mean)) + 
  geom_point()+ 
  facet_wrap(~name)

event_error_met |> 
  ggplot(aes(t_mean, RMSE, colour = rh_mean)) + 
  geom_point() + 
  facet_wrap(~name)

event_error_met |> 
  ggplot(aes(rh_mean, RMSE, colour = t_mean)) + 
  geom_point()+ 
  facet_wrap(~name)

event_error_met |> 
  ggplot(aes(u_mean, RMSE, colour = rh_mean)) + 
  geom_point()+ 
  facet_wrap(~name)

# calculate ablation processes separately for each event ----- 

## crhm_updated crhm model ---- 

# melt_events <- mod_d_drip_smry_frac |> 
#   filter(melt > 0.8) |> 
#   pull(event_id)
# 
# subl_events <- mod_d_drip_smry_frac |> 
#   filter(sublimation > 0.8) |> 
#   pull(event_id)
# 
# wind_events <- mod_d_drip_smry_frac |> 
#   filter(wind > 0.6) |> 
#   pull(event_id)
# 
# mixed_events <- mod_d_drip_smry_frac |> 
#   filter(wind < 0.6,
#          melt < 0.8,
#          sublimation < 0.8) |> 
#   pull(event_id)
# 
# length(mod_d_drip_smry_frac$event_id |> unique()) == length(c(melt_events,
#                                                               subl_events,
#                                                               wind_events,
#                                                               mixed_events))

# plot weighed tree vs crhm canopy snow load again but for characterized events ----  
# obs_mod_tree_comp |> 
#   filter(event_id %in% melt_events) |>
#   pivot_longer(!c(datetime, event_id)) |> 
#   ggplot(aes(datetime, value, 
#              linetype = name)) +
#   geom_line() +
#   facet_wrap(~event_id, scales = 'free') +
#   ylab(expression("Canopy Snow Load (kg m"^-2*")")) +
#   xlab(element_blank()) +
#   labs(linetype = 'Data Type') +
#   theme(legend.position = 'bottom') +
#   scale_x_datetime(date_labels = "%H")
# 
# 
# ggsave(paste0(
#   'figs/crhm-analysis/subl_drip_unld/select_post_agu_events/',
#   fig_tbl_tag,
#   '/obs_mod_canopy_snow_load_baseline_vs_updated_melt_events.png'
# ),
# width = 8, 
# height = 6,
# device = png
# )
# 
# obs_mod_tree_comp |> 
#   filter(event_id %in% wind_events) |>
#   pivot_longer(!c(datetime, event_id)) |> 
#   ggplot(aes(datetime, value, 
#              linetype = name)) +
#   geom_line() +
#   facet_wrap(~event_id, scales = 'free') +
#   ylab(expression("Canopy Snow Load (kg m"^-2*")")) +
#   xlab(element_blank()) +
#   labs(linetype = 'Data Type') +
#   theme(legend.position = 'bottom') +
#   scale_x_datetime(date_labels = "%H")
# 
# 
# ggsave(paste0(
#   'figs/crhm-analysis/subl_drip_unld/select_post_agu_events/',
#   fig_tbl_tag,
#   '/obs_mod_canopy_snow_load_baseline_vs_updated_wind_events.png'
# ),
# width = 8, 
# height = 6,
# device = png
# )
# 
# obs_mod_tree_comp |> 
#   filter(event_id %in% subl_events) |>
#   pivot_longer(!c(datetime, event_id)) |> 
#   ggplot(aes(datetime, value, 
#              linetype = name)) +
#   geom_line() +
#   facet_wrap(~event_id, scales = 'free') +
#   ylab(expression("Canopy Snow Load (kg m"^-2*")")) +
#   xlab(element_blank()) +
#   labs(linetype = 'Data Type') +
#   theme(legend.position = 'bottom') +
#   scale_x_datetime(date_labels = "%H")
# 
# 
# ggsave(paste0(
#   'figs/crhm-analysis/subl_drip_unld/select_post_agu_events/',
#   fig_tbl_tag,
#   '/obs_mod_canopy_snow_load_baseline_vs_updated_subl_events.png'
# ),
# width = 8, 
# height = 6,
# device = png
# )
# 
# obs_mod_tree_comp |> 
#   filter(event_id %in% mixed_events) |>
#   pivot_longer(!c(datetime, event_id)) |> 
#   ggplot(aes(datetime, value, 
#              linetype = name)) +
#   geom_line() +
#   facet_wrap(~event_id, scales = 'free') +
#   ylab(expression("Canopy Snow Load (kg m"^-2*")")) +
#   xlab(element_blank()) +
#   labs(linetype = 'Data Type') +
#   theme(legend.position = 'bottom') +
#   scale_x_datetime(date_labels = "%H")
# 
# 
# ggsave(paste0(
#   'figs/crhm-analysis/subl_drip_unld/select_post_agu_events/',
#   fig_tbl_tag,
#   '/obs_mod_canopy_snow_load_baseline_vs_updated_mixed_events.png'
# ),
# width = 8, 
# height = 6,
# device = png
# )
# 
# # Plot ablation processes separately
# mod_cml_dU <- mod_d_drip |> 
#   inner_join(select_events_long) |> 
#   pivot_longer(c(snowmelt,
#                  # unload_melt,
#                  # unload_wind,
#                  unloading,
#                  sublimation),
#                values_to = 'dU') |> 
#   group_by(event_id, name) |> 
#   mutate(cml_dU = cumsum(dU),
#          cml_dU = cml_dU - first(cml_dU)) |>  # so each event starts at 0
#   select(datetime, event_id, name = name, value = cml_dU) |> 
#   mutate(group = 'Simulated Ablation (mm)')
# 
# mod_cml_dU |> 
#   ggplot(aes(datetime, value, 
#              colour = name)) +
#   geom_line() +
#   facet_wrap(vars(event_id), scales = 'free') +
#   scale_colour_manual(values = c("sublimation" = "#E69F00",
#                                  "unloading" = "#009E73",
#                                  "snowmelt" = "#56B4E9"),
#                       labels = c("sublimation" = "Sublimation",
#                                  "unloading" = "Unloading",
#                                  "snowmelt" = "Snowmelt")) +
#   labs(colour = "Process:") +
#   ylab(expression("Canopy Snow Ablation (kg m"^-2*")")) +
#   xlab(element_blank()) +
#   scale_x_datetime(date_labels = "%H") +
#   theme(legend.position = 'bottom')
# 
# ggsave(paste0(
#   'figs/crhm-analysis/subl_drip_unld/select_post_agu_events/',
#   prj_updt,
#   '/',
#   'mod_canopy_snow_ablation_',
#   run_tag_updt,
#   '_updated.png'
# ),
# width = 8, 
# height = 6,
# device = png
# )
# 
# mod_cml_dU |> 
#   filter(event_id %in% melt_events) |> 
#   ggplot(aes(datetime, value, 
#              colour = name)) +
#   geom_line() +
#   facet_wrap(vars(event_id), scales = 'free') +
#   scale_colour_manual(values = c("sublimation" = "#E69F00",
#                                  "unloading" = "#009E73",
#                                  "snowmelt" = "#56B4E9"),
#                       labels = c("sublimation" = "Sublimation",
#                                  "unloading" = "Unloading",
#                                  "snowmelt" = "Snowmelt")) +
#   labs(colour = "Process:") +
#   ylab(expression("Canopy Snow Ablation (kg m"^-2*")")) +
#   xlab(element_blank()) +
#   scale_x_datetime(date_labels = "%H") +
#   theme(legend.position = 'bottom')
# 
# ggsave(paste0(
#   'figs/crhm-analysis/subl_drip_unld/select_post_agu_events/',
#   prj_updt,
#   '/',
#   'mod_canopy_snow_ablation_',
#   run_tag_updt,
#   '_updated_melt_events.png'
# ),
# width = 8, 
# height = 6,
# device = png
# )
# 
# mod_cml_dU |> 
#   filter(event_id %in% wind_events) |> 
#   ggplot(aes(datetime, value, 
#              colour = name)) +
#   geom_line() +
#   facet_wrap(vars(event_id), scales = 'free') +
#   scale_colour_manual(values = c("sublimation" = "#E69F00",
#                                  "unloading" = "#009E73",
#                                  "snowmelt" = "#56B4E9"),
#                       labels = c("sublimation" = "Sublimation",
#                                  "unloading" = "Unloading",
#                                  "snowmelt" = "Snowmelt")) +
#   labs(colour = "Process:") +
#   ylab(expression("Canopy Snow Ablation (kg m"^-2*")")) +
#   xlab(element_blank()) +
#   scale_x_datetime(date_labels = "%H") +
#   theme(legend.position = 'bottom')
# 
# ggsave(paste0(
#   'figs/crhm-analysis/subl_drip_unld/select_post_agu_events/',
#   prj_updt,
#   '/',
#   'mod_canopy_snow_ablation_',
#   run_tag_updt,
#   '_updated_wind_events.png'
# ),
# width = 8, 
# height = 6,
# device = png
# )
# 
# mod_cml_dU |> 
#   filter(event_id %in% subl_events) |> 
#   ggplot(aes(datetime, value, 
#              colour = name)) +
#   geom_line() +
#   facet_wrap(vars(event_id), scales = 'free') +
#   scale_colour_manual(values = c("sublimation" = "#E69F00",
#                                  "unloading" = "#009E73",
#                                  "snowmelt" = "#56B4E9"),
#                       labels = c("sublimation" = "Sublimation",
#                                  "unloading" = "Unloading",
#                                  "snowmelt" = "Snowmelt")) +
#   labs(colour = "Process:") +
#   ylab(expression("Canopy Snow Ablation (kg m"^-2*")")) +
#   xlab(element_blank()) +
#   scale_x_datetime(date_labels = "%H") +
#   theme(legend.position = 'bottom')
# 
# ggsave(paste0(
#   'figs/crhm-analysis/subl_drip_unld/select_post_agu_events/',
#   prj_updt,
#   '/',
#   'mod_canopy_snow_ablation_',
#   run_tag_updt,
#   '_updated_subl_events.png'
# ),
# width = 8, 
# height = 6,
# device = png
# )
# 
# ## Show baseline partitioning ---- 
# 
# # get the mod ablation partition
# 
# mod_d_drip <- crhm_output_baseline |> 
#   mutate(crhm_canopy_snowmelt = canopy_snowmelt.1,
#          crhm_drip = drip_cpy.1,
#          crhm_unload = SUnload.1,
#          # unload_test = canopy_snowmelt.1^2,
#          crhm_sbl = Subl_Cpy.1) |> 
#   select(datetime, snowmelt = crhm_canopy_snowmelt,
#          crhm_drip, unloading = crhm_unload, sublimation = crhm_sbl)
# 
# mod_cml_dU <- mod_d_drip |> 
#   inner_join(select_events_long) |> 
#   pivot_longer(c(snowmelt,
#                  # crhm_drip,
#                  # unload_test,
#                  unloading,
#                  sublimation),
#                values_to = 'dU') |> 
#   group_by(event_id, name) |> 
#   mutate(cml_dU = cumsum(dU),
#          cml_dU = cml_dU - first(cml_dU)) |>  # so each event starts at 0
#   select(datetime, event_id, name = name, value = cml_dU) |> 
#   mutate(group = 'Simulated Ablation (mm)')
# 
# mod_cml_dU |> 
#   ggplot(aes(datetime, value, 
#              colour = name)) +
#   geom_line() +
#   facet_wrap(vars(event_id), scales = 'free') +
#   scale_colour_manual(values = c("canopy_load" = "black", 
#                                  "sublimation" = "#E69F00", 
#                                  "unloading" = "#009E73",
#                                  "snowmelt" = "#56B4E9"),
#                       labels = c("canopy_load" = "Canopy Load",
#                                  "sublimation" = "Sublimation",
#                                  "unloading" = "Unloading",
#                                  "snowmelt" = "Snowmelt")) +
#   labs(colour = "Process") +
#   ylab(expression("Canopy Snow Ablation (kg m"^-2*")")) +
#   xlab(element_blank()) +
#   scale_x_datetime(date_labels = "%H") +
#   theme(legend.position = 'bottom')
# 
# ggsave(paste0(
#   'figs/crhm-analysis/subl_drip_unld/select_post_agu_events/',
#   prj_base,
#   '/',
#   'mod_canopy_snow_ablation_',
#   run_tag_updt,
#   '_baseline.png'
# ),
# width = 8, 
# height = 6,
# device = png
# )
# 
# # PLOT INDIVIDUALLY ---- 
# 
# obs_mod_tree_long <- obs_mod_tree_comp |>
#   # left_join(baseline_mod_tree) |> 
#   pivot_longer(!c(datetime, event_id)) |> 
#   mutate(group = 'Canopy Snow Load (mm)')
# 
# for (event in unique(select_events_long$event_id)) {
#   
#   obs_mod_tree_err_tbl <- obs_mod_tree_comp |> 
#     filter(event_id == event) |> 
#     mutate(diff = observed - simulated) |> 
#     # group_by(name) |> 
#     summarise(
#       `Mean Bias` = mean(diff, na.rm = T),
#       MAE = mean(abs(diff), na.rm = T),
#       `RMS Error` = sqrt(mean(diff ^ 2, na.rm = T)),
#       # NRMSE = `RMS Error` / (max(observed, na.rm = TRUE) - min(observed, na.rm = TRUE)),
#       NRMSE = `RMS Error` / mean(observed, na.rm = T),
#       R = cor(observed, simulated),
#       `r^2` = R^2) |> 
#     mutate(across(`Mean Bias`:`r^2`, round, digits = 3))
#   
#   stats_text <- obs_mod_tree_err_tbl |> 
#     summarise(
#       text = sprintf(
#         "Mean Bias (mm): %.2f\nRMS Error (mm): %.2f\nNRMSE: %.2f\nR²: %.2f", 
#         `Mean Bias`, `RMS Error`, NRMSE, `r^2`
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
