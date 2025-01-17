# This script plots the selected events used at AGU 2024 and now combined into one df
library(tidyverse)
source('scripts/00-results-figs-setup.R') # define events used in this file

obs_tree <- rbind(obs_tree_cold  |> 
                    select(datetime, event_id, observed = value),
                  obs_tree_warm |> 
                    select(datetime, event_id, observed = value)) 

# Select model run with all unloading events weighed tree snow load assimilated
prj <- "closed_canopy_cc0.88_vector_based_new_ablation_psp"

# specify certain model run
#run_tag <- "turn_off_duration_based_ablation_output.txt" # baseline prior to ratio based unloading
run_tag <- "added_many_events_w_cald_melt_drip_ratio_updated_ft_obs2"

path <- list.files(
  paste0(
    "../../analysis/crhm-analysis/output/",
    prj
  ),
  pattern = run_tag,
  full.names = T
)

# just get last model run
# model_runs <- list.files(
#   paste0(
#     "../../analysis/crhm-analysis/output/",
#     prj
#   ),
#   full.names = T
# )
# path <- tail(model_runs, n = 1)

crhm_output <- CRHMr::readOutputFile(
  path,
  timezone = 'Etc/GMT+6') |> filter(datetime %in% obs_tree$datetime)

select_events_long <- obs_tree |> select(datetime, event_id)

mod_tree <- crhm_output |> 
  select(datetime, simulated = Snow_load.1)
# baseline_mod_tree <- baseline_crhm_output_tree |> 
#   select(datetime, simulated_baseline = Snow_load.1)
obs_mod_tree <- left_join(obs_tree, mod_tree)

# plot weighed tree obs vs. sim facet by event ----

obs_mod_tree |> 
  pivot_longer(!c(datetime, event_id)) |> 
  ggplot(aes(datetime, value, 
             linetype = name)) +
  geom_line() +
  facet_wrap(~event_id, scales = 'free') +
  ylab(expression("Canopy Snow Load (kg m"^-2*")")) +
  xlab(element_blank()) +
  labs(linetype = 'Data Type')

ggsave(paste0(
  'figs/crhm-analysis/subl_drip_unld/select_post_agu_events/',
  prj,
  '/',
  'obs_mod_canopy_snow_load_',
  run_tag,
  '.png'
  ),
  width = 8, 
  height = 6,
  device = png
)

# generate error table 
obs_mod_tree_err_tbl <- obs_mod_tree |> 
  group_by(event_id) |> 
  mutate(diff = observed - simulated) |> 
  # group_by(name) |> 
  summarise(
    runtag = run_tag,
    `Mean Bias` = mean(diff, na.rm = T),
    MAE = mean(abs(diff), na.rm = T),
    `RMS Error` = sqrt(mean(diff ^ 2, na.rm = T)),
    # NRMSE = `RMS Error` / (max(observed, na.rm = TRUE) - min(observed, na.rm = TRUE)),
    NRMSE = `RMS Error` / mean(observed, na.rm = T),
    R = cor(observed, simulated),
    `r^2` = R^2) |> 
  mutate(across(`Mean Bias`:`r^2`, round, digits = 3))
write.csv(obs_mod_tree_err_tbl,
          paste0(
            'tbls/crhm-snow-load-vs-wtree-errortbl_by_event_',
            run_tag,
            '.csv'
          ))
# compare error with met
event_met <- readRDS('data/ablation_event_met_summary.rds')

event_error_met <- left_join(obs_mod_tree_err_tbl, event_met)

event_error_met |> 
  ggplot(aes(t_mean, `Mean Bias`, colour = rh_mean)) + 
  geom_point()

# show ablation processes separately facet by event ----- 

# get the mod ablation partition
mod_d_drip <- crhm_output |> 
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
  xlab(element_blank())

ggsave(paste0(
  'figs/crhm-analysis/subl_drip_unld/select_post_agu_events/',
  prj,
  '/',
  'mod_canopy_snow_ablation_',
  run_tag,
  '.png'
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
