# This script plots the selected events used at AGU 2024

# COLD EVENTS -----
# these are generally cold events used to paramerterise the unloading fns 
cold_events <- c('2022-12-01', '2022-03-09')
obs_tree_cold <-
  readRDS('data/clean-data/all_tree_events_zero_weighed_tree_fsd_closed_0.88_kg_m2_post_cnpy_snow.rds') |> 
  select(datetime, event_id, observed = value) |> 
  filter(event_id %in% cold_events)

# Select model run with all unloading events weighed tree snow load assimilated
# where I added the observed weighed tree to initilize canopy snowmelt
# path <- "../../analysis/crhm-analysis/output/weighed_tree_vector_based_new_ablation_psp/2024-12-03-17-28-48_weighed_tree_vector_based_new_ablation_psp_output.txt"
path <- "../../analysis/crhm-analysis/output/closed_canopy_cc0.88_vector_based_new_ablation_psp/2025-01-06-17-56-37_closed_canopy_cc0.88_vector_based_new_ablation_psp_all_events_init_obs_tree_output.txt"
crhm_output_tree_cold <- CRHMr::readOutputFile(
  path,
  timezone = 'Etc/GMT+6') |> filter(datetime %in% obs_tree_cold$datetime)

# need to keep the warm/cold datasets separate as there are some overlaps between events

# WARM EVENTS ----
# these are generally cold events used to paramerterise the unloading fns 
warm_events <- c('2022-06-14', '2022-04-23', '2023-06-21')
obs_tree_warm <-
  readRDS('data/clean-data/warm_tree_events_zero_weighed_tree_fsd_closed_0.88_kg_m2_post_cnpy_snow.rds') |> 
  select(datetime, event_id, observed = value) |> 
  filter(event_id %in% warm_events)

# Select at commit id c785571b37c1f567ab9590d3e0cb106b1bd46ff3 on branch ablation-test-obs-tree
# where I added the observed weighed tree to initilize canopy snowmelt
# path <- "../../analysis/crhm-analysis/output/weighed_tree_vector_based_new_ablation_psp/2024-12-02-10-04-04_weighed_tree_vector_based_new_ablation_psp_output.txt"
path <- "../../analysis/crhm-analysis/output/closed_canopy_cc0.88_vector_based_new_ablation_psp/2025-01-06-17-44-49_closed_canopy_cc0.88_vector_based_new_ablation_psp_warm_events_init_obs_tree_output.txt"

crhm_output_tree_warm <- CRHMr::readOutputFile(
  path,
  timezone = 'Etc/GMT+6'
) |> filter(datetime %in% obs_tree_warm$datetime)

# COMBINE EVENTS ----

obs_tree <- rbind(obs_tree_cold,
                  obs_tree_warm)

select_events_long <- obs_tree |> select(datetime, event_id)

mod_bind <- rbind(crhm_output_tree_warm, 
                  crhm_output_tree_cold)

mod_tree <- mod_bind |> 
  select(datetime, simulated = Snow_load.1)
# baseline_mod_tree <- baseline_crhm_output_tree |> 
#   select(datetime, simulated_baseline = Snow_load.1)
obs_mod_tree <- left_join(obs_tree, mod_tree)

obs_mod_tree_long <- obs_mod_tree |>
  # left_join(baseline_mod_tree) |> 
  pivot_longer(!c(datetime, event_id)) |> 
  mutate(group = 'Canopy Snow Load (mm)')

# get the mod ablation partition
mod_d_drip <- mod_bind |> 
  mutate(crhm_canopy_snowmelt = canopy_snowmelt.1,
         crhm_drip = drip_cpy.1,
         crhm_unload = SUnload.1,
         crhm_sbl = Subl_Cpy.1) |> 
  select(datetime, snowmelt = crhm_canopy_snowmelt,
         crhm_drip, unloading = crhm_unload, sublimation = crhm_sbl)
mod_cml_dU <- mod_d_drip |> 
  inner_join(select_events_long) |> 
  pivot_longer(c(snowmelt,
                 # crhm_drip,
                 unloading,
                 sublimation),
               values_to = 'dU') |> 
  group_by(event_id, name) |> 
  mutate(cml_dU = cumsum(dU),
         cml_dU = cml_dU - first(cml_dU)) |>  # so each event starts at 0
  select(datetime, event_id, name = name, value = cml_dU) |> 
  mutate(group = 'Simulated Ablation (mm)')

event <- unique(select_events_long$event_id)[5]

obs_mod_tree |> 
  pivot_longer(!c(datetime, event_id)) |> 
  # rbind(mod_cml_dU) |> 
  # filter(event_id == event) |> 
  ggplot(aes(datetime, value, 
             colour = name)) +
  geom_line() +
  facet_wrap(~event_id, scales = 'free')

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
      'figs/crhm-analysis/subl_drip_unld/select_agu_events/crhm_vs_tree_subl_drip_unld_',
      as.Date(event),
      '_',
      format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
      '.png'
    ),
    width = 8,
    height = 6
  )
  
}
