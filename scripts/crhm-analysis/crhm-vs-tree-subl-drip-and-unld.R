# Script to plot obs mass subl + unloading + drip from the weighed tree vs. CRHM
# This is for cold/warm events

obs_tree <-
  readRDS('data/clean-data/all_tree_events_zero_weighed_tree_mm_post_cnpy_snow.rds') |> 
  select(datetime, event_id, observed = value)

# Select model run with all unloading events weighed tree snow load assimilated
# where I added the observed weighed tree to initilize canopy snowmelt
path <- "../../analysis/crhm-analysis/output/weighed_tree_vector_based_new_ablation_psp/2024-12-03-17-28-48_weighed_tree_vector_based_new_ablation_psp_output.txt"
crhm_output_tree <- CRHMr::readOutputFile(
  path,
  timezone = 'Etc/GMT+6'
)

mod_tree <- crhm_output_tree |> 
  select(datetime, simulated = Snow_load.1)
# baseline_mod_tree <- baseline_crhm_output_tree |> 
#   select(datetime, simulated_baseline = Snow_load.1)
obs_mod_tree <- left_join(obs_tree, mod_tree) |>
  # left_join(baseline_mod_tree) |> 
  pivot_longer(!c(datetime, event_id)) |> 
  mutate(group = 'Canopy Snow Load (mm)')

# get the mod ablation partition
mod_d_drip <- crhm_output_tree |> 
  mutate(crhm_canopy_snowmelt = canopy_snowmelt.1,
         crhm_drip = drip_cpy.1,
         crhm_unload = SUnload.1,
         crhm_sbl = Subl_Cpy.1) |> 
  select(datetime, snowmelt = crhm_canopy_snowmelt, crhm_drip, unloading = crhm_unload, sublimation = crhm_sbl)
mod_cml_dU <- mod_d_drip |> 
  inner_join(events_fltr_long) |> 
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

event <- unique(events_fltr_long$event_id)[40]

obs_mod_tree |> 
  # rbind(mod_cml_dU) |> 
  filter(event_id == event) |> 
  ggplot(aes(datetime, value, 
             colour = name)) +
  geom_line() 

for (event in unique(events_fltr_long$event_id)) {
  obs_mod_tree |> 
    rbind(mod_cml_dU) |>
    filter(event_id == event) |> 
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
    labs(colour = "State/Process", linetype = "Data Type") 
  
  ggsave(
    paste0(
      'figs/crhm-analysis/subl_drip_unld/all_events/crhm_vs_tree_subl_drip_unld_',
      as.Date(event),
      '_',
      format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
      '.png'
    ),
    width = 8,
    height = 6
  )
}

# WHY IS THE SIMULATED TREE JITTERING

# looks like observed precip that should be filtereed out
obs_mod_tree |> 
  filter(event_id == '2022-12-01') |>
  ggplot(aes(datetime, value, colour = name)) +
  geom_line() + facet_grid(rows = vars(name))

# Scatter -----

obs_mod_tree <- left_join(obs_tree, mod_tree)

ggplot(obs_mod_tree, aes(observed, simulated)) + 
  geom_point() + 
  geom_abline()

obs_mod_tree_err_tbl <- obs_mod_tree |> 
  mutate(diff = Observed - Simulated) |> 
  # group_by(name) |> 
  summarise(
    `Mean Bias` = mean(diff, na.rm = T),
    MAE = mean(abs(diff), na.rm = T),
    `RMS Error` = sqrt(mean(diff ^ 2, na.rm = T)),
    NRMSE = `RMS Error` / (max(Observed, na.rm = TRUE) - min(Observed, na.rm = TRUE)),
    # NRMSE_mean = `RMS Error` / mean(Observed, na.rm = T),
    R = cor(Observed, Simulated),
    `r^2` = R^2) |> 
  mutate(across(`Mean Bias`:`r^2`, round, digits = 3))

write_csv(obs_mod_tree_err_tbl,
          paste0(
            'tbls/crhm-snow-load-vs-wtree-errortbl',
            '_',
            format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
            '.csv'
          ))


