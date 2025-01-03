# Script to plot obs mass unloading + drip from the troughs vs. CRHM

# WARM EVENT COMPARISON

# Select at commit id c785571b37c1f567ab9590d3e0cb106b1bd46ff3 on branch ablation-test-obs-tree
# where I added the observed weighed tree to initilize canopy snowmelt
path <- "../../analysis/crhm-analysis/output/weighed_tree_vector_based_new_ablation_psp/2024-12-02-10-04-04_weighed_tree_vector_based_new_ablation_psp_output.txt"
crhm_output_tree <- CRHMr::readOutputFile(
  path,
  timezone = 'Etc/GMT+6'
)

# get the mod and obs canopy snow load
obs_tree <- obs_tree_warm_events |> 
  select(datetime, event_id, observed = value)
mod_tree <- crhm_output_tree |> 
  select(datetime, simulated = Snow_load.1)
# baseline_mod_tree <- baseline_crhm_output_tree |> 
#   select(datetime, simulated_baseline = Snow_load.1)
obs_mod_tree <- left_join(obs_tree, mod_tree) |>
  # left_join(baseline_mod_tree) |> 
  pivot_longer(!c(datetime, event_id)) |> 
  mutate(group = 'Canopy Snow Load (mm)')

events_long <- obs_tree |> select(-observed)

# get the mod 
mod_d_drip <- crhm_output_tree |> 
  mutate(crhm_canopy_snowmelt = canopy_snowmelt.1,
         crhm_drip = drip_cpy.1,
         crhm_unload = SUnload.1,
         crhm_sbl = Subl_Cpy.1) |> 
  select(datetime, snowmelt = crhm_canopy_snowmelt, crhm_drip, unloading = crhm_unload, sublimation = crhm_sbl)
mod_cml_dU <- mod_d_drip |> 
  inner_join(events_long) |> 
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

warm_events <- c(
  '2022-06-14',
  '2022-04-23',
  '2022-06-24',
  '2023-05-08',
  '2023-06-15',
  '2023-06-21'
)

event <- warm_events[1]

# rough met summary 

met_stats <- crhm_output_tree |> 
  inner_join(events_long) |> 
  filter(event_id %in% warm_events) |> 
  group_by(event_id) |> 
  mutate(hru_sf = hru_p.1 - hru_rain.1) |> 
  summarise(
    hru_t.1_mean = mean(hru_t.1),
    hru_t.1_min = min(hru_t.1),
            hru_t.1_max = max(hru_t.1),
            hru_rh.1_mean = mean(hru_rh.1),
            hru_rh.1_min = min(hru_rh.1),
            hru_rh.1_max = max(hru_rh.1),
            hru_u.1_mean = mean(hru_u.1),
            hru_u.1_min = min(hru_u.1),
            hru_u.1_max = max(hru_u.1),
            hru_p.1 = sum(hru_p.1),
            hru_sf = sum(hru_sf),
            hru_rain.1 = sum(hru_rain.1))

for (event in warm_events) {
    
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
      'figs/crhm-analysis/subl_drip_unld_warm/indiv_events/select_warm_events/crhm_vs_tree_subl_drip_unld_',
      as.Date(event),
      '_',
      format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
      '.png'
    ),
    width = 8,
    height = 6
  )
}

# INVESTIGATE WHY MORE ABLATION THAN CANOPY SNOW? rough summary is that whenwe
# have precip occuring over the event some rain/snow may be melted/frozen which
# results in our dL not matching total ablation. An example of this is where we
# have liquid water held on the canopy which is added to the snow reservoir and
# although we are unloading 0.5 mm during this event since we added .25 mm of
# rain to the snow reservoir our dL does not match our ablation

# See below during large rain event we have a 2 mm mismatch vs. the one below
# which has minimal precip is much closer aligned.

check_mass_bal <- crhm_output_tree |>
  filter(datetime >= as.POSIXct('2022-06-14 08:15:00', tz = 'Etc/GMT+6'),
         datetime <= as.POSIXct('2022-06-15 10:15:00', tz = 'Etc/GMT+6')) |> 
  mutate(
    dL = Snow_load.1 - lag(Snow_load.1),
    dL_check = - SUnload.1 - Subl_Cpy.1 - canopy_snowmelt.1)

dU <- sum(check_mass_bal$Subl_Cpy.1) + sum(check_mass_bal$SUnload.1) + sum(check_mass_bal$canopy_snowmelt.1)
dU_check <- max(check_mass_bal$Snow_load.1)

check_mass_bal <- crhm_output_tree |>
  inner_join(events_long) |> 
  filter(event_id == '2022-04-21') |> 
  mutate(
    dL = Snow_load.1 - lag(Snow_load.1),
    dL_check = - SUnload.1 - Subl_Cpy.1 - canopy_snowmelt.1)

dU <- sum(check_mass_bal$Subl_Cpy.1) + sum(check_mass_bal$SUnload.1) + sum(check_mass_bal$canopy_snowmelt.1)
dU_check <- max(check_mass_bal$Snow_load.1)