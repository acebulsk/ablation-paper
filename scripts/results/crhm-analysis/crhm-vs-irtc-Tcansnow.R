# Crhm vs. IRTC canopy and snow temps
ft_met_xtra <- readRDS('../../analysis/met-data-processing/data/1000x_wnd_irtc.rds')

irtc_temps <- ft_met_xtra |> 
  ungroup() |> 
  select(datetime, starts_with('IRTC')) 

obs_tree_events <- rbind(obs_tree_cold  |> 
                    select(datetime, event_id, observed = tree_mm),
                  obs_tree_warm |> 
                    select(datetime, event_id, observed = tree_mm)) |> 
  select(datetime, event_id)

# Select model run with all unloading events weighed tree snow load assimilated
prj <- "ffr_closed_canopy_cc0.88_vector_based_new_ablation_psp"

# specify certain model run
#run_tag <- "turn_off_duration_based_ablation_output.txt" # baseline prior to ratio based unloading
run_tag <- "t_canopy_event_analsis_biomass_100"
# run_tag <- "testing123"

path <- list.files(
  paste0(
    "../../analysis/crhm-analysis/output/",
    prj
  ),
  pattern = run_tag,
  full.names = T
)

stopifnot(length(path) == 1)

crhm_output <- CRHMr::readOutputFile(
  path,
  timezone = 'Etc/GMT+6') |> filter(datetime %in% obs_tree$datetime)

mod_temps <- crhm_output |> 
  select(datetime, 
         CRHM_Tsnow = TCanSnow.1,
         # CRHM_Tbiomass = Tbiomass.1,
         CRHM_hru_t = hru_t.1
  )

obs_mod_temps <- inner_join(irtc_temps, mod_temps, by = 'datetime') |> 
  pivot_longer(c(IRTC_midTree:CRHM_hru_t)) |> 
  inner_join(obs_tree_events)


ggplot(obs_mod_temps, aes(datetime, value, colour = name )) +
  geom_line() +
  facet_wrap(~event_id, scales = 'free') +
  ylab('Temperature (°C)') +
  xlab(element_blank())

ggsave(
  paste0(
    'figs/crhm-analysis/irtc/crhm_vs_irtc_',
    format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
    '.png'
  ),
  width = 12,
  height = 8
)

plotly::ggplotly()
