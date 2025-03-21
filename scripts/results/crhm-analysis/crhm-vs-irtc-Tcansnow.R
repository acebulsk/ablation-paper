# Crhm vs. IRTC canopy and snow temps
ft_met_xtra <- readRDS('../../analysis/met-data-processing/data/1000x_wnd_irtc.rds')

irtc_temps <- ft_met_xtra |> 
  ungroup() |> 
  select(datetime, starts_with('IRTC')) 

# Select model run with all unloading events weighed tree snow load assimilated
prj_updt <- "ffr_closed_canopy_cc0.88_cansnobal"

run_tag_updt <- "ess03_vt0.0_avs_0.65_fix2"

path <- list.files(
  paste0(
    "../../analysis/crhm-analysis/output/",
    prj_updt
  ),
  pattern = run_tag_updt,
  full.names = T
)

stopifnot(length(path) == 1)

crhm_output <- CRHMr::readOutputFile(
  path,
  timezone = 'Etc/GMT+6')

mod_temps <- crhm_output |> 
  select(datetime, 
         # CRHM_Tsnow = TCanSnow.1,
         # CRHM_Tbiomass = Tbiomass.1,
         CRHM_hru_t = hru_t.1,
         CRHM_Tsnow_new = T_s_veg.1
  ) |> 
  mutate(CRHM_Tsnow_new = CRHM_Tsnow_new - 2.7316e2)

obs_mod_temps <- left_join(mod_temps, irtc_temps, by = 'datetime') |> 
  pivot_longer(c(CRHM_hru_t:IRTC_trunk)) |> 
  filter(abs(value) < 100)


# ggplot(obs_mod_temps, aes(datetime, value, colour = name )) +
#   geom_line() +
#   # facet_wrap(~event_id, scales = 'free') +
#   ylab('Temperature (°C)') +
#   xlab(element_blank())

# ggsave(
#   paste0(
#     'figs/crhm-analysis/irtc/crhm_vs_irtc_',
#     format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
#     '.png'
#   ),
#   width = 12,
#   height = 8
# )

# plotly::ggplotly()

ggplot(obs_mod_temps |> 
         filter(datetime > '2023-03-13',
                datetime < '2023-03-18',
                name %in% c('CRHM_hru_t', 'CRHM_Tsnow_new', 'IRTC_midTree')), aes(datetime, value, colour = name, group = name )) +
  geom_line() +
  # facet_wrap(~event_id, scales = 'free') +
  ylab('Temperature (°C)') +
  xlab(element_blank())
ggsave(
  paste0(
    'figs/crhm-analysis/irtc/crhm_vs_irtc_event_',
    prj_updt,
    run_tag_updt,
    '_',
    format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
    '.png'
  ),
  width = 5,
  height = 3
)
