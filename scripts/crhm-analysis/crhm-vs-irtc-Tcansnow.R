# Crhm vs. IRTC canopy and snow temps

irtc_temps <- ft_met_xtra |> 
  ungroup() |> 
  select(datetime, starts_with('IRTC')) 

irtc_temps |> 
  inner_join(tb_events_long) |> 
  pivot_longer(starts_with('IRTC')) |> 
  filter(!is.na(event_id)) |> 
  ggplot(aes(datetime, value, colour = name)) + 
  geom_line() +
  facet_wrap(~event_id, scales = 'free')

# mod_temps <- crhm_output_scl |> 
#   select(datetime, 
#          CRHM_Tsnow = TCanSnow.1,
#          CRHM_Tbiomass = Tbiomass.1,
#          CRHM_hru_t = hru_t.1
#          )
mod_temps <- crhm_output_tree |> 
  select(datetime, 
         CRHM_Tsnow = TCanSnow.1,
         CRHM_Tbiomass = Tbiomass.1,
         CRHM_hru_t = hru_t.1
  )

obs_mod_temps <- inner_join(irtc_temps, mod_temps, by = 'datetime') |> 
  pivot_longer(c(IRTC_midTree:CRHM_hru_t)) |> 
  inner_join(tb_events_long)


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
