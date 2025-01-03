# Script to plot obs mass unloading + drip from the troughs vs. CRHM
# TODO change crhm to use obs tree

scl_d_drip_unld <- q_unld_scl |> 
  group_by(datetime, event_id, name) |> 
  summarise(dU = mean(dU)) |> 
  select(datetime, event_id, mean_troughs = dU) 

mod_d_drip_unld <- crhm_output_scl |> 
  mutate(crhm = SUnload.1 + drip_cpy.1) |> 
  select(datetime, crhm)

obs_mod_cml_drip_unld <- inner_join(scl_d_drip_unld,
                                   mod_d_drip_unld,
                                   by = 'datetime') |> 
  pivot_longer(mean_troughs:crhm, values_to = 'dU') |> 
  group_by(event_id, name) |> 
  mutate(cml_drip_unld = cumsum(dU),
         cml_drip_unld = cml_drip_unld - first(cml_drip_unld)) # so each event starts at 0

obs_mod_cml_drip_unld |> 
  ggplot(aes(datetime, cml_drip_unld, colour = name)) + 
  geom_line() +
  facet_wrap(~event_id, scales = 'free') +
  ylab("Cumulative Unloading and Drip (mm)") +
  xlab(element_blank())

ggsave(
  paste0(
    'figs/crhm-analysis/drip_unld/crhm_vs_scl_drip_unld_events_',
    format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
    '.png'
  ),
  width = 12,
  height = 8
)

obs_mod_cml_drip_unld |> 
  ggplot(aes(datetime, dU, colour = name)) + 
  geom_line() +
  facet_wrap(~event_id, scales = 'free')

crhm_output |> 
  pivot_longer(c(SUnload.1, canopy_snowmelt.1, drip_cpy.1)) |> 
  ggplot(aes(datetime, value, colour = name)) + 
  geom_line()  +
  facet_wrap(~event_id, scales = 'free')
