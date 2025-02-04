# Script to compare drip output from CRHM to drip measured by tipping buckets

tb_d_drip <- tb_data_zeroed |> 
  group_by(datetime, event_id) |> 
  summarise(dU = mean(dU)) |> 
  select(datetime, event_id, mean_tipping_buckets = dU) 

mod_d_drip <- crhm_output_tree |> 
  mutate(crhm_canopy_snowmelt = canopy_snowmelt.1,
         crhm_drip = drip_cpy.1) |> 
  select(datetime, crhm_canopy_snowmelt, crhm_drip)

obs_mod_cml_drip <- inner_join(tb_d_drip,
                                    mod_d_drip,
                                    by = 'datetime') |> 
  pivot_longer(mean_tipping_buckets:crhm_drip, values_to = 'dU') |> 
  group_by(event_id, name) |> 
  mutate(cml_drip = cumsum(dU),
         cml_drip = cml_drip - first(cml_drip)) # so each event starts at 0

obs_mod_cml_drip |> 
  filter(!is.na(event_id)) |> 
  ggplot(aes(datetime, cml_drip, colour = name)) + 
  geom_line() +
  facet_wrap(~event_id, scales = 'free') +
  ylab('Cumulative Drip (mm)') +
  xlab(element_blank())

ggsave(
  paste0(
    'figs/crhm-analysis/drip/crhm_vs_tb_drip_events_',
    format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
    '.png'
  ),
  width = 12,
  height = 8
  )

# bring in tree for context 
obs_tree <- tree_drip_events_df |> 
  left_join(tb_events_long) |> 
  select(datetime, event_id, obs = obs_tree)
mod_tree <- crhm_output_tree |> 
  select(datetime, mod = Snow_load.1)
obs_mod_tree <- left_join(obs_tree, mod_tree) |>
  pivot_longer(c(obs, mod)) |> 
  mutate(group = 'Canopy Snow Load (mm)')

for (event in tb_events$event_id) {
  
  obs_mod_cml_drip |> 
    select(datetime, event_id, name, value = cml_drip) |> 
    mutate(group = 'Cumulative Drip (mm)') |> 
    rbind(obs_mod_tree) |>
    filter(event_id == event) |>
    ggplot(aes(datetime, value, colour = name)) + 
    geom_line() +
    facet_grid(rows = vars(group), scales = 'free') +
    ggtitle(as.Date(event)) +
    ylab(element_blank()) +
    xlab(element_blank())
  
    ggsave(
      paste0(
        'figs/crhm-analysis/drip/indiv_events/crhm_vs_tb_drip_',
        as.Date(event),
        '_',
        format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
        '.png'
      ),
      width = 8,
      height = 6
    )
}
