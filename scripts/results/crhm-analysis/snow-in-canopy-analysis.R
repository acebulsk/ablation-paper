# Script to plot snow in canopy over the years

obs_tree <-
  readRDS(paste0(
    'data/clean-data/unloading_events_zero_weighed_tree_kg_m2_pre_post_cnpy_snow_',
    load_suffix,
    '.rds'
  )) |> select(datetime, observed = tree_mm)

# SETUP ----

crhm_met <- crhm_output_newsim |> 
  select(datetime, hru_t.1, hru_p.1, hru_u.1, hru_rain.1) |> 
  pivot_longer(!datetime) |> 
  mutate(group = name)

mod_tree <- crhm_output_newsim |> 
  # left_join(crhm_output_baseline) |> 
  select(datetime, simulated_new = m_s_veg.1)

obs_mod_tree <- left_join(mod_tree, obs_tree)

obs_mod_tree_long <- obs_mod_tree |> 
  pivot_longer(c(observed, simulated_new)) |> 
  mutate(group = 'w_tree') |> 
  rbind(crhm_met)

obs_mod_tree_long |> 
  ggplot(aes(datetime, value, colour = name)) +
  geom_line() +
  facet_grid(rows = vars(group), scales = 'free')
plotly::ggplotly()


