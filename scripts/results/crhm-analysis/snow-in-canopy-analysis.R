# Script to plot snow in canopy over the years

# SETUP ----

prj_updt <- "ffr_closed_canopy_cc0.88_cansnobal"

# specify certain model run
run_tag_updt <- "recal_unld_melt_ratio"

obs_tree <-
  readRDS(paste0(
    'data/clean-data/unloading_events_zero_weighed_tree_kg_m2_pre_post_cnpy_snow_',
    load_suffix,
    '.rds'
  )) |> 
  select(datetime, tree_mm)

path <- list.files(
  paste0(
    "../../analysis/crhm-analysis/output/",
    prj_updt
  ),
  pattern = run_tag_updt,
  full.names = T
)

stopifnot(length(path) == 1)

crhm_output_updated <- CRHMr::readOutputFile(
  path[1],
  timezone = 'Etc/GMT+6')# |> filter(datetime %in% obs_tree$datetime)

crhm_met <- crhm_output_updated |> 
  select(datetime, hru_t.1, hru_p.1, hru_rain.1) |> 
  pivot_longer(!datetime) |> 
  mutate(group = name)

mod_tree <- crhm_output_updated |> 
  select(datetime, ellis2010 = Snow_load.1, simulated_new = m_s_veg.1)

obs_mod_tree <- left_join(mod_tree, obs_tree)

obs_mod_tree_long <- obs_mod_tree |> 
  pivot_longer(c(tree_mm, ellis2010, simulated_new)) |> 
  mutate(group = 'w_tree') |> 
  rbind(crhm_met)

obs_mod_tree_long |> 
  ggplot(aes(datetime, value, colour = name)) +
  geom_line() +
  facet_grid(rows = vars(group), scales = 'free')
plotly::ggplotly()


