# Bring in values for paper
fig_tbl_tag <- 'compare_baseline_updated_psp_w_new_wind_unld_w_temp_par'

obs_mod_w_tree_fig <- paste0(
  'figs/crhm-analysis/subl_drip_unld/select_post_agu_events/',
  fig_tbl_tag,
  '/obs_mod_canopy_snow_load.png'
)

mod_coef_wind <- readRDS('data/model_coef_wind_temp_unld_per_second.rds')
# mod_coef_wind <- readRDS('data/model_coef_wind_unld_per_second.rds') # prior to adding temp inside wind unld fun
mod_coef_temp <- readRDS('data/model_coef_temp_unld_per_second.rds')
mod_coef_duration <- readRDS('data/model_coef_duration_unld_per_second.rds')

event_met <- readRDS('data/ablation_event_met_summary.rds') |> 
  select(event_id, t = t_mean, u = u_mean, rh = rh_mean, Qsi = Qsi_mean) |> 
  mutate(across(t:Qsi, round, 2))
obs_mod_w_tree_err_tbl <- readRDS("tbls/crhm-snow-load-vs-wtree-errortbl_by_event_compare_baseline_updated_psp_w_new_wind_unld_w_temp_par.rds") |> 
  filter(name == 'simulated_updated') |> 
  select(event_id, 
         `Mean Bias`,
         `RMS Error`, 
         `r^2`)
event_ablation_frac <- readRDS('data/ablation_event_fraction_ablation_processes.rds')
