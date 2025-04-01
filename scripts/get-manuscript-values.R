# Bring in values for paper
fig_tbl_tag <- 'compare_baseline_updated_psp_w_new_wind_unld_w_temp_par'

obs_mod_w_tree_fig <- paste0(
  'figs/crhm-analysis/subl_drip_unld/select_post_agu_events/',
  fig_tbl_tag,
  '/obs_mod_canopy_snow_load.png'
)

# mod_coef_wind <- readRDS('data/model_coef_wind_temp_unld_per_second.rds')
mod_coef_wind <- readRDS('data/model_coef_wind_unld_per_second.rds') # prior to adding temp inside wind unld fun
mod_coef_duration <- readRDS('data/model_coef_duration_unld_per_second.rds')

q_unld_melt_lm <- readRDS('data/lm_q_drip_vs_q_unld_melt.rds')
q_unld_melt_b <- coef(q_unld_melt_lm)[[1]] |> round(2)
q_unld_melt_m <- coef(q_unld_melt_lm)[[2]] |> round(2)
q_unld_melt_r2 <- summary(q_unld_melt_lm)$r.squared |> round(2)
q_unld_melt_p <- summary(q_unld_melt_lm)$coefficients[2, 4] |> round(7)

q_unld_subl_lm <- readRDS('data/lm_q_drip_vs_q_unld_subl.rds')
q_unld_subl_b <- coef(q_unld_subl_lm)[[1]] |> round(2)
q_unld_subl_m <- coef(q_unld_subl_lm)[[2]] |> round(2)
q_unld_subl_r2 <- summary(q_unld_subl_lm)$r.squared |> round(2)
q_unld_subl_p <-  summary(q_unld_subl_lm)$coefficients[2, 4] |> round(7)


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

event_met_abl_frac <- left_join(event_met, event_ablation_frac)

obs_mod_w_tree_err_tbl_typed <- read.csv('tbls/obs_mod_canopy_snow_load_err_hourly_event_type0.2_melt_ratio_0.1.csv')

manuscript_run_tag <- "init_run_cansnobal_v_1_1_unld_ratios_no_origin3"
obs_mod_stats_avg <- readRDS(paste0(
  'tbls/',
  'obs_mod_canopy_snow_load_err_tbl_avg_',
  manuscript_run_tag,
  '.rds'
))

new_model_mb_avg <- obs_mod_stats_avg$MB[obs_mod_stats_avg$name == 'simulated_new'] |> round(2)
old_mods_mb_range <- obs_mod_stats_avg$MB[!obs_mod_stats_avg$name == 'simulated_new'] |> range() |> round(2)

obs_mod_stats_event_avg <- readRDS(paste0(
  'tbls/',
  'obs_mod_canopy_snow_load_err_hourly_event_type',
  manuscript_run_tag,
  '.rds'
))

melt_new_model_mb_avg <- obs_mod_stats_event_avg |>
  filter(event_type == 'melt', name == 'simulated_new') |>
  pull(MB) |> 
  round(2)

melt_new_model_mb_avg <- obs_mod_stats_event_avg |>
  filter(event_type == 'melt', !name == 'simulated_new') |>
  pull(MB) |> 
  range() |> 
  round(2)

wind_new_model_mb_avg <- obs_mod_stats_event_avg |>
  filter(event_type == 'wind', name == 'simulated_new') |>
  pull(MB) |> 
  round(2)

wind_roesch_mb_avg <- obs_mod_stats_event_avg |>
  filter(event_type == 'wind', name == 'roesch2001') |>
  pull(MB) |> 
  round(2)

wind_nr_mb_avg <- obs_mod_stats_event_avg |>
  filter(event_type == 'wind', !name %in% c('simulated_new', 'roesch2001')) |>
  pull(MB) |> 
  mean() |> 
  round(1)
