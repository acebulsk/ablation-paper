# Bring in values for paper
model_run_tag <- 'no_subl_unld_updt_wind_pars'
fig_tbl_tag <- 'compare_baseline_updated_psp_w_new_wind_unld_w_temp_par'

obs_mod_w_tree_fig <- paste0(
  'figs/crhm-analysis/subl_drip_unld/select_post_agu_events/',
  fig_tbl_tag,
  '/obs_mod_canopy_snow_load.png'
)

select_models <- c(
  'M1', # best one with L, u and melt
  'M10', # as above but adds temp
  'M24', # as w M1 but adds subl
  'M4', # as w M1 but tau instead of u
  'M7', # just sublimation and melt
  'M40', # L , wind, air temp
  'M63' # L, wind, subl
)

lm_multi_reg_tbl <- readRDS('data/stats/lm_multi_reg_q_unld_bins.rds') 
lm_multi_reg_tbl_slim <- lm_multi_reg_tbl |> 
  filter(`Model Name` %in% select_models) |> 
  select(-T_ib_dep)

# find where only melt term is non 'nan'
melt_only_r2 <- lm_multi_reg_tbl$Adj_R2[lm_multi_reg_tbl$u == '—' &
                                          lm_multi_reg_tbl$q_subl == '—' &
                                          lm_multi_reg_tbl$T_a == '—' &
                                          lm_multi_reg_tbl$tau == '—' &
                                          lm_multi_reg_tbl$T_ib_dep == '—']

wind_tau_err_tbl <- readRDS('data/modelled_combined_wind_tau_unloading_error_table.rds') 
mod_coef_wind_a <- wind_tau_err_tbl$Wind[wind_tau_err_tbl$Metric == 'Coefficient a'] |> as.numeric()
mod_coef_wind_b <- wind_tau_err_tbl$Wind[wind_tau_err_tbl$Metric == 'Coefficient b']
q_unld_wind_r2 <- wind_tau_err_tbl$Wind[wind_tau_err_tbl$Metric == "Coefficient of Determination ($R^2$)"]

mod_coef_tau_a <- wind_tau_err_tbl$`Shear Stress`[wind_tau_err_tbl$Metric == 'Coefficient a'] |> as.numeric()
mod_coef_tau_b <- wind_tau_err_tbl$`Shear Stress`[wind_tau_err_tbl$Metric == 'Coefficient b']
q_unld_tau_r2 <- wind_tau_err_tbl$`Shear Stress`[wind_tau_err_tbl$Metric == "Coefficient of Determination ($R^2$)"]

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

event_ablation_frac <- readRDS('data/ablation_event_fraction_ablation_processes.rds')

event_met_abl_frac <- left_join(event_met, event_ablation_frac)

obs_mod_stats_avg <- readRDS(paste0(
  'tbls/',
  'obs_mod_canopy_snow_load_err_tbl_avg_',
  model_run_tag,
  '.rds'
))

new_model_mb_avg <- obs_mod_stats_avg$MB[obs_mod_stats_avg$name == 'simulated_new'] |> round(2)
old_mods_mb_range <- obs_mod_stats_avg$MB[!obs_mod_stats_avg$name == 'simulated_new'] |> range() |> round(2)

obs_mod_stats_event_avg <- readRDS(paste0(
  'tbls/',
  'obs_mod_canopy_snow_load_err_hourly_event_type',
  model_run_tag,
  '.rds'
))

melt_new_model_mb_avg <- obs_mod_stats_event_avg |>
  filter(event_type == 'melt', name == 'simulated_new') |>
  pull(MB) |> 
  round(2)

melt_other_mb_avg <- obs_mod_stats_event_avg |>
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

