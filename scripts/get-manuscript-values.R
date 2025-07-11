# Bring in values for paper
library(dplyr)
library(gt)

pretty_table <- readRDS('tbls/select_event_met_stats_maxmin_pretty_gt_table.rds')

model_run_tag <- 'store_liquid_new_evap6'

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

event_ablation_frac <- readRDS('data/ablation_event_fraction_ablation_processes.rds') |> 
  mutate(event_id = as.Date(event_id))

event_met_abl_frac <- left_join(event_met, event_ablation_frac)

obs_mod_stats_avg <- readRDS( paste0(
  'tbls/',
  'mb_by_event_w_mean_overall',
  model_run_tag,
  '.rds'
))

new_model_mb_avg <- obs_mod_stats_avg$MB[obs_mod_stats_avg$name == 'CP25' &
                                           obs_mod_stats_avg$manual_event_type == 'all'] |> round(2)
old_mods_mb_range <- obs_mod_stats_avg$MB[!obs_mod_stats_avg$name == 'CP25' &
                                            obs_mod_stats_avg$manual_event_type == 'all'] |> range() |> round(2)

melt_new_model_mb_avg <- obs_mod_stats_avg |>
  filter(manual_event_type == 'warm/humid', name == 'CP25') |>
  pull(MB) |> 
  round(2)

melt_a09_mb_avg <- obs_mod_stats_avg |>
  filter(manual_event_type == 'warm/humid', name == 'SA09') |>
  pull(MB) |> 
  round(2)

melt_other_mb_avg <- obs_mod_stats_avg |>
  filter(manual_event_type == 'warm/humid', name %in% c('E10', 'R01')) |>
  pull(MB) |> 
  range() |> 
  round(2)

wd_all_mb <- obs_mod_stats_avg |>
  filter(manual_event_type == 'warm/dry') |>
  pull(MB) |> 
  range() |> 
  round(3)

wd_new_model_mb_avg <- obs_mod_stats_avg |>
  filter(manual_event_type == 'warm/dry', name == 'CP25') |>
  pull(MB) |> 
  round(3)

wd_other_mb_avg <- obs_mod_stats_avg |>
  filter(manual_event_type == 'warm/dry', name != 'CP25') |>
  pull(MB) |> 
  range() |> 
  round(2)

subl_all_mb <- obs_mod_stats_avg |>
  filter(manual_event_type == 'cold/dry') |>
  pull(MB) |> 
  range() |> 
  round(2)

subl_new_model_mb_avg <- obs_mod_stats_avg |>
  filter(manual_event_type == 'cold/dry', name == 'CP25') |>
  pull(MB) |> 
  round(3)

subl_other_mb_avg <- obs_mod_stats_avg |>
  filter(manual_event_type == 'cold/dry', name != 'CP25') |>
  pull(MB) |> 
  range() |> 
  round(2)

wind_new_model_mb_avg <- obs_mod_stats_avg |>
  filter(manual_event_type == 'cold/humid', name == 'CP25') |>
  pull(MB) |> 
  round(2)

wind_roesch_mb_avg <- obs_mod_stats_avg |>
  filter(manual_event_type == 'cold/humid', name == 'R01') |>
  pull(MB) |> 
  round(2)

wind_nr_mb_avg <- obs_mod_stats_avg |>
  filter(manual_event_type == 'cold/humid', !name %in% c('CP25', 'R01')) |>
  pull(MB) |> 
  min() |> 
  round(2)


atm_ground_part <- readRDS('data/atmosphere_ground_partition_by_model.rds')

