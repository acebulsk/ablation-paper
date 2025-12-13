# Bring in values for paper
library(dplyr)
library(gt)
library(knitr)
library(kableExtra)

convert_sci_to_latex <- function(x) {
  # Ensure x is character
  x <- as.character(x)

  # Pattern for scientific notation: integer/decimal + e/E + signed exponent
  sci_pattern <- "^[+-]?[0-9]*\\.?[0-9]+[eE][+-]?[0-9]+$"

  # Identify which elements match
  is_sci <- grepl(sci_pattern, x)

  # Convert scientific notation to LaTeX
  x[is_sci] <- sub(
    "([+-]?[0-9]*\\.?[0-9]+)[eE]([+-]?[0-9]+)",
    "\\1 \\\\times 10^{\\2}",
    x[is_sci]
  )

  # Only wrap in $...$ if not already wrapped
  x[is_sci] <- ifelse(
    grepl("^\\$.*\\$$", x[is_sci]),
    x[is_sci],
    paste0("$", x[is_sci], "$")
  )

  x
}

# model_run_tag <- 'store_liquid_new_evap6'
model_run_tag <- "new_shear_stress_melt_unld_par_no_hetero" # after permission to defense adjustments to shear stress and melt functions to handle heteroscadesticity

select_models <- c(
  'M1', # best one with L, u and melt
  'M10', # as above but adds temp
  'M24', # as w M1 but adds subl
  'M4', # as w M1 but tau instead of u
  'M7', # just sublimation and melt
  'M40', # L , wind, air temp
  'M63' # L, wind, subl
)

# Results 

## 3.1.1 dry snow unloading error tbls ----

# fltr_models <- c('Shear Stress, Air Temp.') # could leave out as air temp is insig.

dry_snow_unld_stats <- readRDS('data/results/modelled_combined_wind_tau_others_unloading_error_table.rds') |>
  # select(-fltr_models) |> 
  mutate(across(-Metric, ~ convert_sci_to_latex(.))) |> 
  mutate(
    Metric = case_when(
      Metric == "Model"                          ~ "Fit",
      Metric == "Equation"                       ~ "Eq",
      Metric == "Mean Bias (mm/hr)"              ~ "MB",
      Metric == "Root Mean Square Error (mm/hr)" ~ "RMSE",
      Metric == "Coefficient of Determination"   ~ "$R^2$",
      Metric == "Coefficient of Agreement"       ~ "$d$",
      Metric == "Coefficient a"                  ~ "a",
      Metric == "Significance of a"              ~ "$p$(a)",
      Metric == "Coefficient b"                  ~ "b",
      Metric == "Significance of b"              ~ "$p$(b)",
      Metric == "Homoscedasticity"               ~ "HSCD",
      Metric == "Normality"                      ~ "Norm",
      TRUE ~ Metric
    )
  )

q_unld_wind_rmse <- dry_snow_unld_stats$`Wind Speed`[dry_snow_unld_stats$Metric == "RMSE"]
q_unld_tau_rmse <- dry_snow_unld_stats$`Shear Stress`[dry_snow_unld_stats$Metric == "RMSE"]
q_unld_load_rmse <- dry_snow_unld_stats$`Snow Load`[dry_snow_unld_stats$Metric == "RMSE"]

q_unld_wind_r2 <- dry_snow_unld_stats$`Wind Speed`[dry_snow_unld_stats$Metric == "$R^2$"]
q_unld_tau_r2 <- dry_snow_unld_stats$`Shear Stress`[dry_snow_unld_stats$Metric == "$R^2$"]
q_unld_load_r2 <- dry_snow_unld_stats$`Snow Load`[dry_snow_unld_stats$Metric == "$R^2$"]

## 3.1.2 melt unloading error tbls ----

melt_unld_stats <- readRDS('data/results/modelled_melt_unloading_error_table.rds') |>
  mutate(across(-Metric, ~ convert_sci_to_latex(.))) |> 
  mutate(
    Metric = case_when(
      Metric == "Model"                          ~ "Fit",
      Metric == "Equation"                       ~ "Eq",
      Metric == "Mean Bias (mm/hr)"              ~ "MB",
      Metric == "Root Mean Square Error (mm/hr)" ~ "RMSE",
      Metric == "Coefficient of Determination"   ~ "$R^2$",
      Metric == "Coefficient of Agreement"       ~ "$d$",
      Metric == "Coefficient a"                  ~ "a",
      Metric == "Significance of a"              ~ "$p$(a)",
      Metric == "Coefficient b"                  ~ "b",
      Metric == "Significance of b"              ~ "$p$(b)",
      Metric == "Homoscedasticity"               ~ "HSCD",
      Metric == "Normality"                      ~ "Norm",
      TRUE ~ Metric
    )
  )

q_unld_melt_tau_rmse <- melt_unld_stats$`Snowmelt Rate, Shear Stress (from non-melt)`[melt_unld_stats$Metric == "RMSE"]
q_unld_melt_tau_r2 <- melt_unld_stats$`Snowmelt Rate, Shear Stress (from non-melt)`[melt_unld_stats$Metric == "$R^2$"]

q_unld_melt_rmse <- melt_unld_stats$`Snowmelt Rate`[melt_unld_stats$Metric == "RMSE"]
q_unld_melt_r2 <- melt_unld_stats$`Snowmelt Rate`[melt_unld_stats$Metric == "$R^2$"]

q_unld_ta_rmse <- melt_unld_stats$`Air Temperature`[melt_unld_stats$Metric == "RMSE"]
q_unld_ta_r2 <- melt_unld_stats$`Air Temperature`[melt_unld_stats$Metric == "$R^2$"]

q_unld_ti_rmse <- melt_unld_stats$`Ice-Bulb Temperature`[melt_unld_stats$Metric == "RMSE"]
q_unld_ti_r2 <- melt_unld_stats$`Ice-Bulb Temperature`[melt_unld_stats$Metric == "$R^2$"]

## unld to melt ratio tbl ----

unld_melt_ratio_stats <- readRDS('data/results/modelled_unld_melt_ratio_error_table.rds')

q_unld_melt_ratio_b <- unld_melt_ratio_stats$Value[unld_melt_ratio_stats$Metric == 'Coefficient b'] |>
  as.numeric() |> round(2)
q_unld_melt_ratio_b_pval <- unld_melt_ratio_stats$Value[unld_melt_ratio_stats$Metric == 'Significance of a']

q_unld_melt_ratio_m <- unld_melt_ratio_stats$Value[unld_melt_ratio_stats$Metric == 'Coefficient a'] |>
  as.numeric() |> round(2)
q_unld_melt_ratio_r2 <- unld_melt_ratio_stats$Value[unld_melt_ratio_stats$Metric == 'Coefficient of Determination'] |>
  as.numeric() |> round(2)
q_unld_melt_ratio_rmse <- unld_melt_ratio_stats$Value[unld_melt_ratio_stats$Metric == 'Root Mean Square Error (-)'] |>
  as.numeric() |> round(2)

## Event Model Eval -----

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
  filter(manual_event_type == 'warm & humid', name == 'CP25') |>
  pull(MB) |> 
  round(2)

melt_a09_mb_avg <- obs_mod_stats_avg |>
  filter(manual_event_type == 'warm & humid', name == 'SA09') |>
  pull(MB) |> 
  round(2)

melt_other_mb_avg <- obs_mod_stats_avg |>
  filter(manual_event_type == 'warm & humid', name %in% c('E10', 'R01')) |>
  pull(MB) |> 
  range() |> 
  round(2)

wd_all_mb <- obs_mod_stats_avg |>
  filter(manual_event_type == 'warm & dry') |>
  pull(MB) |> 
  round(2)

stopifnot(all(wd_all_mb == wd_all_mb[1])) # currently text states all of these are equal ...

wd_new_model_mb_avg <- obs_mod_stats_avg |>
  filter(manual_event_type == 'warm & dry', name == 'CP25') |>
  pull(MB) |> 
  round(2)

wd_other_mb_avg <- obs_mod_stats_avg |>
  filter(manual_event_type == 'warm & dry', name != 'CP25') |>
  pull(MB) |> 
  range() |> 
  round(2)

subl_all_mb <- obs_mod_stats_avg |>
  filter(manual_event_type == 'cold & dry') |>
  pull(MB) |> 
  range() |> 
  round(2)

subl_new_model_mb_avg <- obs_mod_stats_avg |>
  filter(manual_event_type == 'cold & dry', name == 'CP25') |>
  pull(MB) |> 
  round(3)

subl_other_mb_avg <- obs_mod_stats_avg |>
  filter(manual_event_type == 'cold & dry', name != 'CP25') |>
  pull(MB) |> 
  range() |> 
  round(2)

wind_new_model_mb_avg <- obs_mod_stats_avg |>
  filter(manual_event_type == 'cold & humid', name == 'CP25') |>
  pull(MB) |> 
  round(2)

wind_roesch_mb_avg <- obs_mod_stats_avg |>
  filter(manual_event_type == 'cold & humid', name == 'R01') |>
  pull(MB) |> 
  round(2)

wind_nr_mb_avg <- obs_mod_stats_avg |>
  filter(manual_event_type == 'cold & humid', !name %in% c('CP25', 'R01')) |>
  pull(MB) |> 
  min() |> 
  round(2)

atm_ground_part <- readRDS('data/results/atmosphere_ground_partition_by_model.rds')

## Bootstraping ----

boot <- read.csv(
          paste0(
          'tbls/',
          'bootstrap_output_',
          model_run_tag,
          '.csv'))

cp25_boot <- as.list(boot |> filter(name == 'CP25') |> select(metric, estimate) |> tibble::deframe() |> round(2)) 
not_cp25_boot_kge <- boot |> filter(name != 'CP25', metric == 'KGE') |> select(name, estimate) |> pull(estimate)  |> max()  |> round(2)

other_boot_nse_range <- boot |> 
  filter(name != 'CP25', metric == 'NSE') |>
  pull(estimate) |> range() |> round(2)

other_boot_kge_range <- boot |> 
  filter(name != 'CP25', metric == 'KGE') |>
  pull(estimate) |> range() |> round(2)

# Supporting Information ----

melt_non_melt_var_dist_test <- read.csv('data/stats/melt_non_melt_variable_distribution_tests.csv')
