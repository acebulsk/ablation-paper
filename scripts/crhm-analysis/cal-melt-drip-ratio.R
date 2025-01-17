# Script to find best value for melt to drip ratio
library(tidyverse)

read_obs_par <- function(path, par){
  crhm_output <- CRHMr::readOutputFile(
    path,
    timezone = 'Etc/GMT+6')
  crhm_output$ratio <- par
  return(crhm_output)
}

warm_events <- c('2022-04-23', '2022-06-14', '2022-06-24', '2023-05-08', '2023-06-15', '2023-06-21')

obs_tree <-
  readRDS('data/clean-data/warm_tree_events_zero_weighed_tree_fsd_closed_0.88_kg_m2_post_cnpy_snow.rds') |> 
  select(datetime, event_id, observed = value) |> 
  filter(event_id %in% warm_events)

prj <- "closed_canopy_cc0.88_vector_based_new_ablation_psp"

# specify certain model run
cal_tag <- 'cal_melt_mass_unloading_to_ratio'

# just get last x model runs
model_runs <- list.files(
  paste0(
    "../../analysis/crhm-analysis/output/",
    prj
  ),
  pattern = cal_tag,
  full.names = T
)

ratios <- gsub(".*_ratio_([0-9_\\.]+)_output\\.txt", "\\1", model_runs)
pars <- gsub("_", ".", ratios) |> as.numeric()

crhm_output <- map2(model_runs, pars, read_obs_par) |> bind_rows()
mod_tree <- crhm_output |> 
  select(datetime, simulated = Snow_load.1, ratio)
  
obs_mod_tree <- left_join(obs_tree, mod_tree)

obs_mod_tree_err_tbl <- obs_mod_tree |> 
    group_by(event_id, ratio) |> 
    mutate(diff = observed - simulated) |> 
    # group_by(name) |> 
    summarise(
      `Mean Bias` = mean(diff, na.rm = T),
      MAE = mean(abs(diff), na.rm = T),
      `RMS Error` = sqrt(mean(diff ^ 2, na.rm = T)),
      # NRMSE = `RMS Error` / (max(observed, na.rm = TRUE) - min(observed, na.rm = TRUE)),
      NRMSE = `RMS Error` / mean(observed, na.rm = T),
      R = cor(observed, simulated),
      `r^2` = R^2) |> 
    mutate(across(`Mean Bias`:`r^2`, round, digits = 3))

ggplot(obs_mod_tree_err_tbl, aes(ratio, `Mean Bias`, colour = as.factor(event_id))) + 
  geom_point()

summ <- obs_mod_tree |> 
  group_by(ratio) |> 
  mutate(diff = observed - simulated) |> 
  # group_by(name) |> 
  summarise(
    `Mean Bias` = mean(diff, na.rm = T),
    MAE = mean(abs(diff), na.rm = T),
    `RMS Error` = sqrt(mean(diff ^ 2, na.rm = T)),
    # NRMSE = `RMS Error` / (max(observed, na.rm = TRUE) - min(observed, na.rm = TRUE)),
    NRMSE = `RMS Error` / mean(observed, na.rm = T),
    R = cor(observed, simulated),
    `r^2` = R^2) |> 
  mutate(across(`Mean Bias`:`r^2`, round, digits = 3))
summ

summ |> 
  slice_min(order_by = abs(`Mean Bias`), n = 1)


