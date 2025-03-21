# This script plots the performance of the updated canopy snow ablation model
# for select ablation events

library(tidyverse)

base_path <- 'figs/crhm-analysis/ablation-events/'
fig_tbl_tag <- 'cansnobal_init_testing'

# LOAD DATA ----

## OBSERVED ----
obs_tree <- rbind(obs_tree_cold  |> 
                    select(datetime, event_id, observed = tree_mm),
                  obs_tree_warm |> 
                    select(datetime, event_id, observed = tree_mm)) 
select_events_long <- obs_tree |> select(datetime, event_id)

# obs_tree <- obs_tree_post_sf
# select_events_long <- obs_tree |> select(datetime, event_id)

## MODELLED ----

### UPDATED ABLATION MODEL WITH PSP ----
prj_updt <- "ffr_closed_canopy_cc0.88_cansnobal"

# specify certain model run

run_tag_updt <- "real_z0"

path <- list.files(
  paste0(
    "../../analysis/crhm-analysis/output/",
    prj_updt
  ),
  pattern = run_tag_updt,
  full.names = T
)

dt1 <- as.POSIXct('2023-02-20 00:15:00', tz = 'Etc/GMT+6')
dt2 <- as.POSIXct('2023-02-23 00:00:00', tz = 'Etc/GMT+6')

event_dt <- seq(dt1, dt2, by = 900)
crhm_output_updated <- CRHMr::readOutputFile(
  path[length(path)],
  timezone = 'Etc/GMT+6') |> 
  filter(datetime %in% event_dt)
