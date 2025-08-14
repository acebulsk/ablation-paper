library(tidyverse)
library(purrr)
library(broom)

# options(ggplot2.discrete.colour= palette.colors(palette = "R4")[2:6])
# "#DF536B" "#61D04F" "#2297E6" "#28E2E5" "#CD0BBC"
# functions ----

source('../../../Documents/code/stats/lm-through-the-origin/example-r2-from-lm-through-the-origin.R')


label_bin_fn <- function(bins){
  (bins[-1] + bins[-length(bins)]) / 2
}

to_long <- function(from,
                    to,
                    class,
                    quality,
                    bad_troughs,
                    weighed_tree_quality,
                    is_melt_event,
                    notes,
                    event_id) {
  datetime <- seq(from, to, 900)
  
  out <- data.frame(datetime,
                    class,
                    quality,
                    bad_troughs,
                    weighed_tree_quality,
                    is_melt_event,
                    notes,
                    event_id)
  
  return(out)
}

to_long_melt_subl <- function(from,
                              to,
                              class,
                              quality,
                              weighed_tree_quality,
                              mostly_sublimation,
                              bad_troughs,
                              is_melt_event,
                              notes,
                              event_id) {
  datetime <- seq(from, to, 900)
  
  out <- data.frame(datetime,
                    class,
                    quality,
                    weighed_tree_quality,
                    mostly_sublimation,
                    bad_troughs,
                    is_melt_event,
                    notes,
                    event_id)
  
  return(out)
}

# SETUP ----

# "#000000" "#E69F00" "#56B4E9" "#009E73" "#F0E442" "#0072B2" "#D55E00" "#CC79A7" "#999999"
load_suffix <- 'fsd_closed_0.88'

int_fig_width <- 6
int_fig_height <- 4

## axis labels ----
temp_bin_ax_lab <- 'Air Temperature Bins (°C)'
bin_wnd_ax_lab <- "Wind Speed Bins (m/s)"

## thresholds and filters ----
min_snow_bin <- 0.1 # gives a max 10% error based on max scale output of 50 kg ... 50 * (0.02/100) = +/- 0.01 kg
snow_load_th <- 7
min_canopy_snow <- 1.5 # min mm in weighed tree for ablation analysis
min_qunld <- 0.025 # changes below this appear to be due to noise
manual_t_ice_th <- -7.5 # observed in the trough data for inc in unloading above ice bulb temp of -6 
manual_tau_th <- 0.05 # observed increase in trough unloading above this threshold
scl_names <- c('mixed', 'closed') # removed sparse trough here because was obviously leaking

# LOAD DATA ----

ft_met <- 
  readRDS('../../analysis/met-data-processing/data/ffr_crhm_obs_qaqc_gap_fill.rds') 

pwl_met <- 
  readRDS('../../analysis/met-data-processing/data/pwl_crhm_modelling_obs.rds') 

canopy_snow_events <- 
  read.csv('data/raw-data/snow_in_canopy_post_snowfall_fltr_ablation.csv') |> 
  mutate(from =  as.POSIXct(from, tz = 'Etc/GMT+6'),
         to = as.POSIXct(to, tz = 'Etc/GMT+6'),
          event_id = format(from, "%Y-%m-%d %H")) 

events_fltr_long <-
  purrr::pmap_dfr(canopy_snow_events, to_long_melt_subl)

saveRDS(events_fltr_long, 'data/clean-data/ablation_events_long_post_snowfall.rds')

# met_binned <- readRDS('data/clean-data/met_binned_for_unloading_analysis.rds')

## BIN MET DATA ----

### wind ---- 

wind_binned <- ft_met |> 
  select(datetime, u)

# note zeros are not included in binning, to add set inlcude.lowest = T
min_wind <- round(
  min(wind_binned$u, na.rm = T),3)
max_wind <- round(
  max(wind_binned$u, na.rm = T),3)
wind_step <- 0.5

wind_breaks <- seq(
  min_wind,
  max_wind+wind_step,
  wind_step)

wind_labs_seq <- label_bin_fn(bins = wind_breaks)

stopifnot(tail(wind_breaks, 1) > max(wind_binned$u, na.rm = T))
stopifnot(length(wind_labs_seq) + 1 == length(wind_breaks))

wind_binned$wind_binned <- cut(wind_binned[,'u', drop = TRUE], wind_breaks)

wind_binned$wind_labs <- cut(wind_binned[,'u', drop = TRUE], 
                               wind_breaks, 
                               labels = wind_labs_seq)

wind_binned$wind_labs <- as.numeric(as.character(wind_binned$wind_labs))
wind_binned <- wind_binned |> select(-u)

### shear stress ----

# shear stress is calculated below as tau = wind^2 * coef (coef was found using
# observed tau at 3m at forest tower using OLS regression) since we have a lot
# of EC data gaps when we have unloading data

lm_mid_wnd_sqrd_low_tau <-
  readRDS('../../analysis/eddy-cov/data/est_tau_from_wnd/lm_mid_wnd_sqrd_low_tau.rds')

# since we forced the above through the origin we just need to multiply by the slope 

tau_binned <- ft_met |> 
  select(datetime, u) |> 
  mutate(tau = u^2 * lm_mid_wnd_sqrd_low_tau$slope) |> 
  select(-u)

min_mid_can_tau <- 0
max_mid_can_tau <- round(
  max(tau_binned$tau, na.rm = T))

mid_can_tau_step <- 0.05

mid_can_tau_breaks <- seq(
  min_mid_can_tau,
  max_mid_can_tau+0.2,
  mid_can_tau_step)

mid_can_tau_labs_seq <- label_bin_fn(bins = mid_can_tau_breaks)

stopifnot(tail(mid_can_tau_breaks, 1) > max(tau_binned$tau, na.rm = T))
stopifnot(length(wind_labs_seq) + 1 == length(wind_breaks))

tau_binned$tau_binned <- cut(tau_binned[,'tau', drop = TRUE], mid_can_tau_breaks)

tau_binned$tau_labs <- cut(tau_binned[,'tau', drop = TRUE], 
                        mid_can_tau_breaks, 
                        labels = mid_can_tau_labs_seq
)

tau_binned$tau_labs <- as.numeric(as.character(tau_binned$tau_labs))

tau_binned <- tau_binned |> select(-tau)

### temp ---- 

temp_binned <- ft_met |> 
  select(datetime, t)

# note zeros are not included in binning, to add set inlcude.lowest = T
min_temp <- round(
  min(temp_binned$t, na.rm = T),3)
max_temp <- round(
  max(temp_binned$t, na.rm = T),3)
temp_step <- 5

temp_breaks <- seq(
  min_temp,
  max_temp+temp_step,
  temp_step)

temp_labs_seq <- label_bin_fn(bins = temp_breaks)

stopifnot(tail(temp_breaks, 1) > max(temp_binned$t, na.rm = T))
stopifnot(length(temp_labs_seq) + 1 == length(temp_breaks))

temp_binned$temp_binned <- cut(temp_binned[,'t', drop = TRUE], temp_breaks)

temp_binned$temp_labs <- cut(temp_binned[,'t', drop = TRUE], 
                             temp_breaks, 
                             labels = temp_labs_seq)

temp_binned$temp_labs <- as.numeric(as.character(temp_binned$temp_labs))
temp_binned <- temp_binned |> select(-t)

### ice bulb depression ----

ti_dep_binned <- ft_met |> 
  mutate(ti_dep = t - t_ice_bulb) |> 
  select(datetime, ti_dep)

# note zeros are not included in binning, to add set inlcude.lowest = T
min_ti_dep <- round(
  min(ti_dep_binned$ti_dep, na.rm = T),3)
max_ti_dep <- round(
  max(ti_dep_binned$ti_dep, na.rm = T),3)
ti_dep_step <- 0.5

ti_dep_breaks <- seq(
  min_ti_dep,
  max_ti_dep+ti_dep_step,
  ti_dep_step)

ti_dep_labs_seq <- label_bin_fn(bins = ti_dep_breaks)

stopifnot(tail(ti_dep_breaks, 1) > max(ti_dep_binned$ti_dep, na.rm = T))
stopifnot(length(ti_dep_labs_seq) + 1 == length(ti_dep_breaks))

ti_dep_binned$ti_dep_binned <- cut(ti_dep_binned[,'ti_dep', drop = TRUE], ti_dep_breaks)

ti_dep_binned$ti_dep_labs <- cut(ti_dep_binned[,'ti_dep', drop = TRUE], 
                             ti_dep_breaks, 
                             labels = ti_dep_labs_seq)

ti_dep_binned$ti_dep_labs <- as.numeric(as.character(ti_dep_binned$ti_dep_labs))
ti_dep_binned <- ti_dep_binned |> select(-ti_dep)

## SCL data ----

bad_scl_events <- canopy_snow_events$event_id[canopy_snow_events$quality>2]

q_unld_scl <- 
  readRDS('data/clean-data/ft_scl_data_del_15_min.rds') |> 
  # inner_join(events_fltr_long |> select(datetime, event_id, quality, bad_troughs), by = 'datetime') |> 
  # remove some of the unloading obs where we observed one of the instruments to be faulty
  # mutate(value_flag = name == bad_troughs) |> # this is not a bug!
  filter(!event_id %in% bad_scl_events)

# need this for computing ins. error
scl_df_kg_m2_raw <- readRDS('data/raw-data/treefort_load_main.rds')
scl_df_kg_m2_raw_long <- scl_df_kg_m2_raw |>
  select(datetime, mixed = trough_1, sparse = trough_2, closed = trough_3) |> 
  pivot_longer(!datetime, values_to = 'scl_raw_kg_m2') |> 
  group_by(name) |> 
  fill(scl_raw_kg_m2, .direction = 'down') # NaN fill ok here as just used for error calc
  
## Weighed tree data ----

q_unld_tree <-
  readRDS( paste0(
    'data/clean-data/unloading_events_zero_weighed_tree_kg_m2_pre_post_cnpy_snow_',
    load_suffix,
    '.rds'
  )) |> 
  select(datetime:tree_mm) |> 
  left_join(events_fltr_long |> select(datetime, event_id, weighed_tree_quality, notes), by = 'datetime') |>
  filter(weighed_tree_quality < 3) 

q_unld_tree_all_troughs <-
  readRDS('data/clean-data/unloading_events_zero_weighed_tree_kg_m2_pre_post_cnpy_snow_fsd_cal_for_each_trough_vza_15.rds') |> 
  select(datetime, name = tree_cal_trough_name, tree_mm) |> 
  left_join(events_fltr_long |> select(datetime, event_id, weighed_tree_quality, notes), by = 'datetime') |>
  filter(weighed_tree_quality < 3) 


# need cal value to get weight of tree in kg for instrument error check
tree_cal_val_88 <- readRDS('./../../analysis/interception/data/loadcell/FFR_tree_cal_value_pluvio_fsd_closed.rds')

## obs ----

# warm tree specific events
# these ones differ from the cold ones below and may include some precip
warm_events <- c(
  '2022-04-21',
  '2022-04-23',
  '2022-06-14',
  '2022-06-23',
  '2022-06-24',
  '2023-03-14',
  '2023-03-25',
  '2023-03-26',
  '2023-03-28',
  '2023-04-13',
  '2023-04-17',
  '2023-05-08',
  '2023-06-15',
  '2023-06-21'
)

obs_tree_warm <-
  readRDS(paste0(
    'data/clean-data/warm_tree_events_zero_weighed_tree_',
    load_suffix,
    '_kg_m2_post_cnpy_snow.rds'
  )) |> 
  filter(event_id %in% warm_events)

all(warm_events %in% obs_tree_warm$event_id)

# cold tree events
cold_events <- c(
  # new ones
  #'2021-12-27', # wind event some precip, maybe blowing snow redist.
  # '2022-01-18', # wind event too much precip during (maybe blowing snow redistribution?)
  '2022-02-04', # wind event
  #'2022-02-21', # wind event unloading not associated with wind or other here
  # '2022-02-24', # wind event , tree increased due to vapour deposition likely
  # '2022-03-04', # unloading due to branch bending from warming
  # '2022-03-16', # wind event too much precip during (maybe blowing snow redistribution?
  
  # OG
  '2022-03-02', 
  '2022-03-09',
  '2022-03-20', 
  '2022-03-24',  
  '2022-03-29',  
  '2022-12-01',
  '2023-01-28',
  '2023-02-24',
  '2023-02-26'
)
obs_tree_cold <-
  readRDS(paste0(
    'data/clean-data/all_tree_events_zero_weighed_tree_',
    load_suffix,
    '_kg_m2_post_cnpy_snow.rds'
  )) |> 
  filter(event_id %in% cold_events)

obs_tree <- rbind(obs_tree_cold  |> 
                    select(datetime, event_id, observed = tree_mm),
                  obs_tree_warm |> 
                    select(datetime, event_id, observed = tree_mm)) 

w_tree_q_unld_15 <- obs_tree |>
  group_by(event_id) |> 
  mutate(
    dL = lag(observed) - observed,
    dL = ifelse(dL < 0, 0, dL)
  ) |>
  select(datetime, event_id, tree_mm = observed, dL)

saveRDS(obs_tree |> select(datetime, event_id), 'data/clean-data/ablation_events_long_warm_cold.rds')

# LOAD MODEL DATA ----

mod_d_drip_smry_frac <- readRDS('data/results/ablation_event_fraction_ablation_processes.rds')

# Select model run with all unloading events weighed tree snow load assimilated

prjs <- c('ffr_cc0.19_sparse_cansnobal',
          'ffr_cc0.54_closed_cansnobal',
          'ffr_cc0.63_mixed_cansnobal'
)
keywords <- c("sparse", "closed", "mixed")
# specify certain model run
run_tag <- "run_1"

paths <- list.files(
  paste0(
    "crhm/output/",
    prjs
  ),
  pattern = run_tag,
  full.names = T
)

read_crhm_output <- function(path){
  extracted <- str_extract(path, "sparse|closed|mixed")
  crhm_output <- CRHMr::readOutputFile(
    path,
    timezone = 'Etc/GMT+6') |> 
    mutate(name = extracted)
}

crhm_output <- map_dfr(paths, read_crhm_output) |> 
  select(datetime, name, hru_t.1, m_s_veg.1, hru_p.1,
         delsub_veg_int.1:delunld_subl_int.1) |> 
  mutate(q_subl_veg = -delsub_veg_int.1*4,
         q_melt_veg = delmelt_veg_int.1*4)

### bin mod subl  ----

# note zeros are not included in binning, to add set inlcude.lowest = T
min_subl <- round(
  min(crhm_output$q_subl_veg, na.rm = T),3)
max_subl <- round(
  max(crhm_output$q_subl_veg, na.rm = T),3)
subl_step <- 0.05

subl_breaks <- seq(
  min_subl,
  max_subl+subl_step,
  subl_step)

subl_labs_seq <- label_bin_fn(bins = subl_breaks)

stopifnot(tail(subl_breaks, 1) > max(crhm_output$q_subl_veg, na.rm = T))
stopifnot(length(subl_labs_seq) + 1 == length(subl_breaks))

crhm_output$subl_binned <- cut(crhm_output[,'q_subl_veg', drop = TRUE], subl_breaks)

crhm_output$subl_labs <- cut(crhm_output[,'q_subl_veg', drop = TRUE], 
                             subl_breaks, 
                             labels = subl_labs_seq)

crhm_output$subl_labs <- as.numeric(as.character(crhm_output$subl_labs))

### bin snowmelt rate (s-1) ----

# note zeros are not included in binning, to add set inlcude.lowest = T
min_sm <- 0
max_sm <- round(
  max(crhm_output$q_melt_veg, na.rm = T),3)
sm_step <- 0.5

sm_breaks <- seq(
  0,
  max_sm+0.5,
  sm_step)

sm_labs_seq <- label_bin_fn(bins = sm_breaks)

stopifnot(tail(sm_breaks, 1) > max(crhm_output$q_melt_veg, na.rm = T))
stopifnot(length(sm_labs_seq) + 1 == length(sm_breaks))

crhm_output$canopy_snowmelt_labs <- cut(crhm_output[,'q_melt_veg', drop = TRUE], 
                                          sm_breaks, 
                                          labels = sm_labs_seq)

crhm_output$canopy_snowmelt_labs <- as.numeric(as.character(crhm_output$canopy_snowmelt_labs))
crhm_output$canopy_snowmelt_labs <- ifelse(crhm_output$q_melt_veg == 0, 0, crhm_output$canopy_snowmelt_labs )

## Combine dfs ---- 

met_unld_no_melt <- 
  # q_unld_met_scl |> 
  # met_binned has duplicated data for each scaled weighed tree
  # q_unld_scl has been filtered to remove troughs for erroneous periods
  # left_join(q_unld_scl, crhm_output, by = c('datetime', 'name')) |>
  left_join(
    q_unld_scl |> select(datetime, name, q_unl, dU),
    crhm_output |> select(datetime,
                          name,
                          q_subl_veg,
                          subl_labs,
                          m_s_veg.1,
                          delmelt_veg_int.1),     by = c('datetime', 'name')
  ) |>
  left_join(
    q_unld_tree_all_troughs,
    by = c('datetime', 'name')
  ) |>
  left_join(wind_binned) |> 
  left_join(tau_binned) |> 
  left_join(temp_binned) |> 
  left_join(ft_met) |> 
  filter(
    # name %in% scl_names,
    q_unl < 7,
    q_unl > 0,
    # q_subl_veg > 0,
    delmelt_veg_int.1 == 0
    # hru_t.1 < -6
  ) #|> 
  # mutate(
  #   tree_mm = ifelse(is.na(tree_mm), m_s_veg.1, tree_mm) # fill missing tree data with model
  # )
