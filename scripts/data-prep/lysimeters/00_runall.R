library(dplyr)
library(tidyverse)
library(plotly)
library(viridis)
library(ggpubr)

Lmax <- 50 # after Storck2002
quality_th <- 3
options(ggplot2.discrete.colour= c("#000000", "#E69F00", "#56B4E9"))
options(ggplot2.discrete.fill= c("#000000", "#E69F00", "#56B4E9"))

q_unl_max <- 7
q_unl_min <- 0
ppt_fltr <- 0

hourly <- F # should we convert the 15 min raw data to hourly before processing?

good_wind <- 'u' # this is the qc and gap filled wind from FFR ultrasonic/3cup/pwlrmyoung
good_temp <- 't' # this is the mid tree FFR air temp
# good_temp <- 't_ice_bulb' # this is the mid tree FFR air temp converted to icebulb
# good_tau <- 'tau_high' # selects the 15 m high EC data at 30 min intervals
good_tau <- 'tau_low' # selects the 3 m high EC data (at 15 min intervals)

# q_unl_max <- 2
stdep_subl_fltr <- 10
stdep_tree_abl_fltr <- 40
stdep_unl_fltr <- 25 # we want unloading rates less than 25 standard devs from the mean, above this val obs seem to be outliers not associated with high wind or temp
# q_unl_max_cold <- 0.15 # something else going on below -25 deg celcius, after checking the timelapse imagery it doesnt appear to be related to wind or temps
# temp_th <- -25 

q_unl_ax_lab <- 'Unloading Rate (mm/hr)'
wind_bin_ax_lab <- 'Average Wind Speed Bins (m/s)'

if(good_temp == 't'){
  temp_bin_ax_lab <- 'Air Temperature Bins (°C)'
  temp_ax_lab <- 'Air Temperature (°C)'
}else{
  temp_bin_ax_lab <- 'Ice Bulb Temperature Bins (°C)'
  temp_ax_lab <- 'Ice Bulb Temperature (°C)'
}
wind_ax_lab <- 'Average Wind Speed (m/s)'
if(good_tau == 'tau_high'){
  tau_ax_lab <- 'Shear Stress at Canopy Top (N m-2)'
}
if(good_tau == 'tau_low'){
  tau_ax_lab <- 'Shear Stress Below Canopy (N m-2)'
}
vs_ax_lab <- 'Dimensionless Sublimation Rate (s-1)'

warm_event_class <- c(
  ' windy and warming',
  " warming"
)

windy_event_class <- c(
  ' windy and warming',
  " windy"
)

source('scripts/data-prep/lysimeters/01_define_functions.R')
source('scripts/data-prep/lysimeters/02_load_data.R')
source('scripts/data-prep/lysimeters/03_compute_del_unload.R')
source('scripts/data-prep/lysimeters/04_compute_sublimation_rate.R')
source('scripts/data-prep/lysimeters/05_bin_dependent_vars.R')
source('scripts/data-prep/lysimeters/06_combine_and_save_data.R')
source('scripts/data-prep/lysimeters/07_plot_loads.R')
