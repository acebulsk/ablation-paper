# Plot some 'laught tests' aka parameterization sensitivity tests
library(tidyverse)
palette.colors(palette = "Okabe-Ito")

theory_colours <- c("#30123BFF", "#1AE4B6FF", "#FABA39FF", "#7A0403FF") #

wind_ax_lab <- 'Wind Speed (m/s)'
temp_ax_lab <- 'Air Temperature (°C)'
int_fig_width <- 6
int_fig_height <- 4

## Wind Induced Unloading ----

min_t_unld <- 270.16 # K, default from Roesch 2001
rate_t_unld <- 1.87e5 # K s-1, default roesch 2001 unloading rate due to temp
min_u_unld <- 0 # m/s, default from Roesch 2001
rate_u_unld <- 1.56e5 # m , default roesch 2001 unloading rate due to wind

# roesch 2001 temp unloading function, well assume air temp equivilent to the
# canopy temp
roesch_t_unld <- function(Ta){
  temp_unld <- max(c((Ta + 273.15) - min_t_unld, 0))/rate_t_unld
  return(temp_unld) # s-1
}

# roesch 2001 wind unloading function, well assume our 10 m wind speed is at the
# canopy top
roesch_u_unld <- function(u){
  wind_unld <- if_else(u >= min_u_unld,
                       abs(u)/rate_u_unld,
                       0)
  
  return(wind_unld) # s-1
}

roesch_unld <- function(forcing) {
  for (i in 1:(nrow(forcing))) {
    forcing$q_t_unld[i] <- roesch_t_unld(forcing$Ta[i])
    forcing$q_u_unld[i] <- roesch_u_unld(forcing$U[i])
    forcing$q_ablate[i] <- forcing$W[i] * (forcing$q_t_unld[i] + forcing$q_u_unld[i]) * 60 * 60
  }
  return(forcing)
}

winds <- seq(0, 5, by = 0.5)
met_windy_5 <- data.frame(
  U = winds,
  Ta = rep(-10, length(winds)),
  W = rep(5,  length(winds)),
  canopy_load = 5
) |>
  mutate(roesch_t_unld = roesch_t_unld(Ta),
         roesch_u_unld = roesch_u_unld(U),
         q_unld = W * (roesch_t_unld + roesch_u_unld) * 60 * 60 ) # mm/hr

met_windy_10 <- data.frame(
  U = winds,
  Ta = rep(-10, length(winds)),
  W = rep(10,  length(winds)),
  canopy_load = 10
) |>
  mutate(roesch_t_unld = roesch_t_unld(Ta),
         roesch_u_unld = roesch_u_unld(U),
         q_unld = W * (roesch_t_unld + roesch_u_unld) * 60 * 60 ) # mm/hr

met_windy_15 <- data.frame(
  U = winds,
  Ta = rep(-10, length(winds)),
  W = rep(15,  length(winds)),
  canopy_load = 15
) |>
  mutate(roesch_t_unld = roesch_t_unld(Ta),
         roesch_u_unld = roesch_u_unld(U),
         q_unld = W * (roesch_t_unld + roesch_u_unld) * 60 * 60 ) # mm/hr

met_windy_20 <- data.frame(
  U = winds,
  Ta = rep(-10, length(winds)),
  W = rep(20,  length(winds)),
  canopy_load = 20
) |>
  mutate(roesch_t_unld = roesch_t_unld(Ta),
         roesch_u_unld = roesch_u_unld(U),
         q_unld = W * (roesch_t_unld + roesch_u_unld) * 60 * 60 ) # mm/hr

all_dfs_windy <- rbind(met_windy_5, met_windy_10) |>
  rbind(met_windy_15) |>
  rbind(met_windy_20) |> 
  select(`Wind Speed (m/s)` = U, canopy_load, q_unld) |> 
  mutate(group = 'R01')

wind_unld <- ggplot(
  all_dfs_windy,
  aes(`Wind Speed (m/s)`, q_unld, colour = canopy_load, group = canopy_load)
) +
  geom_line() +
  scale_color_viridis_c(end = 0.95) +
  ylab(expression("Unloading Rate (mm hr"^{-1}*")")) +
  xlab(expression("Wind Speed (m s"^{-1}*")")) +
  labs(colour = 'Canopy Load (mm)') +
  facet_grid(~group) 
wind_unld
ggsave(
  'figs/examples/roesch_unloading.png',
  device = png,
  width = int_fig_width,
  height = int_fig_height,
  units = "in"
)

## Time induced unloading ----

# constants for interception
c <- 0.678 # (-) unloading coefficient for ~ weekly application from hp98
k_unld_cold <- -log(c) / (7*24) # per hour, unloading coefficient for cold snow 
k_unld_md <- 6 # unload all canopy snow over 6 hours

# constants for unloading 

Ti_drip <- 4 # deg C, from Floyd2012
Ti_clumps <- 2 # deg C, from Floyd2012

hours <- seq(0, 48, by = 1)

met_load <- data.frame(
  U = 0,
  Ta = -10,
  RH = 0.9,
  W = seq(0, 20, by = 1),
  canopy_load = seq(0, 20, by = 1),
  q_drip = NA,
  q_unload = NA
) 

pseudo_crhm_canopy <- function(forcing) {
  
  for (i in 1:(nrow(forcing))) {
      forcing$q_unload[i] <- forcing$W[i] * k_unld_cold # mm/hr
  }
  
  forcing$q_unload <- ifelse(is.na(forcing$q_unload), 0, forcing$q_unload)
  forcing$q_drip <- ifelse(is.na(forcing$q_drip), 0, forcing$q_drip)
  forcing$q_ablate <- forcing$q_unload + forcing$q_drip
  
  return(forcing)
}

met_temp_hp_time <- pseudo_crhm_canopy(met_load) |> mutate(group = 'E10') |> 
  select(canopy_load, q_unld = q_ablate, group)


load_unld <- ggplot(met_temp_hp_time, aes(canopy_load, q_unld, colour = canopy_load)) + 
  geom_line() +
  ylab(expression("Unloading Rate (mm hr"^{-1}*")")) +
  xlab('Canopy Load (mm)') +
  labs(colour = 'Canopy Load (mm)') + 
  # theme(legend.position = 'none') +
  scale_color_viridis_c(end = 0.95) +
  facet_grid(~group)
  
load_unld
ggsave(
  'figs/examples/unloading_time_hp98.png',
  device = png,
  width = int_fig_width,
  height = int_fig_height,
  units = "in"
)

## Plot wind and time together ---- 

cowplot::plot_grid(wind_unld + theme(legend.position = 'none'),
                   load_unld + ylab(element_blank()), nrow = 1,
                   rel_widths = c(0.41, 0.59))

ggsave(
  'figs/final/figure1.png',
  # 'figs/examples/unloading_wind_load_r01_hp98.png',
  device = png,
  width = 8,
  height = 3.5,
  units = "in"
)

## Temperature induced unloading ----

# constants for interception
c <- 0.678 # (-) unloading coefficient for ~ weekly application from hp98
k_unld_cold <- -log(c) / (7*24) # per hour, unloading coefficient for cold snow 
k_unld_md <- 6 # unload all canopy snow over 6 hours

# constants for unloading 

Ti_drip <- 4 # deg C, from Floyd2012
Ti_clumps <- 2 # deg C, from Floyd2012

temps <- seq(-5, 7, by = 1)

met_temp_5 <- data.frame(
  U = rep(0,  length(temps)),
  Ta = temps,
  RH = rep(.9,  length(temps)),
  W = rep(5,  length(temps)),
  canopy_load = rep(5,  length(temps)),
  q_drip = NA,
  q_unload = NA
) |>
  mutate(
    Ti = psychRomet::ice_bulb_iter(Ta, RH)
  ) 
met_temp_10 <- data.frame(
  U = rep(0,  length(temps)),
  Ta = temps,
  RH = rep(.9,  length(temps)),
  W = rep(10,  length(temps)),
  canopy_load = rep(10,  length(temps)),
  q_drip = NA,
  q_unload = NA
) |>
  mutate(
    Ti = psychRomet::ice_bulb_iter(Ta, RH)
  ) 
met_temp_15 <- data.frame(
  U = rep(0,  length(temps)),
  Ta = temps,
  RH = rep(.9,  length(temps)),
  W = rep(15,  length(temps)),
  canopy_load = rep(15,  length(temps)),
  q_drip = NA,
  q_unload = NA
) |>
  mutate(
    Ti = psychRomet::ice_bulb_iter(Ta, RH)
  ) 
met_temp_20 <- data.frame(
  U = rep(0,  length(temps)),
  Ta = temps,
  RH = rep(.9,  length(temps)),
  W = rep(20,  length(temps)),
  canopy_load = rep(20,  length(temps)),
  q_drip = NA,
  q_unload = NA
) |>
  mutate(
    Ti = psychRomet::ice_bulb_iter(Ta, RH)
  ) 

met_temp <- rbind(met_temp_5, met_temp_10) |> 
  rbind(met_temp_15) |> 
  rbind(met_temp_20) 

pseudo_crhm_canopy <- function(forcing) {
  
  for (i in 1:(nrow(forcing))) {
    
    # # canopy snow unloading
    if (forcing$Ti[i] >= Ti_drip) {
      forcing$q_drip[i] <- forcing$W[i] / k_unld_md # mm/hr
      delWdrip <- forcing$q_drip[i] # mm unloaded due to drip
      forcing$W[i] <- forcing$W[i] - delWdrip
    } else if (forcing$Ti[i] >= Ti_clumps) {
      delWdrip = 0
      forcing$q_unload[i] <- forcing$W[i] / k_unld_md # mm/hr
      delWunld <- forcing$q_unload[i] # mm unloaded due to mass clumps 
      forcing$W[i] <- forcing$W[i] - delWunld
    } else if (forcing$Ti[i] < Ti_clumps) {
      delWdrip = 0
      forcing$q_unload[i] <- forcing$W[i] * k_unld_cold # mm/hr
    }
  }
  
  forcing$q_unload <- ifelse(is.na(forcing$q_unload), 0, forcing$q_unload)
  forcing$q_drip <- ifelse(is.na(forcing$q_drip), 0, forcing$q_drip)
  forcing$q_ablate <- forcing$q_unload + forcing$q_drip
  
  return(forcing)
}

met_temp_hp <- pseudo_crhm_canopy(met_temp) |> mutate(group = 'E10') |> 
  select(Ta, q_ablate, group, canopy_load)

met_temp_ra <- roesch_unld(met_temp) |> mutate(group = 'R01') |> 
  select(Ta, q_ablate, group, canopy_load)

plot_unld_temp <- rbind(met_temp_ra, met_temp_hp)

ggplot(plot_unld_temp, aes(Ta, q_ablate, colour = canopy_load, group = canopy_load)) + 
  geom_line() +
  ylab(expression("Unloading + Drip Rate (mm hr"^{-1}*")")) +
  xlab(temp_ax_lab) +
  labs(colour = 'Canopy Load (mm)') + 
  # theme_bw() +
  # theme_bw(base_size = 14) +
  facet_grid(~group) +
  scale_color_viridis_c(end = 0.95) +
  theme(legend.position = 'right')

ggsave(
  'figs/final/figure2.png',
  # 'figs/examples/unloading_drip_hp98_rw01.png',
  device = png,
  width = 7.5,
  height = 3.25,
  units = "in"
)

# Show over discrete time intervals ----

# over time HP98
L0 <- 20
time_forcing <- data.frame(
  time = seq(1:24),
  u = 2,
  Ta = -10,
  L_crhm = NA,
  L_analytical = NA,
  L_ode = NA
)

time_forcing$L_crhm[1] <- L0
time_forcing$L_analytical[1] <- L0
time_forcing$L_ode[1] <- L0
dt <- 60*60*24*7 # seconds 
k_unld_cold_crhm <- -log(c) / ((7*24*60*60)/(dt)) # per hour, unloading coefficient for cold snow 
k_unld <- -log(c) / (7*24*60*60) # per hour, unloading coefficient for cold snow 

for (i in 1:nrow(time_forcing)) {
  time_forcing$L_analytical[i+1] <- time_forcing$L_analytical[i] * exp(-k_unld*dt)
  time_forcing$L_crhm[i+1] <- time_forcing$L_crhm[i] - (time_forcing$L_crhm[i] * k_unld_cold_crhm)
  time_forcing$L_ode[i+1] <- time_forcing$L_ode[i] - (time_forcing$L_ode[i] * k_unld)*dt
  
}

time_forcing |> pivot_longer(starts_with("L_")) |> 
ggplot(aes(time, value, colour = name)) + geom_point()
plotly::ggplotly()

# New fortress parameterisations
a_T = 2.584003e-05 # Cebulski & Pomeroy coef from exponential function of unloading + drip and air temp measurements at Fortress mountain when wind speed <= 1 m/s.
b_T = 1.646875e-01 # Cebulski & Pomeroy coef from exponential function of unloading + drip and air temp measurements at Fortress mountain when wind speed <= 1 m/s.
a_u = 5.204024e-06      # Cebulski & Pomeroy coef from exponential function of unloading + drip and wind speed measurements at Fortress mountain when air temp < -6 C.
b_u = 7.363594e-02     # Cebulski & Pomeroy coef from exponential function of unloading + drip and wind speed measurements at Fortress mountain when air temp < -6 C.
a_t = 2.058989e-06  # Cebulski & Pomeroy coef from exponential function of unloading + drip and duration snow has been intercepted in the canopy at Fortress mountain when wind speed <= 1 m/s and air temperature < -6 C.
b_t = -1.188307e-05 # Cebulski & Pomeroy coef from exponential function of unloading + drip and duration snow has been intercepted in the canopy at Fortress mountain when wind speed <= 1 m/s and air temperature < -6 C.

L0 <- 20
dt <- 60*15 # seconds 
time_forcing <- data.frame(
  time = seq(as.POSIXct('2024-01-01 00:00:00'), as.POSIXct('2024-01-07 00:00:00'), by = dt),
  u = 2,
  Ta = -10,
  L_analytical = NA,
  L_ode = NA
)

time_forcing$L_analytical[1] <- L0
time_forcing$L_ode[1] <- L0
t_snow_in_canopy <- dt

for (i in 1:nrow(time_forcing)) {
  t_snow_in_canopy <- t_snow_in_canopy + dt
  fT = a_T * exp(b_T * time_forcing$Ta[i]) # unloading rate based on warming of snow in the canopy (s-1), still need to partition out the portion of this that is drip vs mass unloading
  fu = time_forcing$u[i] * a_u * exp(b_u * time_forcing$u[i]) # unloading rate due to wind (s-1)
  ft = a_t * exp(b_t * t_snow_in_canopy) # unloading due to time in canopy
  
  k_unld <- fT + fu + ft
  time_forcing$L_analytical[i+1] <- time_forcing$L_analytical[i] - (time_forcing$L_analytical[i] * (1-exp(-k_unld*dt)))
  # time_forcing$L_analytical[i+1] <- time_forcing$L_analytical[i] * exp(-k_unld*dt) ## same as above
  time_forcing$L_ode[i+1] <- time_forcing$L_ode[i] - (time_forcing$L_ode[i] * k_unld)*dt
}
time_forcing_15min <- time_forcing |> 
  mutate(group = '15min')

dt <- 60*60*7 # seconds 
time_forcing <- data.frame(
  time = seq(as.POSIXct('2024-01-01 00:00:00'), as.POSIXct('2024-01-07 00:00:00'), by = dt),
  u = 2,
  Ta = -10,
  L_analytical = NA,
  L_ode = NA
)

time_forcing$L_analytical[1] <- L0
time_forcing$L_ode[1] <- L0
t_snow_in_canopy <- dt

for (i in 1:nrow(time_forcing)) {
  t_snow_in_canopy <- t_snow_in_canopy + dt
  fT = a_T * exp(b_T * time_forcing$Ta) # unloading rate based on warming of snow in the canopy (s-1), still need to partition out the portion of this that is drip vs mass unloading
  fu = time_forcing$u * a_u * exp(b_u * time_forcing$u) # unloading rate due to wind (s-1)
  ft = a_t * exp(b_t * t_snow_in_canopy) # unloading due to time in canopy
  
  k_unld <- fT + fu + ft
  time_forcing$L_analytical[i+1] <- time_forcing$L_analytical[i] * exp(-k_unld*dt)
  time_forcing$L_ode[i+1] <- time_forcing$L_ode[i] - (time_forcing$L_ode[i] * k_unld)*dt
}

time_forcing_60min <- time_forcing |> 
  mutate(group = 'Weekly')

rbind(time_forcing_15min,
      time_forcing_60min) |> pivot_longer(starts_with("L_")) |> 
  ggplot(aes(time, value, colour = group, linetype = name)) +
  geom_line() +
  ylab('Canopy Snow Load (mm)')

ggsave('figs/examples/numerical_deamons.png', width = 7, height = 5)
plotly::ggplotly()

