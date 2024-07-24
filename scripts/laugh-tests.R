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

all_dfs <- rbind(met_windy_5, met_windy_10) |>
  rbind(met_windy_15) |>
  rbind(met_windy_20)

ggplot(all_dfs, aes(U, q_unld, colour = canopy_load, group = canopy_load)) + 
  geom_line() +
  scale_color_viridis_c(end = 0.95) +
  ylab('Wind Induced Unloading Rate (mm/hr)') +
  xlab(wind_ax_lab) +
  labs(colour = 'Canopy Load (mm)') + 
  # theme_bw(base_size = 14) #+
  theme_bw()

ggsave(
  'figs/examples/roesch_unloading.png',
  device = png,
  width = int_fig_width,
  height = int_fig_height,
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

met_temp_hp <- pseudo_crhm_canopy(met_temp) |> mutate(group = 'HP98') |> 
  select(Ta, q_ablate, group, canopy_load)

met_temp_ra <- roesch_unld(met_temp) |> mutate(group = 'RW01') |> 
  select(Ta, q_ablate, group, canopy_load)

plot_unld_temp <- rbind(met_temp_ra, met_temp_hp)

ggplot(plot_unld_temp, aes(Ta, q_ablate, colour = canopy_load, group = canopy_load)) + 
  geom_line() +
  ylab('Unloading + Drip Rate (mm/hr)') +
  xlab(temp_ax_lab) +
  labs(colour = 'Canopy Load (mm)') + 
  theme_bw() +
  # theme_bw(base_size = 14) +
  facet_grid(~group) +
  scale_color_viridis_c(end = 0.95) +
  theme(legend.position = 'bottom')


ggsave(
  'figs/examples/unloading_drip_hp98_rw01.png',
  device = png,
  width = int_fig_width,
  height = int_fig_height,
  units = "in"
)
