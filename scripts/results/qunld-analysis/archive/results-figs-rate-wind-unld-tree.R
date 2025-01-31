# This script analyzes the relationship between observed weighed tree ablation
# due to mass unloading of snow due to wind and mid canopy wind spee. Canopy 
# snow unloading due to wind is determined as q_unld^wind = delL - q_subl - q_drip
#  NOTES: this overfits to the lower qunl , should stick to original binning

library(tidyverse)

load_suffix <- 'fsd_closed_0.88'

# LOAD DATA ----

## model ---- 

# Select model run with all unloading events weighed tree snow load assimilated
prj <- "ffr_closed_canopy_cc0.88_vector_based_new_ablation_psp"

# specify certain model run
run_tag <- "updt_canopy_unloading_wind_fn_pars"

path <- list.files(
  paste0(
    "../../analysis/crhm-analysis/output/",
    prj
  ),
  pattern = run_tag,
  full.names = T
)

crhm_output <- CRHMr::readOutputFile(
  path,
  timezone = 'Etc/GMT+6') |> 
  select(datetime, hru_t.1, hru_u.1, hru_p.1,
         Subl_Cpy.1:SUnloadWind.1)

## obs ----

### all events snow in canopy ----
# obs_tree <-
#   readRDS(paste0(
#     'data/clean-data/unloading_events_zero_weighed_tree_kg_m2_pre_post_cnpy_snow_',
#     load_suffix,
#     '.rds'
#   )) |>  filter(name == 'weighed tree')

# warm tree specific events
warm_events <- c(
  '2022-04-21',
  '2022-04-23',
  '2022-06-14',
  '2022-06-24',
  '2023-03-14',
  '2023-03-25',
  '2023-03-26',
  '2023-03-28',
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

# cold tree events
cold_events <- c(
  '2022-03-02', 
  '2022-03-09',
  '2022-03-20', 
  '2022-03-24',  
  '2022-03-29',  
  '2022-12-01',
  '2023-01-28',
  '2023-02-24',
  '2023-02-26'
  #'2023-04-12' could add back if filter to start later.. also removed jjst so have clean 20 events
)
obs_tree_cold <-
  readRDS(paste0(
    'data/clean-data/all_tree_events_zero_weighed_tree_',
    load_suffix,
    '_kg_m2_post_cnpy_snow.rds'
  )) |> 
  filter(event_id %in% cold_events)

obs_tree <- rbind(obs_tree_cold  |> 
                    select(datetime, event_id, tree_mm),
                  obs_tree_warm |> 
                    select(datetime, event_id, tree_mm)) 

# PLOT -----

w_tree_q_unld_15 <- obs_tree |>
  mutate(
    dL = lag(tree_mm) - tree_mm,
    dL = ifelse(dL < 0, 0, dL),
    q_unl = (dL / 15) * 60
  ) |>
  ungroup() |>
  filter(is.na(q_unl) == F) |> 
  select(datetime, event_id, tree_mm, dL)

obs_mod <- left_join(w_tree_q_unld_15, crhm_output) |> 
  mutate(est_q_unld_wind = dL - Subl_Cpy.1 - canopy_snowmelt.1 - SUnloadMelt.1)

# aggregate temporyally doesnt seem to help noise problem
obs_mod <- obs_mod |> 
  mutate(
    datetime_hour = lubridate::ceiling_date(datetime, unit = '12 hours') # checked and this is the same as above
  ) |> 
  group_by(datetime_hour) |> 
  # mutate(n = n()) |> 
  # filter(n == 4) |> 
  summarise(
    tree_mm = nth(tree_mm, which.max(datetime)), # Get tree_mm with the largest datetime    t = mean(t),
    dL = sum(dL),
    hru_t.1 = mean(hru_t.1),
    hru_u.1 = mean(hru_u.1),
    Subl_Cpy.1 = sum(Subl_Cpy.1),
    SUnload.1 = sum(SUnload.1),
    canopy_snowmelt.1 = sum(canopy_snowmelt.1),
    SUnloadMelt.1 = sum(SUnloadMelt.1),
    SUnloadWind.1 = sum(SUnloadWind.1)
  ) |> 
  as.data.frame() |> 
  mutate(est_q_unld_wind = dL - Subl_Cpy.1 - SUnloadWind.1 - canopy_snowmelt.1) 

obs_mod_fltr <- obs_mod |> 
  filter(canopy_snowmelt.1 == 0,
         # tree_mm > 5,
         hru_t.1 < -6) |> 
  # convert mm/interval to mm/hour
  mutate(est_q_unld_wind = est_q_unld_wind*4,
         est_q_unld_wind_log = log(est_q_unld_wind))

ggplot(obs_mod_fltr, aes(x = hru_u.1, y = est_q_unld_wind)) +
  geom_point(color = "black", size = 2) +                      # Scatter plot of the data
  labs(
    x = expression("Canopy Snowmelt (" ~ kg ~ m^-2 ~ hr^-1 ~ ")"),
    # y = expression(q[unld]^{melt} ~ "(" ~ kg ~ m^-2 ~ hr^-1 ~ ")"),
    y = expression("Melt Unloading Rate (" ~ kg ~ m^-2 ~ hr^-1 ~ ")")
  ) 

model_lm <- lm(est_q_unld_wind_log ~ hru_u.1, data = obs_mod_fltr |> filter(!is.infinite(est_q_unld_wind_log),
                                                                            !is.na(est_q_unld_wind_log)
                                                                            ))
coefs <- coef(model_lm) |> as.numeric()
summary(model_lm)
a_lm <- exp(coefs[1])
b_lm <- coefs[2]
q_unld_wind_nls <- nls(est_q_unld_wind ~ hru_u.1 * a * tree_mm * exp(b * hru_u.1),
                      data = obs_mod_fltr)
obs_mod_fltr$mod_q_unld_wind <- predict(q_unld_wind_nls)

ex_wind_labs <- seq(0,5,0.1)
ex_avg_w_tree <- c(3, 11)
wind_ex_df <- expand.grid(hru_u.1 = ex_wind_labs, tree_mm = ex_avg_w_tree)
wind_ex_df$new_predicted_y <- predict(q_unld_wind_nls, newdata = wind_ex_df)
# Create the plot
ggplot(obs_mod_fltr, aes(x = hru_u.1, y = est_q_unld_wind)) +
  geom_point(color = "black", size = 2) +                      # Scatter plot of the data
  geom_line(data = wind_ex_df, aes(hru_u.1, new_predicted_y)) +
  labs(
    x = expression("Canopy Snowmelt (" ~ kg ~ m^-2 ~ hr^-1 ~ ")"),
    # y = expression(q[unld]^{melt} ~ "(" ~ kg ~ m^-2 ~ hr^-1 ~ ")"),
    y = expression("Melt Unloading Rate (" ~ kg ~ m^-2 ~ hr^-1 ~ ")")
  ) 


# Create observed vs predicted plot
ggplot(obs_mod_fltr, aes(x = est_q_unld_wind, y = mod_q_unld_wind)) +
  geom_point(color = "blue", size = 2) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(
    title = "Observed vs Predicted",
    x = "Observed Values",
    y = "Predicted Values"
  ) +
  theme_minimal()
