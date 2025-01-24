# Compare crhm model to snow survey data
snow_survey <- CRHMr::readObsFile(
  'data/snow_survey/Powerline_obs_SWE_2013-23.obs',
  timezone = 'Etc/GMT+6'
) |> select(datetime, obs_swe = Powerline_Forest.1)

# Read the file while skipping non-relevant lines
snow_scale <- read.table('data/snow_survey/Powerline_SommerSnowScale_SWE_29Oct2013-16Oct2023.obs', 
                   skip = 3,       # Skip the first 4 lines (including column names)
                   fill = T,
                   col.names = c("datetime", "SWE"),  # Assign column names
                   header = FALSE) # Data doesn't have a header row

snow_scale$datetime <- as.POSIXct(as.numeric(obs[, 1]) * 24 * 3600, 
                       origin = "1899-12-30", tz = "UTC")
snow_scale$datetime <- lubridate::force_tz(datetime, tzone = 'Etc/GMT+6')
snow_scale <- CRHMr::makeRegular(snow_scale, 'Etc/GMT+6')

prj <- 'fortress_powerline_clearing_snowsurveytransect_baseline'
prj_out_file_path <- paste0('../../analysis/crhm-analysis/output//',
                            prj)
recent_prj_out_files <- list.files(prj_out_file_path,
                                         full.names = T)
baseline_crhm <- CRHMr::readOutputFile(
  tail(recent_prj_out_files, n = 1),
  timezone = 'Etc/GMT+6'
)

# baseline_crhm |> 
#   pivot_longer(starts_with('SWE')) |> 
#   ggplot(aes(datetime, value, colour = name)) + 
#   geom_line()

prj <- 'fortress_powerline_clearing_snowsurveytransect_pspnew'
prj_out_file_path <- paste0('../../analysis/crhm-analysis/output/',
                            prj)
recent_prj_out_files <- list.files(prj_out_file_path,
                                   full.names = T)
pspnew_crhm <- CRHMr::readOutputFile(
  tail(recent_prj_out_files, n = 1),
  timezone = 'Etc/GMT+6'
)

# pspnew_crhm |> 
#   pivot_longer(starts_with('SWE')) |> 
#   ggplot(aes(datetime, value, colour = name)) + 
#   geom_line()

# PLOT NEW and Snow Scale ---- 

# SWE ---- 

snowscale_and_new <- left_join(pspnew_crhm,
                              snow_scale,
                              by = 'datetime')
# left_join(crhm_output_tree |> 
#             select(datetime, Snow_load.1_15min = Snow_load.1))

# Plot subcanopy SWE
snowscale_and_new |> 
  rename(Observed = SWE,
          Simulated = SWE.1) |> 
  pivot_longer(
    c(Observed, Simulated),
    names_to = "model", 
    values_to = "SWE"
  ) |> 
  ggplot(aes(x = datetime, y = SWE, colour = model)) +
  geom_line() +
  geom_point(
    data = snow_survey, 
    aes(x = datetime, y = obs_swe, colour = "Snow Survey"), 
    inherit.aes = FALSE
  ) +
  scale_colour_manual(
    values = c("Observed" = "blue", 
               "Simulated" = "red", 
               "Snow Survey" = "black"),
    labels = c(
      "Observed" = "Observed-Clearing",
      "Simulated" = "Simulated-Forest"
    ),
    name = "Legend"
  ) +
  guides(colour = guide_legend(override.aes = list(
    linetype = c(1, 1, 0), # Line styles for the first two, none for points
    shape = c(NA, NA, 16)  # Points only for "Snow Survey"
  ))) +
  ylab(expression(Snow~Water~Equivalent~(kg~m^{-2}))) +
  xlab(element_blank()) +
  theme(legend.position = 'bottom')

ggsave(
  paste0(
    'figs/crhm-analysis/crhm_swe_vs_snow_survey/crhm_swe_vs_snow_survey_vs_snowscale_timeseries_',
    '_',
    format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
    '.png'
  ),
  device = png,
  width = 12,
  height = 6
)

# Plot TREE
baseline_and_new  |> 
  mutate(year = year(datetime)) |> 
  pivot_longer(c(Snow_load.1_baseline, Snow_load.1_new)) |> 
  ggplot(aes(datetime, value, colour = name)) + 
  geom_line(linetype = 'dashed') +
  ylab(expression(Canopy~Snow~Load~(kg~m^{-2}))) +
  xlab(element_blank()) + 
  facet_wrap(~year, scales = 'free') +
  theme(legend.position = 'bottom')
# plotly::ggplotly()

baseline_and_new  |> 
  mutate(year = year(datetime)) |> 
  filter(year == 2023) |> 
  pivot_longer(c(Snow_load.1_baseline, Snow_load.1_new)) |> 
  ggplot(aes(datetime, value, colour = name)) + 
  geom_line(linetype = 'dashed') +
  ylab(expression(Canopy~Snow~Load~(kg~m^{-2}))) +
  xlab(element_blank()) + 
  facet_wrap(~year, scales = 'free') +
  theme(legend.position = 'bottom')

ggsave(
  paste0(
    'figs/crhm-analysis/crhm_swe_vs_snow_survey/crhm_snowload_timeseries_2013',
    '_',
    format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
    '.png'
  ),
  device = png,
  width = 12,
  height = 6
)

# PLOT BASELINE VS NEW ---- 

# SWE ---- 

baseline_and_new <- left_join(baseline_crhm,
                              pspnew_crhm,
                              by = 'datetime',
                              suffix = c("_baseline", "_new"))
  # left_join(crhm_output_tree |> 
  #             select(datetime, Snow_load.1_15min = Snow_load.1))

# Plot subcanopy SWE
baseline_and_new |> 
  pivot_longer(
    c(SWE.1_baseline, SWE.1_new),
    names_to = "model", 
    values_to = "SWE"
  ) |> 
  mutate(
    model = recode(model, 
                   SWE.1_baseline = "Baseline", 
                   SWE.1_new = "Updated Model"),
    model = factor(model, levels = c("Baseline", "Updated Model", "Snow Survey")) # Set order
  ) |> 
  ggplot(aes(x = datetime, y = SWE, colour = model)) +
  geom_line() +
  geom_point(
    data = snow_survey, 
    aes(x = datetime, y = obs_swe, colour = "Snow Survey"), 
    inherit.aes = FALSE
  ) +
  scale_colour_manual(
    values = c("Baseline" = "blue", 
               "Updated Model" = "red", 
               "Snow Survey" = "black"),
    name = "Legend"
  ) +
  guides(colour = guide_legend(override.aes = list(
    linetype = c(1, 1, 0), # Line styles for the first two, none for points
    shape = c(NA, NA, 16)  # Points only for "Snow Survey"
  ))) +
  ylab(expression(Snow~Water~Equivalent~(kg~m^{-2}))) +
  xlab(element_blank()) +
  theme(legend.position = 'bottom')

ggsave(
  paste0(
    'figs/crhm-analysis/crhm_swe_vs_snow_survey/crhm_swe_vs_snow_survey_timeseries_',
    '_',
    format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
    '.png'
  ),
  device = png,
  width = 12,
  height = 6
)

# Plot TREE
baseline_and_new  |> 
  mutate(year = year(datetime)) |> 
  pivot_longer(c(Snow_load.1_baseline, Snow_load.1_new)) |> 
  ggplot(aes(datetime, value, colour = name)) + 
  geom_line(linetype = 'dashed') +
  ylab(expression(Canopy~Snow~Load~(kg~m^{-2}))) +
  xlab(element_blank()) + 
  facet_wrap(~year, scales = 'free') +
  theme(legend.position = 'bottom')
  # plotly::ggplotly()

baseline_and_new  |> 
  mutate(year = year(datetime)) |> 
  filter(year == 2023) |> 
  pivot_longer(c(Snow_load.1_baseline, Snow_load.1_new)) |> 
  ggplot(aes(datetime, value, colour = name)) + 
  geom_line(linetype = 'dashed') +
  ylab(expression(Canopy~Snow~Load~(kg~m^{-2}))) +
  xlab(element_blank()) + 
  facet_wrap(~year, scales = 'free') +
  theme(legend.position = 'bottom')

ggsave(
  paste0(
    'figs/crhm-analysis/crhm_swe_vs_snow_survey/crhm_snowload_timeseries_2013',
    '_',
    format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
    '.png'
  ),
  device = png,
  width = 12,
  height = 6
)

# scatter ----

## SWE ----

mod_snow_survey <- left_join(
  snow_survey,
  baseline_and_new |> select(datetime, Baseline = SWE.1_baseline, Updated = SWE.1_new)
) |> 
  pivot_longer(c(Baseline, Updated))

ggplot(mod_snow_survey,
       aes(obs_swe, value, colour = name)) +
  geom_point() + geom_abline() +
  ylab(expression(Simulated~Snow~Water~Equivalent~(kg~m^{-2}))) +
  xlab(expression(Observed~Snow~Water~Equivalent~(kg~m^{-2}))) +
  theme(legend.title = element_blank())
  
ggsave(
  paste0(
    'figs/crhm-analysis/crhm_swe_vs_snow_survey/crhm_swe_vs_snow_survey_scatter_',
    '_',
    format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
    '.png'
  ),
  device = png,
  width = 7,
  height = 6
)

# error table ----

mod_snow_survey_err_tbl <- mod_snow_survey |> 
  mutate(diff = obs_swe - value) |> 
  group_by(name) |> 
  summarise(
    `Mean Bias` = mean(diff, na.rm = T),
    MAE = mean(abs(diff), na.rm = T),
    `RMS Error` = sqrt(mean(diff ^ 2, na.rm = T)),
    # NRMSE = `RMS Error` / (max(obs_swe, na.rm = TRUE) - min(obs_swe, na.rm = TRUE)),
    NRMSE = `RMS Error` / mean(obs_swe, na.rm = T),
    R = cor(obs_swe, value),
    `r^2` = R^2) |> 
  mutate(across(`Mean Bias`:`r^2`, round, digits = 3))

write_csv(mod_snow_survey_err_tbl,
          paste0(
            'tbls/crhm-swe-vs-snowsurvey-errortbl',
            '_',
            format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
            '.csv'
          ))
  
