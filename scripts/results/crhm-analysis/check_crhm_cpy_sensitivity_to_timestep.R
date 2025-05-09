# Script to check if new interception module has similar output for fifteen and hourly timesteps

# HOURLY
prj <- "ffr_closed_canopy_cc0.88_cansnobal_hourly"
run_tag_updt <- "no_asm_r1"

path <- list.files(
  paste0(
    "crhm/output/",
    prj
  ),
  pattern = run_tag_updt,
  full.names = T
)

stopifnot(length(path) == 1)

crhm_output_hourly <- CRHMr::readOutputFile(
  path,
  timezone = 'Etc/GMT+6') 

# FIFTEEN Min

prj <- "ffr_closed_canopy_cc0.88_cansnobal"
run_tag_updt <- "no_asm_r2"

path <- list.files(
  paste0(
    "crhm/output/",
    prj
  ),
  pattern = run_tag_updt,
  full.names = T
)

stopifnot(length(path) == 1)

crhm_output_fifteen <- CRHMr::readOutputFile(
  path,
  timezone = 'Etc/GMT+6')# |> filter(datetime %in% obs_tree$datetime)

# PLOT 

plot_df <- rbind(crhm_output_hourly |> mutate(group = 'hourly'), crhm_output_fifteen |> mutate(group = 'fifteen'))

ggplot(plot_df, aes(datetime, SWE.1, colour = group)) + geom_line()
plotly::ggplotly()
ggplot(plot_df, aes(datetime, m_s_veg.1, colour = group)) + geom_line()
plotly::ggplotly()

