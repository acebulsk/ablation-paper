
options(ggplot2.discrete.colour= c("black", "#DF536B", "dodgerblue", "#F2B701", "#9467BD"))

# options(ggplot2.discrete.colour= cols4all::c4a('cols4all.friendly7'))

# LOAD DATA ----

events <- readRDS('data/clean-data/select_cpy_snow_ablation_events.rds')

## CRHM OUTPUTs ----

prj_updt <- "ffr_cc0.54_closed_cansnobal"
run_tag_updt <- "19_ffr_cc0.54_closed_cansnobal_store_liq_output"

path <- list.files(
  paste0(
    "crhm/output/",
    prj_updt
  ),
  pattern = run_tag_updt,
  full.names = T
)

stopifnot(length(path) == 1)

crhm_output <- CRHMr::readOutputFile(
  path,
  timezone = 'Etc/GMT+6') |> 
  mutate(name = 'CP25', melt_unld = deldrip_veg_int.1 + delunld_int.1) |> 
  inner_join(events) |>
  group_by(name, event_id) |> 
  mutate(melt_unld = cumsum(melt_unld)) |> 
  select(datetime, event_id, name, melt_unld)

## OBSERVED DATA ---- 

q_unld_scl <- 
  readRDS('data/raw-data/treefort_load_main.rds') |> 
  select(datetime, mixed = trough_1, sparse = trough_2, closed = trough_3) |> 
  pivot_longer(!datetime) |> 
  inner_join(events) |> 
  group_by(name, event_id) |> 
  mutate(melt_unld = value - first(value)) |> 
  ungroup() |> 
  select(datetime, event_id, name, melt_unld)


q_unld_scl <- 
  readRDS('data/clean-data/treefort_scl_qaqc_crhm_events.rds') |>
  select(datetime, event_id, name, melt_unld = value)

# ANALYSIS ----

ablation_by_event <- 
  rbind(crhm_output, q_unld_scl) |> 
  filter(!event_id %in% bad_events) |> 
  left_join(mod_d_drip_smry_frac |> mutate(event_id = event_id |> as.character())) |> 
  mutate(facet_title = paste(event_type, '-', event_id),
         name = factor(name, c('CP25', 'sparse', 'mixed', 'closed')))

ggplot(ablation_by_event, aes(datetime, melt_unld, colour = name)) + 
  geom_line() + 
  facet_wrap(~facet_title, scales = 'free', ncol = 3) + 
  scale_x_datetime(date_labels = "%H") +
  labs(y = 'Cumulative Unloading + Drip (mm)',
       x = element_blank(),
       colour = 'Data Type'
       ) +
  theme(legend.position = 'bottom')

# plotly::ggplotly()


ggsave(
  paste0(base_path, 'obs_mod_unld_plus_drip_', run_tag_updt, '.png'),
  width = 8.5,
  height = 9,
  device = png
)

## subl and wind events ----

ggplot(ablation_by_event |> filter(!event_type %in% c('melt')), aes(datetime, melt_unld, colour = name)) + 
  geom_line() + 
  facet_wrap(~facet_title, scales = 'free', ncol = 3) + 
  scale_x_datetime(date_labels = "%H") +
  labs(y = 'Cumulative Unloading + Drip (mm)',
       x = element_blank(),
       colour = 'Data Type'
  ) +
  theme(legend.position = 'bottom')
# plotly::ggplotly()

ggsave(
  paste0(base_path, 'obs_mod_unld_plus_drip_subl_wind_', run_tag_updt, '.png'),
  width = 8.5,
  height = 7,
  device = png
)

## just melt events ----

melt_events <- c('2022-04-23',
                 # '2022-06-23', # snow/rain partitioning off in crhm
                 '2022-06-14',
                 '2022-06-24',
                 '2023-05-08',
                 '2023-06-15')

ggplot(ablation_by_event |> filter(event_id %in% melt_events),
       aes(datetime, melt_unld, colour = name)) + 
  geom_line() + 
  facet_wrap(~event_id, scales = 'free')

plotly::ggplotly()

## just wind/subl events ----

non_melt_events <- c('2022-02-24',
                 # '2022-06-23', # snow/rain partitioning off in crhm
                 '2022-03-02',
                 '2022-03-09',
                 '2022-03-20',
                 '2022-12-01',
                 '2023-02-24',
                 '2023-02-24'
                 )

ggplot(ablation_by_event |> filter(event_id %in% non_melt_events),
       aes(datetime, melt_unld, colour = name)) + 
  geom_point() + 
  facet_wrap(~event_id, scales = 'free')

plotly::ggplotly()