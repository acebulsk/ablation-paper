
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
  mutate(name = 'crhm', melt_unld = deldrip_veg_int.1 + delunld_int.1) |> 
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

# ANALYSIS ----

ablation_by_event <- 
  rbind(crhm_output, q_unld_scl) 

ggplot(ablation_by_event, aes(datetime, melt_unld, colour = name)) + 
  geom_point() + 
  facet_wrap(~event_id, scales = 'free')

plotly::ggplotly()

## just melt events ----

melt_events <- c('2022-04-23',
                 # '2022-06-23', # snow/rain partitioning off in crhm
                 '2022-06-14',
                 '2022-06-24',
                 '2023-05-08',
                 '2023-06-15')

ggplot(ablation_by_event |> filter(event_id %in% melt_events),
       aes(datetime, melt_unld, colour = name)) + 
  geom_point() + 
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