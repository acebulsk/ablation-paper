# qaqc subcanopy lysimeter data
library(dplyr)
library(tidyverse)
library(plotly)
library(viridis)
library(ggpubr)

library(CRHMr)
library(weatherdash)
library(wxlogR)

# chrmr params

spike_th <- 10
spike_th_init <- 200
max_gap_length <- 35314 # fill all gaps with linear interp
smooth_window <- 15
polynomial_order <- 3
small_drop_th <- 0.0001 # this is used to eliminate any points with a delta precip less than this

load_select <- c('sparse', 'mixed', 'closed')

scl_df <- scl_df_kg_m2_raw |> 
  inner_join(crhm_events) |> 
  # filter(event_id %in% c('2021-12-14', '2022-01-09')) |>
  rename(mixed = trough_1,
         sparse = trough_2,
         closed = trough_3) |> 
  pivot_longer(tree:TB4_mm)

# check no duplicates (since cal period had multi measurements per 15 min then these came in as duplicates)

# duplicated_rows <- duplicated(scl_df$datetime) | duplicated(scl_df$datetime, fromLast = TRUE)
# filtered_scl_df <- scl_df[duplicated_rows, ]

scl_fltr <- scl_df |> 
  mutate(wat_yr = wtr_yr(datetime)) |> 
  filter(name %in% load_select) |> 
  select(datetime, event_id, name, value) |> 
  group_by(event_id, name) |> 
  mutate(value = value - first(value)) |> 
  filter(!all(is.na(value))) |> 
  as.data.frame()

# scl_fltr |>
#   # filter(event_id %in% c('2022-03-09')) |>
#   ggplot(aes(datetime, value, colour = name)) +
#   facet_wrap(~event_id, scales = 'free') +
#   geom_line()
# 
# plotly::ggplotly()

# Craig Smiths weighing gauge workflow involves
# wg1 - interpolates gaps
# wg2 - rms spikes
# wg3 - Savitzky-Golay polynomial filter
# wg4 - Alan Barr's iterative filter to rm jitter
# wg5 - removes gauge resets from services
# manually remove large spikes / empties before the gap interpolation i.e. we
# dont want to interpolate over a 800 mm empty


# ---- prelim tidy data ----

# rm empties (smaller drops will get removed later)

pc_empty_amounts <- scl_fltr |>
  group_by(name, event_id) |> 
  filter(is.na(value) == F) |> 
  mutate(
    empty_amount = lag(value) - value) |> 
  filter(empty_amount > 0.5) |>
  arrange(datetime) |>
  select(datetime, name, event_id, empty_amount)|>
  mutate(empty_amount_rolling = cumsum(empty_amount))


pc_empty_correct <- scl_fltr |>
  group_by(name, event_id) |> 
  left_join(pc_empty_amounts, by = c('datetime', 'name', 'event_id')) |>
  fill(empty_amount_rolling, .direction = 'down') |>
  mutate(
    empty_amount_rolling = ifelse(is.na(empty_amount_rolling) == T,
                                  0, empty_amount_rolling),
    value = value + empty_amount_rolling
  ) |> as.data.frame() |> 
  select(datetime, name, event_id, value)

# pc_empty_correct |>
#   # filter(event_id %in% c('2021-12-14')) |>
#   ggplot(aes(datetime, value, colour = name)) +
#   facet_wrap(~event_id, scales = 'free') +
#   geom_line()
# 
# plotly::ggplotly()

# rm large positive jumps (need to do this prior to the nan fill because jumps
# that have gaps inbetween are not caught later because they are interpolated)

pc_empty_amounts <- pc_empty_correct |>
  group_by(name, event_id) |> 
  filter(is.na(value) == F) |> 
  mutate(empty_amount = lag(value) - value) |> 
  filter(empty_amount < -50) |>
  arrange(datetime) |>
  select(datetime, name, event_id, empty_amount) |>
  mutate(empty_amount_rolling = cumsum(empty_amount))

pc_jump_correct <- pc_empty_correct |>
  group_by(name, event_id) |> 
  left_join(pc_empty_amounts, by = c('datetime', 'name', 'event_id')) |>
  fill(empty_amount_rolling, .direction = 'down') |>
  mutate(
    empty_amount_rolling = ifelse(is.na(empty_amount_rolling) == T,
                                  0, empty_amount_rolling),
    value = value + empty_amount_rolling
  ) |> as.data.frame()

pc_jump_correct |>
  ggplot(aes(datetime, value, colour = name)) +
  facet_wrap(~event_id, scales = 'free') +
  geom_line()

plotly::ggplotly()

# find peak value and set vals after it as constant, otherwise method below will
# limit the cumulation to the lowest point.

pc_limit <- pc_jump_correct |> 
  group_by(name, event_id) |> 
  mutate(peak_val = max(value),
         peak_index = which.max(value),
         peak_dt_at_pk_val = datetime[peak_index],
         value = if_else(datetime > peak_dt_at_pk_val, peak_val, value),
         ) |>
  fill(value, .direction = 'down') |> 
  as.data.frame()

pc_limit |>
  ggplot(aes(datetime, value, colour = name)) +
  facet_wrap(~event_id, scales = 'free') +
  geom_line()

plotly::ggplotly()

# ---- 1 - apply weighing gauge 1 ----

# this fills all nans, we need to do this prior to deaccumulating and removing
# negative changes otherwise we will not be able to reaccumulate

pc_limit$split_var <- paste0(pc_limit$event_id, '_', pc_limit$name)
df_split <- split(pc_limit, pc_limit$split_var)

gap_fill <- lapply(df_split, weighingGauge1, 
                   precipCol = 3,
                   maxGapLength = 100)

# gap_fill <- weighingGauge1(obs = pc_jump_correct[[1]],
#                                  precipCol = 2,
#                                  maxGapLength = 100) |> 
#   filter(is.na(value) == F)

for (i in 1:length(gap_fill)) {
  if(all(gap_fill[[i]] == F)){
    gap_fill[[i]] <- df_split[[i]] |> select(datetime, event_id, name, value)
    warning(paste('No gaps found for the',  i, 'list element, replacing df'))
  } else {
    gap_fill[[i]]$event_id <- df_split[[i]]$event_id |> unique()
    gap_fill[[i]]$name <- df_split[[i]]$name |> unique()
    gap_fill[[i]] <- gap_fill[[i]] |> select(datetime, event_id, name, value)
    warning(paste('Gaps were found for this element.'))
  }
}

gap_fill_df <- do.call("rbind", gap_fill)

# gap_fill_df |>
#   filter(event_id %in% c('2023-04-03')) |>
#   ggplot(aes(datetime, value, colour = name)) +
#   facet_wrap(~event_id, scales = 'free') +
#   geom_line()
# 
# plotly::ggplotly()

# manually rm all negatives from troughs 

# gap_fill_rm_neg <- lapply(gap_fill, cumulate_non_negative, 2)
# 
# plot <- do.call("rbind", gap_fill_rm_neg)
# plot$name <- sub("\\..*", "", rownames(plot))
# 
# plot |>
#   ggplot(aes(datetime, value)) +
#   geom_line() +
#   facet_wrap(~name)
# 
# plotly::ggplotly()

# ---- 2 - apply weighing gauge 2 ----

# rms spikes

spike_fill <- lapply(gap_fill, weighingGauge2, 
                     precipCol = 3, 
                     spikeThreshold = spike_th, 
                     maxSpikeGap = max_gap_length,
                     quiet = F)

# spike_fill[[3]] |>
#   ggplot(aes(datetime, value)) +
#   geom_point()
# 
# plotly::ggplotly()

# above will return false if no spikes so to keep data in the df we run the for loop below

for (i in 1:length(spike_fill)) {
  if(all(spike_fill[[i]] == F)){
    spike_fill[[i]] <- gap_fill[[i]] |> select(datetime, event_id, name, value)
    warning(paste('No spikes found for the',  i, 'list element, replacing df'))
  } else {
    spike_fill[[i]]$event_id <- gap_fill[[i]]$event_id |> unique()
    spike_fill[[i]]$name <- gap_fill[[i]]$name |> unique()
    spike_fill[[i]] <- spike_fill[[i]] |> select(datetime, event_id, name, value)
    warning(paste('Spikes were found for each element and nothing was done here.'))
  }
}

spike_fill_df <- do.call("rbind", spike_fill)

#   rbind(gap_fill_df |> mutate(group = 'gap_fill'),
#         spike_fill_df |> mutate(group = 'spike_fill')) |> 
#   ggplot(aes(datetime, value, colour = name, linetype = group)) +
#   facet_wrap(~event_id, scales = 'free') +
#   geom_line()
# 
# plotly::ggplotly()

# rm this filter as was smoothing data too much
# ---- 3 - apply weighing gauge 3 ----

# wg3 - Savitzky-Golay polynomial filter

# smooth <- lapply(spike_fill, weighingGauge3,
#                  precipCol = 3,
#                  filterLength = smooth_window,
#                  polynomial_order = polynomial_order)
# 
# plot <- do.call("rbind", smooth)
# plot$name <- sub("\\..*", "", rownames(plot))
# 
# plot |>
#   ggplot(aes(datetime, value_sg_filtered)) +
#   geom_line() +
#   facet_wrap(~name)
# 
# plotly::ggplotly()

# ---- 4 - apply weighing gauge 4 ----

# wg4 - Alan Barr's iterative filter to rm jitter

no_jitter <- lapply(spike_fill, weighingGauge4,
                    precipCol = 3,
                    quiet = F,
                    smallDropThreshold = small_drop_th,
                    serviceThreshold = -100,
                    serviceGapLength = 1)

plot <- do.call("rbind", no_jitter)
plot$name <- sub("\\..*", "", rownames(plot))

plot |>
  filter(grepl("2022-02",name)) |>
  ggplot(aes(datetime, value_PcpFiltPosT)) +
  geom_line() +
  facet_wrap(~name)

plotly::ggplotly()

# ---- 5 - output data ----

df_out <- do.call("rbind", no_jitter)

df_out <- df_out |> rename(value = value_PcpFiltPosT)

parts <- strsplit(rownames(df_out), "_")
name <- sapply(parts, function(x) x[[2]])
name <-  sub("\\..*", "", name)

df_out$event_id <- sapply(parts, function(x) x[[1]])

df_out$name <- name

df_out |>
  # filter(year(datetime) %in% c(2023)) |> 
  # filter(event_id == '2022-02-01') |>
  ggplot(aes(datetime, value, colour = name)) +
  geom_line() +
  facet_wrap(~event_id, scales = 'free')

plotly::ggplotly()

scl_out <- df_out |> 
  mutate(units = 'kg m^-2') |> 
  select(datetime, event_id, name, units, value = value) |> 
  dplyr::arrange(datetime)
rownames(scl_out) <- NULL

saveRDS(scl_out, 'data/clean-data/treefort_scl_qaqc_crhm_events.rds')

# show comparible to before

new_df_to_compare <- df_out |>
  mutate(name = paste0(name, '_qc')) |>
  select(datetime, event_id, name, value = value)

# raw tared data
loads <- scl_df_kg_m2_raw |>
  left_join(events) |> 
  select(datetime, event_id, trough_1, trough_2, trough_3) |>
  pivot_longer(trough_1:trough_3) |>
  rbind(new_df_to_compare)

stroms_zeroed <- loads |>
  group_by(event_id, name) |>
  mutate(zeroed = value - first(value))

ggplot(stroms_zeroed, aes(datetime, y = zeroed, colour = name, group = name)) +
  geom_line() +
  facet_wrap(~event_id, scales = 'free', nrow = 5) +
  ylab('Cumulative Precip. (mm)') +
  xlab('')

plotly::ggplotly()
