# Script to compare drip output from CRHM to drip measured by tipping buckets

# by each bucket
tb_d_drip <- tb_data_zeroed |> 
  group_by(datetime, event_id, name) |> 
  summarise(dU = mean(dU)) |> 
  select(datetime, event_id, name, dU) |> 
  group_by(event_id, name) |> 
  mutate(cml_drip = cumsum(dU),
         cml_drip = cml_drip - first(cml_drip),
         ) |> ungroup() # so each event starts at 0

# stats over buckets
tb_d_drip_stat <- tb_d_drip |> 
  group_by(datetime, event_id) |> 
  summarise(
    name = 'TB_mean',
    cml_drip_mean = mean(cml_drip, na.rm = T),
            sd = sd(cml_drip, na.rm = T)) 

mod_d_drip <- crhm_output_newsim |> 
  left_join(tb_d_drip |> select(datetime, event_id) |> distinct()) |> 
  mutate(crhm_canopy_snowmelt = delmelt_veg_int.1,
         crhm_drip = delmelt_veg_int.1,
         name = 'crhm_drip') |> 
  select(datetime, event_id, name, dU = crhm_drip) |> 
  group_by(event_id) |> 
  mutate(cml_drip_mean = cumsum(dU),
         cml_drip_mean = cml_drip_mean - first(cml_drip_mean)) # so each event starts at 0

obs_mod_cml_drip <- rbind(tb_d_drip_stat, mod_d_drip)

obs_mod_cml_drip |> 
  filter(!is.na(event_id)) |> 
  ggplot(aes(datetime, cml_drip_mean, colour = name)) + 
  geom_line() +
  geom_ribbon(
    data = . %>% filter(name == "TB_mean"),
    aes(ymin = cml_drip_mean - sd, 
        ymax = cml_drip_mean + sd,
        fill = "± 1 SD"),
    alpha = 0.3,
    colour = NA
  ) +
  facet_wrap(~event_id, nrow = 5, scales = 'free') +
  ylab('Cumulative Drip (mm)') +
  xlab(element_blank())

ggsave(
  paste0(
    'figs/results/crhm_vs_tb_drip_events_',
    run_tag_updt,
    # format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
    '.png'
  ),
  width = 6,
  height = 8
  )

# calculate the frac of unloading vs drip for each event

tb_d_drip_smry <- tb_d_drip |> 
  filter(!is.na(event_id)) |> 
  group_by(event_id, name) |> 
  summarise(obs_cml_drip = sum(dU))

tree_dL_smry <- obs_tree |> 
  filter(event_id %in% tb_d_drip_smry$event_id) |> 
  group_by(event_id) |> 
  mutate(dL = lag(observed) - observed) |> 
  summarise(obs_cml_dL = sum(dL, na.rm = T))

mod_subl_smry <- crhm_output_newsim |> 
  left_join(select_events_long) |> 
  filter(event_id %in% tb_d_drip_smry$event_id) |> 
  group_by(event_id) |> 
  summarise(mod_cml_subl = sum(delsub_veg_int.1))

met_smry <- crhm_output_newsim |> 
  left_join(select_events_long) |> 
  filter(event_id %in% tb_d_drip_smry$event_id) |> 
  group_by(event_id) |> 
  summarise(event_u = mean(hru_u.1),
            event_p = sum(hru_p.1))
  
event_stats_indiv_tb <- left_join(tb_d_drip_smry, tree_dL_smry) |> 
  left_join(mod_subl_smry) |> 
  left_join(met_smry) |> 
  mutate(
    unld_melt = obs_cml_dL - obs_cml_drip - mod_cml_subl,
    frac_unld_melt = unld_melt/obs_cml_drip) |> 
  filter(abs(mod_cml_subl) < 1,
         !event_id == '2022-06-14'#,
         # frac_unld_melt > 0
         )

ggplot(event_stats_indiv_tb, aes(obs_cml_dL, frac_unld_melt)) +
  geom_point() +
  xlim(c(0, NA))

# model
unld_melt_lm <- lm(frac_unld_melt ~ obs_cml_dL, data = event_stats_indiv_tb)
summary(unld_melt_lm)

event_frac_unld_melt <- event_stats_indiv_tb |> 
  group_by(event_id) |> 
  summarise(
    obs_cml_dL = mean(obs_cml_dL),
    mean_ = mean(frac_unld_melt, na.rm = T),
    sd_ = sd(frac_unld_melt, na.rm = T))

ggplot(event_frac_unld_melt, aes(obs_cml_dL, mean_)) +
  geom_point() +
  geom_errorbar(aes(ymax = mean_+sd_, ymin = mean_-sd_)) +
  xlim(c(0, NA)) +
  labs(y = 'Unloading to Melt Ratio (-)',
       x = 'Initial Canopy Snow Load (mm)')

ggsave(
  paste0(
    'figs/results/tb_unload_to_melt_ratio',
    run_tag_updt,
    # format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
    '.png'
  ),
  width = 4,
  height = 4
)

event_frac_unld_melt_out <- event_frac_unld_melt |> 
  mutate(name = 'weighed tree unld / tipping bucket drip') |> 
  select(tree_labs = obs_cml_dL,
         event_id,
         unld_melt_ratio = mean_,
         unld_melt_ratio_sd = sd_,
         name
         )

saveRDS(
  event_frac_unld_melt_out,
  'data/tipping-buckets/tipping_bucket_event_frac_unld_melt.rds'
)

# plot w tree with obs drip 
# 
# obs_tree_drip <- obs_tree |> 
#   mutate(name = 'tree') |> 
#   select(datetime, event_id, name, value = observed) |> 
#   rbind(obs_mod_cml_drip |> 
#               filter(name == 'tipping_buckets') |> 
#               select(datetime, event_id, name, value = cml_drip)) |> filter(event_id %in% tb_d_drip_smry$event_id)
# 
# # plot
# obs_tree_drip |>
#   ggplot(aes(datetime, value, colour = name)) +
#   geom_line() +
#   facet_wrap(~event_id, scales = 'free')


# # bring in tree for context 
# obs_tree <- tree_drip_events_df |> 
#   left_join(tb_events_long) |> 
#   select(datetime, event_id, obs = obs_tree)
# mod_tree <- crhm_output_tree |> 
#   select(datetime, mod = Snow_load.1)
# obs_mod_tree <- left_join(obs_tree, mod_tree) |>
#   pivot_longer(c(obs, mod)) |> 
#   mutate(group = 'Canopy Snow Load (mm)')
# 
# for (event in tb_events$event_id) {
#   
#   obs_mod_cml_drip |> 
#     select(datetime, event_id, name, value = cml_drip) |> 
#     mutate(group = 'Cumulative Drip (mm)') |> 
#     rbind(obs_mod_tree) |>
#     filter(event_id == event) |>
#     ggplot(aes(datetime, value, colour = name)) + 
#     geom_line() +
#     facet_grid(rows = vars(group), scales = 'free') +
#     ggtitle(as.Date(event)) +
#     ylab(element_blank()) +
#     xlab(element_blank())
#   
#     ggsave(
#       paste0(
#         'figs/crhm-analysis/drip/indiv_events/crhm_vs_tb_drip_',
#         as.Date(event),
#         '_',
#         format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
#         '.png'
#       ),
#       width = 8,
#       height = 6
#     )
# }
