# Script to total canopy snow sublimation vs. unloading + melt + drip for each model

crhm_output_newsim_subl_tf <- crhm_output_newsim |> 
  inner_join(obs_tree) |>
  mutate(atmosphere = -(delsub_veg_int.1+delevap_veg_int.1),
         ground = deldrip_veg_int.1 + delunld_int.1 + throughfall_snow.1 + throughfall_rain.1,
         group = 'CP25') |> 
  select(datetime, event_id, atmosphere, ground, group)  

crhm_output_baseline_subl_tf <- crhm_output_baseline |> 
  inner_join(obs_tree) |> 
  mutate(
    ground = net_rain.1 + net_snow.1,
    group = 'E10') |> 
  select(datetime, event_id, atmosphere = Subl_Cpy.1, ground, group) 

crhm_output_roesch_subl_tf <- crhm_output_roesch |> 
  inner_join(obs_tree) |> 
  mutate(
    ground = net_rain.1 + net_snow.1,
    group = 'R01') |> 
  select(datetime, event_id, atmosphere = Subl_Cpy.1, ground, group) 

crhm_output_andreadis_subl_tf <- crhm_output_andreadis |> 
  inner_join(obs_tree) |> 
  mutate(atmosphere = -(delsub_veg_int.1+delevap_veg_int.1),
         ground = deldrip_veg_int.1 + delunld_int.1 + throughfall_snow.1 + throughfall_rain.1,
         group = 'SA09') |> 
  select(datetime, event_id, atmosphere, ground, group)  

subl_tf_smry <- rbind(crhm_output_newsim_subl_tf, crhm_output_baseline_subl_tf) |> 
  rbind(crhm_output_roesch_subl_tf) |> 
  rbind(crhm_output_andreadis_subl_tf)

subl_tf_smry_by_model_and_event <- subl_tf_smry |> 
  group_by(group, event_id) |> 
  summarise(
    atmosphere = sum(atmosphere),
    ground = sum(ground),
    total_ablation = atmosphere + ground,
    atmosphere = atmosphere/total_ablation,
    ground = ground/total_ablation
  )  |> 
  left_join(mod_d_drip_smry_frac |> mutate(event_id = event_id |> as.character())) |> 
  mutate(facet_title = paste(event_type, '-', event_id))

ggplot(subl_tf_smry_by_model_and_event |>
         pivot_longer(c(atmosphere, ground)),
       aes(x = group, y = value, fill = name)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~facet_title, ncol = 3) +
  labs(fill = element_blank(),
       x = 'Model',
       y = 'Fraction of Total Ablation (-)')

ggsave('figs/crhm-analysis/partitioning/ablation_partition_atmosphere_ground_by_event.png',
       width = 7,
       height = 10,
       )


subl_tf_smry_by_model <- subl_tf_smry |> 
  group_by(group) |> 
  summarise(
    atmosphere = sum(atmosphere),
    ground = sum(ground),
    total_ablation = atmosphere + ground,
    atmosphere = atmosphere/total_ablation,
    ground = ground/total_ablation
  )

saveRDS(subl_tf_smry_by_model, 'data/atmosphere_ground_partition_by_model.rds')

ggplot(subl_tf_smry_by_model |>
         pivot_longer(c(atmosphere, ground)),
       aes(x = group, y = value, fill = name)) +
  geom_bar(stat = 'identity') +
  labs(fill = element_blank(),
       x = 'Model',
       y = 'Fraction (-)')
  
