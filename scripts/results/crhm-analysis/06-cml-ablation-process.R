
crhm_output_newsim_subl_tf <- crhm_output_newsim |> 
  inner_join(obs_tree) |> 
  mutate(subl = -delsub_veg_int.1,
         melt_unld = deldrip_veg_int.1 + delunld_int.1,
         evap = delevap_veg_int.1,
         group = 'C25') |> 
  select(datetime, event_id, subl, melt_unld, evap = delevap_veg_int.1, group)  

crhm_output_baseline_subl_tf <- crhm_output_baseline |> 
  inner_join(obs_tree) |> 
  mutate(
    melt_unld = SUnload.1 + SUnload_H2O.1,
    evap = 0,
    group = 'E10') |> 
  select(datetime, event_id, subl = Subl_Cpy.1, melt_unld, evap, group) 

crhm_output_roesch_subl_tf <- crhm_output_roesch |> 
  inner_join(obs_tree) |> 
  mutate(
    melt_unld = SUnload.1,
    evap = 0,
    group = 'R01') |> 
  select(datetime, event_id, subl = Subl_Cpy.1, melt_unld, evap, group) 

crhm_output_andreadis_subl_tf <- crhm_output_andreadis |> 
  inner_join(obs_tree) |> 
  mutate(subl = -delsub_veg_int.1,
         melt_unld = delmelt_veg_int.1 + delunld_int.1,
         group = 'A09') |> 
  select(datetime, event_id, subl, melt_unld, evap = delevap_veg_int.1, group) 

subl_tf_smry <- 
  crhm_output_newsim_subl_tf |> 
  # rbind(crhm_output_newsim_subl_tf, crhm_output_baseline_subl_tf) |> 
  # rbind(crhm_output_roesch_subl_tf) |> 
  # rbind(crhm_output_andreadis_subl_tf) |> 
  pivot_longer(c(subl,
                 evap,
                 melt_unld),
             values_to = 'dU') |> 
  group_by(event_id, name, group) |> 
  mutate(cml_dU = cumsum(dU),
         cml_dU = cml_dU - first(cml_dU)) |>  # so each event starts at 0
  select(datetime, event_id, name, group, value = cml_dU)

ggplot(subl_tf_smry, aes(datetime, value, colour = name)) +
  geom_line() +
  facet_wrap(~event_id, scales = 'free')

plotly::ggplotly()
