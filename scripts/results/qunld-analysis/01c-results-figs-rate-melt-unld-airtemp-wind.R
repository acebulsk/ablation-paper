
unld_met_smry <- obs_mod_met_melt |> 
  filter(is.na(tree_mm) == F) |> 
  group_by(tree_labs, temp_labs, wind_labs) |> 
  summarise(q_unl_avg = mean(q_unl, na.rm = T),
            q_unl_sd = sd(q_unl, na.rm = T),
            sd_low = ifelse((q_unl_avg - q_unl_sd)<0,0, q_unl_avg - q_unl_sd),
            sd_hi = q_unl_avg + q_unl_sd,
            ci_low = quantile(q_unl,0.05),
            ci_hi = quantile(q_unl, 0.95),
            sum_snow = sum(dU),
            n = n()) |> 
  filter(n >= 3,
         # tree_labs > 1,
         # tau_labs < 3, # tau transport potential above this threshold
         sum_snow > 0.1)

stop('not enough range in wind speeds over these events...')