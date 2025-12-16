# Plot exploratory relationship between unloading and binned independent vars

## Exploratory plots ---- 

### wind vs unloading rate ----

xlabs_dict <- data.frame(
  name = c('temp_labs',
           'wind_labs',
           'tau_labs',
           'ti_dep_labs',
           'canopy_snowmelt_labs',
           'subl_labs'),
  name_pretty = c('Air Temperature (°C)',
                  'Wind Speed (m s⁻¹)',
                  'Shear Stress (N m⁻²)',
                  'Ice-Bulb Temp. Depression (°C)',
                  'Simulated Snowmelt (mm hr⁻¹)',
                  'Simulated Sublimation (mm hr⁻¹)')
)

xlabs_dict$name_pretty <- factor(
  xlabs_dict$name_pretty,
  levels = c('Air Temperature (°C)',
             'Wind Speed (m s⁻¹)',
             'Shear Stress (N m⁻²)',
             'Ice-Bulb Temp. Depression (°C)',
             'Simulated Snowmelt (mm hr⁻¹)',
             'Simulated Sublimation (mm hr⁻¹)')
)

# Create a small data frame with manual labels for the specific facets

met_unld_w_bins_smry_wind <- summarise_met_data(met_unld_w_bins,
                                                wind_labs,
                                                tree_labs,
                                                3,
                                                0.1)

met_unld_w_bins_smry_tau <- summarise_met_data(met_unld_w_bins,
                                                tau_labs,
                                                tree_labs,
                                                3,
                                                0.1)

met_unld_w_bins_smry_temp <- summarise_met_data(met_unld_w_bins,
                                                temp_labs,
                                                tree_labs,
                                                3,
                                                0.1)

met_unld_w_bins_smry_melt <- summarise_met_data(met_unld_w_bins,
                                                canopy_snowmelt_labs,
                                                tree_labs,
                                                3,
                                                0.1)

met_unld_w_bins_smry_subl <- summarise_met_data(met_unld_w_bins,
                                                subl_labs,
                                                tree_labs,
                                                3,
                                                0.1)

met_unld_w_bins_smry_ti_dep <- summarise_met_data(met_unld_w_bins,
                                                ti_dep_labs,
                                                tree_labs,
                                                3,
                                                0.1)


bins_df <- rbind(met_unld_w_bins_smry_wind, met_unld_w_bins_smry_temp) |> 
  rbind(met_unld_w_bins_smry_melt) |> 
  rbind(met_unld_w_bins_smry_subl) |> 
  rbind(met_unld_w_bins_smry_tau) |> 
  rbind(met_unld_w_bins_smry_ti_dep) |> 
  left_join(xlabs_dict)

# check OLS regression assumptions prior to running stats (linearity, independence, Heteroscedasticity, multicolinerity, normality)

lm_q_melt <- lm(q_unl_avg ~ value, data = met_unld_w_bins_smry_melt)
summary(lm_q_melt)
par(mfrow = c(2,3))
plot(lm_q_melt, which = 1)  # Residuals vs Fitted
durbinWatsonTest(lm_q_melt)
plot(lm_q_melt, which = 3)  # Scale-Location plot
plot(lm_q_melt, which = 2)  # QQ plot
hist(resid(lm_q_melt))
shapiro.test(resid(lm_q_melt))
vif(lm_q_melt)

# all plots together
ggplot(bins_df, 
       aes(x = value, y = q_unl_avg, colour = as.factor(round(tree_labs)))) + 
  # geom_point(data = met_unld_w_bins, aes(u, q_unl), alpha = 0.1, colour = 'black') +
  geom_errorbar(aes(
    x = value, 
    ymax = sd_hi,
    ymin = sd_low
  ), width = 0)  +
  geom_point(size = 3) +
  ylab(expression("Unloading Rate (mm hr"^"-1"*")")) +
  xlab(element_blank()) +
  theme_bw() +
  theme(
    legend.position = 'bottom',
    strip.background = element_blank(),
    strip.placement = 'outside',
    panel.spacing = unit(0.5, "lines")  # <-- increase spacing between facets
  ) +
  # theme_bw(base_size = 14) +
  # ylim(NA, 3.1) +
  # xlim(NA, 3.5) +
  scale_color_viridis_d(begin = 0, end = 0.8) +
  # scale_color_manual(values = c("#f89540", "#0072B2","#f89540", "#0072B2")) +
  labs(color = 'Canopy Load (mm)') + 
  facet_wrap(~name_pretty, scales = 'free_x', strip.position = 'bottom') 


# ggsave('figs/results/explore/scl_q_unld_vs_bins.png', width = 6.1, height = 5)
ggsave('figs/supplement/figureS3.png', width = 6.5, height = 5)
