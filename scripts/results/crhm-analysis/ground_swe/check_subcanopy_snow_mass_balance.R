# Script to check subcanopy snow mass balance

crhm_output_newsim_check <- crhm_output_newsim |> 
  mutate(del_swe = SWE.1 - lag(SWE.1),
         new_swe = delunld_int.1 + deldrip_veg_int.1 + throughfall_snow.1 + throughfall_rain.1,
         int_snow_check = (hru_p.1 - hru_rain.1) - throughfall_snow.1,
         int_rain_check = hru_rain.1 - throughfall_rain.1)


ggplot(crhm_output_newsim_check |> 
         pivot_longer(c(del_swe, new_swe, hru_p.1, delunld_int.1, deldrip_veg_int.1)),
       aes(datetime, value, colour = name)) +
  geom_line()
plotly::ggplotly()


crhm_output_newsim_check
ggplot(crhm_output_newsim_check |> 
         pivot_longer(c(hru_p.1, hru_rain.1, intercepted_snow.1, intercepted_rain.1, )),
       aes(datetime, value, colour = name)) +
  geom_line()
plotly::ggplotly()
