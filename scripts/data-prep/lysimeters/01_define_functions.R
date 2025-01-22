# functions

to_long_melt <- function(from,
                         to,
                         class,
                         quality,
                         weighed_tree_quality,
                         bad_troughs,
                         is_melt_event,
                         notes,
                         event_id) {
  datetime <- seq(from, to, 900)
  
  out <- data.frame(datetime,
                    class,
                    quality,
                    weighed_tree_quality,
                    bad_troughs,
                    is_melt_event,
                    notes,
                    event_id)
  
  return(out)
}

to_long <- function(from, to, class, quality, weighed_tree_quality, notes, event_id){
  datetime <- seq(from, to, 900)
  
  out <- data.frame(datetime, class, quality, weighed_tree_quality, notes, event_id)
  
  return(out)
}

rho_fresh_snow <- function(Ta){
  fsd <- 67.92 + 51.25 * exp(Ta / 2.59)
  return(fsd)
}

species_capacity <- function(S_bar, fsd){
  S <- S_bar * (0.27 + (46/fsd))
  return(S)
}

W_max <- function(species_capacity, lai){
  species_capacity * lai
}

snow_exposure_coef <- function(W_max, canopy_load){
  ks <- 0.0114 # snow shape coefficient for jack pine from CRHM code 2023-10-04
  fract <- 0.37 # fractal dimension of intercepted snow from CRHM code 2023-10-04
  
  frac_canopy_load <- canopy_load/W_max 
  
  Ce <- ifelse(frac_canopy_load <= 0,
               0.07,
               ks * (frac_canopy_load)^-fract) # yes this ifelse works as expected w.o. having to do a forloop 
  return(Ce)
}

subl_cpy <- function(diml_subl, snow_exposure_coef, canopy_load, timestep_seconds){
  potential_subl <- diml_subl * snow_exposure_coef # potential sublimation rate from the canopy without considering the canopy load
  
  subl_cpy_per_second <- -canopy_load*potential_subl # per second
  
  subl_cpy_per_timestep <- subl_cpy_per_second*timestep_seconds # per timestep
  return(subl_cpy_per_timestep)
  
  # limit sublimation to canopy snow available and take sublimated snow away from canopy snow at timestep start
  # if(subl_cpy_per_timestep > canopy_load){
  #   subl_cpy_per_timestep <- canopy_load
  #   canopy_load <- 0
  # } else{
  #   
  # }
}

plot_facet_one_vars <- function(cur_event_id, cur_load_met, cur_var_names, filename_prefix) {
  if(is.na(filename_prefix) == T){
    filename <- paste0(cur_event_id, '.png')
  } else {
    filename <- paste0(filename_prefix, cur_event_id, '.png')
  }
  
  p <-  cur_load_met |> 
    filter(name %in% cur_var_names,
           event_id == cur_event_id,
           is.na(value) == F) |>
    ggplot(aes(datetime, value)) +
    geom_line() +
    geom_point()+
    facet_grid(rows = vars(name),
               scales = 'free',
               switch = "y", # flip the facet labels along the y axis from the right side to the left
               # labeller = as_labeller( # redefine the text that shows up for the facets
               #   c(cur_var_names))
    ) +
    ylab(NULL) + # remove the word "values"
    xlab(NULL) +
    theme(strip.background = element_blank(), # remove the background
          strip.placement = "outside") +
    ggtitle(cur_event_id)
  
  ggsave(plot = p, filename = paste0(filename), width = 8, height = 8)
  
  return(p)
}

plot_facet_two_vars <- function(cur_event_id, cur_load_met, cur_var_names, filename_prefix) {
  if(is.na(filename_prefix) == T){
    filename <- paste0(cur_event_id, '.png')
  } else {
    filename <- paste0(filename_prefix, cur_event_id, '.png')
  }
  
  p <-  cur_load_met |> 
    filter(name %in% cur_var_names,
           event_id == cur_event_id,
           is.na(value) == F) |>
  ggplot(aes(datetime, value)) +
  geom_line() +
  geom_point()+
  facet_grid(rows = vars(name),
             cols = vars(tree_cal_trough_name),
             scales = 'free',
             switch = "y", # flip the facet labels along the y axis from the right side to the left
             # labeller = as_labeller( # redefine the text that shows up for the facets
             #   c(cur_var_names))
  ) +
  ylab(NULL) + # remove the word "values"
  xlab(NULL) +
  theme(strip.background = element_blank(), # remove the background
        strip.placement = "outside") +
  ggtitle(cur_event_id)

  ggsave(plot = p, filename = paste0(filename), width = 8, height = 8)

  return(p)
}

cut_variable <- function(df, var_name, var_step){
  bin_col_name <- paste0(var_name, '_binned')
  lab_col_name <- paste0(var_name, '_labs')
  
  min_var <- floor(
    min(df[,var_name], na.rm = T))
  max_var <- ceiling(
    max(df[,var_name], na.rm = T))
  

    var_breaks <- seq(
      min_var,
      max_var,
      var_step)


  
  var_labs_seq <- seq(min_var, max_var-(var_step/2), var_step)
  
  stopifnot(length(var_labs_seq) + 1 == length(var_breaks))
  
  df[,bin_col_name] <- cut(df[,var_name, drop = TRUE], var_breaks)
  
  df[,lab_col_name] <- cut(df[,var_name, drop = TRUE], 
                                var_breaks, 
                                labels = var_labs_seq
  )
  
  return(df)
}
