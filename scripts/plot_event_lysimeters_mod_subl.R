
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

troughs <- met_unld |> 
  filter(name %in% scl_names) |> 
  select(datetime, name, value)

tree <- met_unld |> 
  filter(name == 'tree_mm') |> 
  mutate(name = 'tree_mm') |> 
  select(datetime, name, value = tree_mm)

mod_subl <- met_unld |> 
  filter(name == 'tree_mm') |> 
  left_join(q_unld_tree) |> 
  mutate(
    obs_tree_mod_subl_cpy = ifelse(
      obs_tree_mod_subl_cpy > q_tree_ablation,
      q_tree_ablation,
      obs_tree_mod_subl_cpy
    )
  ) |>
  group_by(event_id) |> 
  mutate(subl_cpy_cml = cumsum(obs_tree_mod_subl_cpy/4),
         name = 'mod_cuml_subl_cpy') |> 
  select(datetime, name, value = subl_cpy_cml) |> 
  ungroup()

plot_df <- rbind(tree, troughs) |> 
  group_by(event_id, name) |> 
  mutate(value = value - min(value)) |> 
  rbind(mod_subl) |> 
  mutate(name = factor(
    name,
    levels = c(
      'tree_mm',
      'mod_cuml_subl_cpy',
      'sparse_forest',
      'medium_density_forest',
      'dense_forest'
    )
  ))

plot_df |>
  filter(event_id == '2022-02-01') |>
  ggplot(aes(datetime, value, colour = name)) +
  geom_line() +
  geom_point() +
  facet_grid(rows = vars(name), scales = 'free')
ggplotly()

# plot all and save to figs
lapply(
  events_fltr,
  plot_facet_two_vars,
  cur_load_met = plot_df,
  cur_var_names = plot_df$name |> unique(),
  filename_prefix = 'figs/supplement/load_plots/timeseries_lysimeters_mod_subl_cpy_'
)
