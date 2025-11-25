# Script to check distribution of melt and non-melt events to determine if justified
#  to split into two populations for subsequent relationship testing

crhm_output_tree <- rbind(crhm_output |> select(datetime, name, delsub_veg_int.1, delmelt_veg_int.1),
                          crhm_output_w_tree |> select(datetime, name, delsub_veg_int.1, delmelt_veg_int.1))

# get weighed tree unloading over melt events (turned off for now)
w_tree_unld_melting <- left_join(w_tree_q_unld_15, crhm_output_w_tree) |>
  mutate(wtr_year = weatherdash::wtr_yr(datetime)) |> 
  filter(delmelt_veg_int.1 > 0) |> 
  group_by(datetime, event_id) |>
  summarise(
            name = 'w_tree',
            tree_mm = last(tree_mm), # leave summarise in case adding time aggregation
            dL = sum(dL),
            delmelt_veg_int.1 = sum(delmelt_veg_int.1),
            delsub_veg_int.1 = -sum(delsub_veg_int.1), # comes in as negative wrt canopy snow already
            q_unl = (dL - delsub_veg_int.1)*4,
            q_unl = ifelse(q_unl < 0, 0, q_unl)) |> 
  select(datetime, name, q_unl)

#add in weighed tree just over melt events 

q_unld_scl_tree <- rbind(q_unld_scl |> select(datetime, name, q_unl), w_tree_unld_melting) 

# agg just the unloading data so sufficient snow in measurement intervals 
q_unld_scl_tree_agg <- q_unld_scl_tree |> 
  left_join(crhm_output_tree) |> 
  mutate(datetime = ceiling_date(datetime, unit = '6 hour')) |> # ceiling ensures the timestamp corresponds to preeceeding records
  group_by(datetime, name) |>
  summarise(
    del_unl = sum(q_unl/4),
    q_unl = del_unl/6,
    q_melt = sum(delmelt_veg_int.1)/6,
    q_subl = -sum(delsub_veg_int.1)/6
  ) |> 
  mutate(
    event_type = case_when(
      q_melt == 0 ~ 'non-melt',
      TRUE ~ 'melt'
    )
  ) |> 
  filter(q_unl > 0, q_unl < 7, del_unl > 0.1) |> 
  select(datetime, q_unl:q_subl, event_type) |> 
  pivot_longer(q_unl:q_subl) 

met_long <- 
  # q_unld_met_scl |> 
  # met_binned has duplicated data for each scaled weighed tree
  # q_unld_scl has been filtered to remove troughs for erroneous periods
  # left_join(q_unld_scl, crhm_output, by = c('datetime', 'name')) |>
  left_join(q_unld_scl_tree, crhm_output_tree, by = c('datetime', 'name')) |> 
  left_join(ft_met) |>
  filter(q_unl > 0, q_unl < 7) |> 
  mutate(
    event_type = case_when(
      delmelt_veg_int.1 == 0 ~ 'non-melt',
      TRUE ~ 'melt'
    )
  ) |> 
  select(datetime, t:u, event_type) |> 
  pivot_longer(t:u)

event_df_long <- rbind(q_unld_scl_tree_agg, met_long) |>
  left_join(var_name_dict, by = 'name') |>
  mutate(pretty_name = factor(pretty_name, levels = c(pretty_names_vect)))

event_df_long |> 
  group_by(event_type, name) |> summarise(
    mean(value),
    n())

# PLOT all events / data -----

event_df_long |>
  ggplot()+
  geom_density(aes(x = value, colour = event_type), fill = NA) +
  # geom_vline(aes(xintercept = mean_value), color = 'red', linetype = 'dashed') +  # Add vertical line at mean
  theme(
    legend.position = 'bottom',
    strip.background = element_blank(),
    strip.placement = 'outside',
    panel.spacing = unit(0.5, "lines")  # <-- increase spacing between facets
  ) +
  facet_wrap(~pretty_name, scales = 'free', strip.position = 'bottom') +
  labs(
    x = element_blank(),
    y = 'Probability Density (-)',
    colour = 'Event Type'
  )

# ggsave('figs/final/figure4a', width = 6, height = 5, device = png)

event_df_long |>
  ggplot(aes(x = value, colour = event_type)) +
  stat_ecdf() +
  # optional: vertical line for mean
  # geom_vline(aes(xintercept = mean_value), color = 'red', linetype = 'dashed') +
  theme(
    legend.position = 'bottom',
    strip.background = element_blank(),
    strip.placement = 'outside',
    panel.spacing = unit(0.5, "lines")
  ) +
  facet_wrap(~pretty_name, scales = 'free', strip.position = 'bottom') +
  xlab(NULL) +
  labs(
    x = element_blank(),
    y = 'Cumulative Probability (-)',
    colour = 'Event Type'
  )

ggsave('figs/final/figure4a.png', width = 6, height = 5, device = png)

# V different number of obs so doesnt make sense to show couts 
event_df_long |>
  # filter(event_type != 'melt') |> 
  ggplot(aes(value, group = event_type))+
  geom_histogram(aes(fill = event_type), 
                 alpha = 0.3,
                 position = "identity") +
  facet_wrap(~pretty_name, scales = 'free') +
  xlab(element_blank()) +
  ylab('Count (-)')


# STATS on all events ----

library(effsize)
library(tidyr)
library(purrr)
library(combinat)  # for pairwise combinations

fmt_p <- function(p) {
    if (p < 0.05) {
      "p < 0.05"
    } else {
      "n.s."
    }
  }

# Function for pairwise comparison
stats_summary <- event_df_long %>%
  group_by(Variable = pretty_name) %>%
  summarise(
    # event_type1 = unique(event_type)[1],
    # event_type2 = unique(event_type)[2],

    # Tests whether one distribution tends to have larger values than the other.
    Wilcox = wilcox.test(
      value[event_type == unique(event_type)[1]],
      value[event_type == unique(event_type)[2]]
    )$p.value |> fmt_p(),

    # Tests for differences in the entire distribution
    KS = ks.test(
      value[event_type == unique(event_type)[1]],
      value[event_type == unique(event_type)[2]]
    )$p.value |> fmt_p(),

    # Gives a scale of how different the distributions are, independent of sample size

    # Outputs delta value:

    # 0 → identical

    # ±0.147 → small difference

    # ±0.33 → medium

    # ±0.474 → large
    
    cliff_delta = cliff.delta(
      value[event_type == unique(event_type)[1]],
      value[event_type == unique(event_type)[2]]
    )$estimate |> round(2),

    median_diff =
      median(value[event_type == unique(event_type)[1]]) -
      median(value[event_type == unique(event_type)[2]]) |> round(2)
  )

stats_summary

write.csv(stats_summary, 'data/stats/melt_non_melt_variable_distribution_tests.csv', row.names = F)
