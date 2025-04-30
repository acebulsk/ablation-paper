# LOAD DATA ----

source('scripts/results/qunld-analysis/00-results-figs-setup.R')

# COMBINED BINNED DF ----

met_unld_w_bins <- 
  # q_unld_met_scl |> 
  # met_binned has duplicated data for each scaled weighed tree
  # q_unld_scl has been filtered to remove troughs for erroneous periods
  # left_join(q_unld_scl, crhm_output, by = c('datetime', 'name')) |>
  left_join(
    q_unld_scl |> select(datetime, name, q_unl, dU),
    crhm_output |> select(datetime,
                          name,
                          q_subl_veg,
                          subl_labs,
                          canopy_snowmelt_labs,
                          m_s_veg.1,
                          delmelt_veg_int.1),     by = c('datetime', 'name')
  ) |>
  left_join(
    q_unld_tree_all_troughs,
    by = c('datetime', 'name')
  ) |>
  left_join(wind_binned) |> 
  left_join(temp_binned) |> 
  left_join(tau_binned) |> 
  left_join(ti_dep_binned) |> 
  left_join(ft_met) |> 
  filter(
    # name %in% scl_names,
    q_unl < 7,
    q_unl > 0,
    # q_subl_veg > 0,
    # delmelt_veg_int.1 == 0
    # hru_t.1 < -6
  )# |> 
# mutate(
#   tree_mm = ifelse(is.na(tree_mm), m_s_veg.1, tree_mm) # fill missing tree data with model
# )

## BIN TREE DATA ----

# note zeros are not included in binning, to add set inlcude.lowest = T
min_tree <- round(
  min(met_unld_w_bins$tree_mm, na.rm = T),3)
max_tree <- round(
  max(met_unld_w_bins$tree_mm, na.rm = T),3)
tree_step <- 15

tree_breaks <- seq(
  min_tree,
  max_tree+tree_step,
  tree_step)

# tree_breaks <- c(0, 5,20)
tree_breaks <- c(0, 2, 6,  20) # works well for wind but breaks sublimation fn

tree_labs_seq <- label_bin_fn(bins = tree_breaks)

stopifnot(tail(tree_breaks, 1) > max(met_unld_w_bins$tree_mm, na.rm = T))
stopifnot(length(tree_labs_seq) + 1 == length(tree_breaks))

met_unld_w_bins$tree_binned <- cut(met_unld_w_bins[,'tree_mm', drop = TRUE], tree_breaks, include.lowest = T)

met_unld_w_bins$tree_labs <- cut(met_unld_w_bins[,'tree_mm', drop = TRUE], 
                                  tree_breaks, include.lowest = T, 
                                  labels = tree_labs_seq)

met_unld_w_bins$tree_labs <- as.numeric(as.character(met_unld_w_bins$tree_labs))

met_unld_w_bins |> 
  group_by(tree_labs) |> 
  summarise(tree_mean = mean(tree_mm))

met_unld_w_bins |> 
  group_by(tree_labs) |> 
  tally()


# AGG TIME INTERVALs ----

# going to try multiple regression between unloading and wind, sublimation, and
# tree load at diff agg time intervals, also check dU has acceptable relative
# error

scl_weight <- 15 # kg, the 'raw' kg is still zeroed to the amount of water in the trough
scl_rel_accuracy <- 0.02/100 # rel accuracy is 0.02% as stated in the strain gauge manual

t_int <- '6 hours'

# Extract number and unit
parts <- strsplit(t_int, " ")[[1]]
value <- as.numeric(parts[1])
unit <- tolower(parts[2])

# Convert to seconds
seconds <- switch(unit,
                  "second" = value,
                  "seconds" = value,
                  "minute" = value * 60,
                  "minutes" = value * 60,
                  "hour" = value * 3600,
                  "hours" = value * 3600,
                  "day" = value * 86400,
                  "days" = value * 86400,
                  stop("Unknown time unit"))

hours <- seconds/(60*60)

scl_met_agg <- left_join(ft_met, q_unld_scl) |> 
  left_join(q_unld_tree_all_troughs |> select(datetime, name, tree_mm)) |> 
  left_join(crhm_output |> select(datetime, name, delmelt_veg_int.1, delsub_veg_int.1)) |> 
  left_join(scl_df_kg_m2_raw_long) |> 
  select(datetime, event_id, name, t, u, p, dU, delmelt_veg_int.1, delsub_veg_int.1, tree_mm, scl_raw_kg_m2) |> 
  mutate(datetime = ceiling_date(datetime, unit = t_int)) |> # ceiling ensures the timestamp corresponds to preeceeding records
  group_by(datetime, event_id, name) |>
  summarise(t = mean(t),
            u = mean(u),
            p = sum(p),
            dU = sum(dU),
            dmelt_veg = sum(delmelt_veg_int.1),
            dsub_veg = sum(delsub_veg_int.1),
            q_unl = dU / hours,
            q_subl = -dsub_veg / hours,
            q_melt = dmelt_veg / hours,
            tree_mm = first(tree_mm),
            scl_raw_kg_m2 = first(scl_raw_kg_m2)+scl_weight) |> 
  mutate(scl_abs_accuracy = scl_rel_accuracy * scl_raw_kg_m2,
         scl_rel_perc_error = (scl_abs_accuracy/dU)*100
  ) |> 
  ungroup()

err_th <- 15
frac_above_err_th <- sum(scl_met_agg$scl_rel_perc_error[!is.na(scl_met_agg$scl_rel_perc_error)] > err_th)/length(!is.na(scl_met_agg$scl_rel_perc_error))
frac_above_err_th <- round(frac_above_err_th, 2)
print(paste0(frac_above_err_th*100, ' % of records are above error threshold of ', err_th, '%'))

# Functions 

summarise_met_data <- function(df, bin_col1, bin_col2, min_n, min_snow) {
  df |> 
    filter(!is.na(tree_mm)) |> 
    group_by({{ bin_col1 }}, {{ bin_col2 }}) |> 
    summarise(
      q_unl_avg = mean(q_unl, na.rm = TRUE),
      q_unl_sd = sd(q_unl, na.rm = TRUE),
      sd_low = ifelse((q_unl_avg - q_unl_sd) < 0, 0, q_unl_avg - q_unl_sd),
      sd_hi = q_unl_avg + q_unl_sd,
      ci_low = quantile(q_unl, 0.05, na.rm = TRUE),
      ci_hi = quantile(q_unl, 0.95, na.rm = TRUE),
      sum_snow = sum(dU, na.rm = TRUE),
      n = n(),
      .groups = "drop"
    ) |> 
    pivot_longer(cols = {{ bin_col1 }}) |> 
    filter(n >= min_n, sum_snow > min_snow)
}

summarize_by_predictors <- function(df, predictor_vars) {
  df |> 
    filter(!is.na(tree_mm)) |> 
    group_by(across(all_of(predictor_vars))) |> 
    summarise(
      q_unl_avg = mean(q_unl, na.rm = TRUE),
      q_unl_sd = sd(q_unl, na.rm = TRUE),
      sd_low = ifelse((q_unl_avg - q_unl_sd) < 0, 0, q_unl_avg - q_unl_sd),
      sd_hi = q_unl_avg + q_unl_sd,
      ci_low = quantile(q_unl, 0.05, na.rm = TRUE),
      ci_hi = quantile(q_unl, 0.95, na.rm = TRUE),
      sum_snow = sum(dU, na.rm = TRUE),
      n = n(),
      .groups = "drop"
    ) |> 
    filter(n >= 3, sum_snow > 0.1)
}

# Main function to generate linear model summaries
generate_lm_model_table <- function(df_raw, predictors_named) {
  
  # Ensure 'tree_labs' is always included in the model
  fixed_predictor <- "L"
  
  # remove tree here to vary the other ones
  variable_predictors <- setdiff(names(predictors_named), fixed_predictor)
  
  combos <- unlist(
    lapply(1:length(variable_predictors), function(i) combn(variable_predictors, i, simplify = FALSE)),
    recursive = FALSE
  )
  
  format_estimate <- function(estimate, pval) {
    sig <- case_when(
      pval < 0.05  ~ "*",
      TRUE ~ "ns"
    )
    sprintf("%.2f (%s)", estimate, sig)
  }
  
  results <- purrr::map_dfr(combos, function(vars) {
    # Include 'tree_labs' in the combination
    var_names_with_tree <- c(fixed_predictor, vars)
    
    actual_vars <- unname(predictors_named[var_names_with_tree])
    
    df_smry <- summarize_by_predictors(df_raw, actual_vars)
    
    if (nrow(df_smry) == 0) return(NULL)
    
    formula <- as.formula(paste("q_unl_avg ~", paste(actual_vars, collapse = " + ")))
    model <- lm(formula, data = df_smry)
    tidy_model <- broom::tidy(model)
    glance_model <- broom::glance(model)
    
    # Initialize blank terms
    terms <- setNames(rep("—", length(predictors_named)), names(predictors_named))
    
    for (term in tidy_model$term) {
      if (term == "(Intercept)") next
      estimate <- tidy_model$estimate[tidy_model$term == term]
      pval <- tidy_model$p.value[tidy_model$term == term]
      name <- names(predictors_named)[predictors_named == term]
      terms[[name]] <- format_estimate(estimate, pval)
    }
    
    intercept_row <- tidy_model |> dplyr::filter(term == "(Intercept)")
    intercept_val <- format_estimate(intercept_row$estimate, intercept_row$p.value)
    
    tibble::tibble(
      intercept = intercept_val,
      !!!setNames(terms, names(terms)),
      Adj_R2 = round(glance_model$adj.r.squared, 2),
      AIC = round(glance_model$AIC, 1)
    )
  })
  
  return(results)
}