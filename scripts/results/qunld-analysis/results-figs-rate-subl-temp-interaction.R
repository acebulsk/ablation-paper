# Since the scripts results-figs-rate-subl and ...-wind both show assocaitions
# with unloading we need to test a relationship with both wind and subl as
# predictors of unloading

# LOAD MODEL DATA ----

# Select model run with all unloading events weighed tree snow load assimilated

prjs <- c('ffr_cc0.19_sparse_cansnobal',
          'ffr_cc0.54_closed_cansnobal',
          'ffr_cc0.63_mixed_cansnobal'
)
keywords <- c("sparse", "closed", "mixed")
# specify certain model run
run_tag <- "run_1"

paths <- list.files(
  paste0(
    "crhm/output/",
    prjs
  ),
  pattern = run_tag,
  full.names = T
)

read_crhm_output <- function(path){
  extracted <- str_extract(path, "sparse|closed|mixed")
  crhm_output <- CRHMr::readOutputFile(
    path,
    timezone = 'Etc/GMT+6') |> 
    mutate(name = extracted)
}

crhm_output <- map_dfr(paths, read_crhm_output) |> 
  select(datetime, name, hru_t.1, m_s_veg.1, u, hru_p.1,
         delsub_veg_int.1:delunld_subl_int.1) |> 
  mutate(q_subl_veg = -delsub_veg_int.1*4)

### bin mod subl  ----

# note zeros are not included in binning, to add set inlcude.lowest = T
min_subl <- round(
  min(crhm_output$q_subl_veg, na.rm = T),3)
max_subl <- round(
  max(crhm_output$q_subl_veg, na.rm = T),3)
subl_step <- 0.05

subl_breaks <- seq(
  min_subl,
  max_subl+subl_step,
  subl_step)

subl_labs_seq <- label_bin_fn(bins = subl_breaks)

stopifnot(tail(subl_breaks, 1) > max(crhm_output$q_subl_veg, na.rm = T))
stopifnot(length(subl_labs_seq) + 1 == length(subl_breaks))

crhm_output$subl_binned <- cut(crhm_output[,'q_subl_veg', drop = TRUE], subl_breaks)

crhm_output$subl_labs <- cut(crhm_output[,'q_subl_veg', drop = TRUE], 
                             subl_breaks, 
                             labels = subl_labs_seq)

crhm_output$subl_labs <- as.numeric(as.character(crhm_output$subl_labs))

## COMPUTE AVERAGES OVER SUBL BINS ---- 

met_unld_no_melt <- 
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
                          m_s_veg.1,
                          delmelt_veg_int.1),     by = c('datetime', 'name')
  ) |>
  left_join(
    q_unld_tree_all_troughs,
    by = c('datetime', 'name')
  ) |>
  left_join(temp_binned) |> 
  left_join(ft_met) |> 
  filter(
    # name %in% scl_names,
    q_unl < 7,
    q_unl > 0,
    # q_subl_veg > 0,
    delmelt_veg_int.1 == 0
    # hru_t.1 < -6
  ) |>
  mutate(
    tree_mm = ifelse(is.na(tree_mm), m_s_veg.1, tree_mm),
    `Canopy Load (mm)` = case_when(
      tree_mm < snow_load_th ~ 'Light',
      # tree_mm >= 5 & tree_mm <= 10 ~ 'Med',
      is.na(tree_mm) ~ 'NA',
      TRUE  ~ 'Heavy'
    )) |> group_by(`Canopy Load (mm)`) |> 
  mutate(avg_w_tree = mean(tree_mm))

met_unld_no_melt |> 
  group_by(`Canopy Load (mm)`) |> 
  summarise(avg_w_tree = mean(tree_mm))

met_unld_no_melt_smry <- met_unld_no_melt |> 
  group_by(subl_labs, temp_labs, `Canopy Load (mm)`, avg_w_tree) |> 
  summarise(q_unl_avg = mean(q_unl, na.rm = T),
            q_unl_sd = sd(q_unl, na.rm = T),
            sd_low = ifelse((q_unl_avg - q_unl_sd)<0,0, q_unl_avg - q_unl_sd),
            sd_hi = q_unl_avg + q_unl_sd,
            ci_low = quantile(q_unl,0.05),
            ci_hi = quantile(q_unl, 0.95),
            sum_snow = sum(dU),
            n = n()) |> 
  filter(n >= 3,
         sum_snow > min_snow_bin,
         !is.na(`Canopy Load (mm)`),
         !is.na(avg_w_tree))

## PLOT BINS ----

### check wind subl interaction ----

# binned data 

met_unld_no_melt_smry |> 
  ggplot(aes(temp_labs, subl_labs)) + 
  geom_point()

cor(met_unld_no_melt_smry$subl_labs, met_unld_no_melt_smry$temp_labs)

# raw data 

met_unld_no_melt |> 
  ggplot(aes(t, q_subl_veg)) + 
  geom_point()

cor(met_unld_no_melt$t, met_unld_no_melt$q_subl_veg)

### check linear relationship ----

model <- lm(q_unl_avg ~ subl_labs + temp_labs, data = met_unld_no_melt_smry)
summary(model)
library(car)
vif(model)
model <- lm(q_unl_avg ~ temp_labs, data = met_unld_no_melt_smry)
summary(model)
model <- lm(q_unl_avg ~ subl_labs, data = met_unld_no_melt_smry)
summary(model)

### check non linear relationship ----

# all vars
model_nls <- nls(q_unl_avg ~ temp_labs * subl_labs * a * avg_w_tree * exp(b * subl_labs + c * temp_labs), 
                 data = met_unld_no_melt_smry, 
                 start = list(a = 0.02, b = 0.6, c = 0.01))
summary(model_nls)
RSS.p <- sum(residuals(model_nls)^2)  # Residual sum of squares
TSS <- sum((met_unld_no_melt_smry$q_unl_avg - mean(met_unld_no_melt_smry$q_unl_avg))^2)  # Total sum of squares
rsq_nls <- 1 - (RSS.p/TSS) |> round(2)  # R-squared measure
rsq_nls
modelr::rsquare(model_nls, met_unld_no_melt_smry) # check is the same as our manually defined method

# just subl
model_nls <- nls(q_unl_avg ~ subl_labs * a * avg_w_tree * exp(b * subl_labs), 
                 data = met_unld_no_melt_smry, 
                 start = list(a = 0.02, b = 0.6))
summary(model_nls)
RSS.p <- sum(residuals(model_nls)^2)  # Residual sum of squares
TSS <- sum((met_unld_no_melt_smry$q_unl_avg - mean(met_unld_no_melt_smry$q_unl_avg))^2)  # Total sum of squares
rsq_nls <- 1 - (RSS.p/TSS) |> round(2)  # R-squared measure
rsq_nls
modelr::rsquare(model_nls, met_unld_no_melt_smry) # check is the same as our manually defined method

# little difference between temp and subl and the subl only model so suggest not including temp

# just temp
model_nls <- nls(q_unl_avg ~ temp_labs * a * avg_w_tree * exp(b * temp_labs), 
                 data = met_unld_no_melt_smry, 
                 start = list(a = 0.02, b = 0.6))
summary(model_nls)
RSS.p <- sum(residuals(model_nls)^2)  # Residual sum of squares
TSS <- sum((met_unld_no_melt_smry$q_unl_avg - mean(met_unld_no_melt_smry$q_unl_avg))^2)  # Total sum of squares
rsq_nls <- 1 - (RSS.p/TSS) |> round(2)  # R-squared measure
rsq_nls
modelr::rsquare(model_nls, met_unld_no_melt_smry) # check is the same as our manually defined method

### subl vs unloading rate ----

ggplot(met_unld_no_melt_smry, 
       aes(x = subl_labs, y = q_unl_avg, colour = as.character(round(avg_w_tree)))) + 
  geom_point(size = 3) +
  ylab(bin_unl_ax_lab) +
  xlab('Sublimation Rate (mm/hr)') +
  theme_bw() +
  # theme_bw(base_size = 14) +
  theme(legend.position = 'bottom') +
  # ylim(NA, 3.1) +
  # xlim(NA, 3.5) +
  scale_color_manual(values = c("#f89540", "#0072B2","#f89540", "#0072B2")) +
  labs(color = 'Mean Canopy Load (mm)')# + facet_grid(cols = vars(name))

ggplot(met_unld_no_melt_smry, 
       aes(x = temp_labs, y = q_unl_avg, colour = as.character(round(avg_w_tree)))) + 
  geom_point(size = 3) +
  ylab(bin_unl_ax_lab) +
  xlab('Air Temperature (°C)') +
  theme_bw() +
  # theme_bw(base_size = 14) +
  theme(legend.position = 'bottom') +
  # ylim(NA, 3.1) +
  # xlim(NA, 3.5) +
  scale_color_manual(values = c("#f89540", "#0072B2","#f89540", "#0072B2")) +
  labs(color = 'Mean Canopy Load (mm)')# + facet_grid(cols = vars(name))
