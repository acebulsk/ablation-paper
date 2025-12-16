# Script to check correlation between indepenedent variables 

p_mat_th <- 0.7 # threshold  to bold cor vals

# agg met data over time intervals -----
# helps check for spurious correlations 
t_int <- '3 hour'

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

met_agg <- left_join(ft_met, q_unld_tree |> select(datetime, tree_mm)) |> 
  left_join(crhm_output_w_tree |> select(datetime, m_s_veg.1, delsub_veg_int.1, delmelt_veg_int.1)) |> 
  mutate(datetime = ceiling_date(datetime, unit = t_int)) |> # ceiling ensures the timestamp corresponds to preeceeding records
  group_by(datetime) |>
  summarise(t = mean(t),
            u = mean(u),
            tau = mean(tau),
            t_ice_bulb = mean(t_ice_bulb),
            t_ice_dep = t - t_ice_bulb,
            dmelt_veg = sum(delmelt_veg_int.1),
            dsub_veg = sum(delsub_veg_int.1),
            q_subl = -dsub_veg / hours,
            q_melt = dmelt_veg / hours,
            m_s_veg.1 = mean(m_s_veg.1, na.rm = T),
            tree_mm = mean(tree_mm, na.rm = T),
            q_melt_diml = q_melt / tree_mm
          ) |> 
  ungroup()

# dry snow unloading periods (no melt) -----
dry_snow_met <- met_agg |> filter(q_melt == 0)

predictors <- dry_snow_met[, c("tree_mm", "tau", "u", "t", "t_ice_dep", "q_subl")] # moderate linear correlation between tau and subl
colnames(predictors) <- c(
  "Snow Load", 
  "Shear Stress", 
  "Wind Speed", 
  "Air Temp", 
  "Ice-bulb Dep.", 
  "Sublimation"
)

library(corrplot)

# Pearson
cor_pear <- cor(
  predictors,
  method = "pearson",
  use = "pairwise.complete.obs"
)

# Spearman
cor_spear <- cor(
  predictors,
  method = "spearman",
  use = "pairwise.complete.obs"
)

# Combined matrix: upper = Pearson, lower = Spearman
cor_combined <- cor_spear
cor_combined[upper.tri(cor_combined)] <- cor_pear[upper.tri(cor_pear)]

# Plot
png(
  "figs/final/figureS3.png",
  width = 8,          # in inches
  height = 7,         # in inches
  units = "in",       # important!
  res = 300           # high resolution
)

# png(
#   paste0("figs/results/corplot_dry_snow_", hours, ".png"),
#   width = 8,          # in inches
#   height = 7,         # in inches
#   units = "in",       # important!
#   res = 300           # high resolution
# )

par(mar = c(2, 2, 4, 2))       # inner plot margins
par(oma = c(0, 0, 3, 0))       # outer margins for title

corrplot(
  cor_combined,
  method = "color",
  tl.cex = 0.8,
  tl.col = "black",          # <- variable names in black
  addCoef.col = "black",
  number.digits = 2,
  number.font = ifelse(cor_combined >= p_mat_th | cor_combined <= -p_mat_th, 2, 1) # bold >0.5
)

# Add title in outer margin
mtext(
  "Non-melt periods: Pearson (upper) and Spearman (lower) correlations among predictors",
  side = 3,      # top
  line = 1,      # distance into outer margin
  outer = TRUE,  # use outer margin
  cex = 1
)


dev.off()

# melt periods -----

melt_met <- met_agg |> filter(q_melt != 0)

melt_met$q_melt_diml <- ifelse(melt_met$q_melt_diml > 5, NA, melt_met$q_melt_diml)

predictors <- melt_met[, c("tree_mm", "tau", "u", "t", "t_ice_bulb", "q_melt")] # moderate linear correlation between tau and subl
colnames(predictors) <- c(
  "Snow Load", 
  "Shear Stress", 
  "Wind Speed", 
  "Air Temp", 
  "Ice-bulb Temp.", 
  "Snowmelt"
)

# Pearson
cor_pear <- cor(
  predictors,
  method = "pearson",
  use = "pairwise.complete.obs"
)

# Spearman
cor_spear <- cor(
  predictors,
  method = "spearman",
  use = "pairwise.complete.obs"
)

# Combined matrix: upper = Pearson, lower = Spearman
cor_combined <- cor_spear
cor_combined[upper.tri(cor_combined)] <- cor_pear[upper.tri(cor_pear)]
p.mat <- abs(cor_combined) <= p_mat_th

# Plot
png(
  "figs/final/figureS4.png",
  width = 8,          # in inches
  height = 7,         # in inches
  units = "in",       # important!
  res = 300           # high resolution
)

# png(
#   paste0("figs/results/corplot_melt_", hours, ".png"),
#   width = 8,          # in inches
#   height = 7,         # in inches
#   units = "in",       # important!
#   res = 300           # high resolution
# )

par(mar = c(2, 2, 4, 2))       # inner plot margins
par(oma = c(0, 0, 3, 0))       # outer margins for title

corrplot(
  cor_combined,
  method = "color",
  tl.cex = 0.8,
  tl.col = "black",          # <- variable names in black
  addCoef.col = "black",
  number.digits = 2,
  number.font = ifelse(cor_combined >= p_mat_th | cor_combined <= -p_mat_th, 2, 1) # bold >0.5
)

# Add title in outer margin
mtext(
  "Melt periods: Pearson (upper) and Spearman (lower) correlations among predictors",
  side = 3,      # top
  line = 1,      # distance into outer margin
  outer = TRUE,  # use outer margin
  cex = 1
)
dev.off()
