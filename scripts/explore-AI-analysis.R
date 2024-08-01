# Try analysis steps suggested by AI

# Load necessary libraries
library(tidyverse)
library(corrplot)

# Tidy Data

met_unld_vars <- met_unld |>
  ungroup() |> 
  filter(name %in% scl_names,
         q_unl < 7,
         q_unl > min_qunld,
         # t < -6,
        tree_mm >= min_canopy_snow
         ) |> 
  select(t, u, tree_mm, time_elapsed_event, q_unl)
 

# Create scatter plots

pairs(met_unld_vars)

# Create correlation matrix
cor_matrix <- cor(met_unld_vars)
corrplot(cor_matrix, method = "circle")


# Compare Linear models

model4 <- lm(q_unl ~ t + u + tree_mm, data = met_unld_vars)
summary(model4)
model3 <- lm(q_unl ~ t + u, data = met_unld_vars)
summary(model3)
model2 <- lm(q_unl ~ t, data = met_unld_vars)
summary(model2)
model1 <- lm(q_unl ~ u, data = met_unld_vars)
summary(model1)

anova(model1, model3)
anova(model2, model3)
anova(model3, model4)

AIC(model1, model2, model3, model4)
BIC(model1, model2, model3, model4)

# more complex model is better, now lets test some interactions
model5 <- lm(q_unl ~ t * u + tree_mm, data = met_unld_vars)
summary(model5)

anova(model4, model5)
AIC(model4, model5)
BIC(model4, model5)

model6 <- lm(q_unl ~ t * u * tree_mm, data = met_unld_vars)
summary(model6)

anova(model5, model6)
AIC(model5, model6)
BIC(model5, model6)

# Non-linear models

nls_model1 <- nls(q_unl ~ a * exp(b * t) + c * exp(d * u) + e * tree_mm,
                  data = met_unld_vars,
                  start = list(a = 1, b = 0.1, c = 1, d = 1, e = 1))
summary(nls_model1)

nls_model_w_interaction <- nls(q_unl ~ a * tree_mm * exp(b * t) + c * exp(d * u) + e * t * u,
                  data = met_unld_vars,
                  start = list(a = 1, b = 0.1, c = 1, d = 1, e = 1))
summary(nls_model_w_interaction)

anova(nls_model1, nls_model_w_interaction)
AIC(nls_model1, nls_model_w_interaction)
BIC(nls_model1, nls_model_w_interaction)

# Visualize the best model (assuming model4 is best, adjust as needed)
ggplot(met_unld_vars, aes(x = t, y = q_unl)) +
  geom_point() +
  geom_line(aes(y = predict(nls_model_w_interaction)), color = "red") +
  theme_minimal() +
  labs(title = "Non-linear Model Fit", x = "Temperature", y = "Unloading")

ggplot(met_unld_vars, aes(x = u, y = q_unl)) +
  geom_point() +
  geom_line(aes(y = predict(nls_model_w_interaction)), color = "red") +
  theme_minimal() +
  labs(title = "Non-linear Model Fit", x = "Wind Speed", y = "Unloading")

ggplot(met_unld_vars, aes(x = tree_mm, y = q_unl)) +
  geom_point() +
  geom_line(aes(y = predict(nls_model_w_interaction)), color = "red") +
  theme_minimal() +
  labs(title = "Non-linear Model Fit", x = "Canopy Snow Load (mm)", y = "Unloading")
