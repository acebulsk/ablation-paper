# Sample data
set.seed(123)  # for reproducibility
x <- 1:100
z <- sample(c(4, 10), 100, replace = TRUE)
a <- 2.5
b <- 0.1
c <- .2
# Simulate some y data with noise
y <- a * exp(b * x) * exp(c * z) + rnorm(100, mean = 0, sd = 0.5)

# Combine into a data frame
data <- data.frame(x = x, y = y, z = z)

# Plot the data
plot(data$x, data$y, main = "Simulated Data", xlab = "x", ylab = "y", col = ifelse(data$z == 4, "blue", "green"))

# Define the exponential model using nls
nls_model <- nls(y ~ a * exp(b * x) * exp(c * z), 
                 start = list(a = 2.5, b = 0.1, c = 0.05), 
                 data = data,
                 control = nls.control(maxiter = 200))

# Summary of the model
summary(nls_model)

# Predicted values for the observed data
predicted_y <- predict(nls_model, data)

# Plot the fitted model for observed data
points(data$x, predicted_y, col = ifelse(data$z == 4, "blue", "green"), pch = 20)

# Add legend to the plot
legend("topright", legend = c("Observed (z=4)", "Observed (z=10)", "Fitted (z=4)", "Fitted (z=10)"), 
       col = c("blue", "green", "blue", "green"), pch = c(1, 1, 20, 20), lty = c(NA, NA, 1, 1))

# Now make predictions for any z value from 0 to inf (e.g., z = seq(0, 20, by = 1))
new_z <- seq(0, 20, by = 1)
new_data <- expand.grid(x = unique(data$x), z = new_z)
new_predicted_y <- predict(nls_model, newdata = new_data)

# Plot the predictions for new z values
library(ggplot2)
ggplot(data = new_data, aes(x = x, y = new_predicted_y, color = factor(z))) +
  geom_line() +
  labs(title = "Predicted values for new z values", x = "x", y = "Predicted y") +
  scale_color_discrete(name = "z value")
