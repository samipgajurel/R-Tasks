# Load necessary libraries
library(ggplot2)

# Assuming the dataset is loaded into the 'data' variable with columns x1, x2 (output), x3, x4, x5

# Step 1: Define input (X) and output (y) variables
X <- data[, c("x1", "x3", "x4", "x5")]
y <- data$x2  # Output variable

# Create a PDF to save all plots
pdf("Model_Plots.pdf", width = 12, height = 8)

# Model 1: Linear Model
theta_hat_1 <- lm(y ~ x1 + x3 + x4 + x5, data = data)
theta_hat_1_coeffs <- coef(theta_hat_1)  # Estimated coefficients
y_hat_1 <- predict(theta_hat_1)  # Predicted values for Model 1

# Print the theta_hat (coefficients)
cat("Model 1 (Linear) - Theta Hat:\n")
print(theta_hat_1_coeffs)

# Model 2: Polynomial Model (Second-degree polynomial)
theta_hat_2 <- lm(y ~ poly(x1, 2) + poly(x3, 2) + poly(x4, 2) + poly(x5, 2), data = data)
theta_hat_2_coeffs <- coef(theta_hat_2)  # Estimated coefficients
y_hat_2 <- predict(theta_hat_2)  # Predicted values for Model 2

# Print the theta_hat (coefficients)
cat("Model 2 (Polynomial) - Theta Hat:\n")
print(theta_hat_2_coeffs)

# Model 3: Interaction Model
theta_hat_3 <- lm(y ~ x1 * x3 + x1 * x4 + x1 * x5 + x3 * x4 + x3 * x5 + x4 * x5, data = data)
theta_hat_3_coeffs <- coef(theta_hat_3)  # Estimated coefficients
y_hat_3 <- predict(theta_hat_3)  # Predicted values for Model 3

# Print the theta_hat (coefficients)
cat("Model 3 (Interaction) - Theta Hat:\n")
print(theta_hat_3_coeffs)

# Model 4: Logarithmic Model
theta_hat_4 <- lm(log(y) ~ x1 + x3 + x4 + x5, data = data)
theta_hat_4_coeffs <- coef(theta_hat_4)  # Estimated coefficients
y_hat_4 <- exp(predict(theta_hat_4))  # Predicted values for Model 4 (exponentiated back)

# Print the theta_hat (coefficients)
cat("Model 4 (Logarithmic) - Theta Hat:\n")
print(theta_hat_4_coeffs)

# Model 5: Exponential Model (using log transformation for linearization)
log_y <- log(y)
theta_hat_5_linearized <- lm(log_y ~ x1 + x3 + x4 + x5, data = data)
theta_hat_5_coeffs <- coef(theta_hat_5_linearized)  # Estimated coefficients
y_hat_5_linearized <- exp(predict(theta_hat_5_linearized))  # Predicted values for Model 5

# Print the theta_hat (coefficients)
cat("Model 5 (Exponential) - Theta Hat:\n")
print(theta_hat_5_coeffs)

# Plotting Predicted vs Actual for each model
par(mfrow=c(3, 2))  # Set up for multiple plots in a grid

# Predicted vs Actual for Model 1 (Linear)
plot(y, y_hat_1, main = "Model 1: Predicted vs Actual (Linear)", xlab = "Actual", ylab = "Predicted")
abline(a = 0, b = 1, col = "red")

# Predicted vs Actual for Model 2 (Polynomial)
plot(y, y_hat_2, main = "Model 2: Predicted vs Actual (Polynomial)", xlab = "Actual", ylab = "Predicted")
abline(a = 0, b = 1, col = "red")

# Predicted vs Actual for Model 3 (Interaction)
plot(y, y_hat_3, main = "Model 3: Predicted vs Actual (Interaction)", xlab = "Actual", ylab = "Predicted")
abline(a = 0, b = 1, col = "red")

# Predicted vs Actual for Model 4 (Logarithmic)
plot(y, y_hat_4, main = "Model 4: Predicted vs Actual (Log)", xlab = "Actual", ylab = "Predicted")
abline(a = 0, b = 1, col = "red")

# Predicted vs Actual for Model 5 (Exponential)
plot(y, y_hat_5_linearized, main = "Model 5: Predicted vs Actual (Exponential)", xlab = "Actual", ylab = "Predicted")
abline(a = 0, b = 1, col = "red")

# Close the PDF device to save the plot
dev.off()

