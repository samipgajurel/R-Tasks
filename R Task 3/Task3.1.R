# Load necessary libraries
library(ggplot2)

# Load the dataset
data <- read.csv("E:/R Task/R Task 3/data_03cf4b0d-8941-4323-998c-a7ff1a83d0f2_1733812247131.csv")

# Ensure numeric data
data <- data.frame(lapply(data, function(col) if(is.factor(col) || is.character(col)) as.numeric(as.character(col)) else col))

# Split data into 70% training and 30% testing
set.seed(123)
train_indices <- sample(1:nrow(data), size = 0.7 * nrow(data))
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

# Define the selected 'best' model
best_model <- lm(x2 ~ x4 + x1 + x3, data = train_data)

# Get the model coefficients and their absolute values
model_coeffs <- summary(best_model)$coefficients[, 1]  # Estimated coefficients
abs_model_coeffs <- abs(model_coeffs)

# Identify the two parameters with the largest absolute values
sorted_coeffs <- sort(abs_model_coeffs, decreasing = TRUE)
top_two_params <- names(sorted_coeffs)[1:2]

# Get the values of those parameters from the model
param_values <- model_coeffs[top_two_params]
param_ranges <- data.frame(
  param = top_two_params,
  lower = param_values - 0.1 * abs(param_values),  # Define range for uniform prior
  upper = param_values + 0.1 * abs(param_values)   # 10% of the coefficient value for prior range
)

# Rejection ABC
n_samples <- 5000
accepted_samples <- data.frame(theta_bias = numeric(), theta_one = numeric())
tolerance <- 500  # Adjusted tolerance

for (i in 1:n_samples) {
  # Sample from uniform priors
  theta_bias_sample <- runif(1, param_ranges$lower[1], param_ranges$upper[1])
  theta_one_sample <- runif(1, param_ranges$lower[2], param_ranges$upper[2])
  
  # Fix all other parameters in the model (based on the estimated values)
  fixed_params <- model_coeffs
  fixed_params[top_two_params] <- c(theta_bias_sample, theta_one_sample)
  
  # Calculate predicted values from the model using sampled parameters
  y_pred <- fixed_params[1] + fixed_params[2] * test_data$x4 +
    fixed_params[3] * test_data$x1 + fixed_params[4] * test_data$x3
  
  # Compute residuals (difference between predicted and actual values)
  residuals <- test_data$x2 - y_pred
  sse <- sum(residuals^2)  # Sum of squared errors
  
  # Rejection step: Accept if SSE is within tolerance
  if (sse < tolerance) {
    accepted_samples <- rbind(accepted_samples, data.frame(theta_bias = theta_bias_sample, theta_one = theta_one_sample))
  }
}

# Ensure the accepted samples are not empty
if (nrow(accepted_samples) == 0) {
  stop("No samples accepted. Try adjusting the tolerance or prior ranges.")
}

# Save plots to PDF
pdf("Posterior_Plots.pdf", width = 10, height = 8)

# Joint Posterior Distribution (Scatter Plot)
ggplot(accepted_samples, aes(x = theta_bias, y = theta_one)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +  # Add regression line
  labs(
    title = "Joint Posterior Distribution and Correlation",
    x = "Theta Bias",
    y = "Theta One"
  ) +
  theme_minimal()

# Marginal Posterior Distribution for Theta Bias
ggplot(accepted_samples, aes(x = theta_bias)) +
  geom_histogram(bins = 30, fill = "blue", alpha = 0.5) +
  labs(
    title = "Marginal Posterior Distribution of Theta Bias",
    x = "Theta Bias",
    y = "Frequency"
  ) +
  theme_minimal()

# Marginal Posterior Distribution for Theta One
ggplot(accepted_samples, aes(x = theta_one)) +
  geom_histogram(bins = 30, fill = "red", alpha = 0.5) +
  labs(
    title = "Marginal Posterior Distribution of Theta One",
    x = "Theta One",
    y = "Frequency"
  ) +
  theme_minimal()

# Annotated Joint Posterior Distribution with Correlation
ggplot(accepted_samples, aes(x = theta_bias, y = theta_one)) +
  geom_point(alpha = 0.5, color = "black") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +  # Add regression line
  labs(
    title = "Annotated Joint Posterior Distribution",
    x = "Theta Bias",
    y = "Theta One"
  ) +
  annotate("text", x = mean(accepted_samples$theta_bias), y = max(accepted_samples$theta_one),
           label = paste("Correlation:", round(correlation, 4)), hjust = 1, color = "black") +
  theme_minimal()

# Close the PDF device
dev.off()


