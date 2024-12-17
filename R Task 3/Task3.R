# Load necessary libraries
library(ggplot2)
library(MASS)

# Load dataset
data <- read.csv("E:/R Task/R Task 3/data_03cf4b0d-8941-4323-998c-a7ff1a83d0f2_1733812247131.csv")

# Ensure numeric data
data <- data.frame(lapply(data, function(col) if (is.factor(col) || is.character(col)) as.numeric(as.character(col)) else col))

# Split data into 70% training and 30% testing
set.seed(123)
train_indices <- sample(1:nrow(data), size = 0.7 * nrow(data))
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

# Define the selected 'best' model
best_model <- lm(x2 ~ I(x4) + I(x1^3) + I(x3^4), data = train_data)

# Get the model coefficients and their absolute values
model_coeffs <- summary(best_model)$coefficients[, 1]  # Estimated coefficients
abs_model_coeffs <- abs(model_coeffs)

# Identify the two parameters with the largest absolute values
sorted_coeffs <- sort(abs_model_coeffs, decreasing = TRUE)
top_two_params <- names(sorted_coeffs)[1:2]

# Get the values of those parameters from the model
param_values <- model_coeffs[top_two_params]

# Define prior ranges based on model coefficients
param_ranges <- data.frame(
  param = top_two_params,
  lower = param_values - 0.5 * abs(param_values),
  upper = param_values + 0.5 * abs(param_values)
)

# Rejection ABC
n_samples <- 1000
accepted_samples <- data.frame(param1 = numeric(), param2 = numeric())
tolerance <- 10000  # Increased tolerance for rejection ABC

for (i in 1:n_samples) {
  # Sample from uniform priors
  param1_sample <- runif(1, param_ranges$lower[1], param_ranges$upper[1])
  param2_sample <- runif(1, param_ranges$lower[2], param_ranges$upper[2])
  
  # Fix all other parameters in the model
  fixed_params <- model_coeffs
  fixed_params[top_two_params] <- c(param1_sample, param2_sample)
  
  # Calculate predicted values using sampled parameters
  y_pred <- fixed_params[1] * test_data$x4 + fixed_params[2] * (test_data$x1^3) + fixed_params[3] * (test_data$x3^4)
  
  # Compute residuals and SSE
  residuals <- test_data$x2 - y_pred
  sse <- sum(residuals^2)
  
  # Rejection step
  if (sse < tolerance) {
    accepted_samples <- rbind(accepted_samples, data.frame(param1 = param1_sample, param2 = param2_sample))
  }
}

# Save plots in a PDF
pdf("ABC_Posteriors.pdf", width = 10, height = 8)

# Joint Posterior Distribution
ggplot(accepted_samples, aes(x = param1, y = param2)) +
  geom_point(alpha = 0.5, color = "blue") +
  labs(title = "Joint Posterior Distribution of the Two Parameters", x = "Parameter 1", y = "Parameter 2") +
  theme_minimal()

# Marginal Posterior for Parameter 1
ggplot(accepted_samples, aes(x = param1)) +
  geom_histogram(bins = 30, fill = "blue", alpha = 0.5) +
  labs(title = "Marginal Posterior Distribution of Parameter 1", x = "Parameter 1", y = "Frequency") +
  theme_minimal()

# Marginal Posterior for Parameter 2
ggplot(accepted_samples, aes(x = param2)) +
  geom_histogram(bins = 30, fill = "red", alpha = 0.5) +
  labs(title = "Marginal Posterior Distribution of Parameter 2", x = "Parameter 2", y = "Frequency") +
  theme_minimal()

dev.off()

