# Load necessary libraries
library(MASS)
library(ggplot2)

# Load the dataset
data <- read.csv("E:/R Task/R Task 3/data_03cf4b0d-8941-4323-998c-a7ff1a83d0f2_1733812247131.csv")

# Clean the dataset
data_clean <- data[complete.cases(data[, c("x1", "x3", "x2")]), ]

# Define training data for Task 2 model
set.seed(123)
train_index <- sample(1:nrow(data_clean), 0.7 * nrow(data_clean))
train_data <- data_clean[train_index, ]

# Fit the 'selected' model: x2 ~ x1 + x3
best_model <- lm(x2 ~ x1 + x3, data=train_data)
summary_model <- summary(best_model)
print(summary_model)

# Get the two parameters with the largest absolute values
coef_values <- coef(best_model)
coef_abs <- abs(coef_values[-1]) # Ignore intercept
largest_params <- sort(coef_abs, decreasing = TRUE)[1:2]
selected_params <- names(largest_params)
cat("Selected parameters for ABC:", selected_params, "\n")

# Fix other parameters (set them to estimated values)
fixed_params <- coef_values
theta_rest <- setdiff(names(coef_values), selected_params)

# Set Uniform prior for the selected parameters
prior_range <- 1.0 * abs(coef_values[selected_params]) # Range is ±100%
prior_lower <- coef_values[selected_params] - prior_range
prior_upper <- coef_values[selected_params] + prior_range

cat("Prior ranges for selected parameters:\n")
print(data.frame(Parameter=selected_params, Lower=prior_lower, Upper=prior_upper))

# ---- Rejection ABC ----
set.seed(123)
N <- 10000 # Number of prior samples
epsilon <- 5.0 # Tolerance for rejection (relaxed)

# Simulated parameters and acceptance storage
accepted_params <- matrix(NA, ncol=2, nrow=N)
colnames(accepted_params) <- selected_params

# Generate samples and accept/reject based on discrepancy
observed_y <- train_data$x2
x1 <- train_data$x1
x3 <- train_data$x3

# Contribution from fixed parameters
fixed_contribution <- fixed_params["(Intercept)"]

count <- 0
for (i in 1:N) {
  # Sample from the Uniform prior
  theta1 <- runif(1, prior_lower[1], prior_upper[1])
  theta2 <- runif(1, prior_lower[2], prior_upper[2])
  
  # Compute simulated y using the regression equation
  y_sim <- theta1 * x1 + theta2 * x3 + fixed_contribution
  
  # Compute discrepancy (mean squared error)
  discrepancy <- mean((observed_y - y_sim)^2)
  
  # Accept the sample if discrepancy is within tolerance
  if (discrepancy < epsilon) {
    count <- count + 1
    accepted_params[count, ] <- c(theta1, theta2)
  }
}

# Remove empty rows
accepted_params <- na.omit(accepted_params)

# Check if any samples were accepted
if (nrow(accepted_params) == 0) {
  stop("No samples were accepted. Try increasing epsilon or widening the prior range.")
}
cat("Number of accepted samples:", nrow(accepted_params), "\n")

# ---- Plot Results ----
# Joint Posterior Distribution
plot(accepted_params[,1], accepted_params[,2], 
     col="blue", pch=16, xlab=selected_params[1], ylab=selected_params[2],
     main="Joint Posterior Distribution (Rejection ABC)")
grid()

# Marginal Distributions
par(mfrow=c(1,2)) # Split the window for two plots

hist(accepted_params[,1], col="skyblue", main=paste("Posterior of", selected_params[1]),
     xlab=selected_params[1], freq=FALSE)
hist(accepted_params[,2], col="lightgreen", main=paste("Posterior of", selected_params[2]),
     xlab=selected_params[2], freq=FALSE)

par(mfrow=c(1,1)) # Reset plotting window

# ---- Explanation ----
cat("Explanation of Results:\n")
cat("1. The rejection ABC method uses a Uniform prior centered around the estimated coefficients from Task 2.\n")
cat("2. The discrepancy metric is the mean squared error (MSE) between the observed y values and simulated y values.\n")
cat("3. Samples with discrepancies less than the tolerance (epsilon) are accepted, forming the posterior distribution.\n")
cat("4. The joint and marginal distributions show the range of plausible values for the two parameters.\n")
