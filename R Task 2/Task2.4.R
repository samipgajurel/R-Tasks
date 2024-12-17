# Load necessary libraries
library(MASS)

# Function to calculate log-likelihood from Residual Sum of Squares (RSS)
calculate_log_likelihood <- function(RSS, n) {
  return(-n / 2 * (log(2 * pi) + log(RSS / n) + 1))
}

# Function to calculate AIC
calculate_AIC <- function(RSS, n, k) {
  log_likelihood <- calculate_log_likelihood(RSS, n)
  return(2 * k - 2 * log_likelihood)
}

# Function to calculate BIC
calculate_BIC <- function(RSS, n, k) {
  log_likelihood <- calculate_log_likelihood(RSS, n)
  return(k * log(n) - 2 * log_likelihood)
}

# Load dataset
data <- read.csv("E:/R Task/R Task 2/data_03cf4b0d-8941-4323-998c-a7ff1a83d0f2_1733812247131.csv")

# Define output (y) and input variables (x1, x3, x4, x5)
y <- data$x2
n <- length(y) # Number of observations

# Fit models
model_1 <- lm(y ~ x1 + x3, data = data)
model_2 <- lm(y ~ I(x1^2) + I(x3^2), data = data)
model_3 <- lm(y ~ I(x1^3) + I(x3^3), data = data)
model_4 <- lm(y ~ x1 + x3 + I(x4^2), data = data)
model_5 <- lm(y ~ x1 + x3 + I(x4^2) + I(x5^3), data = data)

# Calculate RSS for each model
RSS_1 <- sum(residuals(model_1)^2)
RSS_2 <- sum(residuals(model_2)^2)
RSS_3 <- sum(residuals(model_3)^2)
RSS_4 <- sum(residuals(model_4)^2)
RSS_5 <- sum(residuals(model_5)^2)

# Number of parameters (including intercept)
k_1 <- length(coef(model_1))
k_2 <- length(coef(model_2))
k_3 <- length(coef(model_3))
k_4 <- length(coef(model_4))
k_5 <- length(coef(model_5))

# Compute AIC and BIC for each model
AIC_1 <- calculate_AIC(RSS_1, n, k_1)
BIC_1 <- calculate_BIC(RSS_1, n, k_1)

AIC_2 <- calculate_AIC(RSS_2, n, k_2)
BIC_2 <- calculate_BIC(RSS_2, n, k_2)

AIC_3 <- calculate_AIC(RSS_3, n, k_3)
BIC_3 <- calculate_BIC(RSS_3, n, k_3)

AIC_4 <- calculate_AIC(RSS_4, n, k_4)
BIC_4 <- calculate_BIC(RSS_4, n, k_4)

AIC_5 <- calculate_AIC(RSS_5, n, k_5)
BIC_5 <- calculate_BIC(RSS_5, n, k_5)

# Combine results into a table
results_table <- data.frame(
  Model = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5"),
  Num_Parameters = c(k_1, k_2, k_3, k_4, k_5),
  AIC = c(AIC_1, AIC_2, AIC_3, AIC_4, AIC_5),
  BIC = c(BIC_1, BIC_2, BIC_3, BIC_4, BIC_5)
)

# Print the results
print("AIC and BIC Calculations:")
print(results_table)

