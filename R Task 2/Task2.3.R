# Load necessary libraries
library(MASS)

# Load dataset
data_path <- "E:/R Task/R Task 2/data_03cf4b0d-8941-4323-998c-a7ff1a83d0f2_1733812247131.csv"
data <- read.csv(data_path)

# Define input and output signals
X <- data[, c("x1", "x3", "x4", "x5")]
y <- data$x2

# Define the models
# Model 1: y = θ0 + θ1*x1 + θ2*x3 + θbias
model_1 <- lm(y ~ x1 + x3, data=data)

# Model 2: y = θ0 + θ1*x1^2 + θ2*x3^2 + θbias
model_2 <- lm(y ~ I(x1^2) + I(x3^2), data=data)

# Model 3: y = θ0 + θ1*x1^3 + θ2*x3^3 + θbias
model_3 <- lm(y ~ I(x1^3) + I(x3^3), data=data)

# Model 4: y = θ0 + θ1*x1 + θ2*x3 + θ3*x4^2 + θbias
model_4 <- lm(y ~ x1 + x3 + I(x4^2), data=data)

# Model 5: y = θ0 + θ1*x1 + θ2*x3 + θ3*x4^2 + θ4*x5^3 + θbias
model_5 <- lm(y ~ x1 + x3 + I(x4^2) + I(x5^3), data=data)

# Residual Sum of Squares (RSS) for each model
RSS_1 <- sum(residuals(model_1)^2)
RSS_2 <- sum(residuals(model_2)^2)
RSS_3 <- sum(residuals(model_3)^2)
RSS_4 <- sum(residuals(model_4)^2)
RSS_5 <- sum(residuals(model_5)^2)

# Variance of residuals for each model
n <- length(y)  # Number of data points
p_1 <- length(coef(model_1))  # Number of parameters in model 1
p_2 <- length(coef(model_2))
p_3 <- length(coef(model_3))
p_4 <- length(coef(model_4))
p_5 <- length(coef(model_5))

variance_1 <- RSS_1 / (n - p_1)
variance_2 <- RSS_2 / (n - p_2)
variance_3 <- RSS_3 / (n - p_3)
variance_4 <- RSS_4 / (n - p_4)
variance_5 <- RSS_5 / (n - p_5)

# Log-likelihood function
log_likelihood <- function(RSS, n) {
  return(-n / 2 * log(2 * pi) - n / 2 * log(RSS / n) - n / 2)
}

# Compute log-likelihood for each model
log_likelihood_1 <- log_likelihood(RSS_1, n)
log_likelihood_2 <- log_likelihood(RSS_2, n)
log_likelihood_3 <- log_likelihood(RSS_3, n)
log_likelihood_4 <- log_likelihood(RSS_4, n)
log_likelihood_5 <- log_likelihood(RSS_5, n)

# Create a table with results
results_table <- data.frame(
  Model = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5"),
  Variance = c(variance_1, variance_2, variance_3, variance_4, variance_5),
  Log_Likelihood = c(log_likelihood_1, log_likelihood_2, log_likelihood_3, log_likelihood_4, log_likelihood_5)
)

# Print the results table
print("Log-Likelihood and Variance Results Table:")
print(results_table)

# Optional: Save the table as a CSV
write.csv(results_table, "Model_Results_Table.csv", row.names = FALSE)

