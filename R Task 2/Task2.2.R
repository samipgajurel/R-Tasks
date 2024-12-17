# Load necessary libraries
library(MASS)

# Assuming the dataset is loaded into 'data' 
# data <- read.csv(""E:/R Task/R Task 2/data_03cf4b0d-8941-4323-998c-a7ff1a83d0f2_1733812247131.csv"")

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

# Summary of each model
summary(model_1)
summary(model_2)
summary(model_3)
summary(model_4)
summary(model_5)

# Residual Sum of Squares (RSS) for each model
RSS_1 <- sum(residuals(model_1)^2)
RSS_2 <- sum(residuals(model_2)^2)
RSS_3 <- sum(residuals(model_3)^2)
RSS_4 <- sum(residuals(model_4)^2)
RSS_5 <- sum(residuals(model_5)^2)

# Output RSS
list(
  Model_1_RSS = RSS_1,
  Model_2_RSS = RSS_2,
  Model_3_RSS = RSS_3,
  Model_4_RSS = RSS_4,
  Model_5_RSS = RSS_5
)

# Log-likelihood function
log_likelihood <- function(RSS, n) {
  return(-n / 2 * log(2 * pi) - n / 2 * log(RSS / n) - n / 2)
}

n <- length(y)  # Number of data points

# Compute log-likelihood for each model
log_likelihood_1 <- log_likelihood(RSS_1, n)
log_likelihood_2 <- log_likelihood(RSS_2, n)
log_likelihood_3 <- log_likelihood(RSS_3, n)
log_likelihood_4 <- log_likelihood(RSS_4, n)
log_likelihood_5 <- log_likelihood(RSS_5, n)

# Output log-likelihood values
list(
  Model_1_Log_Likelihood = log_likelihood_1,
  Model_2_Log_Likelihood = log_likelihood_2,
  Model_3_Log_Likelihood = log_likelihood_3,
  Model_4_Log_Likelihood = log_likelihood_4,
  Model_5_Log_Likelihood = log_likelihood_5
)
