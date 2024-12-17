# Load necessary libraries
library(MASS)

# Load the dataset (make sure the file path is correct)
data <- read.csv("E:/R Task/R Task 2/data_03cf4b0d-8941-4323-998c-a7ff1a83d0f2_1733812247131.csv")

# Check if data has been loaded correctly
head(data)

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
model_1_summary <- summary(model_1)
model_2_summary <- summary(model_2)
model_3_summary <- summary(model_3)
model_4_summary <- summary(model_4)
model_5_summary <- summary(model_5)

# Display summaries
print(model_1_summary)
print(model_2_summary)
print(model_3_summary)
print(model_4_summary)
print(model_5_summary)

# Compute AIC and BIC for each model
AIC_1 <- AIC(model_1)
AIC_2 <- AIC(model_2)
AIC_3 <- AIC(model_3)
AIC_4 <- AIC(model_4)
AIC_5 <- AIC(model_5)

BIC_1 <- BIC(model_1)
BIC_2 <- BIC(model_2)
BIC_3 <- BIC(model_3)
BIC_4 <- BIC(model_4)
BIC_5 <- BIC(model_5)

# Output AIC and BIC values
model_eval <- list(
  Model_1_AIC = AIC_1, Model_1_BIC = BIC_1,
  Model_2_AIC = AIC_2, Model_2_BIC = BIC_2,
  Model_3_AIC = AIC_3, Model_3_BIC = BIC_3,
  Model_4_AIC = AIC_4, Model_4_BIC = BIC_4,
  Model_5_AIC = AIC_5, Model_5_BIC = BIC_5
)

# Print evaluation results
print(model_eval)
