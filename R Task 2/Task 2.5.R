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


# Compute AIC and BIC for each model (already computed previously)
model_eval <- list(
  Model_1_AIC = AIC_1, Model_1_BIC = BIC_1,
  Model_2_AIC = AIC_2, Model_2_BIC = BIC_2,
  Model_3_AIC = AIC_3, Model_3_BIC = BIC_3,
  Model_4_AIC = AIC_4, Model_4_BIC = BIC_4,
  Model_5_AIC = AIC_5, Model_5_BIC = BIC_5
)

# Identify the model with the lowest AIC and BIC
best_model_by_AIC <- which.min(c(AIC_1, AIC_2, AIC_3, AIC_4, AIC_5))
best_model_by_BIC <- which.min(c(BIC_1, BIC_2, BIC_3, BIC_4, BIC_5))

best_model_by_AIC
best_model_by_BIC

# Visualize residuals for each model
# Residuals from Model 1
residuals_1 <- model_1$residuals
residuals_2 <- model_2$residuals
residuals_3 <- model_3$residuals
residuals_4 <- model_4$residuals
residuals_5 <- model_5$residuals

# PDF to save all plots
pdf("Task_2_Residuals_Plots.pdf", width = 12, height = 8)

# Plot residuals for Model 1
par(mfrow=c(2, 3))  # Set up 2x3 grid for plots
hist(residuals_1, col='blue', main='Residuals of Model 1', xlab='Residuals', ylab='Frequency')
qqnorm(residuals_1, main='Q-Q Plot of Residuals for Model 1')
qqline(residuals_1, col='red')

# Plot residuals for Model 2
hist(residuals_2, col='green', main='Residuals of Model 2', xlab='Residuals', ylab='Frequency')
qqnorm(residuals_2, main='Q-Q Plot of Residuals for Model 2')
qqline(residuals_2, col='red')

# Plot residuals for Model 3
hist(residuals_3, col='purple', main='Residuals of Model 3', xlab='Residuals', ylab='Frequency')
qqnorm(residuals_3, main='Q-Q Plot of Residuals for Model 3')
qqline(residuals_3, col='red')

# Plot residuals for Model 4
hist(residuals_4, col='orange', main='Residuals of Model 4', xlab='Residuals', ylab='Frequency')
qqnorm(residuals_4, main='Q-Q Plot of Residuals for Model 4')
qqline(residuals_4, col='red')

# Plot residuals for Model 5
hist(residuals_5, col='brown', main='Residuals of Model 5', xlab='Residuals', ylab='Frequency')
qqnorm(residuals_5, main='Q-Q Plot of Residuals for Model 5')
qqline(residuals_5, col='red')

# Close the PDF device
dev.off()

# Based on AIC and BIC, print the best model
best_model_by_AIC_name <- c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5")[best_model_by_AIC]
best_model_by_BIC_name <- c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5")[best_model_by_BIC]

best_model_by_AIC_name
best_model_by_BIC_name

