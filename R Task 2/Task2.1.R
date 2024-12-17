# Load necessary libraries
library(MASS)

# Load the dataset
data <- read.csv("E:/R Task/R Task 2/data_03cf4b0d-8941-4323-998c-a7ff1a83d0f2_1733812247131.csv")

# Define input and output signals
X <- data[, c("x1", "x3", "x4", "x5")]
y <- data$x2

# Clean the dataset by removing rows with NA
data_clean <- data[complete.cases(data[, c("x1", "x3", "x2")]), ]

# Split data into training (70%) and testing (30%)
set.seed(123)
train_index <- sample(1:nrow(data_clean), 0.7 * nrow(data_clean))
train_data <- data_clean[train_index, ]
test_data <- data_clean[-train_index, ]

# Fit the best model (Model 1): y ~ x1 + x3
best_model <- lm(x2 ~ x1 + x3, data=train_data)

# Summary of the model
summary(best_model)

# Predictions and confidence intervals for training data
train_predictions_ci <- predict(best_model, train_data, interval = "confidence", level = 0.95)

# Predictions and confidence intervals for testing data
test_predictions_ci <- predict(best_model, test_data, interval = "confidence", level = 0.95)

# Save plots to a PDF
pdf("model_predictions_train_test.pdf", width = 12, height = 8)

# Plot for Training Data
plot(train_data$x1, train_data$x2, col = "blue", pch = 16, 
     xlab = "x1", ylab = "x2", main = "Model Prediction vs Training Data")
arrows(train_data$x1, train_predictions_ci[,2], train_data$x1, train_predictions_ci[,3], 
       angle = 90, code = 3, length = 0.05)
lines(train_data$x1, train_predictions_ci[,1], col = "red", lwd = 2)

# Plot for Testing Data
plot(test_data$x1, test_data$x2, col = "green", pch = 16, 
     xlab = "x1", ylab = "x2", main = "Model Prediction vs Testing Data")
arrows(test_data$x1, test_predictions_ci[,2], test_data$x1, test_predictions_ci[,3], 
       angle = 90, code = 3, length = 0.05)
lines(test_data$x1, test_predictions_ci[,1], col = "red", lwd = 2)

# Close the PDF device
dev.off()

# Welch Two Sample t-test for mean comparison between predictions
train_hat <- predict(best_model, train_data)
test_hat <- predict(best_model, test_data)
t_test_result <- t.test(train_hat, test_hat, alternative = "two.sided", conf.level = 0.95)

# Print the Welch t-test result
print("Welch Two Sample t-test:")
print(t_test_result)
