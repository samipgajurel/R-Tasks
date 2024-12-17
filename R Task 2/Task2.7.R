# Load necessary libraries
library(MASS)

# Load the dataset (replace path with the correct location)
data <- read.csv("E:/R Task/R Task 2/data_03cf4b0d-8941-4323-998c-a7ff1a83d0f2_1733812247131.csv")

# Check for NA values and clean the dataset
data_clean <- data[complete.cases(data[, c("x1", "x3", "x4", "x5", "x2")]), ]

# Define input and output signals
X <- data_clean[, c("x1", "x3", "x4", "x5")]
y <- data_clean$x2

# Set seed for reproducibility
set.seed(123)

# Split data into training (70%) and testing (30%)
train_index <- sample(1:nrow(data_clean), 0.7 * nrow(data_clean))
train_data <- data_clean[train_index, ]
test_data <- data_clean[-train_index, ]

# Fit the selected 'best' model (example: Model 4)
best_model <- lm(x2 ~ x1 + x3 + I(x4^2), data=train_data)

# Summary of the selected model
cat("Summary of the selected model:\n")
print(summary(best_model))

# Predictions and confidence intervals for train and test data
train_predictions <- predict(best_model, train_data)
train_ci <- predict(best_model, train_data, interval="confidence", level=0.95)

test_predictions <- predict(best_model, test_data)
test_ci <- predict(best_model, test_data, interval="confidence", level=0.95)

# Calculate mean of predictions for training and testing datasets
Y_training_hat <- train_predictions
Y_testing_hat <- test_predictions
mean_train <- mean(Y_training_hat)
mean_test <- mean(Y_testing_hat)

cat("\nMean of training predictions (Y_training_hat):", mean_train, "\n")
cat("Mean of testing predictions (Y_testing_hat):", mean_test, "\n")

# Save the plots to a PDF file
pdf("model_predictions_train_and_test.pdf", width=12, height=8)

# ---- Plot for Training Data ----
plot(train_data$x1, train_data$x2, col="blue", pch=16, xlab="x1", ylab="x2",
     main="Training Data: Model Predictions with 95% Confidence Intervals")
arrows(train_data$x1, train_ci[,2], train_data$x1, train_ci[,3], 
       angle=90, code=3, length=0.05, col="gray") # Error bars
lines(train_data$x1, train_predictions, col="red", lwd=2) # Predictions

legend("topright", legend=c("Training Data", "Model Predictions", "95% CI"), 
       col=c("blue", "red", "gray"), pch=c(16, NA, NA), lty=c(NA, 1, 1), bty="n")

# ---- Plot for Test Data ----
plot(test_data$x1, test_data$x2, col="green", pch=16, xlab="x1", ylab="x2",
     main="Testing Data: Model Predictions with 95% Confidence Intervals")
arrows(test_data$x1, test_ci[,2], test_data$x1, test_ci[,3], 
       angle=90, code=3, length=0.05, col="gray") # Error bars
lines(test_data$x1, test_predictions, col="red", lwd=2) # Predictions

legend("topright", legend=c("Test Data", "Model Predictions", "95% CI"), 
       col=c("green", "red", "gray"), pch=c(16, NA, NA), lty=c(NA, 1, 1), bty="n")

# Close the PDF device
dev.off()

cat("\nPlots saved to 'model_predictions_train_and_test.pdf'\n")

# Calculate mean of predictions for training and testing datasets
Y_training_hat <- train_predictions
Y_testing_hat <- test_predictions
mean_train <- mean(Y_training_hat)
mean_test <- mean(Y_testing_hat)

cat("\nMean of training predictions (Y_training_hat):", mean_train, "\n")
cat("Mean of testing predictions (Y_testing_hat):", mean_test, "\n")

# Perform Welch Two Sample t-test
t_test_result <- t.test(Y_training_hat, Y_testing_hat, alternative = "two.sided")

# Display the t-test result
cat("\nWelch Two Sample t-test Output:\n")
print(t_test_result)


