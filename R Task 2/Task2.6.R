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

# Load necessary libraries
library(MASS)

# Load the dataset (use the appropriate path if necessary)
# data <- read.csv("E:/R Task/R Task 2/data_03cf4b0d-8941-4323-998c-a7ff1a83d0f2_1733812247131.csv")

# Check for NA values in x1, x3, and x2 (output variable)
cat("Number of NA in x1:", sum(is.na(data$x1)), "\n")
cat("Number of NA in x3:", sum(is.na(data$x3)), "\n")
cat("Number of NA in x2:", sum(is.na(data$x2)), "\n")

# Remove rows with NA values in any of the relevant columns (x1, x3, x2)
data_clean <- data[complete.cases(data[, c("x1", "x3", "x2")]), ]

# Check the dimensions of cleaned data
cat("Dimensions of cleaned data:", dim(data_clean), "\n")

# Define input and output signals (after removing NA rows)
X <- data_clean[, c("x1", "x3", "x4", "x5")]
y <- data_clean$x2

# Set seed for reproducibility
set.seed(123)

# Split data into training and testing (70% for training, 30% for testing)
train_index <- sample(1:nrow(data_clean), 0.7 * nrow(data_clean))
train_data <- data_clean[train_index, ]
test_data <- data_clean[-train_index, ]

# Check dimensions of train and test data
cat("Dimensions of train data:", dim(train_data), "\n")
cat("Dimensions of test data:", dim(test_data), "\n")

# Ensure the relevant columns exist in both train_data and test_data
str(train_data)
str(test_data)

# Fit the selected model on the training data (choose the best model, e.g., model_1)
best_model <- lm(x2 ~ x1 + x3, data=train_data)

# Check the summary of the model
summary(best_model)

# Predict on the test data
predictions <- predict(best_model, test_data)

# Compute the 95% confidence intervals for predictions
predictions_ci <- predict(best_model, test_data, interval = "confidence", level = 0.95)

# Save the plot to a PDF
pdf("model_predictions.pdf", width = 12, height = 8)

# Plot predictions with error bars and testing data
plot(test_data$x1, test_data$x2, col = "blue", pch = 16, xlab = "x1", ylab = "x2", main = "Model Prediction vs Test Data")
arrows(test_data$x1, predictions_ci[,2], test_data$x1, predictions_ci[,3], angle = 90, code = 3, length = 0.05)
lines(test_data$x1, predictions, col = "red", lwd = 2)

# Close the PDF device
dev.off()

