# Load necessary libraries
library(MASS)

# Load the dataset 
data <- read.csv("E:/R Task/R Task 2/data_03cf4b0d-8941-4323-998c-a7ff1a83d0f2_1733812247131.csv")

# Check if data has been loaded correctly
head(data)

# Define input and output signals
X <- data[, c("x1", "x3", "x4", "x5")]
y <- data$x2

# Define the models
model_1 <- lm(y ~ x1 + x3, data=data)
model_2 <- lm(y ~ I(x1^2) + I(x3^2), data=data)
model_3 <- lm(y ~ I(x1^3) + I(x3^3), data=data)
model_4 <- lm(y ~ x1 + x3 + I(x4^2), data=data)
model_5 <- lm(y ~ x1 + x3 + I(x4^2) + I(x5^3), data=data)

# Create a PDF to save the plots
pdf("qq_plots.pdf")  # This will save the plots in the working directory

# Set up the layout for 3x2 grid of plots
par(mfrow = c(3, 2))  

# Q-Q plot for Model 1
qqnorm(residuals(model_1))
qqline(residuals(model_1), col = "red")

# Q-Q plot for Model 2
qqnorm(residuals(model_2))
qqline(residuals(model_2), col = "red")

# Q-Q plot for Model 3
qqnorm(residuals(model_3))
qqline(residuals(model_3), col = "red")

# Q-Q plot for Model 4
qqnorm(residuals(model_4))
qqline(residuals(model_4), col = "red")

# Q-Q plot for Model 5
qqnorm(residuals(model_5))
qqline(residuals(model_5), col = "red")

# Close the PDF device
dev.off()
