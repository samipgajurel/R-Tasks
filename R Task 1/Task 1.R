# Load necessary libraries
library(ggplot2)
library(moments)   # For skewness calculation

# Set working directory (optional, adjust path)
setwd("E:/R Task/R Task 1")

# Load the dataset
data <- read.csv("E:/R Task/R Task 1/data_03cf4b0d-8941-4323-998c-a7ff1a83d0f2_1733812247131.csv")

# Step 1: Time Series Plots (Input and Output Signals)
pdf("Task_1_Time_Series_Plots.pdf", width = 12, height = 8)
par(mfrow = c(3, 2))  # Arrange plots in 3x2 grid

# Time series plots for x1, x2, x3, x4, x5
plot(data$x1, type = 'l', col = 'blue', main = 'Time Series Plot of x1 (Input)', xlab = 'Time', ylab = 'x1')
plot(data$x2, type = 'l', col = 'red', main = 'Time Series Plot of x2 (Output)', xlab = 'Time', ylab = 'x2')
plot(data$x3, type = 'l', col = 'green', main = 'Time Series Plot of x3 (Input)', xlab = 'Time', ylab = 'x3')
plot(data$x4, type = 'l', col = 'purple', main = 'Time Series Plot of x4 (Input)', xlab = 'Time', ylab = 'x4')
plot(data$x5, type = 'l', col = 'orange', main = 'Time Series Plot of x5 (Input)', xlab = 'Time', ylab = 'x5')
dev.off()

# Step 2: Distribution and Skewness Plots
pdf("Task_1_Distribution_and_Skewness_Plots.pdf", width = 12, height = 8)
par(mfrow = c(2, 3))  # Arrange plots in 2x3 grid

# Function to plot density with skewness
plot_density_with_skew <- function(data_column, column_name, color) {
  skew_val <- round(skewness(data_column), 3)  # Calculate skewness
  plot(density(data_column), col = color, main = paste("Density of", column_name, "\nSkewness =", skew_val),
       xlab = column_name, ylab = "Density", lwd = 2)
}

# Plot density and skewness for x1 to x5
plot_density_with_skew(data$x1, "x1", "blue")
plot_density_with_skew(data$x2, "x2 (Output)", "red")
plot_density_with_skew(data$x3, "x3", "green")
plot_density_with_skew(data$x4, "x4", "purple")
plot_density_with_skew(data$x5, "x5", "orange")
dev.off()

# Step 3: Individual Scatter Plots (Inputs vs Output)

# Scatter plot for x1 (Input) vs x2 (Output)
pdf("Task_1_Scatter_Plot_x1_vs_x2.pdf", width = 8, height = 6)
plot(data$x1, data$x2, main = "Scatter Plot of x1 vs x2 (Output)", xlab = "x1 (Input)", ylab = "x2 (Output)", col = "blue")
dev.off()

# Scatter plot for x3 (Input) vs x2 (Output)
pdf("Task_1_Scatter_Plot_x3_vs_x2.pdf", width = 8, height = 6)
plot(data$x3, data$x2, main = "Scatter Plot of x3 vs x2 (Output)", xlab = "x3 (Input)", ylab = "x2 (Output)", col = "green")
dev.off()

# Scatter plot for x4 (Input) vs x2 (Output)
pdf("Task_1_Scatter_Plot_x4_vs_x2.pdf", width = 8, height = 6)
plot(data$x4, data$x2, main = "Scatter Plot of x4 vs x2 (Output)", xlab = "x4 (Input)", ylab = "x2 (Output)", col = "purple")
dev.off()

# Scatter plot for x5 (Input) vs x2 (Output)
pdf("Task_1_Scatter_Plot_x5_vs_x2.pdf", width = 8, height = 6)
plot(data$x5, data$x2, main = "Scatter Plot of x5 vs x2 (Output)", xlab = "x5 (Input)", ylab = "x2 (Output)", col = "orange")
dev.off()

# Step 4: Display First Few Rows of Data
# Add a Time column (assuming row index represents time)
data$Time <- 1:nrow(data)

# Combine into a time table
time_table <- data.frame(
  Time = data$Time,
  x1 = data$x1,  # Input
  x3 = data$x3,  # Input
  x4 = data$x4,  # Input
  x5 = data$x5,  # Input
  x2 = data$x2   # Output
)

# Display the first 6 rows in tabulated format
head(time_table)

# Print the table in RStudio's console
print(time_table[1:6, ])

