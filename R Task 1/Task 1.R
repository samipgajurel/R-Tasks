# Load necessary libraries
library(ggplot2)

# Load the dataset
data <- read.csv("E:/R Task/R Task 1/data_03cf4b0d-8941-4323-998c-a7ff1a83d0f2_1733812247131.csv")

# Check first few rows of the dataset
head(data)

# Set working directory (optional, if you want to ensure it's correct)
setwd("E:/R Task/R Task 1")  # Adjust this path to your working directory

# Step 1: Time Series Plots (of input and output signals)
# Assuming the columns in the data represent different signals (x1, x2, x3, x4, x5)

# Create and save time series plots for the signals
pdf("Task_1_Time_Series_Plots.pdf", width = 12, height = 8)
par(mfrow=c(3, 2))  # Setting up multiple plots in a grid

# Time series plot for x1 (input)
plot(data$x1, type='l', col='blue', main='Time Series Plot of x1 (Input)', xlab='Time', ylab='x1')

# Time series plot for x2 (output)
plot(data$x2, type='l', col='red', main='Time Series Plot of x2 (Output)', xlab='Time', ylab='x2')

# Time series plot for x3 (input)
plot(data$x3, type='l', col='green', main='Time Series Plot of x3 (Input)', xlab='Time', ylab='x3')

# Time series plot for x4 (input)
plot(data$x4, type='l', col='purple', main='Time Series Plot of x4 (Input)', xlab='Time', ylab='x4')

# Time series plot for x5 (input)
plot(data$x5, type='l', col='orange', main='Time Series Plot of x5 (Input)', xlab='Time', ylab='x5')

# Close PDF device
dev.off()

# Step 2: Distribution for Each Signal (histograms for each signal)
# Plot distribution (histogram) for each signal in the dataset

pdf("Task_1_Distribution_Plots.pdf", width = 12, height = 8)
par(mfrow=c(2, 3))  # Setting up multiple plots in a grid

# Histogram for x1 (input)
hist(data$x1, breaks=20, col='blue', main='Distribution of x1', xlab='x1', ylab='Frequency')

# Histogram for x2 (output)
hist(data$x2, breaks=20, col='red', main='Distribution of x2 (Output)', xlab='x2', ylab='Frequency')

# Histogram for x3 (input)
hist(data$x3, breaks=20, col='green', main='Distribution of x3', xlab='x3', ylab='Frequency')

# Histogram for x4 (input)
hist(data$x4, breaks=20, col='purple', main='Distribution of x4', xlab='x4', ylab='Frequency')

# Histogram for x5 (input)
hist(data$x5, breaks=20, col='orange', main='Distribution of x5', xlab='x5', ylab='Frequency')

# Close PDF device
dev.off()

