install.packages("MLmetrics")
install.packages("smooth")
install.packages("forecast")

# Call the required libraries
library(forecast)
library(MLmetrics)
library("zoo")
library(ggplot2)
library(ggpubr)
library(readxl)
library(tidyr)
theme_set(theme_pubr())

# Import the Data
# Assuming you have saved the data to an Excel file, otherwise adjust the file format accordingly
df <- read_excel(file.choose())
colnames(df) <- c("Year", "Rent")

# Convert the 'df' data frame into a time series object
ts_data <- ts(df$Rent, start = df$Year[1], end = df$Year[length(df$Year)], frequency = 1)

# Split the dataset into training and validation sets
# Train - means that you'll be using this portion of the data to develop your forecasting model
# Validation/Test - means that you'll be using this portion to test if your model is working or not and measure the accuracy
df_train <- window(ts_data, start = c(2005), end = c(2021))
df_val <- window(ts_data, start = c(2021))

#Description Statistics
summary(df)
#Interpretation:
#Average: $1936
#Highest price and year: 2021 - $2997
#Lowest price and year: 2005 - $1553

#Visualization - Time Series Plot
p <- plot(df, col="blue", xlab="Year", ylab="SJ Rent Price", main="SJ Rent Price Forecast")
#Interpretation: The plot gives us an increasing trend

#Let's use some of the forecasting methods to analyze the given data
### 1. Naive Method ###
naive = naive(df_train, h=length(df_val))
#"h' is how long we want to forecast forward

# Fit the Holt's Linear Trend model to the training data
holt_linear_model <- holt(df_train, h = 3) # h = 3 for the next 3 years

# Forecast rent prices for the next 3 years
forecasted_rent_prices <- holt_linear_model$mean

# Print the forecasted rent prices
print(forecasted_rent_prices)

# Calculate the error (MAPE) for the validation data (2021 only)
mape_error <- MAPE(holt_linear_model$mean[1], df_val) * 100
print(paste("MAPE Error:", mape_error, "%"))

# Plot the original data, training data, and the forecast
autoplot(ts_data) +
  autolayer(df_train, series = "Training Data", PI = FALSE, size = 1.2, color = "blue") +
  autolayer(holt_linear_model, series = "Holt's Linear Trend Forecast", PI = FALSE, size = 1.2, color = "red") +
  geom_point(data = df, aes(x = Year, y = Rent), color = "blue", size = 2, shape = 19) + # Original data points
  geom_point(data = data.frame(Year = c(2022, 2023, 2024), Rent = forecasted_rent_prices), aes(x = Year, y = Rent), color = "red", size = 2, shape = 19) + # Forecasted data points
  geom_text(data = df, aes(x = Year, y = Rent, label = round(Rent, 0)), vjust = 1.8, size = 5, color = "blue") + # Original data labels with larger text size
  geom_text(data = data.frame(Year = c(2022, 2023, 2024), Rent = forecasted_rent_prices), aes(x = Year, y = Rent, label = round(Rent, 0)), vjust = 2, size = 5, color = "red") + # Forecasted data labels with larger text size
  xlab("Year") +
  ylab("SJ Rent Price") +
  ggtitle("SJ Rent Price Forecast: Holt's Linear Trend Method") +
  guides(color = guide_legend(title = "Data & Forecast")) +
  theme(plot.title = element_text(hjust = 0.5, size = 20)) # Center and increase title size
