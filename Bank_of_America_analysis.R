# Set working directory
setwd("C:/Users/aishp/OneDrive/Desktop")

# Load necessary libraries
library(ggplot2)
library(readr)
library(dplyr)
library(tidyr)
library(forecast)
library(tseries)
library(zoo)
library(lubridate)
library(xts)
library(splines)
library(FinTS)


# Load data
data <- read.csv("all_stocks_5yr.csv")

# Filter data for GS
bac_data <- data %>% filter(Name == "BAC")

# Convert date column to Date type
bac_data$date <- as.Date(bac_data$date, format = "%Y-%m-%d")

# Function to add missing dates and fill NA values for close column only
fill_missing_dates_and_values <- function(stock_data) {
  full_dates <- seq(min(stock_data$date), max(stock_data$date), by = "day")
  full_dates <- full_dates[!(wday(full_dates) %in% c(1, 7))]
  full_data <- stock_data %>%
    select(date, close) %>%
    complete(date = full_dates) %>%
    arrange(date)
  full_data$close <- zoo::na.locf(full_data$close, na.rm = FALSE)
  return(full_data)
}

# Apply the function to GS data
bac_data <- fill_missing_dates_and_values(bac_data)
bac_data$date <- as.Date(bac_data$date)

# Convert to xts object
bac_data_xts <- xts(bac_data$close, order.by = bac_data$date)
colnames(bac_data_xts) <- "close"

plot(bac_data_xts)

adf.test(bac_data_xts)

# Set up the plotting area to show 3 plots in one row
par(mfrow = c(1, 3))

# Plot the trend of BAC stock prices
plot(bac_data_xts, main = "BAC Stock Close Price", ylab = "Close Price", xlab = "Date",col="red")

# Plot ACF of BAC stock prices
acf(bac_data_xts, main = "ACF of BAC Stock Close Prices", lag.max = 50, cex.lab = 1.5)

# Plot PACF of BAC stock prices
pacf(bac_data_xts, main = "PACF of BAC Stock Close Prices", lag.max = 50 ,cex.lab = 1.5)

# Reset plotting layout to default
par(mfrow = c(1, 1))


log_bac_data_xts = log(bac_data_xts)
diff_bac_data_xts = na.omit(diff(log_bac_data_xts,differences=1))

# Set up the plotting area to show 3 plots in one row
par(mfrow = c(1, 3))

# Plot the log-differenced trend of BAC stock prices
plot(diff_bac_data_xts, main = "Log-Differenced Close Prices", ylab = "Log-Differenced Close Price", xlab = "Date", col = "darkgreen")

# Plot ACF of log-differenced BAC stock prices
acf(diff_bac_data_xts, main = "ACF of Log-Differenced Close Prices", lag.max = 50)

# Plot PACF of log-differenced BAC stock prices
pacf(diff_bac_data_xts, main = "PACF of Log-Differenced Close Prices", lag.max = 50)

# Reset plotting layout to default
par(mfrow = c(1, 1))



describe(as.numeric(bac_data_xts))


adf.test(diff_bac_data_xts)


# Split data into train and test sets (80% train, 20% test)
train_data <- window(bac_data_xts, end = as.Date("2017-12-31"))
test_data <- window(bac_data_xts, start = as.Date("2018-01-01"))

tsdisplay(train_data)

# Apply log transformation to stabilize variance
train_data_log <- log(train_data)
test_data_log <- log(test_data)

# Apply first-order differencing to remove trend
train_detrended_log <- na.omit(diff(train_data_log, differences = 1))
test_detrended_log <- diff(test_data_log, differences = 1)

tsdisplay(train_detrended_log,lag=40)
qqnorm(train_detrended_log)
qqline(train_detrended_log, col = "red")

# Fit SARIMA model on detrended train data
sarima_fit_1 <- auto.arima(train_detrended_log, seasonal = FALSE, stepwise = FALSE, approximation = FALSE)
sarima_fit_1
tsdisplay(residuals(sarima_fit_1))

#For train data
sarima_fit = arima(train_detrended_log,order=c(5,0,2),seasonal = list(order=c(2,1,0),period=30))
sarima_fit
tsdisplay(residuals(sarima_fit))
checkresiduals(sarima_fit)
Box.test(resid(sarima_fit),lag=17)
Box.test(resid(sarima_fit),lag=18)
Box.test(resid(sarima_fit),lag=30)

qqnorm(resid(sarima_fit))
qqline(resid(sarima_fit), col = "red")

ArchTest(residuals(sarima_fit), lags = 40)  #---arch effects are present as value is too small than 0.05



# Forecast detrended test data
forecast_horizon <- length(test_detrended_log)
forecasts_detrended_log <- forecast(sarima_fit, h = forecast_horizon)
forecasts_detrended_log
plot(forecasts_detrended_log)

# Reverse the differencing and log transformation for test data
forecasts_detrended_log_xts <- xts(forecasts_detrended_log$mean, order.by = index(test_detrended_log))
final_forecasts_test_log <- cumsum(forecasts_detrended_log_xts) + as.numeric(last(train_data_log))
final_forecasts_test <- exp(final_forecasts_test_log) # Reverse the log transformation

# Ensure the final forecasted values have the correct index
final_forecasts_test <- xts(coredata(final_forecasts_test), order.by = index(test_data))



plot(final_forecasts_test)
plot(test_data)

# Plot just the test data and forecasted data
plot(index(test_data), coredata(test_data), type = "l", col = "blue", 
     main = "Test Data and Forecasted Close Prices", ylab = "Close Price", xlab = "Date")
lines(index(final_forecasts_test), coredata(final_forecasts_test), col = "purple", lwd = 2)
legend("topleft", legend = c("Test Data", "Forecasted Data"), col = c("blue", "purple"), lty = 1, lwd = 2)

# Calculate evaluation metrics for the forecasted test data
rmse <- sqrt(mean((coredata(test_data) - coredata(final_forecasts_test))^2))
mae <- mean(abs(coredata(test_data) - coredata(final_forecasts_test)))
mape <- mean(abs((coredata(test_data) - coredata(final_forecasts_test)) / coredata(test_data))) * 100

cat("RMSE: ", rmse, "\n")
cat("MAE: ", mae, "\n")
cat("MAPE: ", mape, "\n")


#For entire data
sarima_auto_fit = auto.arima(bac_data_xts,seasonal=FALSE)
sarima_auto_fit
tsdisplay(residuals(sarima_auto_fit))

sarima_fit_2 = arima(bac_data_xts,order=c(5,1,2),seasonal = list(order=c(2,1,0),period=30))
sarima_fit_2
tsdisplay(residuals(sarima_fit_2))

Box.test(residuals(sarima_fit_2),lag=18)
Box.test(residuals(sarima_fit_2),lag=30)
ArchTest(residuals(sarima_fit_2), lags = 40)  #---arch effects are present as value is too small than 0.05


fcast2 = forecast(sarima_fit_2,h=60)
fcast2

plot(fcast2)
# Adding a legend
legend("topleft", legend = c("Forecasted Data"), col = c("blue"), lty = 1, lwd = 2)

accuracy(fcast2)





