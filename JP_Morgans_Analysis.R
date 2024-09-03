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
jpm_data <- data %>% filter(Name == "JPM")

# Convert date column to Date type
jpm_data$date <- as.Date(jpm_data$date, format = "%Y-%m-%d")

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
jpm_data <- fill_missing_dates_and_values(jpm_data)
jpm_data$date <- as.Date(jpm_data$date)

# Convert to xts object
jpm_data_xts <- xts(jpm_data$close, order.by = jpm_data$date)
colnames(jpm_data_xts) <- "close"
jpm_data_xts
length(jpm_data_xts)
plot(jpm_data_xts)

install.packages("psych")
library(psych)

describe(as.numeric(jpm_data_xts))

# Set up the plotting area to show 2 plots in one row
par(mfrow = c(1, 2))

# Plot ACF
acf(jpm_data_xts,lag=50, main = "ACF of Original Close Prices")

# Plot PACF
pacf(jpm_data_xts,lag=50, main = "PACF of Original Close Prices")

# Reset plotting layout to default
par(mfrow = c(1, 1))

adf.test(jpm_data_xts)

log_jpm_data_xts = log(jpm_data_xts)
diff_jpm_data_xts = na.omit(diff(log_jpm_data_xts,differences=1))

plot(diff_jpm_data_xts, col="darkgreen", main="Log-Differenced Close Prices of JPMorgan Chase & Co. Over Time")

# Set up the plotting area to show 3 plots in one row
par(mfrow = c(1, 3))

# Plot the log-differenced close prices without x-axis labels
plot(diff_jpm_data_xts, col="darkgreen", main="Log-Differenced Close Prices")


# Plot ACF
acf(diff_jpm_data_xts, main = "ACF of Log-Differenced Close Prices")

# Plot PACF
pacf(diff_jpm_data_xts, main = "PACF of Log-Differenced Close Prices")

# Reset plotting layout to default
par(mfrow = c(1, 1))

adf.test(diff_gs_data_xts)




# Split data into train and test sets (80% train, 20% test)

train_data <- window(jpm_data_xts, end = as.Date("2017-12-31"))
test_data <- window(jpm_data_xts, start = as.Date("2018-01-01"))

length(train_data)

tsdisplay(train_data)

# Apply log transformation to stabilize variance
train_data_log <- log(train_data)
test_data_log <- log(test_data)

# Apply first-order differencing to remove trend
train_detrended_log <- na.omit(diff(train_data_log, differences = 1))
test_detrended_log <- diff(test_data_log, differences = 1)
test_detrended_log
tsdisplay(train_detrended_log,lag=40)
qqnorm(train_detrended_log)
qqline(train_detrended_log, col = "red")

# Set global font size for titles, labels, and axis text
par(cex.main = 2, cex.lab = 1.2, cex.axis = 1.5)

# Set up the plotting area to show 3 plots in one row
par(mfrow = c(1, 3))

# Plot the original time series with reduced x-axis label font size
plot(jpm_data_xts, 
     main = "JPM Stock Close Prices", 
     ylab = "Close Price", 
     col = "blue")  # Adjust the value as needed to reduce the font size

# Plot ACF with increased title font size
acf(jpm_data_xts, 
    lag.max = 50, 
    main = "ACF of Original Close Prices", cex.lab = 1.5)

# Plot PACF with increased title font size
pacf(jpm_data_xts, 
     lag.max = 50, 
     main = "PACF of Original Close Prices", cex.lab = 1.5)


# Reset plotting layout to default
par(mfrow = c(1, 1))

# Apply log transformation and differencing
log_jpm_data_xts <- log(jpm_data_xts)
diff_jpm_data_xts <- na.omit(diff(log_jpm_data_xts, differences = 1))

# Set up the plotting area to show 3 plots in one row
par(mfrow = c(1, 3))

# Plot the log-differenced time series
plot(diff_jpm_data_xts, main = "Log-Differenced Close Prices", col = "darkgreen", ylab = "Log-Differenced Close Price", cex.lab = 1)

# Plot ACF of the log-differenced series
acf(diff_jpm_data_xts, lag.max = 50, main = "ACF of Log-Differenced Close Prices", cex.lab=1.2)

# Plot PACF of the log-differenced series
pacf(diff_jpm_data_xts, lag.max = 50, main = "PACF of Log-Differenced Close Prices",cex.lab=1.2 )

# Reset plotting layout to default
par(mfrow = c(1, 1))


# Fit SARIMA model on detrended train data
sarima_fit_1 <- auto.arima(train_detrended_log, seasonal = FALSE, stepwise = FALSE, approximation = FALSE)
sarima_fit_1
tsdisplay(residuals(sarima_fit_1))

#For train data
sarima_fit = arima(train_detrended_log,order=c(5,0,2),seasonal = list(order=c(2,1,0),period=30))

sarima_fit
tsdisplay(residuals(sarima_fit))
checkresiduals(sarima_fit)
Box.test(resid(sarima_fit),lag=15)
Box.test(resid(sarima_fit),lag=17)
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

tsdisplay(residuals(sarima_fit))


#For entire data
sarima_auto_fit = auto.arima(jpm_data_xts,seasonal=FALSE)
sarima_auto_fit
tsdisplay(residuals(sarima_auto_fit))

sarima_fit_2 = arima(jpm_data_xts,order=c(5,1,2),seasonal = list(order=c(2,1,0),period=30))
sarima_fit_2
tsdisplay(residuals(sarima_fit_2))

Box.test(residuals(sarima_fit_2),lag=5)
Box.test(residuals(sarima_fit_2),lag=30)
ArchTest(residuals(sarima_fit_2), lags = 40)  #---arch effects are present as value is too small than 0.05


fcast2 = forecast(sarima_fit_2,h=60)
fcast2

plot(fcast2)
accuracy(fcast2)

# Plotting the original data and forecasted data with a custom legend
plot(jpm_data_xts, main = "JPMorgan Chase & Co. Stock Price Forecast", ylab = "Price", xlab = "Date")
lines(fcast2$mean, col = "blue", lwd = 2)

# Adding a legend
legend("topleft", legend = c("Forecasted Data"), col = c("blue"), lty = 1, lwd = 2)

# Calculate accuracy of the forecast
accuracy(fcast2)








