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
gs_data <- data %>% filter(Name == "GS")

# Convert date column to Date type
gs_data$date <- as.Date(gs_data$date, format = "%Y-%m-%d")

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
gs_data <- fill_missing_dates_and_values(gs_data)
gs_data$date <- as.Date(gs_data$date)

# Convert to xts object
gs_data_xts <- xts(gs_data$close, order.by = gs_data$date)
colnames(gs_data_xts) <- "close"

plot(gs_data_xts)
describe(as.numeric(gs_data_xts))

# Set up the plotting area to show 2 plots in one row
par(mfrow = c(1, 2))

# Plot the histogram
hist(as.numeric(gs_data_xts$close), 
     main = "Histogram of Close Prices", 
     xlab = "Close Price", 
     ylab = "Frequency", 
     col = "lightblue", 
     border = "black", 
     prob = TRUE)  # Set prob = TRUE for density plot

# Add a density line
lines(density(as.numeric(gs_data_xts$close)), 
      col = "red", 
      lwd = 2)

# Plot the Q-Q plot
qqnorm(as.numeric(gs_data_xts$close), 
       main = "Q-Q Plot of Close Prices",
       xlab = "Theoretical Quantiles",
       ylab = "Sample Quantiles")

# Add a reference line
qqline(as.numeric(gs_data_xts$close), 
       col = "red", 
       lwd = 2)

# Reset plotting layout to default
par(mfrow = c(1, 1))




# Set up the plotting area to show 2 plots in one row
par(mfrow = c(1, 2))

# Plot ACF
acf(gs_data_xts,lag=50, main = "ACF of Original Close Prices")

# Plot PACF
pacf(gs_data_xts,lag=50, main = "PACF of Original Close Prices")

# Reset plotting layout to default
par(mfrow = c(1, 1))


# Split data into train and test sets (80% train, 20% test)
#train_size <- floor(0.90 * length(gs_data_xts))

# Perform the ADF test
adf_result <- adf.test(gs_data_xts)

log_gs_data_xts = log(gs_data_xts)
diff_gs_data_xts = na.omit(diff(log_gs_data_xts,differences=1))

plot(diff_gs_data_xts, col="darkgreen", main="Log-Differenced Close Prices of Goldman Sachs Over Time")

# Set up the plotting area to show 2 plots in one row
par(mfrow = c(1, 2))

# Plot ACF
acf(diff_gs_data_xts, main = "ACF of Log-Differenced Close Prices")

# Plot PACF
pacf(diff_gs_data_xts, main = "PACF of Log-Differenced Close Prices")

# Reset plotting layout to default
par(mfrow = c(1, 1))

adf.test(diff_gs_data_xts)



train_data <- window(gs_data_xts, end = as.Date("2017-12-31"))
test_data <- window(gs_data_xts, start = as.Date("2018-01-01"))

length(train_data)
length(test_data)

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
auto_arima <- auto.arima(train_detrended_log, seasonal = FALSE, stepwise = FALSE, approximation = FALSE)
auto_arima
tsdisplay(residuals(auto_arima))
checkresiduals(auto_arima)


#For train data
sarima_fit = arima(train_detrended_log,order=c(5,1,3),seasonal = list(order=c(2,1,0),period=40))
sarima_fit

sarima_fit_2 = arima(train_detrended_log,order=c(5,1,3),seasonal = list(order=c(2,1,0),period=30))
sarima_fit_2

sarima_fit = arima(train_detrended_log,order=c(5,0,1),seasonal = list(order=c(0,1,1),period=30))
sarima_fit


tsdisplay(residuals(sarima_fit))
checkresiduals(sarima_fit)
Box.test(resid(sarima_fit),lag=15)
Box.test(resid(sarima_fit),lag=18)
Box.test(resid(sarima_fit),lag=29)

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

# Plotting the Train, Test, and Forecasted Data
plot(index(train_data), coredata(train_data), type = "l", col = "blue", 
     main = "Train, Test, and Forecasted Close Prices", ylab = "Close Price", xlab = "Date")
lines(index(test_data), coredata(test_data), col = "green", lwd = 2)
lines(index(final_forecasts_test), coredata(final_forecasts_test), col = "red", lwd = 2)
legend("topleft", legend = c("Train Data", "Test Data", "Forecasted Data"), 
       col = c("blue", "green", "red"), lty = 1, lwd = 2)


# Calculate evaluation metrics for the forecasted test data
rmse <- sqrt(mean((coredata(test_data) - coredata(final_forecasts_test))^2))
mae <- mean(abs(coredata(test_data) - coredata(final_forecasts_test)))
mape <- mean(abs((coredata(test_data) - coredata(final_forecasts_test)) / coredata(test_data))) * 100



cat("RMSE: ", rmse, "\n")
cat("MAE: ", mae, "\n")
cat("MAPE: ", mape, "\n")

#Garch model

spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                   distribution.model = "sstd",
                   mean.model = list(armaOrder = c(0, 0)))


fit_garch <- ugarchfit(spec = spec, data = residuals(sarima_fit))
summary(fit_garch)
fit_garch

# Diagnostic plots for GARCH model
tsdisplay(residuals(fit_garch))

# Ljung-Box test on standardized residuals
Box.test(residuals(fit_garch, standardize = TRUE), lag = 18, type = "Ljung-Box")
Box.test(residuals(fit_garch, standardize = TRUE), lag = 29, type = "Ljung-Box")
plot(fit_garch, which = "all")



length(sigma(fit_garch))
length(garch_sigma_forecast)

garch_forecast <- ugarchforecast(fit_garch, n.ahead = length(test_data))
garch_forecast



garch_sigma_forecast <- sigma(garch_forecast)
forecast_dates <- index(test_data)
volatality_forecast_series <- xts(garch_sigma_forecast, order.by = index(test_data))
volatality_forecast_series

plot(final_forecasts_test)
plot(garch_sigma_forecast,col="red")
garch_sigma_forecast


# Load necessary libraries
library(rugarch)
library(xts)
library(forecast)
library(zoo)

# Assuming sigma_forecast contains the forecasted volatilities from the GARCH model
# Assuming final_forecasts_test_log contains the log-transformed ARIMA forecast

# Back-transform sigma to the original scale
# Since sigma_forecast is in the log-differenced scale, we need to reverse the differencing and log transformation

# Reverse the log differencing to get back to the log scale
sigma_forecast_log <- cumsum(volatality_forecast_series) + last(train_data_log)
last(train_data_log)
cumsum(volatality_forecast_series)
sigma_forecast_log
# Reverse the log transformation
sigma_forecast_original <- exp(sigma_forecast_log)

# Ensure the final forecasted volatilities have the correct index
sigma_forecast_original_xts <- xts(coredata(sigma_forecast_original), order.by = index(test_data))

# Calculate realized volatility from actual return data (using squared returns and rolling sum)
# Realized volatility over each day can be approximated as the standard deviation of returns over a rolling window
window_size <- 1 # Modify this to the appropriate window size for realized volatility
realized_volatility <- sqrt(rollapply(test_data^2, width = window_size, FUN = sum, align = "right", fill = NA))

# Ensure both series are aligned
realized_volatility <- realized_volatility[!is.na(realized_volatility)]
sigma_forecast_original_xts <- sigma_forecast_original_xts[index(realized_volatility)]

# Calculate MSE of the forecasted volatility
mse_volatility <- mean((coredata(realized_volatility) - coredata(sigma_forecast_original_xts))^2)
cat("MSE of Forecasted Volatility: ", mse_volatility, "\n")

# Plot the forecasted and realized volatilities
plot(index(realized_volatility), coredata(realized_volatility), type = "l", col = "blue",
     main = "Forecasted vs Realized Volatility", ylab = "Volatility", xlab = "Date")
lines(index(sigma_forecast_original_xts), coredata(sigma_forecast_original_xts), col = "red", lwd = 2)
legend("topright", legend = c("Realized Volatility", "Forecasted Volatility"), col = c("blue", "red"), lty = 1, lwd = 2)















upper_bound <- final_forecasts_test * exp(1.96 * volatality_forecast_series)
lower_bound <- final_forecasts_test * exp(-1.96 * volatality_forecast_series)

# Plot with forecast intervals
plot(index(test_data), coredata(test_data), type = "l", col = "blue", 
     main = "Test Data and Forecasted Close Prices with GARCH Intervals", ylab = "Close Price", xlab = "Date")
lines(index(final_forecasts_test), coredata(final_forecasts_test), col = "purple", lwd = 2)
lines(index(final_forecasts_test), coredata(upper_bound), col = "red", lwd = 2, lty = 2)
lines(index(final_forecasts_test), coredata(lower_bound), col = "red", lwd = 2, lty = 2)
legend("topleft", legend = c("Test Data", "ARIMA Forecast", "95% Confidence Interval"), col = c("blue", "purple", "red"), lty = 1, lwd = 2)













#For entire data
sarima_auto_fit = auto.arima(gs_data_xts,seasonal=FALSE)
sarima_auto_fit
tsdisplay(residuals(sarima_auto_fit))

sarima_fit_2 = arima(gs_data_xts,order=c(5,1,3),seasonal = list(order=c(2,1,0),period=30))
sarima_fit_2

sarima_fit_2 = arima(gs_data_xts,order=c(5,1,1),seasonal = list(order=c(0,1,1),period=30))
sarima_fit_2

tsdisplay(residuals(sarima_fit_2))

Box.test(residuals(sarima_fit_2),lag=15)
Box.test(residuals(sarima_fit_2),lag=29)
ArchTest(residuals(sarima_fit_2), lags = 40)  #---arch effects are present as value is too small than 0.05


fcast2 = forecast(sarima_fit_2,h=60)
fcast2

plot(fcast2)
accuracy(fcast2)


# Load necessary library for GARCH modeling
library(rugarch)

# Specify and fit the GARCH model
spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
  distribution.model = "norm"
)
garch_fit <- ugarchfit(spec = spec, data = residuals(sarima_fit_2))
print(garch_fit)

# Forecast using the GARCH model
garch_forecast <- ugarchforecast(garch_fit, n.ahead = 30)
sigma_forecast <- sigma(garch_forecast) # Volatility forecast
sigma_forecast <- as.numeric(sigma_forecast)

# Extract ARIMA forecast values
arima_forecast_values <- as.numeric(fcast2$mean)

# Calculate the upper and lower bounds of the forecast using the GARCH volatility forecast
upper_bound <- arima_forecast_values + 1.96 * sigma_forecast
lower_bound <- arima_forecast_values - 1.96 * sigma_forecast

# Convert forecasts to a time series object
forecast_dates <- seq.Date(from = index(gs_data_xts)[length(gs_data_xts)], by = "days", length.out = 30)
arima_forecast_ts <- xts(arima_forecast_values, order.by = forecast_dates)
upper_bound_ts <- xts(upper_bound, order.by = forecast_dates)
lower_bound_ts <- xts(lower_bound, order.by = forecast_dates)

# Plot the forecasts with confidence intervals
plot(index(gs_data_xts), coredata(gs_data_xts), type = "l", col = "blue",
     main = "Forecasted Close Prices with GARCH Confidence Intervals",
     ylab = "Close Price", xlab = "Date")
lines(index(arima_forecast_ts), coredata(arima_forecast_ts), col = "purple", lwd = 2)
lines(index(upper_bound_ts), coredata(upper_bound_ts), col = "red", lwd = 2, lty = 2)
lines(index(lower_bound_ts), coredata(lower_bound_ts), col = "red", lwd = 2, lty = 2)
legend("topleft", legend = c("Actual Data", "ARIMA Forecast", "95% Confidence Interval"),
       col = c("blue", "purple", "red"), lty = 1, lwd = 2)

# Calculate evaluation metrics for the ARIMA + GARCH forecast
# Note: Since we're forecasting 30 days ahead, we cannot directly compare these forecasts to actual data
# because we don't have future actual values. However, for demonstration, we'll compute metrics assuming we have them.

# Create placeholder actual future values (replace this with actual future data if available)
actual_future_values <- gs_data_xts[(length(gs_data_xts) - 29):length(gs_data_xts)]

# Calculate evaluation metrics assuming we have actual future values
rmse_garch <- sqrt(mean((coredata(actual_future_values) - arima_forecast_values)^2))
mae_garch <- mean(abs(coredata(actual_future_values) - arima_forecast_values))
mape_garch <- mean(abs((coredata(actual_future_values) - arima_forecast_values) / coredata(actual_future_values))) * 100

cat("ARIMA + GARCH RMSE: ", rmse_garch, "\n")
cat("ARIMA + GARCH MAE: ", mae_garch, "\n")
cat("ARIMA + GARCH MAPE: ", mape_garch, "\n")








