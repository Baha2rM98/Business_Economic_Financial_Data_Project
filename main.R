library(lmtest)
library(forecast)
library(splines)
library(gam)
library(ggcorrplot)


#Datasets Heating Map
data_euro_super_95 <- read.csv(paste0(getwd(), "/dataset/monthly_prices_euro_super_95.csv"))
data_heating_gas_oil <- read.csv(paste0(getwd(), "/dataset/monthly_prices_heating_gas_oil_from_1996_to_20240105.csv"))
data_heavy_fuel_oil <- read.csv(paste0(getwd(), "/dataset/monthly_prices_heavy_fuel_oil_from_1996_to_20240107.csv"))
price_euro_super_95 <- data_euro_super_95$PRICE
price_heating_gas_oil <- data_heating_gas_oil$PRICE[1:334]
price_heavy_fuel_oil <- data_heavy_fuel_oil$PRICE[1:334]
merged_data <- cbind(price_euro_super_95, price_heating_gas_oil, price_heavy_fuel_oil)
# Calculate the correlation matrix
correlation_matrix <- cor(merged_data)
# Plot the heatmap
p <- ggcorrplot(correlation_matrix[, 3:1], type = 'full', lab = TRUE, title = '           The Correlation Between Prices')

p + scale_fill_gradient2(limit = c(0.8, 1), low = "red", high = "blue", mid = "white", midpoint = 0.9)


# EDA
data <- read.csv(paste0(getwd(), "/dataset/monthly_prices_euro_super_95.csv"))
data$DATE <- as.Date(data$DATE)
price <- data$PRICE
date <- data$DATE
# Plot with Date on the x-axis
plot(date, price, type = "l", xlab = "Date", ylab = "Price", main = "Euro Super 95 Price")
# Plot the ACF and PACF
acf(price, main = "ACF of Euro Super 95 Prices", xlab = "Lag", ylab = "ACF")
pacf(price, main = "PACF of Euro Super 95 Prices", xlab = "Lag", ylab = "PACF")
# Box plotting
boxplot(price, col = "orange", main = "Boxplot of Euro Super 95 Prices")
# Create a time series object
ts_data <- ts(price, frequency = 12)
# Decompose the time series
decomposition <- stl(ts_data, s.window = "periodic")
# Plot the decomposed components
plot(decomposition)


# Fitting the time series linear model
price_ts <- ts(price, frequency = 12)
fit_ts <- tslm(price_ts ~ trend + season)
summary(fit_ts)
actual_values <- as.numeric(price_ts)
fitted_values <- fitted(fit_ts)
cat("AIC of TSLM:", AIC(fit_ts), "\n")
print(paste("RMSE of TSLM:", sqrt(mean((actual_values - fitted_values)^2))))
dwtest(fit_ts)
plot(price_ts, ylab = "Euro Super 95 Price", xlab = "Time")
lines(fitted(fit_ts), col = 2)
res <- residuals(fit_ts)
plot(res, main = 'Residuals of Euro Super 95')
acf(res, main = "ACF of residuals of Euro Super 95")
pacf(res, main = "PACF of residuals of Euro Super 95")


# Fitting the ARIMA model
arima_model <- auto.arima(price)
# Obtain the fitted values
arima_fitted <- fitted(arima_model)
# Display summary information about the ARIMA model
summary(arima_model)
plot(price, type = 'l', main = "ARIMA (1, 1, 2)", ylab = "Price", xlab = "Observations")
lines(arima_fitted, col = 2)
arima_forecast <- forecast(arima_model)
plot(arima_forecast)
lines(fitted(arima_model), col = 2)
arima_residulas <- residuals(arima_model)
tsdisplay(arima_residulas)


# Fitting the SARIMA model
sarima_model <- arima(price, order = c(1, 1, 2), seasonal = list(order = c(1, 1, 2), period = 12))
# Obtain the fitted values
sarima_fitted <- fitted(sarima_model)
# Display summary information about the SARIMA model
summary(sarima_model)
# Plot the original time series and the SARIMA fitted values
plot(price, type = 'l', main = "SARIMA (1, 1, 2)(1, 1, 2)[12]", ylab = "Price", xlab = "Observations")
lines(sarima_fitted, col = 2)
# Obtain SARIMA forecast
sarima_forecast <- forecast(sarima_model, h = 12)
# Plot the SARIMA forecast
plot(sarima_forecast)
lines(fitted(sarima_model), col = 2)
# Obtain SARIMA residuals
sarima_residuals <- residuals(sarima_model)
# Display ACF and PACF of residuals
tsdisplay(sarima_residuals)


# Fitting SPLine
time <- 1:length(price)
# Create a spline regression model with cubic splines
degree <- 3  # Degree of the spline
# Calculate data density
data_density <- length(time) / (max(time) - min(time))
# Choose length.out based on data density
length_out <- round(data_density * 10)
knots <- seq(min(time), max(time), length.out = length_out)
spline_model <- lm(price ~ bs(time, df = degree, knots = knots))
# Make predictions for the existing time points
predicted_prices <- predict(spline_model)
# Plot the data, fitted spline regression curve, and predictions
plot(time, type = "l", price, main = "Spline Regression", xlab = "Observations", ylab = "Euro Super 95 Price", col = "blue", lwd = 3)
lines(time, predict(spline_model), col = "black", lwd = 3, type = "l", lty = 1)
points(time, predicted_prices, col = "green")
summary(spline_model)
actual_values <- as.numeric(price)
fitted_values <- fitted(spline_model)
cat("AIC of SPLine:", AIC(spline_model), "\n")
print(paste("RMSE of SPLine:", sqrt(mean((actual_values - fitted_values)^2))))
plot(time, residuals(spline_model), main = "Residuals", col = "black", type = "l")
acf(residuals(spline_model), main = "ACF of Residuals")
pacf(residuals(spline_model), main = "PACF of Residuals")


# Fitting a GAM model
time <- 1:length(price)
gam_model <- gam(price ~ lo(time, span = 0.15), data = data)
predicted_prices <- predict(gam_model)
plot(time, type = "l", price, main = "General Additive Model", xlab = "Observations", ylab = "Euro Super 95 Price", col = "blue", lwd = 3)
lines(time, predict(gam_model), col = "black", lwd = 3, type = "l", lty = 1)
points(time, predicted_prices, col = "green")
# Summary of the GAM model
summary(gam_model)
actual_values <- as.numeric(price)
fitted_values <- fitted(gam_model)
cat("AIC of GAM:", AIC(gam_model), "\n")
print(paste("RMSE of GAM:", sqrt(mean((actual_values - fitted_values)^2))))
plot(time, residuals(gam_model), main = "Residuals", col = "black", type = "l")
acf(residuals(gam_model), main = "ACF of Residuals")
pacf(residuals(gam_model), main = "PACF of Residuals")


#################################################################################################################################################


data <- read.csv(paste0(getwd(), "/dataset/monthly_prices_heating_gas_oil_from_1996_to_20240105.csv"))
data$Date <- as.Date(paste0(data$YEAR, "-", data$MONTH_ID, "-01"))
price <- data$PRICE
date <- data$Date
# Plot with Date on the x-axis
plot(date, price, type = "l", xlab = "Date", ylab = "Price", main = "Heating Gas Oil Price")
# Plot the ACF and PACF
acf(price, main = "ACF Heating Gas Oil Price", xlab = "Lag", ylab = "ACF")
pacf(price, main = "PACF Heating Gas Oil Price", xlab = "Lag", ylab = "PACF")
# Box plotting
boxplot(price, col = "orange", main = "Boxplot of Heating Gas Oil Price")
# Create a time series object
ts_data <- ts(price, frequency = 12)
# Decompose the time series
decomposition <- stl(ts_data, s.window = "periodic")
# Plot the decomposed components
plot(decomposition)


# Fitting the time series linear model
price_ts <- ts(price, frequency = 12)
fit_ts <- tslm(price_ts ~ trend + season)
summary(fit_ts)
actual_values <- as.numeric(price_ts)
fitted_values <- fitted(fit_ts)
# Print AIC, RMSE, and conduct Durbin-Watson test
cat("AIC of TSLM:", AIC(fit_ts), "\n")
print(paste("RMSE of TSLM:", sqrt(mean((actual_values - fitted_values)^2))))
dwtest(fit_ts)
# Plotting
plot(price_ts, ylab = "Heating Gas Oil Price", xlab = "Time")
lines(fitted(fit_ts), col = 2)
# Residuals analysis
res <- residuals(fit_ts)
plot(res, main = 'Residuals of Heating Gas Oil')
acf(res, main = "ACF of residuals of Heating Gas Oil")
pacf(res, main = "PACF of residuals of Heating Gas Oil")


# Fitting the ARIMA model
arima_model <- auto.arima(price)
arima_fitted <- fitted(arima_model)
summary(arima_model)
plot(price, type = 'l', main = "ARIMA (0, 1, 1)", ylab = "Price", xlab = "Observations")
lines(arima_fitted, col = 2)
arima_forecast <- forecast(arima_model)
plot(arima_forecast)
lines(fitted(arima_model), col = 2)
arima_residulas <- residuals(arima_model)
tsdisplay(arima_residulas)


# Fitting the SARIMA model
sarima_model <- arima(price, order = c(0, 1, 1), seasonal = list(order = c(0, 1, 1), period = 12))
# Obtain the fitted values
sarima_fitted <- fitted(sarima_model)
# Display summary information about the SARIMA model
summary(sarima_model)
# Plot the original time series and the SARIMA fitted values
plot(price, type = 'l', main = "SARIMA (0, 1, 1)(0, 1, 1)[12]", ylab = "Price", xlab = "Time")
lines(sarima_fitted, col = 2)
# Obtain SARIMA forecast
sarima_forecast <- forecast(sarima_model, h = 12)
# Plot the SARIMA forecast
plot(sarima_forecast)
lines(fitted(sarima_model), col = 2)
# Obtain SARIMA residuals
sarima_residuals <- residuals(sarima_model)
# Display ACF and PACF of residuals
tsdisplay(sarima_residuals)


# Fitting SPLine
time <- 1:length(price)
# Create a spline regression model with cubic splines
degree <- 3  # Degree of the spline
# Calculate data density
data_density <- length(time) / (max(time) - min(time))
# Choose length.out based on data density
length_out <- round(data_density * 10)
knots <- seq(min(time), max(time), length.out = length_out)
spline_model <- lm(price ~ bs(time, df = degree, knots = knots))
# Make predictions for the existing time points
predicted_prices <- predict(spline_model)
# Plot the data, fitted spline regression curve, and predictions
plot(time, type = 'l', price, main = "Spline Regression", xlab = "Observations", ylab = "Heating Gas Oil Price Price", col = "black", lwd = 3)
lines(time, predict(spline_model), col = "red", lwd = 3, type = "l", lty = 1)
points(time, predicted_prices, col = "green")
summary(spline_model)
actual_values <- as.numeric(price)
fitted_values <- fitted(spline_model)
cat("AIC of SPLine:", AIC(spline_model), "\n")
print(paste("RMSE of SPLine:", sqrt(mean((actual_values - fitted_values)^2))))
plot(time, residuals(spline_model), main = "Residuals", col = "black", type = "l")
acf(residuals(spline_model), main = "ACF of Residuals")
pacf(residuals(spline_model), main = "PACF of Residuals")


# Fitting a GAM model
time <- 1:length(price)
gam_model <- gam(price ~ lo(time, span = 0.1), data = data)
predicted_prices <- predict(gam_model)
plot(time, type = "l", price, main = "General Additive Model", xlab = "Observations", ylab = "Heating Gas Oil Price", col = "black", lwd = 3)
lines(time, predict(gam_model), col = "red", lwd = 3, type = "l", lty = 1)
points(time, predicted_prices, col = "green")
# Summary of the GAM model
summary(gam_model)
actual_values <- as.numeric(price)
fitted_values <- fitted(gam_model)
cat("AIC of GAM:", AIC(gam_model), "\n")
print(paste("RMSE of GAM:", sqrt(mean((actual_values - fitted_values)^2))))
plot(time, residuals(gam_model), main = "Residuals", col = "black", type = "l")
acf(residuals(gam_model), main = "ACF of Residuals")
pacf(residuals(gam_model), main = "PACF of Residuals")


#################################################################################################################################################


data <- read.csv(paste0(getwd(), "/dataset/monthly_prices_heavy_fuel_oil_from_1996_to_20240107.csv"))
data$Date <- as.Date(paste0(data$YEAR, "-", data$MONTH_ID, "-01"))
price <- data$PRICE
date <- data$Date
# Plot with Date on the x-axis
plot(date, price, type = "l", xlab = "Date", ylab = "Price", main = "Heavy Fuel Oil Price")
# Plot the ACF and PACF
acf(price, main = "ACF Heavy Fuel Oil Price", xlab = "Lag", ylab = "ACF")
pacf(price, main = "PACF Heavy Fuel Oil Price", xlab = "Lag", ylab = "PACF")
# Box plotting
boxplot(price, col = "orange", main = "Boxplot of Heavy Fuel Oil Price")
# Create a time series object
ts_data <- ts(price, frequency = 12)
# Decompose the time series
decomposition <- stl(ts_data, s.window = "periodic")
# Plot the decomposed components
plot(decomposition)


# Fitting the time series linear model
price_ts <- ts(price, frequency = 12)
fit_ts <- tslm(price_ts ~ trend + season)
summary(fit_ts)
actual_values <- as.numeric(price_ts)
fitted_values <- fitted(fit_ts)
# Print AIC, RMSE, and conduct Durbin-Watson test
cat("AIC of TSLM:", AIC(fit_ts), "\n")
print(paste("RMSE of TSLM:", sqrt(mean((actual_values - fitted_values)^2))))
dwtest(fit_ts)
# Plotting
plot(price_ts, ylab = "Heating Gas Oil Price", xlab = "Time")
lines(fitted(fit_ts), col = 2)
# Residuals analysis
res <- residuals(fit_ts)
plot(res, main = 'Residuals of Heavy Fuel Oil')
acf(res, main = "ACF of residuals of Heavy Fuel Oil")
pacf(res, main = "PACF of residuals of Heavy Fuel Oil")


# Fitting the ARIMA model
arima_model <- auto.arima(price)
arima_fitted <- fitted(arima_model)
summary(arima_model)
plot(price, type = 'l', main = "ARIMA (0,1,1)", ylab = "Price", xlab = "Observations")
lines(arima_fitted, col = 2)
arima_forecast <- forecast(arima_model, h = 12)
plot(arima_forecast)
lines(fitted(arima_model), col = 2)
arima_residulas <- residuals(arima_model)
tsdisplay(arima_residulas)


# Fitting the SARIMA model
sarima_model <- arima(price, order = c(0, 1, 1), seasonal = list(order = c(0, 1, 1), period = 12))
# Obtain the fitted values
sarima_fitted <- fitted(sarima_model)
# Display summary information about the SARIMA model
summary(sarima_model)
# Plot the original time series and the SARIMA fitted values
plot(price, type = 'l', main = "SARIMA (0,1,1)(0,1,1)[12]", ylab = "Price", xlab = "Time")
lines(sarima_fitted, col = 2)
# Obtain SARIMA forecast
sarima_forecast <- forecast(sarima_model, h = 12)
# Plot the SARIMA forecast
plot(sarima_forecast)
lines(fitted(sarima_model), col = 2)
# Obtain SARIMA residuals
sarima_residuals <- residuals(sarima_model)
# Display ACF and PACF of residuals
tsdisplay(sarima_residuals)


# Fitting SPLine
time <- 1:length(price)
# Create a spline regression model with cubic splines
degree <- 3  # Degree of the spline
# Calculate data density
data_density <- length(time) / (max(time) - min(time))
# Choose length.out based on data density
length_out <- round(data_density * 10)
knots <- seq(min(time), max(time), length.out = length_out)
spline_model <- lm(price ~ bs(time, df = degree, knots = knots))
# Make predictions for the existing time points
predicted_prices <- predict(spline_model)
# Plot the data, fitted spline regression curve, and predictions
plot(time, type = 'l', price, main = "Spline Regression", xlab = "Observations", ylab = "Heavy Fuel Oil Price", col = "orange", lwd = 3)
lines(time, predict(spline_model), col = "purple", lwd = 3, type = "l", lty = 1)
points(time, predicted_prices, col = "green")
summary(spline_model)
actual_values <- as.numeric(price)
fitted_values <- fitted(spline_model)
cat("AIC of SPLine:", AIC(spline_model), "\n")
print(paste("RMSE of SPLine:", sqrt(mean((actual_values - fitted_values)^2))))
plot(time, residuals(spline_model), main = "Residuals", col = "black", type = "l")
acf(residuals(spline_model), main = "ACF of Residuals")
pacf(residuals(spline_model), main = "PACF of Residuals")


# Fitting a GAM model
time <- 1:length(price)
gam_model <- gam(price ~ lo(time, span = 0.1), data = data)
predicted_prices <- predict(gam_model)
plot(time, type = "l", price, main = "General Additive Model", xlab = "Observations", ylab = "Heavy Fuel Oil Price", col = "orange", lwd = 3)
lines(time, predict(gam_model), col = "purple", lwd = 3, type = "l", lty = 1)
points(time, predicted_prices, col = "green")
# Summary of the GAM model
summary(gam_model)
actual_values <- as.numeric(price)
fitted_values <- fitted(gam_model)
cat("AIC of GAM:", AIC(gam_model), "\n")
print(paste("RMSE of GAM:", sqrt(mean((actual_values - fitted_values)^2))))
plot(time, residuals(gam_model), main = "Residuals", col = "black", type = "l")
acf(residuals(gam_model), main = "ACF of Residuals")
pacf(residuals(gam_model), main = "PACF of Residuals")
