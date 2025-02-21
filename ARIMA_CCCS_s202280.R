# Install necessary packages
if (!require(quantmod)) install.packages("quantmod") # pakiet do ładowania danych
if (!require(forecast)) install.packages("forecast")
if (!require(tseries)) install.packages("tseries")

library(quantmod)
library(forecast)
library(tseries)
library(ggplot2)
library(dplyr)

### 1st STEP - DATA PREP 

# download stock data from YAHOO - CCCS
symbol <- "CCCS"
start_date <- "2021-02-08" # Monday (stock is open) 
end_date <- "2025-02-05" # End date -1 so accordingly to requirements is 2025-02-04
getSymbols(symbol, src = "yahoo", from = start_date, to = end_date)
ohlc_data <- get(symbol)

# extract stock closing prices
stock_prices <- CCCS$CCCS.Close 
sp <- as.numeric(stock_prices)

# check no. of observations
nrow(stock_prices)

# prepare time series data 
CCCS_ts <- ts(stock_prices, start = c(2021, 2), frequency = 252)

# plot the time series
plot(CCCS_ts, main = "Ceny zamknięcia akcji CCCS (2021-02-08 do 2025-02-04)", xlab = "Data", ylab = "Cena zamknięcia", col = "#900C3F")

# stationary test 
adf.test(CCCS_ts, alternative = "stationary") # ts not stationary (p-value > 0.05)
pp.test(CCCS_ts) #  ts not stationary (p-value > 0.05)

# partial autocorrelation test 
pacf(CCCS_ts, lag.max = 20, plot = T, main = "PACF dla CCCS")

# autocorrelation test 
acf(CCCS_ts, lag.max = 20, plot = T, main = "ACF dla CCCS")

### 2nd STEP - ARIMA MODEL

# fit an ARIMA model (automated selection)
fit <- auto.arima(stock_prices)
autoplot(fit)

# check the model summary
summary(fit) 

#p (autoregressive, AR): liczba opóźnionych wartości szeregowych używanych do przewidywania aktualnej wartości.
#d (differencing): liczba przekształceń różnicowych, które zostały zastosowane do szeregów czasowych w celu osiągnięcia stacjonarności.
#q (moving average, MA): liczba składników średniej ruchomej, które reprezentują wpływ błędów (szumów) w przeszłości na bieżącą wartość.
#MA błędy skorelowane ; RM - błędy skorelowane wcześniejsze

# extract AR and MA coefficients
ar_coeffs <- coef(fit)[grep("^ar", names(coef(fit)))] # AR coefficients
ma_coeffs <- coef(fit)[grep("^ma", names(coef(fit)))] # MA coefficients
sma_coeffs <- coef(fit)[grep("^sma", names(coef(fit)))] #SMA coefficients

# check MA
if (length(ma_coeffs) > 0) {
  ma_poly <- c(1, -ma_coeffs) # Non-seasonal MA polynomial
  ma_roots <- polyroot(ma_poly)
  ma_inverse_roots <- abs(1 / ma_roots)
  cat("Non-seasonal MA Inverse Roots:\n", ma_inverse_roots, "\n")
  if (all(ma_inverse_roots < 1)) {
    cat("The non-seasonal MA component is invertible.\n")
  } else {
    cat("The non-seasonal MA component is not invertible.\n")
  }
} else {
  cat("No non-seasonal MA terms in the model.\n")
}

# check seasonal MA roots
if (length(sma_coeffs) > 0) {
  sma_poly <- c(1, -sma_coeffs) # Seasonal MA polynomial
  sma_roots <- polyroot(sma_poly)
  sma_inverse_roots <- abs(1 / sma_roots)
  cat("Seasonal MA Inverse Roots:\n", sma_inverse_roots, "\n")
  if (all(sma_inverse_roots < 1)) {
    cat("The seasonal MA component is invertible.\n")
  } else {
    cat("The seasonal MA component is not invertible.\n")
  }
} else {
  cat("No seasonal MA terms in the model.\n")
}


# check MA roots for invertibility
if (length(ma_coeffs) > 0) {
  ma_poly <- c(1, -ma_coeffs) # MA polynomial
  ma_roots <- polyroot(ma_poly)
  ma_inverse_roots <- abs(1 / ma_roots)
  cat("MA Inverse Roots:\n", ma_inverse_roots, "\n")
  if (all(ma_inverse_roots < 1)) {
    cat("The MA component is invertible.\n")
  } else {
    cat("The MA component is not invertible.\n")
  }
} else {
  cat("No MA terms in the model.\n")
}

# despite not existing MA component (basing on previous tests) I wanted to confirm it via provided code 

# check AR roots for stationarity
if (length(ar_coeffs) > 0) {
  ar_poly <- c(1, -ar_coeffs) # AR polynomial
  ar_roots <- polyroot(ar_poly)
  ar_inverse_roots <- abs(1 / ar_roots)
  cat("AR Inverse Roots:\n", ar_inverse_roots, "\n")
  if (all(ar_inverse_roots < 1)) {
    cat("The AR component is stationary.\n")
  } else {
    cat("The AR component is not stationary.\n")
  }
} else {
  cat("No AR terms in the model.\n")
}

# check residual diagnostics
checkresiduals(fit)

# check manual combinations for ARIMA 
fit1 <- Arima(stock_prices, order = c(0, 1, 2))  # ARIMA(0,1,2)
AIC(fit1)
BIC(fit1)

fit2 <- Arima(stock_prices, order = c(2, 1, 0))  # ARIMA(2,1,0)
AIC(fit2)
BIC(fit2)

fit3 <- Arima(stock_prices, order = c(1, 1, 2))  # ARIMA(1,1,2)
AIC(fit3)
BIC(fit3)

fit4 <- Arima(stock_prices, order = c(1, 1, 1))  # ARIMA(1,1,1)
AIC(fit4)
BIC(fit4)

### 3rd STEP - FORECASTING FOR NEXT 3 DAYS

# display forecasted values
forecasted <- forecast(fit, h = 3)
print(forecasted)

# Extract the last 17 days of data
last_17_days <- tail(sp, 17)

# Extract dates for plotting
last_17_dates <- tail(index(ohlc_data), 17)
forecast_dates <- seq(from = as.Date(last(last_17_dates)) + 1, by = "day", length.out = 3)
all_dates <- c(last_17_dates, forecast_dates)

# create a combined series for plotting
values <- c(last_17_days, forecasted$mean)

# plot the last 17 days
plot(all_dates, values, type = "o", col = "#900C3F", lwd = 1.5, pch = 16,
     main = "Last 17 Days and 3-Day Forecast for CCCS",
     xlab = " ", ylab = "Price", xaxt = "n")

# add x-axis with formatted dates
axis(1, at = all_dates, labels = format(all_dates, "%Y-%m-%d"), las = 2, cex.axis = 0.7)

# highlight the forecasted points in blue
lines(forecast_dates, forecasted$mean, col = "navyblue", type = "o", lwd = 1.5, pch = 16)

# add a legend
legend("topleft", legend = c("Last 17 Days", "3-Day Forecast"), 
       col = c("#900C3F", "navyblue"), lty = 1, lwd = 2, pch = 16, bty = "o", inset = 0.02, 
       cex = 0.8, x.intersp = 0.5, y.intersp = 0.8)

### 4th STEP - CHECK PREDICTION ACCURACY

symbol <- "CCCS"
start_date <- "2021-02-08" # Monday (stock is open) 
end_date <- "2025-02-08" # End date -1
getSymbols(symbol, src = "yahoo", from = start_date, to = end_date)
ohlc_data1 <- get(symbol)

# extract stock closing prices
stock_prices1 <- CCCS$CCCS.Close 
sp1 <- as.numeric(stock_prices1)

stock_prices1 %>%
  filter(Date > as.Date("2025-02-04"))

# check no. of observations
nrow(stock_prices1)

# prepare time series data 
CCCS_ts1 <- ts(stock_prices1, start = c(2021, 2), frequency = 252)

# plot the time series
plot(CCCS_ts1, main = "Ceny zamknięcia akcji CCCS (2021-02-08 do 2025-02-07)", xlab = "Data", ylab = "Cena zamknięcia", col = "#900C3F")

# stationary test 
adf.test(CCCS_ts1, alternative = "stationary") # ts not stationary (p-value > 0.05)
pp.test(CCCS_ts1) #  ts not stationary (p-value > 0.05)

# partial autocorrelation test 
pacf(CCCS_ts1, lag.max = 20, plot = T, main = "PACF dla CCCS")

# autocorrelation test 
acf(CCCS_ts1, lag.max = 20, plot = T, main = "ACF dla CCCS")

# fit an ARIMA model (automated selection)
fit1 <- auto.arima(stock_prices1)
autoplot(fit1)

# check the model summary
summary(fit1) 

# Extract the last 17 days of data
last_17_days <- tail(sp1, 17)

# Extract dates for plotting
last_17_dates1 <- tail(index(ohlc_data1), 17)
forecast_dates1 <- seq(from = as.Date(last(last_17_dates1)), by = "day", length.out = 3)
values1 <- c(last_17_days1, forecasted$mean)

# extract close proces from the last 20 days
stock_prices1 <- data.frame(Date = index(ohlc_data1), Close = as.numeric(ohlc_data1$CCCS.Close))
last_20_days <- last_20_days %>%
  mutate(Highlight = ifelse(Date %in% specific_dates, TRUE, FALSE),
         Rounded_Close = ifelse(Highlight, round(Close, 2), NA))

# adjust forecasted days for the plot
specific_dates <- as.Date(c("2025-02-05", "2025-02-06", "2025-02-07"))

# plot creation
ggplot(last_20_days, aes(x = Date, y = Close)) +
  geom_line(color = "#900C3F", size =0.55) +                           
  geom_point(aes(color = Date %in% specific_dates), size = 2) + 
  geom_text(data = last_20_days %>% filter(Highlight),
            aes(label = Rounded_Close), angle = 90, vjust = 0.5, hjust = -0.35, size = 3, color = "navyblue") +# Highlight specific points
  scale_color_manual(values = c("#900C3F", "navyblue"),
                     labels = c("real price", "real value fcst price")) +
  scale_x_date(breaks = last_20_days$Date, date_labels = "%Y-%m-%d") +
  labs(title = "Ceny zamknięcia akcji CCCS w ostatnich 20 dniach",
       x = "Data",
       y = "Cena zamknięcia",
       color = " ") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Forecasted and actual data (example)
forecasted <- c(10.95935, 10.95937, 10.95937)  # Forecasted values
actual <- c(11.1, 10.8, 10.7)                 # Actual values

# Calculate errors
errors <- actual - forecasted                 # Errors (actual - forecasted)
absolute_errors <- abs(errors)                # Absolute errors

# Calculate average error metrics
mae <- mean(absolute_errors)                  # Mean Absolute Error (MAE)
mape <- mean(absolute_errors / actual) * 100  # Mean Absolute Percentage Error (MAPE)

# Display results
cat("Mean Absolute Error (MAE):", round(mae, 4), "\n")
cat("Mean Absolute Percentage Error (MAPE):", round(mape, 2), "%\n")
