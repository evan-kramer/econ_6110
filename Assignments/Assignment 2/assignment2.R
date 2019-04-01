# ECON 6110 - Assignment 2
# Evan Kramer
# 2/25/2019

# Set up
options(java.parameters = "-Xmx16G")
library(tidyverse)
library(lubridate)
library(haven)
library(RJDBC)
library(lmtest)
library(quantmod)
library(forecast)
library(xts)
library(zoo)
library(tseries)
setwd("C:/Users/CA19130/Documents/Projects/ECON 6110/")

# Data
gold = getSymbols("GOLDPMGBD228NLBM", src = "FRED", auto.assign = F)
djia = getSymbols("DJIA", src = "FRED", auto.assign = F)
twexb = getSymbols("TWEXB", src = "FRED", auto.assign = F)
infl = getSymbols("T10YIE", src = "FRED", auto.assign = F)
intr = getSymbols("FEDFUNDS", src = "FRED", auto.assign = F)
data = merge.xts(gold, djia, join = "inner") %>% 
  merge.xts(infl, join = "left") %>% 
  merge.xts(twexb, join = "left") %>% 
  merge.xts(intr, join = "left") 
for(n in c("TWEXB", "FEDFUNDS")) {
  data[, n] = dendextend::na_locf(data[, n])
}
plot_data = data[complete.cases(data) & ymd(index(data)) >= today() - dyears(3)]

# Plot data
head(gold)
plot(gold)
plot(gold[ymd(index(gold)) >= today() - dyears(3)])
ggplot(
  data = NULL,
  aes(
    # x = ymd(index(gold))[!is.na(gold$GOLDPMGBD228NLBM)],
    # y = gold$GOLDPMGBD228NLBM[!is.na(gold$GOLDPMGBD228NLBM)]
    x = ymd(index(gold))[ymd(index(gold)) >= today() - dyears(3) & !is.na(gold$GOLDPMGBD228NLBM)],
    y = gold$GOLDPMGBD228NLBM[ymd(index(gold)) >= today() - dyears(3) & !is.na(gold$GOLDPMGBD228NLBM)]
  )
) + 
  geom_line() + 
  scale_x_date(name = "Time") +
  scale_y_continuous(name = "Gold Price") +
  theme_bw() + 
  ggtitle(str_c("Price of Gold in USD, ", 
              min(year(ymd(index(gold))[ymd(index(gold)) >= today() - dyears(3) & !is.na(gold$GOLDPMGBD228NLBM)])),
              "-",
              max(year(ymd(index(gold))[ymd(index(gold)) >= today() - dyears(3) & !is.na(gold$GOLDPMGBD228NLBM)]))))

# Plot against other indices
ggplot(
  data = plot_data,
  aes(
    # x = ymd(index(na.omit(data)))
  )
) + 
  # Gold
  geom_line(aes(x = ymd(index(plot_data)),
                y = GOLDPMGBD228NLBM,
                color = "Gold")
            ) + 
  # DJIA
  geom_line(aes(x = ymd(index(plot_data)),
                y = DJIA / 10,
                color = "DJIA (divided by 10)")
            ) +
  # Inflation
  geom_line(aes(x = ymd(index(plot_data)),
                y = T10YIE * 1000,
                color = "Inflation (multiplied by 1000)")
            ) +
  # Trade-weighted exchange rate
  geom_line(aes(x = ymd(index(plot_data)),
                y = TWEXB * 10,
                color = "Trade-weighted USD index (multiplied by 10)")
            ) +
  # Interest rates
  geom_line(aes(x = ymd(index(plot_data)),
                y = FEDFUNDS * 1000,
                color = "Interest rates (multiplied by 1000)")
            ) + 
  theme_bw() + 
  scale_color_discrete(name = "Index") + 
  scale_x_date(name = "") + 
  scale_y_continuous(name = "")

# Explore data
# Dickey-Fuller test: Is the model stationary? Can solve by differencing data if needed
adf.test(gold[!is.na(gold)], alternative = "stationary") 
adf.test(na.omit(diff(gold, differences = 1)), alternative = "stationary")
adf.test(na.omit(diff(plot_data[,1], differences = 1)), alternative = "stationary")

# Autocorrelation function
acf(na.omit(gold))
acf(gold[!is.na(gold$GOLDPMGBD228NLBM) & ymd(index(gold)) >= today() - dyears(3)])
# Partial autocorrelation function
pacf(na.omit(gold))
pacf(gold[!is.na(gold$GOLDPMGBD228NLBM) & ymd(index(gold)) >= today() - dyears(3)])

# Fit model
# Auto ARIMAs 
auto.arima(na.omit(gold)) # ARIMA(1,1,2)
auto.arima(gold[!is.na(gold$GOLDPMGBD228NLBM) & ymd(index(gold)) >= today() - dyears(3)]) # ARIMA(1,0,0)
auto.arima(tsclean(gold))
auto.arima(plot_data[,1]) # ARIMA(1,0,0)
auto.arima(diff(log(plot_data[,1]))) # ARIMA(0,0,0)

# Model over last forty years
model1 = arima(na.omit(gold), order = c(1,1,2))
# Model over last three years
model2 = arima(gold[!is.na(gold$GOLDPMGBD228NLBM) & ymd(index(gold)) >= today() - dyears(3)]) 

# Test model fit
# Plot residuals
ggplot(
  data = NULL,
  aes(
    x = ymd(index(plot_data)),
    y = arima(plot_data[, 1], order = c(1,0,0))$residuals
  )
) + 
  geom_point(alpha = 0.3) + 
  geom_hline(yintercept = 0, color = "red") + 
  theme_bw() + 
  scale_x_date(name = "") + 
  scale_y_continuous(name = "") + 
  ggtitle("Residuals from ARIMA(1,0,0) Model")

# Median absolute deviation
sum(abs(arima(plot_data[, 1], order = c(1,0,0))$residuals)) / 
  length(arima(plot_data[, 1], order = c(1,0,0))$residuals)
# acf
# pacf
# boxjenkins
# Replot model with predictions

# Plot predictions
predictions = forecast(arima(plot_data[, 1], order = c(1,0,0)), h = 50)
plot(forecast(arima(plot_data[, 1], order = c(1,0,0)), h = 50))

# Predict average price of gold for the week of March 4-8
predict(arima(plot_data[, 1], order = c(1,0,0)))
predict(arima(plot_data[, 1], order = c(1,0,0)), n.ahead = 10)
arima(plot_data[, 1], order = c(1,0,0))$residuals
index(plot_data) + 10
predict(arima(plot_data[, 1], order = c(1,0,0)), n.ahead = 10)$pred %>% as.numeric()

# Provide an understanding of the overall global macroeconomic overview with specific focus on gold
# How would you determine your model is a robust one? 

# Output data file
write.csv(plot_data, "Assignments/Assignment 2/gold_data.csv", row.names = index(plot_data))
