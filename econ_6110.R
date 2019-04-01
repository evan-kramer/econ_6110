# ECON 6110
# Evan Kramer
# 1/28/2019

# Set up
options(java.parameters = "-Xmx16G")
library(tidyverse)
library(lubridate)
library(haven)
library(RJDBC)
setwd("C:/Users/CA19130/Documents/Projects/ECON 6110/")

# 1/28/2019
demand = readxl::read_excel("Assignments/Econ 6110- Applied demand analysis 1.1.xlsx",
                            skip = 1) %>% 
  janitor::clean_names() %>% 
  mutate_all(funs(as.numeric)) %>% 
  filter(!is.na(fish_demand))

# Where fish demand reflects the per pound fish consumption by locals (annual)
# Q1 What is average per capita consumption of fish and chicken?
mean(demand$fish_demand, na.rm = T)
mean(demand$price, na.rm = T)
mean(demand$p_of_chi, na.rm = T)
mean(demand$income, na.rm = T)
model = lm(data = demand, fish_demand ~ price + p_of_chi + income) 

# Q2 What are the estimated elasticities of demand for fish?
# Own-price elasticity (Coefficient * mean(price) / mean(quantity))
coefficients(model)[2] * mean(demand$price, na.rm = T) / 
  mean(demand$fish_demand, na.rm = T) 
# Cross-price elasticity (chicken)
coefficients(model)[3] * mean(demand$p_of_chi, na.rm = T) / 
  mean(demand$fish_demand, na.rm = T)
# Income elasticity
coefficients(model)[4] * mean(demand$income, na.rm = T) / 
  mean(demand$fish_demand, na.rm = T)

# Q3 Test the hypothesis that price of Chicken is not an important factor in determining the demand for Fish
# Q4 Test the hypothesis that as income increases so does the consumption of fish
# Q5 Test the overall significance of the model

# 2/11/2019

# 2/18/2019 - Time Series Data
data = readxl::read_excel("Assignments/Assignment 2/data.xlsx") %>% 
  rename(money_supply = m2)

# Plot data
plot(data$money_supply)
plot(data$interest)
plot(data$deficit)

# Correlations between errors 
#forecasting -- autocorrelations -- bartlett's approximation
acf(data$money_supply)
pacf(data$money_supply)
acf(data$interest)
pacf(data$interest)
acf(data$deficit)
pacf(data$deficit)

# Step 0: Declare data to be time-series?
# Step 1: Identify the number of lags
# Step 2: Fit ARIMA model
model = arima(data$money_supply, c(1,0,0))
model2 = arima(data$money_supply, c(1,1,1))

# Step 3: Predict values
data$residuals = model$residuals
data$residuals2 = model2$residuals

# Step 4: Mean squared error
mean(abs(data$residuals), na.rm = T)
mean(abs(data$residuals2), na.rm = T)

# Plot predicted values
ggplot(mutate(data, n = row_number()),
       # aes(x = n, y = money_supply)) +
       aes(x = n, y = residuals2)) +
  # geom_line() +
  geom_point() 

# What is the difference between autocorrelation function and partial autocorrelation function?

# 3/18/2019
# Log data
# Run regression
# Use coefficients to compute production and cost functions

