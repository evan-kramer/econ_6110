# ECON 6110 - Assignment 4
# Evan Kramer
# 4/16/2019

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
glencore = getSymbols("GLNCY", src = "yahoo", auto.assign = F)
bhp = getSymbols("BBL", src = "yahoo", auto.assign = F)
vale = getSymbols("VALE", src = "yahoo", auto.assign = F)
rio = getSymbols("RTNTF", src = "yahoo", auto.assign = F)
iron_ore = getSymbols("PIORECRUSDM", src = "FRED", auto.assign = F)
gold = getSymbols("GOLDPMGBD228NLBM", src = "FRED", auto.assign = F)
silver = getSymbols("SLVPRUSD", src = "FRED", auto.assign = F)
oil_wti = getSymbols("DCOILWTICO", src = "FRED", auto.assign = F)
natural_gas = getSymbols("MHHNGSP", src = "FRED", auto.assign = F)
oil_brent = getSymbols("DCOILBRENTEU", src = "FRED", auto.assign = F)
heating_oil = getSymbols("DHOILNYH", src = "FRED", auto.assign = F)
aluminum = getSymbols("PALUMUSDM", src = "FRED", auto.assign = F)
copper = getSymbols("PCOPPUSDM", src = "FRED", auto.assign = F)
nickel = getSymbols("PNICKUSDM", src = "FRED", auto.assign = F)
zinc = getSymbols("PZINCUSDM", src = "FRED", auto.assign = F)

# Plot and save data
for(s in c("PIORECRUSDM", "GOLDPMGBD228NLBM", "SLVPRUSD", "PALUMUSDM", "PCOPPUSDM", "PNICKUSDM", "PZINCUSDM")) {
  png(
    file = str_c(getwd(), "/Assignments/Assignment 4/", s, ".png"),
    width = 8,
    height = 5,
    units = "in",
    res = 300
  ) 
  getSymbols(s, src = "FRED", auto.assign = F) %>% 
    plot(main = s) %>% 
    print()
  dev.off()
}

fortify(iron_ore) %>% 
  ggplot(aes(x = Index, y = PIORECRUSDM)) + 
  geom_line() + 
  theme_bw() + 
  ggtitle(str_c("Price of Iron Ore, ", min(year(ymd(index(iron_ore)))), "-", max(year(ymd(index(iron_ore)))))) + 
  ylab("Price of Iron Ore") + 
  xlab("Year")
ggsave("Assignments/Assignment 4/iron_ore.png", width = 8, units = "in")

# ARIMA model
model = auto.arima(iron_ore)

# Test model fit
# ACF and PACF
acf(iron_ore)
pacf(iron_ore)

# Plot residuals
ggplot(
  data = NULL,
  aes(
    x = ymd(index(iron_ore)),
    y = model$residuals
  )
) + 
  geom_point(alpha = 0.3) + 
  geom_hline(yintercept = 0, color = "red") + 
  theme_bw() + 
  scale_x_date(name = "") + 
  scale_y_continuous(name = "") + 
  ggtitle("Residuals from ARIMA(1,1,1) Model")
ggsave("Assignments/Assignment 4/iron_ore_residuals.png", width = 8, units = "in")

# Plot predictions
forecast(model, h = 120)
png(
  file = "Assignments/Assignment 4/iron_ore_predictions.png",
  width = 8,
  height = 5,
  units = "in",
  res = 300
) 
plot(forecast(model, h = 120))
dev.off()

# What's happening to their stock prices?
inner_join(
  fortify(bhp),
  fortify(glencore), 
  by = "Index"
) %>% 
  inner_join(
    fortify(vale), by = "Index"
  ) %>% 
  inner_join(
    fortify(rio), by = "Index"
  ) %>% 
  select(date = Index, ends_with(".Close")) %>% 
  as.tbl() %>% 
  ggplot(aes(x = date)) + 
  geom_line(aes(y = BBL.Close, color = "BHP Billiton")) + 
  geom_line(aes(y = GLNCY.Close, color = "Glencore")) + 
  geom_line(aes(y = VALE.Close, color = "Vale")) + 
  geom_line(aes(y = RTNTF.Close, color = "Rio Tinto")) +
  theme_bw() +
  scale_color_discrete(name = "") + 
  scale_y_continuous(name = "Close Price") + 
  scale_x_date(name = "Year")
ggsave("Assignments/Assignment 4/mining_stock_prices.png", width = 8, units = "in")

# What's happening to their earnings?
