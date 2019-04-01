# ECON 6110 - Assignment 3
# Evan Kramer
# 3/31/2019

# Set up
options(java.parameters = "-Xmx16G")
library(tidyverse)
library(lubridate)
library(haven)
setwd("C:/Users/CA19130/Documents/Projects/ECON 6110/")

# Data
production = readxl::read_excel("Assignments/Assignment 3/Production data- Spring 2019.xlsx", 
                                sheet = 2, skip = 1, n_max = 34) %>% 
  janitor::clean_names() %>% 
  mutate_all(log) 

# Regression
model = lm(output ~ ., production)
summary(model)
sum(model$coefficients)

# Estimate output elasticities of the inputs. Do you believe these estimates?
model$coefficients

# What is the returns-to-scale in this industry?
case_when(
  sum(model$coefficients) == 1 ~ "constant",
  sum(model$coefficients) > 1 ~ "increasing",
  sum(model$coefficients) < 1 ~ "decreasing"
)

# What might be the shape of the long-run average cost curve? 
  # When is output expected to grow 10% per year for the next three years?
  # May want to use average values of IVs as starting point
  # Assuming one can buy as much skilled labor as possible at $40/hr and unskilled at $15/hr
  # Also prices of raw material is $20/board-ft, $200/unit of energy
# Suppose this industry is being considered for a takeover by a foreign company.

  # What would the investors likely be doing in terms of better profitability and why? 
