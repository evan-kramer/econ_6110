# ECON 6110 - Assignment 3
# Evan Kramer
# 4/1/2019

# Set up
options(java.parameters = "-Xmx16G")
library(tidyverse)
library(lubridate)
library(haven)
library(RSocrata)
setwd("C:/Users/CA19130/Documents/Projects/ECON 6110/")

# Config
data = F
clean = F
analysis = F
visuals = F
if(!"pwd" %in% ls()) {
  # em = rstudioapi::askForPassword("Email:")
  # pwd = rstudioapi::askForPassword("Password:")  
}

# Data
if(data) {
  (adm = as.tbl(openxlsx::read.xlsx("https://www.tn.gov/content/dam/tn/education/data/profile/district_profile_2017-18.xlsx")) %>% 
    janitor::clean_names()) %>% 
    select(starts_with("district"), adm = average_daily_membership, ends_with("_pct"),
           administrators:state_state_funding_pct)
  
  
  # Enrollment
  # Number of schools
  # Per-pupil expenditures
  # Amount of academic growth
  # Proficiency rate
  # Urbanicity-rurality
  # County-level economic indicators (% with bachelors degree, % unemployment, median income, etc.)
} else {
  rm(data)
}

# Clean
if(clean) {
  
} else {
  rm(clean)
}

# Analysis
if(analysis) {
  # Model to predict crashes
  summary(lm(number_of_injuries ~ number_of_motor_vehicles + weather + collision_type + illumination + precinct,
             crashes))
  
} else {
  rm(analysis)
}

# Visualizations
if(visuals) {
  
} else {
  rm(visuals)
}
