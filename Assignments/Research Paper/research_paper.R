# ECON 6110 - Assignment 3
# Evan Kramer
# 4/22/2019

# Set up
options(java.parameters = "-Xmx16G")
library(tidyverse)
library(lubridate)
library(haven)
library(RSocrata)
library(RJDBC)
setwd("C:/Users/CA19130/Documents/Projects/ECON 6110/")

# Config
data = T
analysis = F
visuals = F

# Research questions
# What is the impact of per-pupil funding on student growth? 

# Data
if(data) {
  # Aggregate data 
  
  
  # Add multiple years of data 

  df = as.tbl(
    # District directory
    dbGetQuery(
      dbConnect(
        JDBC("oracle.jdbc.OracleDriver", classPath="C:/Users/CA19130/Downloads/ojdbc6.jar"),
        readRegistry("Environment", hive = "HCU")$SDE_DIR_CXN_STR[1],
        "SDE_DIR",
        readRegistry("Environment", hive = "HCU")$SDE_DIR_PWD[1]
      ),
      "select 
        district_number, 
        --bu_name as district_name, 
        --street_number || ' ' || street_name || ' ' || street_type_id as street, 
        --city, 
        zip
      from district
      join business_unit using (bu_id)
      join bu_address using (bu_id)
      where district_type_id < 999 and address_type_id = 6 and status = 'A' and district_number < 990
      order by district_number"
    )
  ) %>% 
    janitor::clean_names() %>% 
    mutate_all(as.integer) %>% 
    # County tax rates
    inner_join(
      read_csv("Assignments/Research Paper/TAXRATES_ZIP5_TN201904.csv") %>% 
        janitor::clean_names() %>% 
        select(zip_code, state_rate:estimated_city_rate),
      by = c("zip" = "zip_code")
    ) %>% 
    # District spending 
    inner_join(
      openxlsx::read.xlsx("https://www.tn.gov/content/dam/tn/education/data/profile/district_profile_2017-18.xlsx") %>% 
      janitor::clean_names() %>% 
      select(starts_with("district"), adm = average_daily_membership, ends_with("_pct"),
           administrators:state_state_funding_pct),
      by = c("district_number" = "district")
    ) %>% 
    # Amount of academic growth
    inner_join(
      readxl::read_excel("N:/ORP_accountability/data/2018_tvaas/District Composite Index.xlsx") %>% 
        janitor::clean_names() %>% 
        transmute(district_number = as.numeric(district_number), tvaas = system_wide_composite),
      by = "district_number"
    ) %>% 
    # Proficiency rate
    inner_join(
      read_csv("N:/ORP_accountability/data/2018_final_accountability_files/district_assessment_file_suppressed.csv") %>% 
        filter(test != "MSAA/Alt-Science/Social Studies" & grade == "All Grades" & subgroup == "All Students") %>% 
        mutate_at(vars(n_below:pct_on_mastered), "as.integer") %>% 
        group_by(year, system) %>% 
        summarize(proficiency_rate = round(100 * sum(n_on_track + n_mastered, na.rm = T) / sum(valid_tests, na.rm = T), 1)) %>% 
        ungroup(),
      by = c("district_number" = "system")
    ) 
    
  # Enrollment
  # Number of schools
  # Per-pupil expenditures
  # Urbanicity-rurality
  
  # County-level economic indicators (% with bachelors degree, % unemployment, median income, etc.)
} else {
  rm(data)
}

# Analysis
if(analysis) {
  # Model to predict impact of tax rates on per-pupil funding?
  lm(per_pupil_expenditures_per_ada ~ estimated_combined_rate, df) %>% 
    summary()
  
  # Model to predict performance based on per-pupil funding
  lm(tvaas ~ per_pupil_expenditures_per_ada, df) %>% 
    summary()
} else {
  rm(analysis)
}

# Visualizations
if(visuals) {
  
} else {
  rm(visuals)
}
