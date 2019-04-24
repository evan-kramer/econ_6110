# ECON 6110 - Research Paper
# Evan Kramer
# 4/23/2019

# Set up
options(java.parameters = "-Xmx16G")
library(tidyverse)
library(lubridate)
library(haven)
library(RSocrata)
library(RJDBC)
library(plm)
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
  df = bind_rows(
    # TVAAS
    # 2018
    openxlsx::read.xlsx("https://www.tn.gov/content/dam/tn/education/data/tvaas/data_2018_TVAAS_District_Composite.xlsx") %>% 
      transmute(year = 2018, system = District.Number, system_name = District.Name, tvaas_composite = Overall.Composite),
    # 2017
    openxlsx::read.xlsx("https://www.tn.gov/content/dam/tn/education/data/tvaas/TVAAS_District_Composites_20171.xlsx") %>% 
      transmute(year = 2017, system = District.Number, tvaas_composite = Overall.Composite),
    # 2016
    openxlsx::read.xlsx("https://www.tn.gov/content/dam/tn/education/data/tvaas/data_district_wide_tvaas_2016.xlsx") %>% 
      transmute(year = 2016, system = District.Number, tvaas_composite = `District-Wide:.Composite`),
    # 2015
    openxlsx::read.xlsx("https://www.tn.gov/content/dam/tn/education/data/tvaas/data_district_wide_tvaas_2015.xlsx") %>% 
      transmute(year = 2015, system = District.Number, tvaas_composite = `District-Wide:.Composite`)
  ) %>% 
    arrange(system, desc(year)) %>% 
    mutate(system_name = dendextend::na_locf(system_name),
           system = as.numeric(system)) %>%
    arrange(year, system) %>% 
    as.tbl() %>% 
    # Achievement data
    inner_join(
      bind_rows(
        # 2018
        read_csv("https://www.tn.gov/content/dam/tn/education/data/data_2018_district_base.csv") %>% 
          filter(test %in% c("EOC", "TNReady") & subgroup == "All Students" & grade == "All Grades") %>% 
          group_by(year, system) %>% 
          summarize(proficiency_rate = round(100 * sum(as.numeric(n_on_track) + as.numeric(n_mastered), na.rm = T) / 
                                               sum(valid_tests, na.rm = T), 1)) %>% 
          ungroup(),
        # 2017
        read_csv("https://www.tn.gov/content/dam/tn/education/data/data_2017_district_base.csv") %>% 
          filter(subgroup == "All Students" & grade == "All Grades") %>% 
          group_by(year, system) %>% 
          summarize(proficiency_rate = round(100 * sum(as.numeric(n_on_track) + as.numeric(n_mastered), na.rm = T) / 
                                               sum(valid_tests, na.rm = T), 1)) %>% 
          ungroup(),
        # 2016
        openxlsx::read.xlsx("https://www.tn.gov/content/dam/tn/education/data/data_2016_suppressed_district_base.xlsx") %>%
          filter(Subgroup == "All Students") %>% 
          group_by(Year, District) %>% 
          summarize(proficiency_rate = round(100 * sum(as.numeric(`#.On.Track.(Prev..Proficient)`) + 
                                                         as.numeric(`#.Mastered.(Prev..Advanced)`), na.rm = T) / 
                                               sum(`#.Valid.Tests`, na.rm = T), 1)) %>% 
          rename(year = Year, system = District) %>% 
          ungroup(),
        # 2015
        openxlsx::read.xlsx("https://www.tn.gov/content/dam/tn/education/data/data_2015_district_base.xlsx") %>%
          filter(subgroup == "All Students" & grade == "All Grades") %>% 
          group_by(year, system) %>% 
          summarize(proficiency_rate = round(100 * sum(as.numeric(n_prof) + as.numeric(n_adv), na.rm = T) / 
                                               sum(valid_tests, na.rm = T), 1)) %>% 
          ungroup()
      ), by = c("year", "system")
    ) %>% 
    # Demographics
    left_join(
      bind_rows(
        # 2018
        openxlsx::read.xlsx("https://www.tn.gov/content/dam/tn/education/data/profile/district_profile_2017-18.xlsx") %>% 
          janitor::clean_names() %>%
          transmute(year = 2018, system = district, adm = average_daily_membership, number_of_schools, 
                    per_pupil_funding = per_pupil_expenditures_per_ada, 
                    pct_swd = students_with_disabilities_pct, pct_ed = economically_disadvantaged_pct),
        # 2017
        openxlsx::read.xlsx("https://www.tn.gov/content/dam/tn/education/data/profile/data_2016-17_district_profile.xlsx") %>% 
          janitor::clean_names() %>%
          transmute(year = 2017, system = district, adm = average_daily_membership, number_of_schools, 
                    per_pupil_funding = per_pupil_expenditures_per_ada, 
                    pct_swd = students_with_disabilities_pct, pct_ed = economically_disadvantaged_pct),
        # 2016
        openxlsx::read.xlsx("https://www.tn.gov/content/dam/tn/education/data/profile/data_2015-16_district_profile.xlsx") %>% 
          janitor::clean_names() %>%
          transmute(year = 2016, system = district, adm = average_daily_membership, number_of_schools, 
                    per_pupil_funding = per_pupil_expenditures_per_ada, 
                    pct_swd = students_with_disabilities_pct, pct_ed = economically_disadvantaged_pct),
        # 2015
        openxlsx::read.xlsx("https://www.tn.gov/content/dam/tn/education/data/profile/data_2015_district_profile.xlsx") %>% 
          janitor::clean_names() %>%
          transmute(year = 2015, system = district, adm = average_daily_membership, number_of_schools, 
                    per_pupil_funding = per_pupil_expenditures_per_ada, 
                    pct_swd = students_with_disabilities_pct, pct_ed = economically_disadvantaged_pct)
      ), by = c("year", "system")
    ) %>% 
    # Community education levels
    left_join(
      readxl::read_excel("Assignments/Research Paper/Percentage of people with Bachelors as of 1.11.19.xlsx",
                         skip = 1, col_names = c("county", "pct_with_bachelors", "X1", "X2")) %>% 
        transmute(county, pct_with_bachelors = pct_with_bachelors * 100) %>% 
        inner_join(
          read_csv("C:/Users/CA19130/Documents/Data/Crosswalks/system system_name county crosswalk.csv") %>% 
            transmute(system, county = str_replace_all(county, " County", "")),
          by = "county"
        ),
      by = "system"
    ) %>% 
    # Crime rates 
    left_join(
      readxl::read_excel("Assignments/Research Paper/Crimes Rates by Jurisdiction 2012-2017.xlsx", skip = 3,
                         col_names = c("county", "year", "crime_rate_per_1000", "pop_est", "n_crimes")) %>% 
        transmute(year = year + 1, county, crime_rate = crime_rate_per_1000),
      by = c("year", "county")
    ) %>% 
    # Home sales
    left_join(
      readxl::read_excel("Assignments/Research Paper/All-Home-Sales-2008-2017.xlsx", skip = 5, 
                         col_names = c("county", str_c("n_", 2009:2018), "X", 2009:2018)) %>% 
        select(county, `2009`:`2018`) %>% 
        mutate_at(vars(`2009`:`2018`), funs(as.numeric(str_replace_all(., "[$*,]", "")))) %>% 
        gather("year", "median_home_sale_price", 2:11) %>% 
        mutate(year = as.numeric(year)),
      by = c("year", "county")
    )
  
  # Output file
  write_csv(df, "Assignments/Research Paper/research_data.csv", na = "")
} else {
  rm(data)
}

# Analysis
if(analysis) {
  # OLS and fixed-effects models
  ols = lm(proficiency_rate ~ adm + number_of_schools + per_pupil_funding + pct_swd + pct_ed + 
             pct_with_bachelors + crime_rate + median_home_sale_price, df) 
  fe = plm(proficiency_rate ~ adm + number_of_schools + per_pupil_funding + pct_swd + pct_ed, df,
      index = c("system", "year"), model = "within") 
  pFtest(ols, fe)
} else {
  rm(analysis)
}

# Visualizations
if(visuals) {
  
} else {
  rm(visuals)
}
