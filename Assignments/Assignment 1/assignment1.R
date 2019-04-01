# ECON 6110 - Assignment 1
# Evan Kramer
# 2/8/2019

# Set up
options(java.parameters = "-Xmx16G")
library(tidyverse)
library(lubridate)
library(haven)
library(RJDBC)
library(lmtest)
setwd("C:/Users/CA19130/Documents/Projects/ECON 6110/")

# Data
cigs = readxl::read_excel("Assignments/Assignment 1/0202.xlsx") %>% 
  janitor::clean_names() %>% 
  mutate_at(vars(ends_with("highest_1")), funs(as.numeric(str_replace_all(., "[strndth]", "")))) %>% 
  select(state = states, avg_price_incl_taxes = average_retail_price_per_pack_with_all_taxes,
         total_state_tax_per_pack) %>% 
  mutate(state = case_when(
    state == "States' Average" ~ "United States",
    state == "DC" ~ "District of Columbia",
    !is.na(state) ~ state
  )) %>%
  # Join demand data
  left_join(read_csv("Assignments/Assignment 1/The_Tax_Burden_on_Tobacco__1970-2017.csv") %>% 
              janitor::clean_names() %>%
              filter(year == max(year, na.rm = T)) %>% 
              transmute(state = location_desc, measure = sub_measure_desc, value = data_value) %>% 
              spread(measure, value) %>% 
              janitor::clean_names(), by = "state") %>% 
  # Join population data - https://www2.census.gov/programs-surveys/popest/datasets/2010-2018/state/detail/
  left_join(read_csv("Assignments/Assignment 1/SCPRC-EST2018-18+POP-RES.csv") %>% 
              transmute(state = NAME, pop_estimate2018 = POPESTIMATE2018), by = "state") %>% 
  filter(state != "United States") 

# Fit model
model = lm(data = cigs, 
           log(cigarette_consumption_pack_sales_per_capita) ~ log(average_cost_per_pack) + 
             log(total_state_tax_per_pack)) 

# https://catalog.data.gov/dataset/the-tax-burden-on-tobacco-volume-49-1970-2014-f77d2
# A) Use these sources, and/or any other to Estimate the Price Elasticity of Demand for Cigarette, 
coefficients(model)[2] #* mean(cigs$average_cost_per_pack, na.rm = T) / 
  # mean(cigs$cigarette_consumption_pack_sales_per_capita, na.rm = T) 

# Elasticity of Tax (all combined) rate on Cigarette.
coefficients(model)[3] #* mean(cigs$total_state_tax_per_pack, na.rm = T) / 
  # mean(cigs$cigarette_consumption_pack_sales_per_capita, na.rm = T)

# Test the Hypothesis that Tax rate has no bearing/impact on Cigarette consumption. 
# What can you conclude about overall Cigarette consumption in the Country?

# B) DIVIDE THE STATES INTO TWO CATEGORIES LESS THAN 6 MILLION IN POPULATION AND MORE THAN 10 MILLION. RUN A DUMMY VARIABLE MODEL TO TEST THE HYPOTHESIS THAT MORE POPULATED STATES HAVE A HIGHER PER-CAPITA CONSUMPTION OF CIGARETTES; AND 
# This makes no sense. Do we drop observations between 6 and 10 million? 
mutate(cigs, pop_dummy = as.integer(pop_estimate2018 < 6000000)) %>% 
  lm(formula = log(cigarette_consumption_pack_sales_per_capita) ~ pop_dummy) %>%
  # lm(formula = cigarette_consumption_pack_sales_per_capita ~ average_cost_per_pack + 
  #      total_state_tax_per_pack + pop_dummy) %>% 
  summary()
t.test(cigs$cigarette_consumption_pack_sales_per_capita[cigs$pop_estimate2018 < 6000000],
       cigs$cigarette_consumption_pack_sales_per_capita[cigs$pop_estimate2018 > 10000000])

# C) Check to see if there is any problems of Multiple linear Regression that may be present here. If THERE IS CORRECT FOR IT
# Multicollinearity
cor(log(cigs$average_cost_per_pack), 
    log(cigs$total_state_tax_per_pack), use = 'complete.obs')
new_model = lm(formula = log(cigarette_consumption_pack_sales_per_capita) ~ 
                 log(average_cost_per_pack), 
               data = cigs) 

# Residuals plot
ggplot(data.frame(y = log(cigs$cigarette_consumption_pack_sales_per_capita)[!is.na(log(cigs$cigarette_consumption_pack_sales_per_capita))],
                  yhat = predict(new_model)) %>% 
         mutate(resid = y - yhat),
       aes(x = y, y = resid)) + 
  geom_point() + 
  theme_bw() + 
  geom_hline(yintercept = 0)

# Solve by dividing by standard error of model
new_model2 = lm(
  data = mutate(
    cigs, 
    y1 = log(cigarette_consumption_pack_sales_per_capita) / 
      summary(new_model)$sigma,
    x1 = log(average_cost_per_pack / summary(new_model)$sigma)
  ),
  formula = y1 ~ x1
)

# Re-plot residuals
ggplot(
  data = data.frame(
    state = cigs$state[2:52],
    y = log(cigs$cigarette_consumption_pack_sales_per_capita)[2:52] / 
      summary(new_model)$sigma,
    yhat = predict(new_model2)
  ) %>% 
    mutate(resid = y - yhat) %>% 
    filter(between(y, 9, 13)),
  aes(x = y, y = resid)
) + 
  geom_point() + 
  theme_bw() + 
  geom_hline(yintercept = 0)

# Plot data
ggplot(cigs, aes(y = cigarette_consumption_pack_sales_per_capita, 
                 x = average_cost_per_pack)) + 
  geom_point() + 
  theme_bw()

# Omitted variables test
resettest(cigarette_consumption_pack_sales_per_capita ~ average_cost_per_pack, 
          power = 2:3, type = "regressor", data = cigs)

# Other checks
# car::vif(model)
car::residualPlots(model)
car::avPlots(model)
car::outlierTest(model)
car::qqPlot(model)
shapiro.test(model$residuals)

# Omitted variables test
resettest(cigarette_consumption_pack_sales_per_capita ~ average_cost_per_pack + 
            total_state_tax_per_pack, power = 2:3, type = "regressor", data = cigs)

# Breusch-Pagan heteroskedasticity test
bptest(cigarette_consumption_pack_sales_per_capita ~ average_cost_per_pack + 
         total_state_tax_per_pack, data = cigs)
car::vif(model)
# r_n
# swilk r_n
# tsset t
# dwstat
# durbina
# kdensity r_n, normal
