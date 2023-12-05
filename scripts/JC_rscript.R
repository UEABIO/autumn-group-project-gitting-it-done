# Correlation between age, gender and hospitalization rate 

rm(list=ls())

#Setting up Script ----

# PACKAGES 
library(tidyverse)
library(janitor)
library(ggplot2)
library(lubridate)
library(plotly)
library(here)
library(ggthemes)


# Creating data set ----

covid_data_age_gender_hospitalized <- select(.data = covid_data_no_duplicates, 
              case_age, case_gender, hospitalized)

covid_data_age_gender_hospitalized <- arrange(.data = covid_data_age_gender_hospitalized,
        case_gender,case_age) %>% 
  view()

# Having looked at the data i would like to know:
# total males and females
# remove all NA values from all three variables 
# remove any unrelaistic ages(-20)
# total males hospitalised 
# total females hospitalised
# average age

# use summary for general overview
summary(covid_data_age_gender_hospitalized)
nrow(covid_data_age_gender_hospitalized)


# managing the NA values 

# Get a sum of how many observations are missing in our dataframe
covid_data_age_gender_hospitalized %>% 
  is.na() %>% 
  sum()

# Removing all NA values 
drop_na(covid_data_age_gender_hospitalized) %>% 
  nrow()



covid_data_age_gender_hospitalized %>% 
  group_by(case_age, case_gender) %>% 
  summarise(n_distinct(hospitalized))

# I want to look at the relationship between sex and rate of hospitalization 
# I want to look at the relationship between age and rate of hospitalization
# I want to create a visual which encompasses both factors and hospitalization 
  
  
  
  