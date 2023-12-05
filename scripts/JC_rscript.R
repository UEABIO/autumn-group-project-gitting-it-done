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


# Creating and tidying data set ----

covid_data_age_gender_hospitalized <- select(.data = covid_data_no_duplicates, 
              case_age, case_gender, hospitalized)

covid_data_age_gender_hospitalized <- arrange(.data = covid_data_age_gender_hospitalized,
        case_gender,case_age) 

# Having looked at the data i would like to know:
# total males and females
# remove all NA values from all three variables 
# remove any unrelaistic ages(-20)
# total males hospitalised 
# total females hospitalised
# average age

# managing the NA values ---- 

# Get a sum of how many observations are missing in our dataframe
covid_data_age_gender_hospitalized %>% 
  is.na() %>% 
  sum()

# Removing all NA values 
covid_data_age_gender_hospitalized_2 <- drop_na(covid_data_age_gender_hospitalized)

covid_data_age_gender_hospitalized_2 %>% 
  is.na() %>% 
  sum()

glimpse(covid_data_age_gender_hospitalized_2)


# Exploring values - *NOT WORKED*

covid_data_age_gender_hospitalized_2 %>% 
  summarise(n=n(), # number of rows of data
            quanitity_female = sum(case_gender == "Female"),
            quanitity_male = sum(case_gender == "Male"),
            quanitity_unknown = sum(case_gender == "Unknown"),
            quantity_hospitalised = sum(hospitalized == "Yes"),
            quantity_not_hospitalised = sum(hospitalized == "No"))

covid_data_age_gender_hospitalized_2 %>% 
  summarise(n=n(),
            number_females_hospitalised = count(case_gender == "Female", hospitalized == "Yes"),
            number_males_hospitalised = count(case_gender == "Male",hospitalized == "Yes"),
            number_females_not_hospitalised = count(case_gender == "Female",hospitalized == "No"),
            number_males_not_hospitalised = count(case_gender == "Male",hospitalized == "No"))
            


# 81 Genders unknown, however cannot discard this data as data entry may not have accounted for non-binary/trans community

# NEED TO CREATE TWO SEPERATE DATA SETS TO LOOK AT RELATIONSHIP BETWEEN GENDER AND HOSPITALISATION 

# I want to look at the relationship between sex and rate of hospitalization 
# I want to look at the relationship between age and rate of hospitalization
# I want to create a visual which encompasses both factors and hospitalization 
  
  
  
  