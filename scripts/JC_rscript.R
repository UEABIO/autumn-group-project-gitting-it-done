# Correlation between age, gender and hospitalization rate 

# I want to look at the relationship between sex and rate of hospitalization 
# I want to look at the relationship between age and rate of hospitalization
# I want to create a visual which encompasses both factors and hospitalization 

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

# Managing the NA values ---- 

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


# Exploring values - 

covid_data_age_gender_hospitalized_2 %>% 
  summarise(n=n(), # number of rows of data
            quanitity_female = sum(case_gender == "Female"),
            quanitity_male = sum(case_gender == "Male"),
            quanitity_unknown = sum(case_gender == "Unknown"),
            quantity_hospitalised = sum(hospitalized == "Yes"),
            quantity_not_hospitalised = sum(hospitalized == "No"))

# 81 Genders unknown, however cannot discard this data as data entry may not have accounted for non-binary/trans community

# Data Manipulation ----

# Totals of each gender of each age 
totals_age_gender <- covid_data_age_gender_hospitalized_2 %>% 
  group_by(case_age, case_gender) %>% 
  summarise(num=n()) 
  
  view(totals_age_gender)

# totals of each gender and age separated by hospitalized or not
totals_age_gender_hospitalized <- covid_data_age_gender_hospitalized_2 %>% 
  group_by(case_age, case_gender, hospitalized) %>%
  summarise(num=n()) %>% 
  mutate()

  view(totals_age_gender_hospitalized)


# number of hospitalized for each age, removing unknown data 

age_vs_hospitalisation_data <- covid_data_age_gender_hospitalized_2 %>% 
  group_by(case_age, case_gender) %>% 
  filter(hospitalized == "Yes", !case_gender == "Unknown") %>% 
  summarise(n = n())
  
view(age_vs_hospitalisation_data)

# creating a data set with percentage hospitalised from age and gender 

age_gender_total_hosp <- merge(age_vs_hospitalisation_data, totals_age_gender, by=c("case_age","case_gender")) %>% 
  mutate(percentage = (n/num)*100)

view(age_gender_total_hosp)

# Creating Graph ----

age_gender_total_hosp %>% 
  ggplot(aes(x = case_age, 
             y = percentage,
             colour = case_gender))+
  geom_smooth()+
  labs(x = "Patient Age",
       y = "% of Patients Hospitalised")

  # Options to add
  geom_smooth(method="lm") # line of best fit 

  coord_flip() # flips axis 
  
  