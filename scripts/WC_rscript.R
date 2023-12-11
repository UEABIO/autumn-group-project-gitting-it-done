#Correlation between time spent in hospital, age and gender

#Clearing Environment at start of session
rm(list=ls())

#Loading packages
library(tidyverse)
library(janitor)
library(ggplot2)
library(lubridate)
library(plotly)
library(here)
library(ggthemes)

#Setting up new dataset

covid_data_age_gender_time_hospital <- select(.data = covid_data_no_duplicates,
                                              case_age, case_gender, hospital_admission_date, hospital_discharge_date)

covid_data_age_gender_time_hospital <- arrange(.data = covid_data_age_gender_time_hospital,
                                               case_gender, case_age)
#checking the number of na values
covid_data_age_gender_time_hospital %>% 
  is.na() %>% 
  sum()

#dropping the na values
covid_data_age_gender_time_hospital <- covid_data_age_gender_time_hospital %>% 
  drop_na(hospital_admission_date,
          hospital_discharge_date)


covid_data_age_gender_time_hospital %>% 
  summarise(admission=min(hospital_admission_date), 
            discharge=max(hospital_discharge_date), 
            hospital_duration = discharge-admission, 
            n=n())

#removing
covid_data_age_gender_time_hospital <- covid_data_age_gender_time_hospital %>% 
  filter(!case_gender == "Unknown") %>% 
  group_by(case_age, case_gender, hospital_admission_date, hospital_discharge_date)

covid_data_hospital_time <- covid_data_age_gender_time_hospital %>%
  summarise(admission = min(hospital_admission_date),
            discharge = max(hospital_discharge_date),
            hospital_duration = discharge - admission,
            n = n())

covid_data_age_gender_time_hospital <- covid_data_age_gender_time_hospital %>%
  mutate(hospital_duration = as.numeric(difftime(hospital_discharge_date, hospital_admission_date, units = "days")))

view(covid_data_age_gender_time_hospital)

