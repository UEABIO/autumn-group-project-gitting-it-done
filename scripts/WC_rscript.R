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

#finding the number of days spent in hospital and filtering out incorrect values
covid_data_age_gender_time_hospital <- covid_data_age_gender_time_hospital %>%
  mutate(hospital_duration = as.numeric(interval(hospital_admission_date, hospital_discharge_date) / ddays(1))) %>%
  filter(hospital_duration >= 0,
         hospital_duration <= 500)

ggplot(covid_data_age_gender_time_hospital, aes(x = case_age,
                                                      y = hospital_duration)) +
  geom_point(aes(colour = case_gender),
                 alpha = 0.3) +
  labs(title = "A Scatter of Age and Gender vs Time Spent in Hospital",
       x = "Age",
       y = "Hospital Duration (days)") 

view(covid_data_age_gender_time_hospital)


