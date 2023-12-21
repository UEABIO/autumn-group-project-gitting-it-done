#Correlation between time spent in hospital and age

#Clearing Environment at start of session
#rm(list=ls())

#Loading packages
library(tidyverse)
library(janitor)
library(ggplot2)
library(lubridate)
library(plotly)
library(here)
library(ggthemes)

#Setting up new dataset

dplyr::select(covid_data_no_duplicates)

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


glimpse(covid_data_age_gender_time_hospital)

#creating five new datasets for each age group
age_gender_time_hospital_0_20 <- covid_data_age_gender_time_hospital %>%
  filter(case_age <=20)

age_gender_time_hospital_21_40 <- covid_data_age_gender_time_hospital %>%
  filter(case_age %in% (21:40))

age_gender_time_hospital_41_60 <- covid_data_age_gender_time_hospital %>%
  filter(case_age %in% (41:60))

age_gender_time_hospital_61_80 <- covid_data_age_gender_time_hospital %>%
  filter(case_age %in% (61:80))

age_gender_time_hospital_81_above <- covid_data_age_gender_time_hospital %>%
  filter(case_age >=81) 


#try merge function for all new data set age groups

combined_data <- bind_rows(
  mutate(age_gender_time_hospital_0_20, Age_Group = "0-20"),
  mutate(age_gender_time_hospital_21_40, Age_Group = "21-40"),
  mutate(age_gender_time_hospital_41_60, Age_Group = "41-60"),
  mutate(age_gender_time_hospital_61_80, Age_Group = "61-80"),
  mutate(age_gender_time_hospital_81_above, Age_Group = "81 and above")) 

time_spent_in_hospital_graph <- ggplot(combined_data, aes(x = Age_Group, y = hospital_duration, fill = Age_Group)) +
  geom_violin(trim = FALSE, scale = "width", width = 0.95) + #creates a violin plot at a specified width
  geom_boxplot(width = 0.12, position = position_dodge(0.8), alpha = 0.7) +
  labs(title = "An increase in age increases the number of days that patients spend in hospital",
       subtitle = "A violin boxplot showing the duration of time that different age groups spent in hospital",
       x = "Age Group",
       y = "Hospital Duration (days)") + #labelling the x axis and y axis
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5, size = 15), #states the size of the title
        plot.subtitle = element_text(hjust = 0.5, size = 10), #states the size of the subtitle
        plot.caption = element_text(hjust = 0, size = 8)) #states the size of the caption
  
print(time_spent_in_hospital_graph)

ggsave("figures/WC_time_spent_in_hospital_age_group.png",
       plot = time_spent_in_hospital_graph,
       width = 35,
       height = 20,
       units = "cm",
       device = "png") #creating a png 


