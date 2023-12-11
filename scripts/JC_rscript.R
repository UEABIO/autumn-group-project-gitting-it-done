# Correlation between age, gender and hospitalization rate 

# I want to look at the relationship between sex and rate of hospitalization 
# I want to look at the relationship between age and rate of hospitalization
# I want to create a visual which encompasses both factors and hospitalization 

#rm(list=ls())

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

age_gender_total_hosp_child <- age_gender_total_hosp %>% 
  filter(case_age <= 17) 

view(age_gender_total_hosp_child)
summary(age_gender_total_hosp_child)

age_gender_total_hosp_adult <- age_gender_total_hosp %>% 
  filter(case_age %in% (18:49)) 

view(age_gender_total_hosp_adult)
summary(age_gender_total_hosp_adult)


age_gender_total_hosp_elderly <- age_gender_total_hosp %>% 
  filter(case_age >= 50) 

view(age_gender_total_hosp_elderly)
summary(age_gender_total_hosp_elderly)


# Creating a data set to add to graph so labels can be added in specific coordinates 
case_age <- c(9,34,77,9, 34,77)
percentage <- c(100,100,100,90,90,90)
label <- c("3.05%", "5.57%", "38.71%","0-17 yrs","18-49 yrs", "50+ yrs ")

my_labels <- tibble(case_age, percentage, label)


# % Patients Hospitalised Figure by age and gender figure 
age_gender_total_hosp %>% 
  ggplot() +
  geom_rect(aes(xmin=0,xmax=18,ymin=-Inf,ymax=Inf),alpha= 0.2,fill= "lightsteelblue1")+
  geom_rect(aes(xmin=18,xmax=50,ymin=-Inf,ymax=Inf),alpha=0.2, fill=  "honeydew2")+
  geom_rect(aes(xmin=50,xmax=110,ymin=-Inf,ymax=Inf),alpha=0.2,fill= "mistyrose1")+
  geom_point(aes(x = case_age, 
                 y = percentage,
                 colour = case_gender)) +
  geom_label(data = my_labels, aes(x = case_age,
                                   y = percentage,
                                   label = label))+
  labs(x = "Patient Age",
       y = "% of Patients Hospitalised",
       title= "Risk of Hospitalisation from Covid-19",
       subtitle= "Percentage of patients hospitalised based on age and gender")+
  theme_clean() 



# need a better background colour scheme 
# need a legend to support 


# checking the factors of my data 
glimpse(age_gender_total_hosp)




  # Options to add
  #geom_smooth(method="lm") # line of best fit 
  #coord_flip() # flips axis 
  
  