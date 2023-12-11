#Clearing Environment at start of session
#rm(list=ls())


#Setting up RScript ----
library(tidyverse)
library(janitor)
library(ggplot2)
library(lubridate)
library(plotly)
library(here)
library(ggthemes)

# Data set Orignial loading ----
covid_data <- read_csv("data/covid_example_data.csv") 


# cleaning our data set 
## making all variable names snake case 

covid_data <- janitor::clean_names(covid_data)

# checking new variable names 
colnames(covid_data) 

# renaming some of the variables 
covid_data <- rename(covid_data,
                     "personal_id"= "pid",
                     "report_date"= "reprt_creationdt_false",
                     "case_dob"="case_dob_false",
                     "sym_start_date"="sym_startdt_false",
                     "sym_loss_taste_smell"="sym_losstastesmell",
                     "sym_resolved_date"="sym_resolveddt_false",
                     "hospital_admission_date"="hosp_admidt_false",
                     "hospital_discharge_date"="hosp_dischdt_false",
                     "died_date"="died_dt_false",
                     "positive_pcr_date"="pos_sampledt_false")

# Data set date correct ----

covid_data_date_correct <- covid_data %>% 
  mutate(report_date = lubridate :: dmy(report_date))%>% 
  mutate(case_dob = lubridate :: dmy(case_dob)) %>% 
  mutate(sym_start_date = lubridate :: dmy(sym_start_date)) %>% 
  mutate(sym_resolved_date = lubridate :: dmy(sym_resolved_date)) %>% 
  mutate(hospital_admission_date = lubridate :: dmy(hospital_admission_date)) %>% 
  mutate(hospital_discharge_date = lubridate :: dmy(hospital_discharge_date)) %>% 
  mutate(died_date = lubridate :: dmy(died_date)) %>% 
  mutate(positive_pcr_date = lubridate :: dmy(positive_pcr_date))

# checking for any missing values 
covid_data_date_correct %>% 
  is.na() %>% 
  sum()

# Removing Duplicated Data ----
# checking for duplicated rows in the data and removing the duplicated data sets 
# Found that case_zip is unreliable 

covid_data_date_correct %>% 
  dplyr::distinct(personal_id, report_date, case_dob) %>% nrow() 

covid_data_date_correct %>% 
  group_by(personal_id, report_date, case_dob) %>% 
  filter(n()>1)

# creating a data set without case_zip 
# Data set without case_zip ----
covid_data_clean <- covid_data_date_correct %>% 
select(-case_zip)

# check for duplicate rows in new data
covid_data_clean %>% 
  duplicated() %>% 
  sum() 

# Data set without duplicates ----
covid_data_no_duplicates <- covid_data_clean[!duplicated(covid_data_clean), ]

# check for duplicate rows in new data, = 0 so no more duplicates 
covid_data_no_duplicates %>% 
  duplicated() %>% 
  sum() 

