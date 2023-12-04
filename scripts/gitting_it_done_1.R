#Clearing Environment at start of session
rm(list=ls())


#Setting up RScript ----
library(tidyverse)
library(janitor)
library(ggplot2)
library(lubridate)
library(plotly)
library(here)
library(ggthemes)

#Loading Data 
covid_data <- read_csv("data/covid_example_data.csv") 


# cleaning our data set 
## making all variable names snake case 

covid_data <- janitor::clean_names(covid_data)

colnames(covid_data) #quickly checks the new variable names 

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

# checking for duplicated rows in the data 
covid_data %>% 
  duplicated() %>% sum()

# removing duplicate 

# checking for missing valuesin whole data set 
covid_data %>% 
  is.na() %>% 
  sum()

# Changing Dates ----

covid_data_date_correct <- covid_data %>% 
  mutate(report_date = lubridate :: dmy(report_date))%>% 
  mutate(case_dob = lubridate :: dmy(case_dob)) %>% 
  mutate(sym_start_date = lubridate :: dmy(sym_start_date)) %>% 
  mutate(sym_resolved_date = lubridate :: dmy(sym_resolved_date)) %>% 
  mutate(hospital_admission_date = lubridate :: dmy(hospital_admission_date)) %>% 
  mutate(hospital_discharge_date = lubridate :: dmy(hospital_discharge_date)) %>% 
  mutate(died_date = lubridate :: dmy(died_date)) %>% 
  mutate(positive_pcr_date = lubridate :: dmy(positive_pcr_date))

# confirming format has changed 
head(covid_data_date_correct)

# checking for any new missing values 
covid_data_date_correct %>% 
  is.na() %>% 
  sum()



