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

head(covid_data)
view(covid_data)

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
