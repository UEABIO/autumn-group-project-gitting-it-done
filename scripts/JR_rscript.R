
# Jasper Raistrick Covid Data Visualisation ----
# What are the most common covid symptoms recorded? ----
#_________________----



#PACKAGES

library(tidyverse)
library(janitor)
library(ggplot2)
library(lubridate)
library(plotly)
library(here)
library(ggthemes)




# Filtering and Selecting variables ----



# Selecting just symptom variables from dataset using dplyr and naming as object

covid_symptoms_data <-  covid_data_no_duplicates %>% 
       select(sym_fever, sym_subjfever, sym_myalgia, sym_loss_taste_smell,
       sym_sorethroat, sym_cough, sym_headache)




# Filtering out 'No', 'Unknown' and 'N/a' results for all symptoms
# And creating new object of filtered dataset 'symptpms_yes_data'
symptoms_data_group <- covid_symptoms_data %>%  
                            group_by(sym_fever, sym_subjfever, 
                            sym_myalgia, sym_loss_taste_smell ,
                            sym_sorethroat, sym_cough,
                            sym_headache) %>% 
  summarise(num=n(na.rm = TRUE))


glimpse(symptoms_data_group)



  
symptoms_yes_data <- covid_symptoms_data %>%  
  filter(sym_fever == "Yes", sym_subjfever == "Yes", 
         sym_myalgia == "Yes", sym_loss_taste_smell == "Yes",
         sym_sorethroat == "Yes", sym_cough == "Yes",
         sym_headache == "Yes") 
  
  
  
  
  
  
  
# Advised: Use group by and count  functions and summarise












  

  #_________________----
# % of people with symptoms ----
# Working out Covid symptoms shown as a proportion of total positive PCR tests



