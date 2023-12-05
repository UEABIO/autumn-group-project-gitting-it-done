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


# Creating data set ----

select(covid_data_no_duplicates %>% 
       personal_id, case_age, case_gender, hospitalized)