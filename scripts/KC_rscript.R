# Differences in symptom start date and resolution date between members of the Hispanic and non-Hispanic communities

# Setting up script ----
rm(list=ls())

# loading packages 
library(tidyverse)
library(janitor)
library(ggplot2)
library(lubridate)
library(plotly)
library(here)
library(ggthemes)

# Using the select function to only include certain variables ----

# creating the data set ----
head(covid_data_no_duplicates)

covid_data_ethnicty_vs_symptons<- select(.data = covid_data_no_duplicates, 
       sym_start_date , sym_resolved_date , case_eth) %>% 
  view()




