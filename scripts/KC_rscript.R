# Differrences in rates of death between hispanic and other races 
## Hispanic/latinos are more likely to die from covid compared to white 

# Setting up script ----


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


covid_data_no_duplicates %>% 
  ggplot(aes(x=case_eth, 
             y = sym_start_date))+
  geom_point() 

