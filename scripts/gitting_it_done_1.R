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