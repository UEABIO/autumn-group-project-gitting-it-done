# what percentage of people died died of covid 

# Setting up script ----


# loading packages 
library(tidyverse)
library(janitor)
library(ggplot2)
library(lubridate)
library(plotly)
library(here)
library(ggthemes)

#creating objects

# Using the select function to only include certain variables ----

# creating the data set ----

head(covid_data_no_duplicates)

covid_died_vs_died_covid_by_race <- select(.data = covid_data_no_duplicates, 
       died , died_covid, case_race) %>% 
  view()

# getting rid of the n/a values 
covid_died_vs_died_covid_by_race <- drop_na(covid_died_vs_died_covid_by_race)

covid_died_vs_died_covid_by_race %>% 
  is.na() %>% 
  sum()

glimpse(covid_died_vs_died_covid_by_race)

# summarising the data
covid_died_vs_died_covid_by_race %>% 
  summarise(n=n(),
            quanitity_died = sum(died_covid == "yes"),


# starting to create the graph  
covid_data_no_duplicates %>% 
  ggplot(aes(x=died, 
             y = died_covid,
             colour= ))+ 
  geom_point()+
  geom_smooth(method="lm",    
              se=FALSE)+
  labs(x = "Died",
       y = "Died of covid",
       title= "Died vs died of covid amoungst different races",
       subtitle= "")