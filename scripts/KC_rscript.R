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
library(dplyr)

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

# changing chr to numerical values 
covid_died_vs_died_covid_by_race$response_numeric <- as.numeric(factor(covid_died_vs_died_covid_by_race, levels = c("no", "yes")))


# Using mutate to change the varibales 
covid_died_vs_died_covid_by_race <- covid_died_vs_died_covid_by_race %>%
  mutate(covid_binary = ifelse(died_covid == "yes", 1, 0))

covid_died_vs_died_covid_by_race <- covid_died_vs_died_covid_by_race %>%
  mutate(covid_binary = ifelse(died == "yes", 1, 0))

covid_died_vs_died_covid_by_race$covid_binary <- factor(covid_died_vs_died_covid_by_race$covid_binary, levels = c("0", "1"))

ggplot(covid_died_vs_died_covid_by_race, aes(x = case_race, fill = factor(covid_binary))) +
  geom_bar(position = "stack", width = 0.7) +
  labs(x = "Race", y = "Number of Deaths", fill = "Cause of Death") +
  scale_fill_manual(values = c("0" = "blue", "1" = "red")) +
  theme_minimal()




