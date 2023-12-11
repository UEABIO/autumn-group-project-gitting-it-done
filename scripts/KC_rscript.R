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
library(forcats)

# creating a tidier data set using the variables I need 
covid_data_died_vs_died_covid <- select(.data = covid_data_no_duplicates, 
                                             case_race, died, died_covid)

covid_data_died_vs_died_covid <- arrange(.data = covid_data_died_vs_died_covid,
                                              case_race,died, died_covid) 

# I am doing to:
# look at deaths in general compared to deaths specifically by covid, using race as the colour 

# eliminating n/a data

# this tells me the sum of how many observations are missing from the entirety of the data frame 
covid_data_died_vs_died_covid %>% 
  is.na() %>% 
  sum()

# using the glimpse function to remove all of the n/a values 
covid_data_died_vs_died_covid_2 <- drop_na(covid_data_died_vs_died_covid)

covid_data_died_vs_died_covid_2 %>% 
  is.na() %>% 
  sum()

glimpse(covid_data_died_vs_died_covid_2)

# removing unknown data from the data set 
covid_data_died_vs_died_covid_2 <- subset(covid_data_died_vs_died_covid_2, !is.na(died) & trimws(died) != "Unknown")
covid_data_died_vs_died_covid_2 <- subset(covid_data_died_vs_died_covid_2, !is.na(died_covid) & trimws(died_covid) != "Under Review")


# Remove rows with "other", "unknown" and "under review" values in case_race
covid_data_died_vs_died_covid_2 <- covid_data_died_vs_died_covid_2 %>%
  filter(!(case_race %in% c("UNKNOWN", "OTHER", "UNDER REVIEW")))

# Remove rows with "other", "unknown" and "under review" values in died
covid_data_died_vs_died_covid_2 <- covid_data_died_vs_died_covid_2 %>%
  filter(!(died %in% c("unknown", "other", "under review")))

# Remove rows with "other", "unknown" and "under review" values in died_covid
covid_data_died_vs_died_covid_2 <- covid_data_died_vs_died_covid_2 %>%
  filter(!(died_covid %in% c("unknown", "other", "under review")))


# USING THE UNIQUE FUNCTION TO CHECK THIS WORKED
unique(covid_data_died_vs_died_covid_2$case_race)
unique(covid_data_died_vs_died_covid_2$died)
unique(covid_data_died_vs_died_covid_2$died_covid)

# changing my yes/no responses to be numerical data 
covid_data_died_vs_died_covid_2 <- covid_data_died_vs_died_covid_2 %>%
  mutate(numeric_column_died = as_factor(died) %>% as.numeric())

covid_data_died_vs_died_covid_2 <- covid_data_died_vs_died_covid_2 %>%
  mutate(numeric_column_died_covid = as_factor(died_covid) %>% as.numeric())

# Remove the 'numeric_column' as i added it by accident 
print(colnames(covid_data_died_vs_died_covid_2))
covid_data_died_vs_died_covid_2 <- covid_data_died_vs_died_covid_2 %>% select(-numeric_column)

# Creating my graph 
