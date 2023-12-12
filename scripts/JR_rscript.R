
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


# Getting a sum of how many observations are missing in dataframe (N/a)
covid_symptoms_data %>% 
  is.na() %>% 
  sum()       # 248173 observations are missing

summary(covid_symptoms_data)

# Changing value names in each column from "Yes" to their symptom name using mutate



covid_symptoms_data_clean <- covid_symptoms_data %>% 
  mutate(sym_cough = case_when(sym_cough == "Yes" ~ "cough",
                             sym_cough == "Unk" ~ "NA",
                             sym_cough == "No" ~ "NA"),
         sym_fever = case_when(sym_fever == "Yes" ~ "fever",
                               sym_fever == "Unk" ~ "NA",
                               sym_fever == "No" ~ "NA"),
         sym_headache = case_when(sym_headache == "Yes" ~ "headache",
                               sym_headache == "Unk" ~ "NA",
                               sym_headache == "No" ~ "NA"),
         sym_subjfever = case_when(sym_subjfever == "Yes" ~ "subjfever",
                               sym_subjfever == "Unk" ~ "NA",
                               sym_subjfever == "No" ~ "NA"),
         sym_myalgia = case_when(sym_myalgia == "Yes" ~ "myalgia",
                               sym_myalgia == "Unk" ~ "NA",
                               sym_myalgia == "No" ~ "NA"),
         sym_sorethroat = case_when(sym_sorethroat == "Yes" ~ "sorethroat",
                               sym_sorethroat == "Unk" ~ "NA",
                               sym_sorethroat == "No" ~ "NA"),
         sym_loss_taste_smell = case_when(sym_loss_taste_smell == "Yes" ~ "loss_taste_smell",
                               sym_loss_taste_smell == "Unk" ~ "NA",
                               sym_loss_taste_smell == "No" ~ "NA"))



# Now to combine these columns into a new colum called symptoms ----

# Changing dataframe to long format
long_symptoms_data_clean <- covid_symptoms_data_clean %>% 
  pivot_longer(cols = sym_fever:sym_headache,
                                         names_to = "Type",
                                         names_prefix = "sym",
                                         values_to = "Symptoms")

# Removing NA values from new Symptoms column

long_symptoms_data_clean <- long_symptoms_data_clean %>% 
  drop_na(Symptoms) %>% 
  filter(Symptoms != "NA") 
 

# Plotting inital bar chart

long_symptoms_data_clean %>% 
  ggplot(aes(y = Symptoms)) +
  geom_bar() +
  theme_minimal()


df2_long <- long_symptoms_data_clean %>% group_by(Symptoms) %>%   mutate(count_name_occurr = n())


g2<-ggplot(data=df2_long, aes(y =reorder(Symptoms,-count_name_occurr,))) +
  geom_bar(stat="count")
g2






#_________________----
# % of people with symptoms ----
# Working out Covid symptoms shown as a proportion of total positive PCR tests









