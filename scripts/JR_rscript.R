
# Jasper Raistrick Covid Outbreak Situation Report ----
# What are the most common covid symptoms recorded by proportion?
# Presenting data visually as a bar chart
#_________________----



#PACKAGES ----

library(tidyverse)
library(janitor)
library(ggplot2)
library(lubridate)
library(plotly)
library(here)
library(ggthemes)
#____________________________----

# CLEANING DATA ----

# Selecting just symptom variables from dataset and naming as object
covid_symptoms_data <-  covid_data_no_duplicates %>% 
       select(sym_fever, sym_myalgia, sym_loss_taste_smell,
       sym_sorethroat, sym_cough, sym_headache)

# Replacing YES in sym_myalgia with Yes
covid_symptoms_data_clean <- covid_symptoms_data %>% 
mutate(sym_myalgia = replace(sym_myalgia, sym_myalgia == 'YES', 'Yes')) 
 

# Turning wide data into Long Data
covid_symptoms_data_clean_long <- covid_symptoms_data_clean %>% 
  pivot_longer(cols = sym_fever:sym_headache,
                                     names_to = "type",
                                     values_to = "symptoms")  
# Removing NA and Unknown values from symptoms
covid_symptoms_data_clean_long <- covid_symptoms_data_clean_long %>% 
  drop_na(symptoms) %>% 
  filter(symptoms != "Unk") %>% 
  group_by( type, symptoms) %>%  # Creating new column of totals
  summarise(n = n()) 

# Creating proportions of Yes and No symptoms data
proportion_covid_data <- covid_symptoms_data_clean_long %>% 
  group_by(type) %>% 
  mutate(prop_sym = n/sum(n)) %>%  
    filter(symptoms != "No") %>%  # Filtering out No proportions
  mutate(type = factor(type, # Ordering categories into ascending freq order
                       levels=c("sym_cough",
                                "sym_headache",
                                "sym_loss_taste_smell",
                                "sym_myalgia",
                                "sym_fever",
                                "sym_sorethroat"))) 

#______________________________----
# DATA VISUALISATION ----

# Creating a bar chart

covid_symptoms_bar_chart <- proportion_covid_data %>% 
    ggplot(aes(y = type, x = prop_sym, fill = type)) + # Choosing axes
  geom_col(width = 0.8, position = position_dodge(0.7)) + # Bar width & spacing
  #Choosing colours for a gradient effect
  scale_fill_manual(values = c("sym_cough" = "#0080bf",
                               "sym_headache" = "#00acdf",
                               "sym_fever" = "lightgrey",
                               "sym_loss_taste_smell"= "#55d0ff",
                               "sym_sorethroat" = "lightgrey",
                               "sym_myalgia" = "#7ce8ff")) +
geom_label(aes(label=scales::percent(prop_sym, # Adding labels as percentages
                                     accuracy = 0.01)),# Two decimal places
            nudge_x = -0.05, # Moves labels inboard of bars
            fill = "white", # White boxes around labels
            fontface = "bold", 
            family = "Fira Sans",
           size = 5) +
  theme_void() +
  theme(legend.position = "none",  # Removing legend
        axis.text.y = element_text(size = 17, hjust = 1, #Setting font sizes
                                   family = "Fira Sans"),
                                   plot.title = element_text(size = 18,
                                                             face = "bold"),
        plot.caption = element_text(size = 16, hjust = 1), 
        plot.subtitle = element_text(size = 17)) + 
  scale_y_discrete(labels=c("Cough", "Headache", "Loss of Taste or Smell",
                            "Myalgia", "Fever","Sore Throat")) + #Symptom labels
  ggtitle(label = # Adding Title, subtitle and figure legend
  "Cough, Headache, Loss of Taste or Smell and Myalgia 
  are the 4 most common Covid-19 Symptoms",
          subtitle = "Each are present in at least 40% of cases ") +
  labs(caption = "The proportions as percentages of patients showing different symptoms for Covid-19(SARS-CoV-2)
                   out of total recorded prescence or abscence of symptoms for each symptom. Percentages rounded to two decimal places.") 
    
print(covid_symptoms_bar_chart)

# To save bar chart as a png in figures
#ggsave("figures/covid_symptoms_bar_chart.png",
      # plot = covid_symptoms_bar_chart,
      # width = 35,
      # height = 20,
      # units = "cm",
      # device = "png")
#_________________________________----