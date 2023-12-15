
# Jasper Raistrick Covid Data Visualisation ----
# What are the most common covid symptoms recorded? ----
#_________________----



#PACKAGES ----

library(tidyverse)
library(janitor)
library(ggplot2)
library(lubridate)
library(plotly)
library(here)
library(ggthemes)


# CLEANING DATA ----

# Selecting just symptom variables from dataset and naming as object

covid_symptoms_data <-  covid_data_no_duplicates %>% 
       select(sym_fever, sym_myalgia, sym_loss_taste_smell,
       sym_sorethroat, sym_cough, sym_headache)

# _________________________----




# Now to combine these columns into a new column called symptoms ----

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
  ggplot(aes(y = Symptoms, fill = Symptoms)) +
  geom_bar(show.legend = FALSE) +
  scale_fill_manual(values = c( "1cough" = "darkblue", "2headache" = "royalblue",
                                "3myalgia" = "skyblue", "4fever" = "lightgrey",
                                "5subjfever" = "lightgrey",
                                "6loss_taste_smell"= "lightgrey",
                                "7sorethroat" = "lightgrey")) +
theme_minimal() +
  scale_y_discrete(labels=c('Cough', 'Headache', 'Myalgia', 'Fever', 'Subjfever',
                            'Loss Taste or Smell', 'Sorethroat'))+
  labs(x = "Count of Symptoms present",
       y = "Symptom",
       title= "Frequency of COVID-19 Symptoms",
       subtitle= "Leading three symptoms are cough, headache and myalgia",
       caption = "Figure 4: Showing counts of symptoms")



#______________________________________----

# Cleaning Data

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




# DATA VISUALISATION ----


# Creating a bar chart

# Creating proportions of Yes and No symptoms data
proportion_covid_data <- covid_symptoms_data_clean_long %>% 
  group_by(type) %>% 
  mutate(prop_sym = n/sum(n)) %>%  
    filter(symptoms != "No") # Filtering out No proportions



# DATA VISUALISATION ----


# Creating a bar chart

covid_symptoms_bar_chart <- proportion_covid_data %>% 
    ggplot(aes(y = type, x = prop_sym, fill = type)) +
  geom_col() +
  scale_fill_manual(values = c( "sym_cough" = "darkblue",
                                "sym_fever" = "lightgrey",
                                "sym_headache" = "blue",
                                "sym_fever" = "lightgrey",
                                "sym_loss_taste_smell"= "royalblue",
                                "sym_sorethroat" = "lightgrey",
                                "sym_myalgia" = "lightgrey")) +
geom_label(aes(label=scales::percent(prop_sym, accuracy = 0.1)), 
            nudge_x = -0.05,
            fill = "white",
            fontface = "bold",
            family = "Fira Sans") +
  theme_minimal() +
  theme(legend.position = "none") # Removing legend

theme(legend.position = "none",
      axis.text.y = element_text(size = 14, hjust = 1, family = "Fira Sans"))
    
covid_symptoms_bar_chart
















