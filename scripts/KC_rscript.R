# Mortality Patterns by Race: General Death vs Deaths Attributed by COVID-19 ----

# loading the packages ----
library(tidyverse)
library(janitor)
library(ggplot2)
library(lubridate)
library(plotly)
library(here)
library(ggthemes)

# Creating a box plot to show case_age and case_race 
covid_data_race_and_sym_numeric <- covid_data_no_duplicates

# Remove NAs from case_age variable
covid_data_race_and_sym_numeric <- covid_data_race_and_sym_numeric[complete.cases(covid_data_race_and_sym_numeric$case_age), ]

# Remove NAs from case_race variable
covid_data_race_and_sym_numeric <- covid_data_race_and_sym_numeric[complete.cases(covid_data_race_and_sym_numeric$case_race), ]

# Check the resulting dataset
head(covid_data_race_and_sym_numeric)

# Remove rows with "other", "unknown" and "under review" values in case_race
covid_data_race_and_sym_numeric <- covid_data_race_and_sym_numeric %>%
  filter(!(case_race %in% c("UNKNOWN", "OTHER", "UNDER REVIEW", "NA", NA)))

# Remove rows with "other", "unknown" and "under review" values in symptom_count
covid_data_race_and_sym_numeric <- covid_data_race_and_sym_numeric %>%
  filter(!(case_age %in% c("UNKNOWN", "OTHER", "UNDER REVIEW", "NA")))

# getting rid of the -20 age from case_age
covid_data_race_and_sym_numeric <- covid_data_race_and_sym_numeric %>%
  filter(case_age >= 0,
         case_age<= 110)

# USING THE UNIQUE FUNCTION TO CHECK THIS WORKED
unique(covid_data_race_and_sym_numeric$case_race)
unique(covid_data_race_and_sym_numeric$case_age)

# Creating a box plot exploring case and age 
age_and_race_boxplot_KC <- ggplot(covid_data_race_and_sym_numeric, aes(x = case_race, y = case_age, fill = case_race)) +
  geom_boxplot(outlier.shape = 16, alpha = 0.7, position = position_dodge(width = 0.8)) +
  stat_ellipse(aes(color = case_race), type = "norm", level = 0.95, position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c("mediumpurple", "paleturquoise", "seagreen2", "lightgoldenrod1", "lightpink")) +
  labs(title = "Figure 3. Box plot to show age and race",
       subtitle = "Graphically demonstarting the distribution of age and race",
       caption = "Figure 3. A boxplot to demonstrate the relationshiop between age and race in the Covid Outbreak data set. The box plot shows the minimum, maximum, medium age as well as the upper and lower quartile.",
       x = "Race",
       y = "Age") +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1), # Rotate x-axis labels by 45 degrees
        axis.text.y = element_text(size = 10),
        plot.title = element_text(size = 14, face = "bold"),
        legend.position = "top",
        legend.title = element_text(size = 12), 
        legend.text = element_text(size = 10),
        panel.grid.major = element_line(color = "gray", size = 0.2),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill = "white", color = "black", size = 0.5),
        panel.border = element_rect(color = "black", fill = NA, size = 0.7),
        strip.background = element_rect(fill = "white", color = "black", size = 0.7),
        strip.text = element_text(face = "bold")) +
  guides(fill = guide_legend(override.aes = list(alpha = 1))) +
  scale_color_manual(values = c("mediumpurple", "paleturquoise", "seagreen2", "lightgoldenrod1", "lightpink")) +
  labs(fill = "Race:")
