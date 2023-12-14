# Mortality Patterns by Race: General Death vs Deaths Attributed by COVID-19 ----

# loading the packages ----
library(tidyverse)
library(janitor)
library(ggplot2)
library(lubridate)
library(plotly)
library(here)
library(ggthemes)
library(forcats)
library(circlize)
library(zoom)

# Tidying my data set ----
# Creating a tidier data set using the variables I need  
covid_data_died_vs_died_covid <- select(.data = covid_data_no_duplicates, 
                                             case_race, died, died_covid)

covid_data_died_vs_died_covid <- arrange(.data = covid_data_died_vs_died_covid,
                                              case_race,died, died_covid) 


# This tells me the sum of how many observations are missing from the entirety of the data frame 
covid_data_died_vs_died_covid %>% 
  is.na() %>% 
  sum()

# using the glimpse function to remove all of the n/a values 
covid_data_died_vs_died_covid_2 <- drop_na(covid_data_died_vs_died_covid)

covid_data_died_vs_died_covid_2 %>% 
  is.na() %>% 
  sum()

# Glimpse(covid_data_died_vs_died_covid_2)

# Removing unknown data from the data set 
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


# Convert "Yes" and "No" to 1 and 0 ----
covid_data_died_vs_died_covid_without_na <- covid_data_died_vs_died_covid_2 %>%
  mutate(died = ifelse(died == "Yes", 1, 0),
         died_covid = ifelse(died_covid == "Yes", 1, 0))

# Summarise the data
summary_data <- covid_data_died_vs_died_covid_2 %>%
  group_by(case_race) %>%
  summarise(total_deaths = sum(died),
            covid_deaths = sum(died_covid))



# Create a grouped bar chart with background color
ggplot(covid_data_died_vs_died_covid_without_na, aes(x = case_race, fill = factor(died_covid))) +
  geom_bar(position = "dodge", stat = "count", alpha = 0.7, width = 0.7, color = "white") +
  scale_fill_manual(values = c("royalblue", "rosybrown2"), name = "Died from COVID-19:", labels = c("No", "Yes")) +
  labs(title = "Mortality Patterns by Race: All Deaths vs Deaths Attributed to COVID-19",
       subtitle = "Using a Grouped Bar Chart",
       x = "Race",
       y = "Count (Deaths)") +
  theme_classic() +
  theme(
    plot.background = element_rect(fill = "white"),  # Set the background color
    axis.text.x = element_text(angle = 35, hjust = 1, size = 7, family = "georgia"),
    legend.position = "top",
    legend.justification = "left",
    text = element_text(family = "arial", face = "bold"),
    legend.title = element_text(face = "italic", size = 10, color = "black"),
    legend.text = element_text(face = "italic", size = 10, color = "black" )
  ) +
  geom_text(aes(label = ..count.., group = factor(died_covid), color = factor(died_covid)), stat = "count",
            position = position_dodge(width = 0.7), vjust = -0.5, size = 3) +
  scale_color_manual(values = c("royalblue4", "rosybrown4")) +
  guides(color = "none") +
  annotate("text", x = 1, y = -10, label = "", size = 5, vjust = -0.5) +
  scale_x_discrete(expand = c(0, 0))

