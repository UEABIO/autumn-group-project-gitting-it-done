# KC Covid Outbreak Situation Report ----
# A boxplot to understand the distribution between Age and Race using data from a Covid Report----

# clearing the environment (added a # so that this doesnt happen when running the code in the markdown )
# rm(list=ls()) 

# loading the packages ----
library(tidyverse)
library(janitor)
library(ggplot2)
library(lubridate)
library(plotly)
library(here)
library(ggthemes)

# Renaming the data set before creating the box plot  ----
covid_data_race_and_sym_numeric <- covid_data_no_duplicates

# Remove NAs from case_age variable ----
covid_data_race_and_sym_numeric <- covid_data_race_and_sym_numeric[complete.cases(covid_data_race_and_sym_numeric$case_age), ]

# Remove NAs from case_race variable ----
covid_data_race_and_sym_numeric <- covid_data_race_and_sym_numeric[complete.cases(covid_data_race_and_sym_numeric$case_race), ]

# Check the resulting dataset
head(covid_data_race_and_sym_numeric)

# Remove rows with "other", "unknown" and "under review" values in case_race ----
covid_data_race_and_sym_numeric <- covid_data_race_and_sym_numeric %>%
  filter(!(case_race %in% c("UNKNOWN", "OTHER", "UNDER REVIEW", "NA", NA)))

# Remove rows with "other", "unknown" and "under review" values in symptom_count ----
covid_data_race_and_sym_numeric <- covid_data_race_and_sym_numeric %>%
  filter(!(case_age %in% c("UNKNOWN", "OTHER", "UNDER REVIEW", "NA")))

# getting rid of the -20 age from case_age ----
covid_data_race_and_sym_numeric <- covid_data_race_and_sym_numeric %>%
  filter(case_age >= 0,
         case_age<= 110)

# Using the unique function to check this worked -----
unique(covid_data_race_and_sym_numeric$case_race)
unique(covid_data_race_and_sym_numeric$case_age)


# creating an age race box plot ----
age_and_race_boxplot_KC <- ggplot(covid_data_race_and_sym_numeric, aes(x = case_race, y = case_age, fill = case_race)) + #plotting the x and y axis 
  geom_boxplot(outlier.shape = 16, alpha = 0.7, position = position_dodge(width = 0.8)) + # adds the data in a  boxplot 
  stat_ellipse(aes(color = case_race), type = "norm", level = 0.95, position = position_dodge(width = 0.8)) + # adds ellipses to the plot, representing the normal distribution of the case_race data set 
  scale_fill_manual(values = c("mediumpurple", "paleturquoise", "seagreen2", "lightgoldenrod1", "lightpink")) + #manually setting the colours for the different races 
  labs(title = "Unpacking COVID-19 Demographics: An insight into the link between Age and Race.", #adding a title to the explain the graph 
       subtitle = "Examining Variability and Trends in Age Distribution among Different Racial Groups; as well as showing the median ages.", #adding a subtitle to add further information about the boxplot 
       x = "Race", # remaning the x axis 
       y = "Age") + # remaning the y axis 
  scale_x_discrete(labels = NULL) +  # Remove x-axis labels as they are already in the legend 
  theme_minimal() + #same theme as the rest of the group to make the plots look good together
  theme(
    axis.title.y = element_text(size = 12), # to adjust the size of the title on the x-axis
    axis.text.x = element_blank(),  # Remove x-axis labels as they are already in the figure legend
    axis.text.y = element_text(size = 10), # to adjust the size of the title on the y-axis
    plot.title = element_text(size = 14, face = "bold"), #making the title bold and big 
    legend.position = "top", #position of the title on the page 
    legend.title = element_text(size = 12), #adjusting the size of the title in the legend box
    legend.text = element_text(size = 10), #adjusting the size of the text in the legend box
    panel.grid.major = element_line(color = "gray", size = 0.2), #to change the colour and size of the major grid lines
    panel.grid.minor = element_blank(), #can adjust the minor grid lines if needed
    legend.background = element_rect(fill = "white", color = "black", size = 0.5), #background colour of the legend can be adjusted 
    panel.border = element_rect(color = "black", fill = NA, size = 0.7), #allows us to change the colour and size of the panel border
    strip.background = element_rect(fill = "white", color = "black", size = 0.7), #allows us to change the colour and size of the strip background
    strip.text = element_text(face = "bold") # making the text bold 
  ) +
  guides(fill = guide_legend(override.aes = list(alpha = 1))) + 
  scale_color_manual(values = c("mediumpurple", "paleturquoise", "seagreen2", "lightgoldenrod1", "lightpink")) + #manually setting the colours for the different races 
  labs(fill = "Race:") + 
  
  
# Adding labels to the boxplot to show the median age values ----
stat_summary( # used to calculate summary statistics and display them on the box plot 
  fun = "median", # the fuction i want to be applied is median 
  geom = "text", #displays it in a text format 
  position = position_dodge(width = 0.8), #controls the position of the values 
  aes(label = sprintf("Median: %.1f", ..y..)), #specifies how the data should be mapped
  size = 3, #changing the size of the values 
  vjust = -1  # Adjust the vertical position of the labels
) 
print(age_and_race_boxplot_KC) # allows us to see the plot 

# saving the figure as a png 
ggsave("figures/KC_age_and_race_boxplot.png",
       plot = age_and_race_boxplot_KC,
       width = 35, #adjusting the width
       height = 20, # adjusting the height
       units = "cm",
       device = "png")