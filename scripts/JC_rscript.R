# Correlation between age, gender and hospitalization rate 

# I want to look at the relationship between sex and rate of hospitalization 
# I want to look at the relationship between age and rate of hospitalization
# I want to create a visual which encompasses both factors and hospitalization 

#rm(list=ls())

#Setting up Script ----

# PACKAGES 
library(tidyverse)
library(janitor)
library(ggplot2)
library(lubridate)
library(plotly)
library(here)
library(ggthemes)

# Creating and tidying data set ----

covid_data_age_gender_hospitalized <- select(.data = covid_data_no_duplicates, 
                                             case_age, case_gender, hospitalized)

covid_data_age_gender_hospitalized <- arrange(.data = covid_data_age_gender_hospitalized,
                                              case_gender,case_age) 

# Having looked at the data i would like to know:
# total males and females
# remove all NA values from all three variables 
# remove any unrelaistic ages(-20)
# total males hospitalised 
# total females hospitalised

# Managing the NA values ---- 

# Get a sum of how many observations are missing in our dataframe
#covid_data_age_gender_hospitalized %>% 
# is.na() %>% 
#sum()

# Removing all NA values 
covid_data_age_gender_hospitalized_2 <- drop_na(covid_data_age_gender_hospitalized)

#covid_data_age_gender_hospitalized_2 %>% 
# is.na() %>% 
#sum()

# glimpse(covid_data_age_gender_hospitalized_2)

# Exploring values - 

covid_data_age_gender_hospitalized_2 %>% 
  summarise(n=n(), # number of rows of data
            quanitity_female = sum(case_gender == "Female"),
            quanitity_male = sum(case_gender == "Male"),
            quanitity_unknown = sum(case_gender == "Unknown"),
            quantity_hospitalised = sum(hospitalized == "Yes"),
            quantity_not_hospitalised = sum(hospitalized == "No"))

# 81 Genders unknown, however cannot discard this data as data entry may not have accounted for non-binary/trans community

# Data Manipulation ----

# Totals of each gender of each age 
totals_age_gender <- covid_data_age_gender_hospitalized_2 %>% 
  group_by(case_age, case_gender) %>% 
  summarise(num=n())

# number of hospitalized for each age, removing unknown data 

age_vs_hospitalisation_data <- covid_data_age_gender_hospitalized_2 %>% 
  group_by(case_age, case_gender) %>% 
  filter(hospitalized == "Yes", !case_gender == "Unknown") %>% 
  summarise(n = n()) 


# creating a data set with percentage hospitalised from age and gender 

age_gender_total_hosp <- merge(age_vs_hospitalisation_data, totals_age_gender, by=c("case_age","case_gender")) %>% 
  mutate(percentage = (n/num)*100) 

# checking the factors of my data 
#glimpse(age_gender_total_hosp)

# separating the data into age brackets to look at summary to find average hospitalization rates for those age ranges  
age_gender_total_hosp_child <- age_gender_total_hosp %>% 
  filter(case_age <= 17) 

age_gender_total_hosp_adult <- age_gender_total_hosp %>% 
  filter(case_age %in% (18:49)) 

age_gender_total_hosp_elderly <- age_gender_total_hosp %>% 
  filter(case_age >= 50) 

# Creating a data set to add to graph so labels can be added in specific coordinates 
case_age <- c(9,34,80,9, 34,80)
percentage <- c(88,88,88,95,95,95)
label <- c("3.05% avg", "5.57% avg.", "38.71% avg.","0-17 yrs.","18-49 yrs", "50+ yrs")

my_labels <- tibble(case_age, percentage, label) 

# Data Visual ----

# % Patients Hospitalised by age and gender figure 
risk_of_hospitalisation_plot <- age_gender_total_hosp %>% #assigning to an object to be pulled to Rmarkdown
  ggplot() + #calling a plot
  scale_x_continuous(expand = c(0, 0), limits = c(0, NA)) + # Changing x-axis to start at zero
  scale_y_continuous(expand = c(0, 0), limits = c(0, 105))+ # Changing y-axis to start at zero
  geom_rect(aes(xmin=0,xmax=18,ymin=-Inf,ymax=Inf),alpha= 0.2,fill= "lightcyan1")+ #adding colour block bachgrounds to indicate age categories
  geom_rect(aes(xmin=18,xmax=50,ymin=-Inf,ymax=Inf),alpha=0.2, fill=  "lightcyan2")+
  geom_rect(aes(xmin=50,xmax=110,ymin=-Inf,ymax=Inf),alpha=10,fill= "lightcyan3")+
  geom_point(aes(x = case_age, #creating scatter plot
                 y = percentage,
                 colour = case_gender))+ 
  geom_segment(x = 50, y = 25, # adding in a an indicative arrow 
               xend = 80, yend = 72,
               color = "black",
               lwd = 0.2,
               arrow = arrow())+
  scale_color_manual(values=c("violetred","gray30"), name = "Case Gender")+ # Altering the color of data plots
  geom_hline(yintercept = 50, # adding in a horizontal intercept line 
             linetype = "dashed",
             color = "black",
             lwd = 0.5)+  geom_label(data = my_labels, aes(x = case_age,
                                                           y = percentage,
                                                           label = label))+  
  labs(x = "Patient Age", #creting axis titles 
       y = "% of Patients Hospitalised",
       title= "Increased risk of hospitalisation from Covid-19 \nwith increased age and gender type",
       subtitle= "Percentage of patients hospitalised based on age and gender",
       caption = "The arrow indicates risk is greater for those over the age of 50.
The colour blocks break the graph into age categories: 0-17, 18-49 and 50+ and show the average percentage of patients hopsitalised in that age category.
Dashed line at 50% to indicate the ages (data points above this line) where over half of confirmed covid patients were hospitalised.")+
  theme(plot.title = element_text(hjust = 0.5, size = 25), # Altering the sizes of text on graph 
        plot.subtitle = element_text(hjust = 0.5, size = 20),
        plot.caption = element_text(hjust = 0, size = 8),
        axis.text = element_text( hjust = 1,size = 10),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        legend.title = element_text(size = 15), 
        legend.text = element_text(size = 12))

print(risk_of_hospitalisation_plot)

# Saving Visual ----

#Saving figure as PNG
ggsave("figures/JC_risk_of_hospitalisation_plot.png",
       plot = risk_of_hospitalisation_plot,
       width = 35,
       height = 20,
       units = "cm",
       device = "png")
