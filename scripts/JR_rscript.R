
# Jasper Raistrick Data Visualisation ----


# What are the most common covid symptoms recorded? ----
#_________________----

# Filtering and Selecting variables ----

# Selecting just symptom variables from dataset using dplyr and naming as object

covid_symptoms_data <- select(.data = covid_data_no_duplicates, 
       sym_fever, sym_subjfever, sym_myalgia, sym_loss_taste_smell,
       sym_sorethroat, sym_cough, sym_headache)


# Filtering out 'No', 'Unknown' and 'N/a' results for all symptoms
# And creating new object of filtered dataset 'symptpms_yes_data'
symptoms_yes_data <- filter(.data = covid_symptoms_data, 
                            sym_fever == "Yes", sym_subjfever == "Yes", 
                            sym_myalgia == "Yes", sym_loss_taste_smell == "Yes",
                            sym_sorethroat == "Yes", sym_cough == "Yes",
                            sym_headache == "Yes") 


symptoms_yes_data %>% 
  glimpse()

symptoms_yes_data %>% 
  count(sym_loss_taste_smell)


symptoms_yes_data %>%
  ggplot(aes(x=)) +
  geom_bar()
  

  
  
  covid_symptoms_data %>% 
  count(sym_fever)
 
  covid_symptoms_data %>% 
    filter(sym_fever== "Yes") %>%
    count(sym_fever)
  
  
  
  
  
  
  
#_________________----
# % of people with symptoms ----
# Working out Covid symptoms shown as a proportion of total positive PCR tests



