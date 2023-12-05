
# Jasper Raistrick Data Visualisation ----


# What are the most common covid symptoms recorded? ----
#_________________----

# Filtering and Selecting variables ----

# Selecting just symptom variables from dataset using dplyr and naming as object

covid_symptoms_data <-  covid_data_no_duplicates %>% 
       select(sym_fever, sym_subjfever, sym_myalgia, sym_loss_taste_smell,
       sym_sorethroat, sym_cough, sym_headache)


# Filtering out 'No', 'Unknown' and 'N/a' results for all symptoms
# And creating new object of filtered dataset 'symptpms_yes_data'
symptoms_yes_data <- covid_symptoms_data %>%  
                            filter(sym_fever == "Yes", sym_subjfever == "Yes", 
                            sym_myalgia == "Yes", sym_loss_taste_smell == "Yes",
                            sym_sorethroat == "Yes", sym_cough == "Yes",
                            sym_headache == "Yes") 

sym_fever_yes <- covid_symptoms_data %>%
  filter(sym_fever == "Yes") %>% 
  select(sym_fever)

covid_symptoms_data %>% 
  group_by(sym_fever) %>% 
  summarise(num=n())    # Getting an idea for numbers of symptom Yes cases


 sym_cough_yes <- covid_symptoms_data %>%
   filter(sym_cough == "Yes") %>% 
   select(sym_cough)

 

# Testing combining yes variables without taking out rows
 
 
 covid_symptoms_data <-  covid_data_no_duplicates %>% 
   select(sym_fever, sym_subjfever, sym_myalgia, sym_loss_taste_smell,
          sym_sorethroat, sym_cough, sym_headache)
 
 
 
  covid_symptoms_data %>% 
    group_by(sym_fever) %>% 
    summarise(num=n())
  
  
  
  
  covid_symptoms_data %>% 
    select_if("Yes") %>% 
    glimpse()
  
  # Now realising i need to create sum columns of Yes for each symptom
  
  
  
  

  
#_________________----
# % of people with symptoms ----
# Working out Covid symptoms shown as a proportion of total positive PCR tests



