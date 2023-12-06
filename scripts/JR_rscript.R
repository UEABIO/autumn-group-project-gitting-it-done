
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
  
  
  # Now realising i need to create sum columns of Yes for each symptom
  
  
  
  new_covid_symptoms_data <- covid_symptoms_data %>% 
    mutate(sym_fever_count = sym_fever)

  
  # Attempt to bodge R... ----
  #Going to try adding up all the Yes symptoms and creating a new Tibble
  
  # Finding out number of Yes for each symptom...
  covid_symptoms_data %>% 
    group_by(sym_fever) %>% 
    summarise(num=n())
  # No. Yes for fever = 15126
  
  covid_symptoms_data %>% 
    group_by(sym_subjfever) %>% 
    summarise(num=n())
  # No. Yes for subjfever = 12711
  
  covid_symptoms_data %>% 
    group_by(sym_myalgia) %>% 
    summarise(num=n())
  # No. Yes for myalgia = 19532
  
  covid_symptoms_data %>% 
    group_by(sym_loss_taste_smell) %>% 
    summarise(num=n())
  # No. Yes for loss taste/ smell = 12734
  
  covid_symptoms_data %>% 
    group_by(sym_sorethroat) %>% 
    summarise(num=n())
  # No. Yes for sorethroat = 12516
  
  covid_symptoms_data %>% 
    group_by(sym_cough) %>% 
    summarise(num=n())
  # No. Yes for cough =  21942
  
  covid_symptoms_data %>% 
    group_by(sym_headache) %>% 
    summarise(num=n())
  # No. Yes for headache = 21673

  
  #Making the vectors
  
  fever <- c(15126)
  subjfever <- c(12711)
  myalgia <- c(19532)
  loss_taste_smell <- c(12734)
  sore_throat <- c(12516)
  cough <- c(21942)
  headache <- c(21673)
  
  # make a tibble
  symptom_totals <- tibble(fever, subjfever, myalgia, loss_taste_smell,
                           sore_throat, cough, headache)
  symptom_totals
  
  
  # Now to plot the new tibble...
  
  symptom_totals %>% 
    ggplot(aes(x= cough))+
    geom_bar()
  
  
  
  ggplot(data = symptom_totals %>% gather(fever, subjfever, myalgia,
                                          loss_taste_smell, sore_throat,
                                          cough, headache), 
        aes(x = fever, y = myalgia )) + 
    geom_bar(stat = 'identity', position = 'dodge')
  
  
  
  

  
  
  
  
#_________________----
# % of people with symptoms ----
# Working out Covid symptoms shown as a proportion of total positive PCR tests



