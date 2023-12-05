

#Importing Data

covid_data_no_duplicates %>%
  ggplot(aes(x=case_age,
             y = positive_pcr_)) +
  geom_point()

