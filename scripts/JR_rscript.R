
# Jasper Raistrick Data Visualisation ----



pallete_1 <- c("#FF8C00", "#A034F0", "#159090")

covid_data_no_duplicates %>% 
  ggplot(aes(x = hospitalized,
             y = case_age,
             fill = hospitalized,
             colour = hospitalized))+
  geom_violin(alpha = 0.2)+
  geom_boxplot(width = 0.2,
               alpha = 0.6)+
  scale_fill_manual(values = pallete_1)+
  scale_colour_manual(values = pallete_1)+
  theme_classic()+
  theme(legend.position = "none")+
  labs(
    x = "Hospitalized",
    y = "Age",
    title = "Age of cases who are hospitalized or not",
    subtitle = "Box and Violin Plot")
