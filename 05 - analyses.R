library(dplyr)
Base_complete_index <-  read.csv("data/final/dataset complete with index.csv", sep = ",")

Base_complete_index_income <- Base_complete_index %>%
  filter(Base_complete_index$bias == "plutocracy")

Base_complete_index_gender<- Base_complete_index %>%
  filter(Base_complete_index$bias == "phallocracy")

Base_complete_index_educ <- Base_complete_index%>%
  filter(Base_complete_index$bias == "epistocracy")

Base_complete_index_age <- Base_complete_index %>%
  filter(Base_complete_index$bias == "gerontocracy")

#Filtre pour travailler sur des bases propres