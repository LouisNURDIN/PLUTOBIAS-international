library(haven)
library(dplyr)
library(tidyr)
#Ouverture de la base ----
GMP_inc <- read_dta("data/raw/wpid/gmp-inc.dta")

GMP_inc <- GMP_inc %>%
  rename(dataset_party_id = vote)

sum(GMP_inc$vote == "", na.rm = TRUE)
sum(GMP_inc$vote == "None", na.rm = TRUE)
sum(GMP_inc$vote == "No vote", na.rm = TRUE)
sum(GMP_inc$vote == "Blank", na.rm = TRUE)
unique(GMP_inc$dinc)  #dinc = décile du répondant
unique(GMP_inc$type)  # type = type d'élection

sum(GMP_inc$turnout == 1 & GMP_inc$vote == "", na.rm = TRUE)
sum(GMP_inc$turnout == 0 & GMP_inc$vote != "", na.rm = TRUE) #a priori erreur dans PF, s'en référer à PF

table(GMP_inc$dinc)
#Ajout Partyfacts avedc id WPID ----
Partyfacts_id <- read.csv("data/raw/partyfacts-external-parties (1).csv", sep = ";")

Partyfacts_id_wpidmicro <- Partyfacts_id %>%
  filter(dataset_key == "wpidmicro")
unique(Partyfacts_id$dataset_key)

Partyfacts_id_parlgov <- Partyfacts_id %>%
  filter(dataset_key == "parlgov")


#2ème méthode = on enlève les élections où turnout = NA + les élections où Turnout = NA et/ou toujours la même valeur----
GMP_inc_2 <- GMP_inc %>%
  group_by(isoname, year,type) %>%
  filter(
    sum(!is.na(turnout)) > 0,          # pas uniquement NA
    length(unique(turnout[!is.na(turnout)])) > 1  # au moins 0 ET 1
  ) %>%
  ungroup()

#On supprime les électeurs où vote = "vide" et turnout = NA ou 1
GMP_inc_2 <- GMP_inc_2 %>%
  filter(
    !(dataset_party_id == "" & (turnout == 1 | is.na(turnout))))


#Abstention dans la 2ème méthode
GMP_inc_2 <- GMP_inc_2 %>%
  mutate(
    dataset_party_id = case_when(
      dataset_party_id == "Blank" ~ "Abstention",
      dataset_party_id == "None" ~ "Abstention",
      dataset_party_id == "No vote" ~ "Abstention", 
      dataset_party_id == "" & turnout == 0 ~ "Abstention",
      TRUE ~ dataset_party_id
    )
  )

##Ajout iso dans le nom des partis ----
GMP_inc_2 <- GMP_inc_2 %>%
  mutate(
    dataset_party_id = case_when(
      is.na(dataset_party_id) | dataset_party_id == "" ~ dataset_party_id,
      TRUE ~ paste0(iso, "-", dataset_party_id)
    )
  )

##Join Partyfacts dans GMP inc ----
Base_all_elections <- GMP_inc_2 %>%
  left_join(
    Partyfacts_id_wpidmicro %>%
      dplyr::select(dataset_party_id,partyfacts_id),
    by = "dataset_party_id"
  )

library(stringr)
Base_all_elections <- Base_all_elections %>%
  mutate(
    partyfacts_id = as.character(partyfacts_id),
    partyfacts_id = case_when(
      str_detect(dataset_party_id, "Abstention$") ~ "Abstention",
      str_detect(dataset_party_id, "Other$") ~ "Other",
      TRUE ~ partyfacts_id
    )
  )


## Filtre législatives dans deuxième méthode ----
Base_elections_legislatives <- GMP_inc_2 %>%
  filter(type == "Lower house")
unique(GMP_inc$dinc)

#Corrections partis problématiques
Base_elections_legislatives <- Base_elections_legislatives %>%
  mutate(
    partyfacts_id = case_when(
      partyfacts_id == "1388" & isoname == "United Kingdom" ~ "540",
      partyfacts_id == "1231" & isoname == "Switzerland" ~ "360",
      partyfacts_id == "7415" & isoname == "Sweden" ~ "199",
      partyfacts_id == "5750" & isoname == "Spain" ~ "441",
      partyfacts_id == "8814" & isoname == "Spain" ~ "441",
      partyfacts_id == "1004" & isoname == "Canada" & year <= 2003 ~ "232",
      partyfacts_id == "1004" & isoname == "Canada" & year >= 2003 ~ "1004" ,
      partyfacts_id == "1044" & isoname == "Finland" ~ "1096" ,
      partyfacts_id == "4785" & isoname == "France" ~ "1478" ,
      partyfacts_id == "5514" & isoname == "France" ~ "1083" ,
      partyfacts_id == "8041" & isoname == "France" ~ "1083" ,
      TRUE ~ partyfacts_id
    )
  )

#Export Base globale et Base législatives
write.csv(
  Base_elections_legislatives,
  "data/intermediary/elections/legislative elections dataset.csv",
  row.names = FALSE
)

write.csv(
  Base_all_elections,
  "data/intermediary/elections/all elections dataset.csv",
  row.names = FALSE
)

