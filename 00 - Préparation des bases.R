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

GMP_inc_2 <- GMP_inc_2 %>%
  mutate(source_recode = "WPID")
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
Base_elections_legislatives <- Base_all_elections %>%
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
      partyfacts_id == "Other" & isoname == "India" & year == 1977  ~ "4788" ,
      partyfacts_id == "1332" & isoname == "Canada" & year == 2000 ~ "1757",
      dataset_party_id == "BW-Umbrella for Democratic Change" & isoname == "Botswana" & year == 2014 ~ "4832",
      partyfacts_id == "604" & isoname == "Belgium" & year == 2007 ~ "756",
      partyfacts_id == "500" & isoname == "Belgium" ~ "480",
      partyfacts_id == "1626" & isoname == "Italy" & year == 2001 ~ "6241",
      partyfacts_id == "813" & isoname == "Italy" & year == 2001 ~ "6241",
      partyfacts_id == "1221" & isoname == "Italy" & year == 2001 ~ "6241",
      partyfacts_id == "962" & isoname == "Italy" & year == 2001 ~ "6241",
      partyfacts_id == "1626" & isoname == "Italy" & year == 2006 ~ "6241",
      partyfacts_id == "813" & isoname == "Italy" & year == 2006 ~ "6241",
      partyfacts_id == "1221" & isoname == "Italy" & year == 2006 ~ "6241",
      partyfacts_id == "878" & isoname == "Italy" & year == 2001 ~ "1737",
      partyfacts_id == "279" & isoname == "Italy" & year == 2001 ~ "1737",
      partyfacts_id == "1635" & isoname == "Italy" & year == 2001 ~ "1737",
      partyfacts_id == "1737" & isoname == "Italy" & year == 2006 ~ "1372",
      partyfacts_id == "279" & isoname == "Italy" & year == 2006 ~ "1372",
      partyfacts_id == "1711" & isoname == "Italy" & year == 2006 ~ "1372",
      partyfacts_id == "3956" & isoname == "Italy" & year == 2006 ~ "1372",
      partyfacts_id == "1404" & isoname == "Italy" & year == 2006 ~ "1372",
      partyfacts_id == "4107" & isoname == "Japan" ~ "3",
      partyfacts_id == "6183" & isoname == "Poland" & year == 2001 ~ "57",
      partyfacts_id == "7594" & isoname == "Iraq" & year == 2010 ~ "5615",
      partyfacts_id == "7594" & isoname == "Iraq" & year == 2014 ~ "5615",
      partyfacts_id == "6561" & isoname == "South Korea" & year == 2000 ~ "2548",
      partyfacts_id == "6561" & isoname == "South Korea" & year == 2008 ~ "2305",
      partyfacts_id == "6561" & isoname == "South Korea" & year == 2012 ~ "2305",
      partyfacts_id == "5766" & isoname == "Senegal" & year == 2012 ~ "4010",
      partyfacts_id == "2379" & isoname == "Senegal" & year == 2012 ~ "4010",
      partyfacts_id == "2380" & isoname == "Senegal" & year == 2012 ~ "4010",
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

unique(GMP_inc$type[GMP_inc$isoname == "Italy"])

unique(GMP_inc_2$type[GMP_inc_2$isoname == "Italy"])
