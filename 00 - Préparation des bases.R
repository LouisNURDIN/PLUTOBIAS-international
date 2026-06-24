library(haven)
library(dplyr)
library(tidyr)
library(stringr)
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

GMP_inc <- GMP_inc %>%
  filter(source != "CSES")
GMP_inc <- GMP_inc %>%
  filter(source != "World Values Survey, Argentina")
GMP_inc <- GMP_inc %>%
  filter(source != "WVS")
GMP_inc <- GMP_inc %>%
  filter(source != "ESS")
GMP_inc <- GMP_inc %>%
  filter(!str_starts(source, "European Social Survey"))
unique(GMP_inc$source)

table(GMP_inc$dinc)
#Ajout Partyfacts avedc id WPID ----
Partyfacts_id <- read.csv("data/raw/partyfacts-external-parties (1).csv", sep = ";")
unique(Partyfacts_id$dataset_key)
Partyfacts_id_wpidmicro <- Partyfacts_id %>%
  filter(dataset_key == "wpidmicro")

Partyfacts_id_parlgov <- Partyfacts_id %>%
  filter(dataset_key == "parlgov")

Partyfacts_id_cses <- Partyfacts_id %>%
  filter(dataset_key == "cses")

Partyfacts_id_wvs <- Partyfacts_id %>%
  filter(dataset_key == "wvs")

Partyfacts_id_ess <- Partyfacts_id %>%
  filter(dataset_key == "essprtv")

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

GMP_inc_2 <- GMP_inc_2 %>%
  rename(gender = sex)

GMP_inc_2 <- GMP_inc_2 %>%
  mutate(
    gender = as.character(gender),
    gender = case_when(
      gender == "0" ~ "men",
      gender == "1" ~ "women",
      TRUE ~ gender
    )
  )

unique(GMP_inc_2$educ)
unique(GMP_inc_2$age)

GMP_inc_2_clean <- GMP_inc_2 %>%
  select(isoname,year, source, source_recode, survey,type, dinc,gender,educ, age, turnout,dataset_party_id)
##Join Partyfacts dans GMP inc ----
Base_all_elections <- GMP_inc_2_clean %>%
  left_join(
    Partyfacts_id_wpidmicro %>%
      dplyr::select(dataset_party_id,partyfacts_id),
    by = "dataset_party_id"
  )


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

cor(Base_elections_legislatives$age, Base_elections_legislatives$educ, 
    use = "complete.obs")
cor(Base_elections_legislatives$age, Base_elections_legislatives$dinc, 
    use = "complete.obs")
cor(Base_elections_legislatives$educ, Base_elections_legislatives$dinc, 
    use = "complete.obs")

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
      partyfacts_id == "604" & isoname == "Belgium" & year == 2003 ~ "622",
      partyfacts_id == "604" & isoname == "Belgium" & year == 2010 ~ "622",
      partyfacts_id == "604" & isoname == "Belgium" & year == 2014 ~ "622",
      partyfacts_id == "8259" & isoname == "Belgium" & year == 1999 ~ "554",
      partyfacts_id == "1680" & isoname == "Belgium" & year == 2003 ~ "1586",
      partyfacts_id == "500" & isoname == "Belgium" & year <= 1977  ~ "480",
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
      partyfacts_id == "5917" & isoname == "Iraq" & year == 2010 ~ "5919",
      partyfacts_id == "6303" & isoname == "Italy" & year == 2013  ~ "1626",
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


#Ajout de nouvelles bases ----
#ESS ----
ess_data <- read.csv("data/raw/ess/Datafile-subset.csv")

ess_data <- ess_data %>%
  select(-age)
ess_data_long <- ess_data %>%
  pivot_longer(cols = starts_with("prtv"),names_to = "variable",values_to = "party_id")

ess_data_long <- ess_data_long %>%
  group_by(cntry, party_id) %>%
  mutate(first_essround = min(essround, na.rm = TRUE)) %>%
  ungroup()

ess_data_long <- ess_data_long %>%
  mutate(
    ess_id = case_when(
      cntry %in% c("DE", "LT") ~ paste(cntry,first_essround,party_id,substr(variable, 4, 4),str_sub(variable, -3, -1),
        sep = "-"
      ),TRUE ~ paste( cntry, first_essround,party_id,substr(variable, 4, 4),sep = "-")))  

sum(is.na(ess_data_long$ess_id))
###Identifier les variables qui nous intéressent pour les harmoniser----
ess_data_long <- ess_data_long %>%
  rename(isoname = cntry)

ess_data_long <- ess_data_long %>%
  rename(source = name)
ess_data_long <- ess_data_long %>%
  mutate(source_recode = "ESS")
ess_data_long <- ess_data_long %>%
  rename(year = essround)
ess_data_long <- ess_data_long %>%
  rename(turnout = vote)
ess_data_long <- ess_data_long %>%
  rename(dataset_party_id = ess_id)
ess_data_long <- ess_data_long %>%
  mutate(type = "?")
ess_data_long <- ess_data_long %>%
  rename(inc = hinctnta)
ess_data_long <- ess_data_long %>%
  rename(gender = gndr)
ess_data_long <- ess_data_long %>%
  rename(educ = eisced)

ess_data_long <- ess_data_long %>%
  rename(age = agea)

ess_data_long <- ess_data_long %>%
  mutate(survey = "Post-electoral")

#Filtrer mes données sur le vote
ess_data_long <- ess_data_long %>%
  filter(!stringr::str_detect(dataset_party_id, "NA"))

ess_data_long <- ess_data_long %>%
  filter(!str_detect(dataset_party_id, "66|77|88|99"))
table(ess_data_long$turnout)
ess_data_long <- ess_data_long %>%
  filter(turnout < 7)

ess_data_clean <- ess_data_long %>%
  select(isoname,year, source, source_recode,survey, type, inc,gender,educ,age, turnout, dataset_party_id)

ess_data_clean <- ess_data_clean %>% mutate(dataset_party_id = trimws(dataset_party_id))
Partyfacts_id_ess <- Partyfacts_id_ess %>% mutate(dataset_party_id = trimws(dataset_party_id)) 

 ess_data_clean <- ess_data_clean %>% mutate(dataset_party_id = toupper(dataset_party_id))
 Partyfacts_id_ess <- Partyfacts_id_ess %>% mutate(dataset_party_id = toupper(dataset_party_id))

#Join partyfacts dans cses
ess_data_clean <- ess_data_clean %>%
  left_join(
    Partyfacts_id_ess %>%
      dplyr::select(dataset_party_id,partyfacts_id),
    by = "dataset_party_id")

sum(is.na(ess_data_clean$partyfacts_id))

ess_data_clean <- ess_data_clean %>%
  mutate(
    dataset_party_id = case_when(
      turnout == 2 ~ "Abstention",
      turnout == 3 ~ "Abstention",
      TRUE ~ as.character(dataset_party_id)
    )
  )
unique(ess_data_clean$dataset_party_id)
unique(Partyfacts_id_ess$dataset_party_id)

#Problème de join 
#Diagnostic
##moyenne de partis qui joinent
mean(ess_data_clean$dataset_party_id %in% Partyfacts_id_ess$dataset_party_id)
#liste des partis ess qui ne trouvent pas d'id partyfacts
View(ess_data_clean %>%
  anti_join(Partyfacts_id_ess, by = "dataset_party_id") %>%
  count(dataset_party_id, sort = TRUE))
Partyfacts_id_ess %>%
  head(20)
names(Partyfacts_id_ess)


ess_data_clean <- ess_data_clean %>%
  mutate(
    gender = case_when(
      gender == "1" ~ "men",   
      gender == "2" ~ "women",
      TRUE ~ as.character(gender)
    )
  )

ess_data_clean <- ess_data_clean %>%
  mutate(
    year = case_when(
      year == "1" ~ "2002",   
      year == "2" ~ "2004",
      year == "3" ~ "2006",
      year == "4" ~ "2008",
      year == "5" ~ "2010",
      year == "6" ~ "2012",
      year == "7" ~ "2014",
      year == "8" ~ "2016",
      year == "9" ~ "2018",
      year == "10" ~ "2020",
      year == "11" ~ "2023",
      
      TRUE ~ as.character(year)))
unique(ess_data_clean$survey_year)

ess_data_clean <- ess_data_clean %>%
  mutate(
    isoname = case_when(
      isoname == "AL" ~ "Albania",   
      isoname == "AT" ~ "Austria",
      isoname == "BE" ~ "Belgium",
      isoname == "BG" ~ "Bulgaria",
      isoname == "CH" ~ "Switzerland",
      isoname == "CY" ~ "Cyprus",
      isoname == "CZ" ~ "Czech Republic",   
      isoname == "DE" ~ "Germany",
      isoname == "DK" ~ "Denmark",
      isoname == "EE" ~ "Estonia",
      isoname == "ES" ~ "Spain",
      isoname == "FI" ~ "Finland",
      isoname == "FR" ~ "France",   
      isoname == "GE" ~ "Georgia",
      isoname == "GB" ~ "United Kingdom",
      isoname == "GR" ~ "Greece",
      isoname == "HR" ~ "Croatia",
      isoname == "HU" ~ "Hungaria",
      isoname == "IE" ~ "Ireland",   
      isoname == "IS" ~ "Island",
      isoname == "IL" ~ "Israel",
      isoname == "IT" ~ "Italy",
      isoname == "LT" ~ "Lithuania",
      isoname == "LU" ~ "Luxembourg",
      isoname == "LV" ~ "Latvia",  
      isoname == "ME" ~ "Montenegro",
      isoname == "MK" ~ "North Macedonia",
      isoname == "NL" ~ "Netherlands",
      isoname == "NO" ~ "Norway",
      isoname == "PL" ~ "Poland",
      isoname == "PT" ~ "Portugal",  
      isoname == "RO" ~ "Romania",
      isoname == "RS" ~ "Serbia",
      isoname == "RU" ~ "Russia",
      isoname == "SE" ~ "Serbia",
      isoname == "SI" ~ "Slovenia",
      isoname == "SK" ~ "Slovakia",   
      isoname == "TR" ~ "Turkey",
      isoname == "UA" ~ "Ukraine",
      isoname == "XK" ~ "Kosovo",
      
      TRUE ~ as.character(isoname)))



#Traitement des partyfacts dans cses pour join 
ess_data_clean <- ess_data_clean %>%
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
      partyfacts_id == "604" & isoname == "Belgium" & year == 2003 ~ "622",
      partyfacts_id == "604" & isoname == "Belgium" & year == 2010 ~ "622",
      partyfacts_id == "604" & isoname == "Belgium" & year == 2014 ~ "622",
      partyfacts_id == "8259" & isoname == "Belgium" & year == 1999 ~ "554",
      partyfacts_id == "1680" & isoname == "Belgium" & year == 2003 ~ "1586",
      partyfacts_id == "500" & isoname == "Belgium" & year <= 1977  ~ "480",
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
      partyfacts_id == "5917" & isoname == "Iraq" & year == 2010 ~ "5919",
      partyfacts_id == "6303" & isoname == "Italy" & year == 2013  ~ "1626",
      TRUE ~ as.character(partyfacts_id)
    )
  )

class(ess_data_clean$partyfacts_id)

###Export Base ESS ----
write.csv(
  ess_data_clean,
  "data/intermediary/elections/ess elections dataset.csv",
  row.names = FALSE
)

unique(Base_all_elections$source)

#CSES ----
cses_data <- read.csv("data/raw/cses/cses_imd.csv")
###Identifier les variables qui nous intéressent pour les harmoniser----
cses_data <- cses_data %>%
  rename(isoname = IMD1006_NAM)
cses_data <- cses_data %>%
  rename(dataset_key= IMD1001)
cses_data <- cses_data %>%
  mutate(source = "CSES")
cses_data <- cses_data %>%
  mutate(source_recode = "CSES")
cses_data <- cses_data %>%
  rename(year = IMD1008_YEAR)
cses_data <- cses_data %>%
  rename(turnout = IMD3001_LH)
cses_data <- cses_data %>%
  rename(dataset_party_id = IMD3002_LH_PL)
cses_data <- cses_data %>%
  rename(type = IMD1009)
cses_data <- cses_data %>%
  rename(inc = IMD2006)
cses_data <- cses_data %>%
  rename(gender = IMD2002)
cses_data <- cses_data %>%
  rename(educ = IMD2003)

cses_data <- cses_data %>%
  rename(age = IMD2001_1)

cses_data <- cses_data %>%
  mutate(survey = "?")


#ne pas oublier de garder le weight 


#Filtre pour ne garder que les données valides sur le revenu, le vote, et les bonnes élections
cses_data <- cses_data %>%
  filter(dataset_party_id < 9999996)
cses_data <- cses_data %>%
  filter(type <= 13)

cses_data_clean <- cses_data %>%
  select(isoname,year, source, source_recode,survey, type, inc,gender,educ,age, turnout, dataset_party_id)

cses_data_clean <- cses_data_clean %>%
  mutate(
    gender = case_when(
      gender == "1" ~ "men",   
      gender == "2" ~ "women",
      TRUE ~ as.character(gender)
    )
  )

cor(cses_data_clean$age, cses_data_clean$educ, 
    use = "complete.obs")
cor(cses_data_clean$age, cses_data_clean$inc, 
    use = "complete.obs")
cor(cses_data_clean$educ, cses_data_clean$inc, 
    use = "complete.obs")

#verif données
unique(cses_data_clean$inc)
unique(cses_data_clean$type)
unique(cses_data_clean$vote[cses_data_clean$isoname == "Albania"])

#Ajout de l'abstention
cses_data_clean <- cses_data_clean %>%
  mutate(
    dataset_party_id = case_when(
      turnout == 0 ~ "Abstention",
      turnout == 9999993 ~ "Abstention",
      dataset_party_id == 9999993 ~ "Abstention",
      dataset_party_id == 9999988 ~ "Other",
      dataset_party_id == 9999989 ~ "Other",
      dataset_party_id == 9999990 ~ "Other",
      dataset_party_id == 9999991 ~ "Other",
      dataset_party_id == 9999992 ~ "Other",
      dataset_party_id == 9999995 ~ "Other",
      TRUE ~ as.character(dataset_party_id)
    )
  )

table(cses_data_clean$turnout)
class(cses_data$turnout)
table(cses_data$dataset_party_id)
sum(cses_data_clean$dataset_party_id == "Abstention", na.rm = TRUE)
#Join partyfacts dans cses
cses_data_clean <- cses_data_clean %>%
  left_join(
    Partyfacts_id_cses %>%
      dplyr::select(dataset_party_id,partyfacts_id),
    by = "dataset_party_id"
  )

cses_data_clean <- cses_data_clean %>%
  mutate(
    partyfacts_id = as.character(partyfacts_id),
    partyfacts_id = case_when(
      dataset_party_id == "Abstention" ~ "Abstention",
      str_detect(dataset_party_id, "Other$") ~ "Other",
      TRUE ~ partyfacts_id
    )
  )

cses_data_clean <- cses_data_clean %>%
  mutate(
    isoname = case_when(
      isoname == "Czech Republic/Czechia" ~ "Czech Republic",
      TRUE ~ isoname
    )
  )

#Traitement des partyfacts dans cses pour join 
cses_data_clean <- cses_data_clean %>%
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
      partyfacts_id == "604" & isoname == "Belgium" & year == 2003 ~ "622",
      partyfacts_id == "604" & isoname == "Belgium" & year == 2010 ~ "622",
      partyfacts_id == "604" & isoname == "Belgium" & year == 2014 ~ "622",
      partyfacts_id == "8259" & isoname == "Belgium" & year == 1999 ~ "554",
      partyfacts_id == "1680" & isoname == "Belgium" & year == 2003 ~ "1586",
      partyfacts_id == "500" & isoname == "Belgium" & year <= 1977  ~ "480",
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
      partyfacts_id == "5917" & isoname == "Iraq" & year == 2010 ~ "5919",
      partyfacts_id == "6303" & isoname == "Italy" & year == 2013  ~ "1626",
      TRUE ~ partyfacts_id
    )
  )
table(cses_data_clean$turnout[cses_data$isoname == "Albania"])
table(cses_data_clean$turnout[cses_data$isoname == "Belgium"])
sum(cses_data_clean$partyfacts_id == "Abstention", na.rm = TRUE)
###Export Base CSES ----
write.csv(
  cses_data_clean,
  "data/intermediary/elections/cses elections dataset.csv",
  row.names = FALSE
)

#WVS ----
wvs_data <- read.csv  ("data/raw/wvs/WVS_Time_Series_1981-2022_csv_v5_0.csv")

wvs_data <- wvs_data %>%
  rename(isoname = COUNTRY_ALPHA)
unique(wvs_data$isoname)
wvs_data <- wvs_data %>%
  mutate(dataset_key= "wvs")
wvs_data <- wvs_data %>%
  mutate(source = "WVS")
wvs_data <- wvs_data %>%
  mutate(source_recode = "WVS")
wvs_data <- wvs_data %>%
  rename(year = S020)
wvs_data <- wvs_data %>%
  rename(dataset_party_id = E179WVS) #turnout inclus dans cette variable
wvs_data <- wvs_data %>%
  mutate(type = "Lower House")
wvs_data <- wvs_data %>%
  mutate(turnout = NA)

wvs_data <- wvs_data %>%
  rename(inc = X047_WVS)
wvs_data <- wvs_data %>%
  rename(gender = X001)
wvs_data <- wvs_data %>%
  rename(educ = X025)
wvs_data <- wvs_data %>%
  rename(age = X003)
wvs_data <- wvs_data %>%
  mutate(survey = "Pre-electoral")

wvs_data_clean <- wvs_data %>%
  select(isoname,year, source, source_recode,survey, type, inc,gender,educ,age, turnout, dataset_party_id)

#Filtre pour ne garder que les données valides sur le revenu, le vote, et les bonnes élections

wvs_data_clean <- wvs_data_clean %>%
  filter(dataset_party_id >= 1)

wvs_data_clean <- wvs_data_clean %>%
  mutate(
    gender = case_when(
      gender == "1" ~ "men",   
      gender == "2" ~ "women",
      TRUE ~ as.character(gender)
    )
  )

cor(wvs_data_clean$age, wvs_data_clean$educ, 
    use = "complete.obs")
cor(wvs_data_clean$age, wvs_data_clean$inc, 
    use = "complete.obs")
cor(wvs_data_clean$educ, wvs_data_clean$inc, 
    use = "complete.obs")

wvs_data_clean <- wvs_data_clean %>%
  mutate(
    dataset_party_id = case_when(
      dataset_party_id == "1" ~ "Abstention",   
      dataset_party_id == "2" ~ "Abstention",
      dataset_party_id == "3" ~ "Abstention",
      dataset_party_id == "4" ~ "Abstention",
      dataset_party_id == "7" ~ "Abstention",
      dataset_party_id == "5" ~ "Other",
      dataset_party_id == "8" ~ "Other",

      TRUE ~ as.character(dataset_party_id)
    )
  )

#Join partyfacts dans wvs ----
Partyfacts_id_wvs <- Partyfacts_id_wvs %>%
  mutate(dataset_party_id = as.character(dataset_party_id))

wvs_data_clean <- wvs_data_clean %>%
  left_join(
    Partyfacts_id_wvs %>%
      dplyr::select(dataset_party_id,partyfacts_id),
    by = "dataset_party_id"
  )

Partyfacts_id_wvs <- Partyfacts_id_wvs %>%
  mutate(partyfacts_id = as.character(partyfacts_id))

wvs_data_clean <- wvs_data_clean %>%
  mutate(
    partyfacts_id = as.character(partyfacts_id),
    partyfacts_id = case_when(
      dataset_party_id == "Abstention" ~ "Abstention",
      str_detect(dataset_party_id, "Other$") ~ "Other",
      TRUE ~ partyfacts_id
    )
  )

wvs_data_clean <- wvs_data_clean %>%
  mutate(
    isoname = case_when(
      isoname == "ALB" ~ "Albania",   
      isoname == "AND" ~ "Andorra",
      isoname == "ARG" ~ "Argentina",
      isoname == "ARM" ~ "Armenia",
      isoname == "AUS" ~ "Australia",
      isoname == "BFA" ~ "Burkina Faso",
      isoname == "BGD" ~ "Bangladesh",   
      isoname == "BIH" ~ "Bosnia and Herzegovina",
      isoname == "BLR" ~ "Belarus",
      isoname == "BOL" ~ "Bolivia",
      isoname == "BRA" ~ "Brazil",
      isoname == "CAN" ~ "Canada",
      isoname == "CHE" ~ "Switzerland",   
      isoname == "CHL" ~ "Chile",
      isoname == "COL" ~ "Colombia",
      isoname == "CYP" ~ "Cyprus",
      isoname == "CZE" ~ "Czech Republic",
      isoname == "DEU" ~ "Germany",
      isoname == "DOM" ~ "Dominican Republic",   
      isoname == "DZA" ~ "Algeria",
      isoname == "ECU" ~ "Ecuador",
      isoname == "EGY" ~ "Egypt",
      isoname == "ESP" ~ "Spain",
      isoname == "EST" ~ "Estonia",
      isoname == "ETH" ~ "Ethiopia",  
      isoname == "FIN" ~ "Finland",
      isoname == "GBR" ~ "United Kingdom",
      isoname == "GEO" ~ "Georgia",
      isoname == "GHA" ~ "Ghana",
      isoname == "GRC" ~ "Greece",
      isoname == "GTM" ~ "Guatemala",  
      isoname == "HKG" ~ "Hong Kong",
      isoname == "HTI" ~ "Haiti",
      isoname == "HUN" ~ "Hungary",
      isoname == "IDN" ~ "Indonesia",
      isoname == "IND" ~ "India",
      isoname == "IRN" ~ "Iran",   
      isoname == "IRQ" ~ "Iraq",
      isoname == "ISR" ~ "Israel",
      isoname == "JOR" ~ "Jordan",
      isoname == "JPN" ~ "Japan",
      isoname == "KAZ" ~ "Kazakhstan",
      isoname == "KEN" ~ "Kenya",   
      isoname == "KGZ" ~ "Kyrgyzstan ",
      isoname == "KOR" ~ "South Korea",
      isoname == "LBN" ~ "Lebanon",
      isoname == "LBY" ~ "Libya",
      isoname == "LTU" ~ "Lithuania",
      isoname == "LVA" ~ "Latvia",   
      isoname == "MAC" ~ "Macau",
      isoname == "MAR" ~ "Morocco",
      isoname == "MDA" ~ "Molodova",
      isoname == "MDV" ~ "Maldives",
      isoname == "MEX" ~ "Mexico",
      isoname == "MKD" ~ "North Macedonia",   
      isoname == "MLI" ~ "Mali",
      isoname == "MMR" ~ "Myanmar",
      isoname == "MNE" ~ "Montenegro",
      isoname == "MNG" ~ "Mongolia",
      isoname == "MYS" ~ "Malaysia",
      isoname == "NGA" ~ "Nigeria",  
      isoname == "NIC" ~ "Nicaragua",
      isoname == "NIR" ~ "North Ireland",
      isoname == "NLD" ~ "Netherlands",
      isoname == "NOR" ~ "Norway",
      isoname == "NZL" ~ "New Zealand",
      isoname == "PAK" ~ "Pakistan",   
      isoname == "PER" ~ "Peru",
      isoname == "PHL" ~ "Philippines",
      isoname == "POL" ~ "Poland",
      isoname == "PRI" ~ "Puerto Rico",
      isoname == "PSE" ~ "Palestine",
      isoname == "ROU" ~ "Romania",   
      isoname == "RUS" ~ "Russia",
      isoname == "RWA" ~ "Rwanda",
      isoname == "SGP" ~ "Singapore",
      isoname == "SLV" ~ "El Salvador",
      isoname == "SRB" ~ "Serbia",
      isoname == "SVK" ~ "Slovakia",  
      isoname == "SVN" ~ "Slovenia",
      isoname == "SWE" ~ "Sweden",
      isoname == "THA" ~ "Thailand",
      isoname == "TJK" ~ "Tajikistan",
      isoname == "TUN" ~ "Tunisia",
      isoname == "TUR" ~ "Turkey",   
      isoname == "TWN" ~ "Taiwan",
      isoname == "TZA" ~ "Tanzania",
      isoname == "UGA" ~ "Uganda",
      isoname == "UKR" ~ "Ukraine",
      isoname == "URY" ~ "Uruguay",
      isoname == "USA" ~ "United States",   
      isoname == "UZB" ~ "Uzbekistan",
      isoname == "VEN" ~ "Venezuela",
      isoname == "YEM" ~ "Yemen",
      isoname == "ZAF" ~ "South Africa",
      isoname == "ZMB" ~ "Zambia",
      isoname == "ZWE" ~ "Zimbabwe",   
      
      
      TRUE ~ as.character(isoname)
    )
  )


#Traitement des partyfacts dans cses pour join 
wvs_data_clean <- wvs_data_clean %>%
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
      partyfacts_id == "604" & isoname == "Belgium" & year == 2003 ~ "622",
      partyfacts_id == "604" & isoname == "Belgium" & year == 2010 ~ "622",
      partyfacts_id == "604" & isoname == "Belgium" & year == 2014 ~ "622",
      partyfacts_id == "8259" & isoname == "Belgium" & year == 1999 ~ "554",
      partyfacts_id == "1680" & isoname == "Belgium" & year == 2003 ~ "1586",
      partyfacts_id == "500" & isoname == "Belgium" & year <= 1977  ~ "480",
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
      partyfacts_id == "5917" & isoname == "Iraq" & year == 2010 ~ "5919",
      partyfacts_id == "6303" & isoname == "Italy" & year == 2013  ~ "1626",

      TRUE ~ partyfacts_id
    )
  )




###Export Base WVS ----
write.csv(
  wvs_data_clean,
  "data/intermediary/elections/wvs elections dataset.csv",
  row.names = FALSE
)





