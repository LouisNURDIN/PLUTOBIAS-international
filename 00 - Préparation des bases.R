library(haven)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(purrr)

#Liste avec élections
all_elections <- read.csv ("data/intermediary/elections/all elections update.csv", sep = ";")

all_elections <- all_elections %>%
  mutate(
    election_date = as.Date(election_date, format = "%Y.%m.%d"),
    year = as.integer(year))

all_elections <- all_elections %>%
  mutate(election_date = as.Date(election_date, format = "%Y.%m.%d")) %>%
  distinct()

all_elections <- all_elections%>%
  mutate(
    isoname = case_when(
      isoname == "United States of America" ~ "United States",
      TRUE ~ isoname))

#Liste des régimes présidentiels
pays_regimes_presidentiels <- read.csv("data/raw/Liste régimes présidentiels.csv", sep = ",")
pays_regimes_presidentiels <- unique(pays_regimes_presidentiels$isoname)


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
unique(GMP_inc$isoname)

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

names(GMP_inc)
unique(GMP_inc$year[GMP_inc$isoname == "United States"])
table(GMP_inc$dataset_party_id[GMP_inc$isoname == "United States" & GMP_inc$year == 2000])


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

table(GMP_inc_2$dataset_party_id[GMP_inc_2$isoname == "United States" & GMP_inc_2$year == 2000])


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

table(GMP_inc_2$dataset_party_id[GMP_inc_2$isoname == "United States" & GMP_inc_2$year == 2000])
##Ajout iso dans le nom des partis ----
GMP_inc_2 <- GMP_inc_2 %>%
  mutate(
    dataset_party_id = case_when(
      is.na(dataset_party_id) | dataset_party_id == "" ~ dataset_party_id,
      TRUE ~ paste0(iso, "-", dataset_party_id)
    )
  )

table(GMP_inc_2$dataset_party_id[GMP_inc_2$isoname == "United States" & GMP_inc_2$year == 2000])
table(GMP_inc$dataset_party_id[GMP_inc$isoname == "United States" & GMP_inc$year == 2000])
GMP_inc_2 <- GMP_inc_2 %>%
  rename(gender = sex)
unique(GMP_inc_2$gender)

GMP_inc_2 <- GMP_inc_2 %>%
  mutate(
    gender = as.character(gender),
    gender = case_when(
      gender == "0" ~ "women",
      gender == "1" ~ "men",
      TRUE ~ gender
    )
  )

GMP_inc_2 <- GMP_inc_2 %>%
  mutate(interview_date = year)

GMP_inc_2 <- GMP_inc_2 %>%
  mutate(
    interview_date = year)

GMP_inc_2 <- GMP_inc_2 %>%
  select(-educ)

GMP_inc_2 <- GMP_inc_2 %>%
  rename(educ = educ2)
unique(GMP_inc_2$educ)
unique(GMP_inc_2$age)

GMP_inc_2_clean <- GMP_inc_2 %>%
  select(isoname,year,interview_date, source, source_recode, survey,type, dinc,gender,educ, age, turnout,dataset_party_id,weight)

table(GMP_inc_2$dataset_party_id[GMP_inc_2$isoname == "United States" & GMP_inc_2$year == 2000])
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

table(Base_all_elections$partyfacts_id[Base_all_elections$isoname == "United States" & Base_all_elections$year == 2008])



## Filtre législatives dans deuxième méthode ----
Base_elections_legislatives <- Base_all_elections %>%
  filter(
    isoname %in% pays_regimes_presidentiels & type %in% c("Presidential", "Presidential, round 1") |
      (!(isoname %in% pays_regimes_presidentiels) & type == "Lower house"))

table(Base_elections_legislatives$partyfacts_id[Base_elections_legislatives$isoname == "United States" & Base_elections_legislatives$year == 2000])


table(Base_elections_legislatives$type)
unique(Base_elections_legislatives$isoname)
unique(Base_elections_legislatives$isoname[Base_elections_legislatives$type == "Presidential, round 1"])
unique(GMP_inc$type[GMP_inc$isoname == "Chile"])
cor(Base_elections_legislatives$age, Base_elections_legislatives$educ, use = "complete.obs")
# - 0,1678806
cor(Base_elections_legislatives$age, Base_elections_legislatives$dinc,  use = "complete.obs")
# - 0,1616936
cor(Base_elections_legislatives$educ, Base_elections_legislatives$dinc, use = "complete.obs")
#0,3013959

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

Base_elections_legislatives <- Base_elections_legislatives %>%
 dplyr::rename(election_year = year)
names(Base_elections_legislatives)



#Export Base globale et Base législatives
write.csv(
  Base_elections_legislatives,
  "data/intermediary/elections/legislative elections dataset.csv",
  row.names = FALSE)

write.csv(
  Base_all_elections,
  "data/intermediary/elections/all elections dataset.csv",
  row.names = FALSE)


#Ajout de nouvelles bases ----


#ESS ----
ess_data <- read.csv("data/raw/ess/Datafile-subset.csv")
names(ess_data)

table(ess_data$inwyys)

ess_data <- ess_data %>%
  mutate(
    year = coalesce(na_if(inwyr, 99), na_if(inwyys, 99)),
    month = coalesce(na_if(inwmm, 99), na_if(inwmms, 99)),
    day = coalesce(na_if(inwdd, 99), na_if(inwdds, 99)),
    
    interview_date = case_when(
      is.na(year) ~ as.Date(NA),
      is.na(month) | is.na(day) ~ as.Date(NA),
      TRUE ~ make_date(year, month, day)
    )
  )

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

names(ess_data_long)
sum(is.na(ess_data_long$ess_id))
###Identifier les variables qui nous intéressent pour les harmoniser----
ess_data_long <- ess_data_long %>%
  rename(isoname = cntry)

ess_data_long <- ess_data_long %>%
  rename(source = name)
ess_data_long <- ess_data_long %>%
  rename(weight = pspwght)
ess_data_long <- ess_data_long %>%
  mutate(source_recode = "ESS")

ess_data_long <- ess_data_long %>%
  mutate(
      year  = coalesce(na_if(inwyr, 9999), na_if(inwyys, 9999)))

ess_data_long <- ess_data_long %>%
  rename(turnout = vote)
ess_data_long <- ess_data_long %>%
  rename(dataset_party_id = ess_id)
ess_data_long <- ess_data_long %>%
  mutate(type = "General election")
ess_data_long <- ess_data_long %>%
  mutate(
    inc = coalesce(hinctnta, hinctnt))
ess_data_long <- ess_data_long %>%
  rename(gender = gndr)
ess_data_long <- ess_data_long %>%
  rename(educ = eduyrs)

ess_data_long <- ess_data_long %>%
  rename(age = agea)

ess_data_long <- ess_data_long %>%
  mutate(survey = "Post-electoral")

sum(is.na(ess_data_long$inwdd))

unique(ess_data_long$educ)
sum(is.na(ess_data_long$educ))
#Filtrer mes données sur le vote
sum(ess_data_long$turnout == 2, na.rm = TRUE)
sum(ess_data_long$turnout == 3, na.rm = TRUE) #32 989 422 ont turnout 2 ou turnout 3 dans ma base

ess_data_long <- ess_data_long %>%
  filter(!stringr::str_detect(dataset_party_id, "NA"))

ess_data_long <- ess_data_long %>%
  filter(!str_detect(dataset_party_id, "77|88|99"))

sum(ess_data_long$turnout == 2, na.rm = TRUE)
sum(ess_data_long$turnout == 3, na.rm = TRUE)  #Après avoir filtré sur la variable précédente, on passe à seulement 32 qui ont turnout 2 ou 3


table(ess_data_long$turnout)
ess_data_long <- ess_data_long %>%
  filter(turnout < 7) 

ess_data_clean <- ess_data_long %>%
  select(isoname,year,interview_date, source, source_recode,survey, type, inc,gender,educ,age, turnout, dataset_party_id,weight)

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
    partyfacts_id = as.character(partyfacts_id),
    partyfacts_id = case_when(
      dataset_party_id == "Abstention" ~ "Abstention",
      str_detect(dataset_party_id, "Other$") ~ "Other",
      TRUE ~ partyfacts_id
    )
  )


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
sum(ess_data_clean$dataset_party_id == "Abstention", na.rm = TRUE) 

cor(ess_data_clean$age, ess_data_clean$educ, use = "complete.obs")
# 0,2033454
cor(ess_data_clean$age, ess_data_clean$inc,  use = "complete.obs")
# 0,05312431
cor(ess_data_clean$educ, ess_data_clean$inc, use = "complete.obs")
#0,08093405
cor(ess_data_clean$gender, ess_data_clean$inc, use = "complete.obs")
# 0,04490973
cor(ess_data_clean$gender, ess_data_clean$educ, use = "complete.obs")
# 0,1391624


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
    ))


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


unique(ess_data_clean$dataset_party_id[ess_data_clean$isoname == "Belgium" & ess_data_clean$source == "ESS4e04_6"])
#Traitement des partyfacts dans cses pour join 
ess_data_clean <- ess_data_clean %>%
  mutate(
    partyfacts_id = case_when(
      dataset_party_id == "BE-1-13-V" & isoname == "Belgium" & year >= 2003 ~ "789",
      dataset_party_id == "BG-5-1-V" & isoname == "Bulgaria" & year >= 2010 ~ "760", #*
      dataset_party_id == "HR-9-3-V" & isoname == "Croatia" & year >= 2018 ~ "4865", #*
      dataset_party_id == "CZ-1-9-V" & isoname == "Czech Republic" & year >= 2008 ~ "1728", #*
      dataset_party_id == "CZ-5-5-V" & isoname == "Czech Republic" & year >= 2010 ~ "223", #*
      dataset_party_id == "CZ-7-4-V" & isoname == "Czech Republic" & year >= 2014 ~ "2141", #*
      dataset_party_id == "CZ-5-6-V" & isoname == "Czech Republic" & year == 2012 ~ "1202", #*
      dataset_party_id == "FI-1-9-V" & isoname == "Finland" & year >= 2012 ~ "1303", #*
      dataset_party_id == "IT-6-4-V" & isoname == "Italy" & year >= 2016 ~ "2046", #*
      dataset_party_id == "NL-4-11-V" & isoname == "Netherlands" & year >= 2014 ~ "298", #*
      dataset_party_id == "RU-3-3-V" & isoname == "Russia" ~ "2245", #*
      dataset_party_id == "SK-6-1-V" & isoname == "Slovakia" ~ "2130", #*
      dataset_party_id == "SI-4-9-V" & isoname == "Slovenia" ~ "474", #*
      dataset_party_id == "SI-6-7-V" & isoname == "Slovenia" ~ "1773", #*
      dataset_party_id == "SI-7-8-v" & isoname == "Slovenia" ~ "3098", #*
      dataset_party_id == "UA-3-1-V" & isoname == "Ukraine" ~ "2234", #*
      dataset_party_id == "UA-2-10-V" & isoname == "Ukraine" ~ "2228", #*
      TRUE ~ as.character(partyfacts_id)
    )
  )

unique(ess_data_clean$dataset_party_id[ess_data_clean$isoname == "France" & 
                                ess_data_clean$year == 2019])

unique(ess_data_clean$year[ess_data_clean$isoname == "France"])
#Rattacher les répondants à la bonne élection 
ess_data_clean <- ess_data_clean %>%
  group_by(isoname) %>%
  group_modify(~{
    
    df <- .x
    country <- .y$isoname[1]
    
    elections <- all_elections %>%
      filter(isoname == country) %>%
      mutate(
        election_date = as.Date(election_date, format = "%Y.%m.%d")
      ) %>%
      arrange(year, election_date)
    
    if (nrow(elections) == 0) return(df)
    
    map_dfr(seq_len(nrow(df)), function(i){
      
      row <- df[i, ]
      
      ## Cas 1 : on connaît la date d'interview ET il existe des dates d'élection
      if (!is.na(row$interview_date) &&
          any(!is.na(elections$election_date))) {
        
        prev_elec <- elections %>%
          filter(!is.na(election_date),
                 election_date <= row$interview_date) %>%
          slice_max(election_date, n = 1)
        
        ## Cas 2 : secours → matching par année
      } else {
        
        prev_elec <- elections %>%
          filter(year <= row$year) %>%
          arrange(year, election_date) %>%
          slice_tail(n = 1)
        
      }
      
      if (nrow(prev_elec) == 0) {
        
        row$election_date <- as.Date(NA)
        row$election_year <- NA_integer_
        
      } else {
        
        row$election_date <- prev_elec$election_date[1]
        row$election_year <- prev_elec$year[1]
        
      }
      
      row
      
    })
    
  }) %>%
  ungroup()

ess_data_clean <- ess_data_clean %>%
  select(-year)

unique(ess_data_long$essround[ess_data_long$isoname == "FR"])
unique(ess_data_long$dataset_party_id[ess_data_long$isoname == "FR"  & ess_data_long$essround == 11])
unique(ess_data$prtvtfr[ess_data$cntry == "FR"  & ess_data$essround == 9])
sum(ess_data$prtvtfr == "FR-9-7-v", na.rm = TRUE)

###Export Base ESS ----
write.csv(
  ess_data_clean,
  "data/intermediary/elections/ess elections dataset.csv",
  row.names = FALSE)



#CSES ----
cses_data <- read.csv("data/raw/cses/cses_imd.csv")

unique(cses_data$IMD1006_NAM)
unique(cses_data$IMD1009)


      
###Identifier les variables qui nous intéressent pour les harmoniser----
cses_data <- cses_data %>%
  rename(isoname = IMD1006_NAM)

cses_data <- cses_data %>%
  mutate(
    isoname = case_when(
      isoname == "United States of America" ~ "United States",
      TRUE ~ isoname))


cses_data <- cses_data %>%
  rename(annee = IMD1013_Y)
cses_data <- cses_data %>%
  rename(mois = IMD1013_M)
cses_data <- cses_data %>%
  rename(jour = IMD1013_D)
cses_data <- cses_data %>%
  rename(dataset_key= IMD1001)
cses_data <- cses_data %>%
  mutate(source = "CSES")
cses_data <- cses_data %>%
  mutate(source_recode = "CSES")
cses_data <- cses_data %>%
  mutate(weight = IMD1010_2)
cses_data <- cses_data %>%
  rename(year = IMD1008_YEAR)
cses_data <- cses_data %>%
  mutate(
    turnout = if_else(
      isoname %in% pays_regimes_presidentiels,
      IMD3001_PR_1,    #turnout à l'élection présidentielle pour les régimes présidentiels
      IMD3001_LH   # turnout à l'élection Lower House
    )
  )
cses_data <- cses_data %>%
  mutate(
    dataset_party_id = if_else(
      isoname %in% pays_regimes_presidentiels,
      as.character(IMD3002_PR_1),    #vote à l'élection présidentielle pour les régimes présidentiels
      as.character(IMD3002_LH_PL)   # vote à l'élection Lower House
    )
  )
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
  mutate(survey = "Post-electoral")



unique(cses_data$year[cses_data$isoname == "Mexico"])
unique(cses_data$dataset_party_id[cses_data$isoname == "United States" & cses_data$year == "2004"])
unique(pays_regimes_presidentiels)

#Recréer la date de l'interview
cses_data <- cses_data %>%
  mutate(
    annee2 = if_else(annee == 9999, year, annee),
    mois2  = na_if(mois, 99),
    jour2  = na_if(jour, 99),
    
    interview_date = case_when(
      is.na(annee2) ~ as.Date(NA),
      is.na(mois2) | is.na(jour2) ~ as.Date(NA),
      TRUE ~ make_date(annee2, mois2, jour2)
    )
  ) %>%
  select(-annee2, -mois2, -jour2)


#ne pas oublier de garder le weight 


#Filtre pour ne garder que les données valides sur le revenu, le vote, et les bonnes élections
sum(cses_data$turnout == 0, na.rm = TRUE) #56048 ont turnout = 0 dans la base

cses_data <- cses_data %>% filter(turnout < 9999996) 

cses_data <- cses_data %>%
  filter(!dataset_party_id %in% c(9999996, 9999997, 9999998))

sum(cses_data$turnout == 0, na.rm = TRUE) 


cses_data <- cses_data %>%
  mutate(
    type = case_when(
      isoname %in% pays_regimes_presidentiels ~ "Presidential",
      TRUE ~ "Parliamentary/Legislative"
    )
  )

cses_data_clean <- cses_data %>%
  select(isoname,year,interview_date, source, source_recode,survey, type, inc,gender,educ,age, turnout, dataset_party_id,weight)

sum(cses_data_clean$turnout == 0, na.rm = TRUE)


cor(cses_data_clean$age, cses_data_clean$educ, use = "complete.obs")
# 0,04659434
cor(cses_data_clean$age, cses_data_clean$inc, use = "complete.obs")
#- 0,007372758
cor(cses_data_clean$educ, cses_data_clean$inc, use = "complete.obs")
#0,165427
cor(cses_data_clean$gender, cses_data_clean$inc, use = "complete.obs")
# - 0,001865792
cor(cses_data_clean$gender, cses_data_clean$educ, use = "complete.obs")
#0,009916835

cses_data_clean <- cses_data_clean %>%
  mutate(
    gender = case_when(
      gender == "1" ~ "men",   
      gender == "2" ~ "women",
      TRUE ~ as.character(gender)
    )
  )



#verif données
unique(cses_data_clean$inc)
unique(cses_data_clean$type)
unique(cses_data_clean$dataset_party_id[cses_data_clean$isoname == "Albania"])

#Ajout de l'abstention
cses_data_clean <- cses_data_clean %>%
  mutate(
    dataset_party_id = case_when(
      turnout == 0 ~ "Abstention",
      turnout == 9999993 ~ "Abstention",
      dataset_party_id == 9999999 ~ "Abstention",
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

table(cses_data_clean$dataset_party_id[cses_data_clean$isoname == "Albania"])
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
      isoname == "Great Britain" ~ "United Kingdom",
      isoname == "Republic of Korea" ~ "South Korea",
      isoname == "Russian Federation" ~ "Russia",
      TRUE ~ isoname))

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

#Rattacher la bonne élection à chaque répondant ----
cses_data_clean <- cses_data_clean %>%
  group_by(isoname) %>%
  group_modify(~{
    
    df <- .x
    country <- .y$isoname[1]
    
    elections <- all_elections %>%
      filter(isoname == country) %>%
      mutate(
        election_date = as.Date(election_date, format = "%Y.%m.%d")
      ) %>%
      arrange(year, election_date)
    
    if (nrow(elections) == 0) return(df)
    
    map_dfr(seq_len(nrow(df)), function(i){
      
      row <- df[i, ]
      
      ## Cas 1 : on connaît la date d'interview ET il existe des dates d'élection
      if (!is.na(row$interview_date) &&
          any(!is.na(elections$election_date))) {
        
        prev_elec <- elections %>%
          filter(!is.na(election_date),
                 election_date <= row$interview_date) %>%
          slice_max(election_date, n = 1)
        
        ## Cas 2 : secours → matching par année
      } else {
        
        prev_elec <- elections %>%
          filter(year <= row$year) %>%
          arrange(year, election_date) %>%
          slice_tail(n = 1)
        
      }
      
      if (nrow(prev_elec) == 0) {
        
        row$election_date <- as.Date(NA)
        row$election_year <- NA_integer_
        
      } else {
        
        row$election_date <- prev_elec$election_date[1]
        row$election_year <- prev_elec$year[1]
        
      }
      
      row
      
    })
    
  }) %>%
  ungroup()

cses_data_clean <- cses_data_clean %>%
  select(-year)

unique(cses_data_clean$partyfacts_id[cses_data_clean$isoname == "United States" & cses_data_clean$election_year == "2004"])
unique(cses_data_clean$partyfacts_id[cses_data_clean$isoname == "Mexico" & cses_data_clean$election_year == "2012"])
unique(cses_data_clean$election_year[cses_data_clean$isoname == "Mexico"])
unique(cses_data_clean$isoname)
###Export Base CSES ----
write.csv(
  cses_data_clean,
  "data/intermediary/elections/cses elections dataset.csv",
  row.names = FALSE)

#WVS ----
wvs_data <- read.csv  ("data/raw/wvs/WVS_Time_Series_1981-2022_csv_v5_0.csv")
sum(is.na(wvs_data$S012))
table(wvs_data$S012)
unique(wvs_data$S018)

mean(wvs_data$S018, na.rm = TRUE)
sum(wvs_data$S018, na.rm = TRUE)


wvs_data <- wvs_data %>%
  rename(isoname = COUNTRY_ALPHA)

wvs_data <- wvs_data %>%
  mutate(dataset_key= "wvs")
wvs_data <- wvs_data %>%
  mutate(source = "WVS")
wvs_data <- wvs_data %>%
  mutate(source_recode = "WVS")
wvs_data <- wvs_data %>%
  mutate(weight = S018)
wvs_data <- wvs_data %>%
  rename(year = S020)
wvs_data <- wvs_data %>%
  rename(dataset_party_id = E179WVS) #turnout inclus dans cette variable
wvs_data <- wvs_data %>%
  mutate(type = "General election")
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


wvs_data <- wvs_data %>%
  mutate(
    interview_date = if_else(
      is.na(S012) | S012 < 0,
      as.Date(NA),
      as.Date(as.character(S012), format = "%Y%m%d")
    ))

wvs_data_clean <- wvs_data %>%
  select(isoname,year,interview_date, source, source_recode,survey, type, inc,gender,educ,age, turnout, dataset_party_id,weight)

#Filtre pour ne garder que les données valides sur le revenu, le vote, et les bonnes élections

wvs_data_clean <- wvs_data_clean %>%
  filter(dataset_party_id >= 1)



#Corrélation dans WVS
cor(wvs_data_clean$age, wvs_data_clean$educ, use = "complete.obs")
# - 0,1278877
cor(wvs_data_clean$age, wvs_data_clean$inc, use = "complete.obs")
# - 0,05413639
cor(wvs_data_clean$educ, wvs_data_clean$inc, use = "complete.obs")
# 0,03074062
cor(wvs_data_clean$gender, wvs_data_clean$inc, use = "complete.obs")
# - 0,03311197
cor(wvs_data_clean$gender, wvs_data_clean$educ, use = "complete.obs")
#- 0,02759841

#Corrélation au sein de wvs par pays année
cor_age_educ_wvs <- wvs_data_clean %>%
  group_by(isoname, year) %>%
  summarise(cor_age_educ = cor(age, educ, use = "complete.obs"),
            n = sum(complete.cases(age, educ)),.groups = "drop")

cor_age_inc_wvs <- wvs_data_clean %>%
  group_by(isoname, year) %>%
  summarise(cor_age_educ = cor(age, inc, use = "complete.obs"),
            n = sum(complete.cases(age, inc)),.groups = "drop")

cor_educ_inc_wvs <- wvs_data_clean %>%
  group_by(isoname, year) %>%
  summarise(cor_educ_inc = cor(educ, inc, use = "complete.obs"),
            n = sum(complete.cases(educ, inc)),.groups = "drop")

cor_gender_inc_wvs <- wvs_data_clean %>%
  group_by(isoname, year) %>%
  summarise(cor_educ_inc = cor(gender, inc, use = "complete.obs"),
            n = sum(complete.cases(gender, inc)),.groups = "drop")



wvs_data_clean <- wvs_data_clean %>%
  mutate(
    gender = case_when(
      gender == "1" ~ "men",   
      gender == "2" ~ "women",
      TRUE ~ as.character(gender)
    )
  )










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

unique(wvs_data_clean$dataset_party_id[wvs_data_clean$isoname == "Estonia" & wvs_data_clean$year == 2011 ])


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
      partyfacts_id == "6303" & isoname == "Italy" & year == 2013  ~ "1626",
      dataset_party_id == "233031" & isoname == "Estonia" ~ "1150",
      TRUE ~ partyfacts_id
    )
  )

wvs_data_clean <- wvs_data_clean %>%
  mutate(
    partyfacts_id = case_when(
      dataset_party_id == "12004" & isoname == "Algeria" & election_year >= 2002  ~ "5222",
      dataset_party_id == "32013" & isoname == "Argentina" & election_year == 1999  ~ "6116",
      dataset_party_id == "32012" & isoname == "Argentina" & election_year >= 2006 & election_year <= 2013  ~ "2530",
      dataset_party_id == "112001" & isoname == "Belarus" & election_year == 1990 ~ "2030",
      dataset_party_id == "70029" & isoname == "Bosnia and Herzegovina" & election_year == 1998 ~ "1340",
      dataset_party_id == "152020" & isoname == "Chile" & election_year == 2005 ~ "6061",   #Attention pour le cas du Chili ce n'est peut-être pas le bon, parti
      dataset_party_id == "218001" & isoname == "Ecuador" ~ "4044",
      dataset_party_id == "288001" & isoname == "Ghana" & election_year == 2012 ~ "2311",
      dataset_party_id == "288002" & isoname == "Ghana" & election_year == 2012 ~ "2312",
      dataset_party_id == "HU-Fidesz" & isoname == "Hungary" & election_year == 2010 ~ "6366",
      dataset_party_id == "HU-Fidesz" & isoname == "Hungary" & election_year == 2014 ~ "6366",
      dataset_party_id == "HU-Fidesz" & isoname == "Iran" & election_year == 2000 ~ "5359",
      dataset_party_id == "HU-Fidesz" & isoname == "Iran" & election_year == 2000 ~ "6875",
      dataset_party_id == "4280043" & isoname == "Latvia" & election_year == 2011 ~ "1704",
      dataset_party_id == "4280043" & isoname == "Latvia" & election_year == 2014 ~ "1704",
      dataset_party_id == "484003" & isoname == "Mexico" & election_year == 2000 ~ "1988",
      dataset_party_id == "499001" & isoname == "Montenegro" & election_year == 1996 ~ "3162",
      dataset_party_id == "499103" & isoname == "Montenegro" & election_year == 1996 ~ "3164",
      dataset_party_id == "499001" & isoname == "Montenegro" & election_year == 2001 ~ "3162",
      dataset_party_id == "499006" & isoname == "Montenegro" & election_year == 2001 ~ "3645",
      dataset_party_id == "499005" & isoname == "Montenegro" & election_year == 2001 ~ "3104",
      dataset_party_id == "504004" & isoname == "Morocco" & election_year == 2007 ~ "2480",
      dataset_party_id == "504004" & isoname == "Morocco" & election_year == 2011 ~ "2480",
      dataset_party_id == "NG-People's Democratic Party" & isoname == "Nigeria" & election_year == 1999 ~ "2354",
      dataset_party_id == "NO-Centrists-Liberals" & isoname == "Norway" & election_year == 1965 ~ "1072",
      dataset_party_id == "608004" & isoname == "Philippines" & election_year == 2001 ~ "2466",
      dataset_party_id == "616025" & isoname == "Poland" & election_year == 1989 ~ "1286",
      dataset_party_id == "616028" & isoname == "Poland" & election_year == 1989 ~ "767",
      dataset_party_id == "616007" & isoname == "Poland" & election_year == 1997 ~ "1566",
      dataset_party_id == "642055" & isoname == "Romania" & election_year == 2012 ~ "2474",
      dataset_party_id == "642063" & isoname == "Romania" & election_year == 2012 ~ "5940",
      dataset_party_id == "642008" & isoname == "Romania" & election_year == 2012 ~ "5941",
      dataset_party_id == "642052" & isoname == "Romania" & election_year == 2012 ~ "5941",
      dataset_party_id == "643018" & isoname == "Russia" & election_year == 1995 ~ "2247",
      dataset_party_id == "703018" & isoname == "Slovakia" & election_year == 1990 ~ "5",
      dataset_party_id == "152004" & isoname == "Chile"  ~ "390",
      dataset_party_id == "152005" & isoname == "Chile" & election_year == 1990 ~ "256",
      dataset_party_id == "152005" & isoname == "Chile" & election_year == 2000 ~ "256",
      dataset_party_id == "233003" & isoname == "Estonia" ~ "1556",
      dataset_party_id == "233005" & isoname == "Estonia" ~ "174",
      dataset_party_id == "268123"&  isoname == "Georgia" ~ "2988",
      dataset_party_id == "268107"&  isoname == "Georgia" ~ "5885",
      dataset_party_id == "8002" & isoname == "Albania" & election_year >= 1998 ~ "7075",
      dataset_party_id == "76001" & isoname == "Brazil" & election_year == 1991 ~ "654",
      dataset_party_id == "76001" & isoname == "Brazil" & election_year == 1997 ~ "654",
      dataset_party_id == "76003" & isoname == "Brazil" & election_year == 1991 ~ "4402",  #76003 ou 76021 pour celui-là, à vérifier
      dataset_party_id == "203019" & isoname == "Czech Republic" & election_year == 1991 ~ "3921",
      dataset_party_id == "203009" & isoname == "Czech Republic" & election_year == 1991 ~ "3921", #alliance civic democratic et christian democratic party
      dataset_party_id == "818128" & isoname == "Egypt" & election_year == 2013 ~ "5871",
      dataset_party_id == "233010" & isoname == "Estonia" & election_year == 1996 ~ "779",
      dataset_party_id == "233031" & isoname == "Estonia" & election_year == 1996 ~ "1150",
      dataset_party_id == "268002" & isoname == "Georgia" & election_year == 1996 ~ "2168",
      dataset_party_id == "276005" & isoname == "Germany" & election_year == 2006 ~ "1545",#le parti n'existait pas encore au momet de l'enquête donc je le rattache au parti qui l'a précédé
      dataset_party_id == "278001" & isoname == "Ghana" & election_year == 2007 ~ "2311",
      dataset_party_id == "288002" & isoname == "Ghana" & election_year == 2012 ~ "2312",
      dataset_party_id == "348018" & isoname == "Hungary" & election_year == 2009 ~ "42",
      dataset_party_id == "356068" & isoname == "India" & election_year == 1990 ~ "1207",
      dataset_party_id == "360007" & isoname == "Indonesia" & election_year == 2006 ~ "2560",
      dataset_party_id == "364012" & isoname == "Iran" & election_year == 2007 ~ "6322",
      dataset_party_id == "364011" & isoname == "Iran" & election_year == 2007 ~ "5358",
      dataset_party_id == "368003" & isoname == "Iraq" & election_year == 2006 ~ "5919",
      dataset_party_id == "368002" & isoname == "Iraq" & election_year == 2006 ~ "5897",
      dataset_party_id == "368018" & isoname == "Iraq" & election_year == 2013 ~ "5927",
      dataset_party_id == "376002" & isoname == "Israel"  ~ "615",
      dataset_party_id == "428032" & isoname == "Latvia" & election_year == 1996 ~ "1043",
      dataset_party_id == "428023" & isoname == "Latvia" & election_year == 1996 ~ "1704", #attention le parti à l'élection 1998 était dans une alliance qui n'existe pas encore au moment de l'enquête
      dataset_party_id == "428002" & isoname == "Latvia" & election_year == 1996 ~ "1719",
      dataset_party_id == "440013" & isoname == "Lithuania" & election_year == 1997 ~ "1357",
      dataset_party_id == "440005" & isoname == "Lithuania" & election_year == 1997 ~ "738",
      dataset_party_id == "458001" & isoname == "Malaysia" & election_year == 2012 ~ "2485",
      dataset_party_id == "458005" & isoname == "Malaysia" & election_year == 2012 ~ "3637",
      dataset_party_id == "566002" & isoname == "Nigeria" & election_year == 2012 ~ "5538",#le parti n'existe pas encore au moment de l'enquête, donc je l'ai associé à son prédécesseur
      dataset_party_id == "586002" & isoname == "Pakistan" & election_year == 2001  ~ "2385",
      dataset_party_id == "604008" & isoname == "Peru" & election_year == 1996  ~ "5130",
      dataset_party_id == "604008" & isoname == "Peru" & election_year == 1996  ~ "5130",
      dataset_party_id == "608004" & isoname == "Philippines" & election_year == 2012  ~ "2466",
      dataset_party_id == "642003" & isoname == "Romania" & election_year == 2005  ~ "660",
      dataset_party_id == "642060" & isoname == "Romania" & election_year == 2005  ~ "481",
      dataset_party_id == "646109" & isoname == "Rwanda" & election_year == 2012  ~ "3658",
      dataset_party_id == "688001" & isoname == "Serbia" & election_year == 2001  ~ "2190", #je ne sais pas si il faut le relier au 2189 ou 2190, les noms sont très proches
      dataset_party_id == "68804" & isoname == "Serbia" & election_year == 2001  ~ "2175",
      dataset_party_id == "688001" & isoname == "Serbia" & election_year == 2006  ~ "2190", #je ne sais pas si il faut le relier au 2189 ou 2190, les noms sont très proches
      dataset_party_id == "68804" & isoname == "Serbia" & election_year == 2006 ~ "2175",
      dataset_party_id == "705006" & isoname == "Slovenia" & election_year == 1995 ~ "472",
      dataset_party_id == "705006" & isoname == "Slovenia" & election_year == 2005 ~ "472",
      dataset_party_id == "705003" & isoname == "Slovenia" & election_year == 2005 ~ "474",
      dataset_party_id == "710008" & isoname == "South Africa" & election_year == 1990 ~ "1630",
      dataset_party_id == "410002" & isoname == "South Korea" & election_year == 2005 ~ "2305",
      dataset_party_id == "410001" & isoname == "South Korea" & election_year == 2005 ~ "2307",
      dataset_party_id == "410002" & isoname == "South Korea" & election_year == 2010 ~ "2305",
      dataset_party_id == "410001" & isoname == "South Korea" & election_year == 2010 ~ "2307",
      dataset_party_id == "756022" & isoname == "Switzerland" & election_year == 1989 ~ "1808",
      dataset_party_id == "756026" & isoname == "Switzerland" & election_year == 1989 ~ "360",
      dataset_party_id == "756024" & isoname == "Switzerland" & election_year == 1989 ~ "308",
      dataset_party_id == "756023" & isoname == "Switzerland" & election_year == 1989 ~ "29",
      dataset_party_id == "788023" & isoname == "Tunisia" & election_year == 2003 ~ "5832",
      dataset_party_id == "788022" & isoname == "Tunisia" & election_year == 2003 ~ "4530",
      dataset_party_id == "792042" & isoname == "Turkey" & election_year == 1990 ~ "1253",
      dataset_party_id == "792015" & isoname == "Turkey" & election_year == 1996 ~ "1463", #le parti n'existait pas au moment de l'enquête je l'ai rattaché à son prédécesseur 
      dataset_party_id == "792025" & isoname == "Turkey" & election_year == 2001 ~ "306", #le parti n'existait pas au moment de l'enquête je l'ai rattaché à son prédécesseur 
      dataset_party_id == "716007" & isoname == "Zimbabwe" & election_year == 2001 ~ "3305",
      dataset_party_id == "716002" & isoname == "Zimbabwe" & election_year == 2012 ~ "3559",
      dataset_party_id == "716007" & isoname == "Zimbabwe" & election_year == 2001 ~ "3305",
      dataset_party_id == "76003" & isoname == "Brazil" & election_year >= 1991 ~ "225",
      dataset_party_id == "BE-1-3-V" & isoname == "Belgium" & election_year == 2004 & election_year == 2006  ~ "1586",
      dataset_party_id == "BE-1-13-V" & isoname == "Belgium" & election_year == 2002 ~ "554", 
      dataset_party_id == "BG-3-1-V" & isoname == "Bulgaria" & election_year >= 2006 ~ "1665",
      dataset_party_id == "CZ-1-10-V" & isoname == "Czech Republic" & election_year == 2004 ~ "676",
      dataset_party_id == "CZ-1-2-V" & isoname == "Czech Republic" & election_year == 2008 ~ "466",
      dataset_party_id == "EE-2-4-V" & isoname == "Estonia" & election_year >= 2008 ~ "685",
      dataset_party_id == "IT-1-8-V" & isoname == "Italy" & election_year == 2002 ~ "6241",
      dataset_party_id == "IT-1-9-V" & isoname == "Italy" & election_year == 2002 ~ "6241",
      dataset_party_id == "IT-1-11-V" & isoname == "Italy" & election_year == 2002 ~ "6241",
      dataset_party_id == "IT-1-10-V" & isoname == "Italy" & election_year == 2002 ~ "6241",
      dataset_party_id == "IT-1-1-V" & isoname == "Italy" & election_year == 2002 ~ "1737",
      dataset_party_id == "IT-1-2-V" & isoname == "Italy" & election_year == 2002 ~ "1737",
      dataset_party_id == "IT-1-3-V" & isoname == "Italy" & election_year == 2002 ~ "1737",
      dataset_party_id == "IT-1-1-V" & isoname == "Italy" & election_year >= 2016 ~ "802",
      dataset_party_id == "PL-1-1-V" & isoname == "Poland" & election_year >= 2002 & election_year <= 2004 ~ "57",
      dataset_party_id == "PL-1-6-V" & isoname == "Poland" & election_year >= 2002 & election_year == 2006 ~ "1117",
      dataset_party_id == "PL-1-1-V" & isoname == "Poland" & election_year >= 2008 & election_year <= 2010 ~ "1588",
      dataset_party_id == "PT-1-11-V" & isoname == "Portugal" & election_year >= 2016  ~ "655",
      dataset_party_id == "RO-4-2-V" & isoname == "Romania" & election_year == 2008  ~ "120",
      
      
      #Cas plus problématique de join dans ESS
      dataset_party_id == "BG-5-1-V" & isoname == "Bulgaria" & election_year >= 2010 ~ "760", #*
      dataset_party_id == "HR-9-3-V" & isoname == "Croatia" & election_year >= 2018 ~ "4865", #*
      dataset_party_id == "CZ-1-9-V" & isoname == "Czech Republic" & election_year >= 2008 ~ "1728", #*
      dataset_party_id == "CZ-5-5-V" & isoname == "Czech Republic" & election_year >= 2010 ~ "223", #*
      dataset_party_id == "CZ-7-4-V" & isoname == "Czech Republic" & election_year >= 2014 ~ "2141", #*
      dataset_party_id == "CZ-5-6-V" & isoname == "Czech Republic" & election_year == 2012 ~ "1202", #*
      dataset_party_id == "FI-1-9-V" & isoname == "Finland" & election_year >= 2012 ~ "1303", #*
      dataset_party_id == "IT-6-4-V" & isoname == "Italy" & election_year >= 2016 ~ "2046", #*
      dataset_party_id == "NL-4-11-V" & isoname == "Netherlands" & election_year >= 2014 ~ "298", #*
      dataset_party_id == "RU-3-3-V" & isoname == "Russia" ~ "2245", #*
      dataset_party_id == "SK-6-1-V" & isoname == "Slovakia" ~ "2130", #*
      dataset_party_id == "SI-4-9-V" & isoname == "Slovenia" ~ "474", #*
      dataset_party_id == "SI-6-7-V" & isoname == "Slovenia" ~ "1773", #*
      dataset_party_id == "SI-7-8-v" & isoname == "Slovenia" ~ "3098", #*
      dataset_party_id == "UA-3-1-V" & isoname == "Ukraine" ~ "2234", #*
      dataset_party_id == "UA-2-10-V" & isoname == "Ukraine" ~ "2228", #*
      TRUE ~ partyfacts_id
    ))

#Rattacher les répondants à la bonne élection 
wvs_data_clean <- wvs_data_clean %>%
  group_by(isoname) %>%
  group_modify(~{
    
    df <- .x
    country <- .y$isoname[1]
    
    elections <- all_elections %>%
      filter(isoname == country) %>%
      mutate(
        election_date = as.Date(election_date, format = "%Y.%m.%d")
      ) %>%
      arrange(year, election_date)
    
    if (nrow(elections) == 0) return(df)
    
    map_dfr(seq_len(nrow(df)), function(i){
      
      row <- df[i, ]
      
      ## Cas 1 : date d'interview connue + dates d'élection disponibles
      if (!is.na(row$interview_date) &&
          any(!is.na(elections$election_date))) {
        
        next_elec <- elections %>%
          filter(!is.na(election_date),
                 election_date > row$interview_date) %>%
          slice_min(election_date, n = 1)
        
        ## Cas 2 : secours → matching par année
      } else {
        
        next_elec <- elections %>%
          filter(year >= row$year) %>%
          arrange(year, election_date) %>%
          slice_head(n = 1)
        
      }
      
      if (nrow(next_elec) == 0) {
        
        row$election_date <- as.Date(NA)
        row$election_year <- NA_integer_
        
      } else {
        
        row$election_date <- next_elec$election_date[1]
        row$election_year <- next_elec$year[1]
        
      }
      
      row
      
    })
    
  }) %>%
  ungroup()

wvs_data_clean <- wvs_data_clean %>%
  select(-year)

###Export Base WVS ----
write.csv(
  wvs_data_clean,
  "data/intermediary/elections/wvs elections dataset.csv",
  row.names = FALSE)







#Espace pour empiler les bases votes entre elles ----
cses_data_clean <- cses_data_clean %>% mutate(year = as.integer(year))
ess_data_clean <- ess_data_clean %>% mutate(year = as.integer(year))
wvs_data_clean <- wvs_data_clean %>% mutate(year = as.integer(year))

ess_data_clean <- ess_data_clean %>% mutate(type = "Lower House")
cses_data_clean <- cses_data_clean %>% mutate(type = "Lower House")

bases <- list(
  cses_data_clean,
  ess_data_clean,
  wvs_data_clean
)

Base_cses_ess_wvs <- bind_rows(bases)

###Export Base CSES ESS WVS ----
write.csv(
  Base_cses_ess_wvs,
  "data/intermediary/elections/dataset cses ess wvs.csv",
  row.names = FALSE
)
