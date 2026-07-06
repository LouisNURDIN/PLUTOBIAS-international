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

GMP_inc_2 <- GMP_inc_2 %>%
  mutate(interview_date = year)

GMP_inc_2 <- GMP_inc_2 %>%
  mutate(
    interview_date = year)

unique(GMP_inc_2$educ)
unique(GMP_inc_2$age)

GMP_inc_2_clean <- GMP_inc_2 %>%
  select(isoname,year,interview_date, source, source_recode, survey,type, dinc,gender,educ, age, turnout,dataset_party_id)


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

sum(is.na(ess_data_long$inwdd))

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
  select(isoname,year,interview_date, source, source_recode,survey, type, inc,gender,educ,age, turnout, dataset_party_id)

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

unique(ess_data_clean$dataset_party_id[ess_data_clean$isoname == "Ukraine" & 
                                ess_data_clean$year == 2012
                              & ess_data_clean$source_recode == "ESS"])

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

###Export Base ESS ----
write.csv(
  ess_data_clean,
  "data/intermediary/elections/ess elections dataset.csv",
  row.names = FALSE
)

unique(Base_all_elections$source)

#je le mets ici en attendant

#CSES ----
cses_data <- read.csv("data/raw/cses/cses_imd.csv")


###Identifier les variables qui nous intéressent pour les harmoniser----
cses_data <- cses_data %>%
  rename(isoname = IMD1006_NAM)
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
  mutate(survey = "Post-electoral")


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
  filter(type <= 13)



cses_data_clean <- cses_data %>%
  select(isoname,year,interview_date, source, source_recode,survey, type, inc,gender,educ,age, turnout, dataset_party_id)

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

###Export Base CSES ----
write.csv(
  cses_data_clean,
  "data/intermediary/elections/cses elections dataset.csv",
  row.names = FALSE
)

#WVS ----
wvs_data <- read.csv  ("data/raw/wvs/WVS_Time_Series_1981-2022_csv_v5_0.csv")
sum(is.na(wvs_data$S012))
table(wvs_data$S012)
unique(wvs_data$S018)

mean(wvs_data$S018, na.rm = TRUE)
sum(wvs_data$S018, na.rm = TRUE)


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
  mutate(weight = S018)
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


wvs_data <- wvs_data %>%
  mutate(
    interview_date = if_else(
      is.na(S012) | S012 < 0,
      as.Date(NA),
      as.Date(as.character(S012), format = "%Y%m%d")
    ))

wvs_data_clean <- wvs_data %>%
  select(isoname,year,interview_date, source, source_recode,survey, type, inc,gender,educ,age, turnout, dataset_party_id)

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
