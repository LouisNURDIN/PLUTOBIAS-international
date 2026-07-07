#Base parlement 
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)

#Import base élections législatives
elections_legislatives_valides <- read.csv("data/intermediary/elections/valid legislative elections.csv", sep = ",")

#Import parlgov ----
parlgov <- read.csv("data/raw/parlgov/parlgov_elections.csv", sep = ";")
parlgov <- parlgov %>% rename(isoname = country_name)
parlgov <- parlgov %>% filter(election_type == "parliament") 

parlgov <- parlgov %>%
  mutate(election_date = as.Date(election_date, format = "%d/%m/%Y"),
    election_year = as.integer(format(election_date, "%Y")))

parlgov <- parlgov  %>% 
  mutate(seats_share = seats / seats_total * 100)

Partyfacts_id <- read.csv("data/raw/partyfacts-external-parties (1).csv", sep = ";")
unique(Partyfacts_id$dataset_key)

Partyfacts_id_parlgov <- Partyfacts_id %>% filter(dataset_key == "parlgov")

parlgov <- parlgov  %>% rename(dataset_party_id = party_id)

parlgov <- parlgov %>% mutate(dataset_party_id = as.character(dataset_party_id))

parlgov <- parlgov %>% left_join(Partyfacts_id_parlgov %>%
      dplyr::select(dataset_party_id,partyfacts_id),
    by = "dataset_party_id")

parlgov <- parlgov %>% mutate(partyfacts_id = as.character(partyfacts_id))

#Import de ma base avec mes clivages ----
Base_all_clivages <- read.csv("data/intermediary/elections/dataset with all clivages and elections.csv", sep = ",")

Base_all_clivages <- Base_all_clivages %>%
  mutate(election_year = as.integer(election_year)) %>%
  filter(election_year <= 2020)

Base_all_clivages <- Base_all_clivages %>%
  filter(isoname != "Hong Kong")
Base_all_clivages <- Base_all_clivages %>%
  filter(isoname != "Taiwan")

#Elections global ----
Data_elections_global <- read.csv ("data/raw/elections global/elections-global-release.csv" , sep = ";")

#correctif données Malaysie 2004
Data_elections_global <- Data_elections_global  %>%
  mutate(
    seats_total = case_when(
      country_name == "Malaysia" & year == 2004  ~ 219,
      TRUE ~ seats_total))
      
      
pays_gmp_legislatives <- unique(Base_all_clivages$isoname)
annees_gmp_legislatives <- unique(Base_all_clivages$year)

#filtrer pour avoir que les pays dans WPID
Data_elections_global_group <- Data_elections_global %>%
  filter(country_name%in% pays_gmp_legislatives)

Data_elections_global_elections_valides <- Data_elections_global_group %>%
  group_by(country_name,year) %>%
  summarise(.groups = "drop")

Data_elections_global_elections_valides <- Data_elections_global_elections_valides %>%
  rename(isoname = country_name)

check_electionsglobal_wpid_legislatives <- elections_legislatives_valides %>%
  left_join(
    Data_elections_global_elections_valides, by = c("isoname", "year")
  )



#Vérifier le nombre d'élections compatibles entre les deux groupes
intersect(
  paste(elections_legislatives_valides$isoname, elections_legislatives_valides$year),
  paste(Data_elections_global_elections_valides$isoname, Data_elections_global_elections_valides$year)
) %>% length()

#Join entre base élections et base parlement ----
##Rajouter l'abstention dans elections_global----
library(dplyr)

abstention_rows <- Data_elections_global %>%
  group_by(country_name, year, election_date) %>%
  summarise(
    seats_total = first(seats_total),
    .groups = "drop"
  ) %>%
  mutate(
    party = "Abstention",
    partyfacts_id = "Abstention",
    seats = 0
  )

Data_elections_global <- Data_elections_global %>%
  mutate(partyfacts_id = as.character(partyfacts_id))
abstention_rows <- abstention_rows %>%
  mutate(partyfacts_id = as.character(partyfacts_id))

Elections_global <- Data_elections_global %>%
  bind_rows(abstention_rows) %>%
  arrange(country_name, year, election_date, party)

Elections_global <- Elections_global %>%
  rename(isoname = country_name)

##Traitement sur données manquantes ----
Elections_global <- Elections_global %>%
  group_by(isoname, year, election_date) %>%
  mutate(
    seats = if_else(
      !is.na(alliance_seats),
      alliance_seats / sum(!is.na(alliance_seats)),
      seats
    ),
    seats_total = first(seats_total)
  ) %>%
  ungroup()

#Correctifs sur mes bases votes et parlement pour faire join les partis ----
Elections_global <- Elections_global %>%
  mutate(
    partyfacts_id = case_when(
      party == "other (other-script)" ~ "Other",
      partyfacts_id == "1246" & isoname == "France" ~ "1083",
      partyfacts_id == "1629" & isoname == "France" & year == 1956 ~ "737",
      partyfacts_id == "2603"&  isoname == "Belarus" ~ "Other",
      partyfacts_id == "3605"&  isoname == "Libya" ~ "Other",
      partyfacts_id == "5766"&  isoname == "Senegal" & year == 2012 ~ "4010",
      partyfacts_id == "2577"&  isoname == "Argentina" ~ "Other",
      partyfacts_id == "2577"&  isoname == "Armenia" ~ "Other",
      partyfacts_id == "54"&  isoname == "Chile" & year == 1993 ~ "390",
      partyfacts_id == "561"&  isoname == "Colombia" & year == 1993 ~ "759", #je les rattache à l'alliance principale pour cette élection
      partyfacts_id == "2670"&  isoname == "Egypt" ~ "Other",
      partyfacts_id == "1096"&  isoname == "Finland" & year > 1990 ~ "1044",
      partyfacts_id == "524"&  isoname == "Guatemala" & year == 2007 ~ "538",
      partyfacts_id == "6366"&  isoname == "Hungary" & year == 2010 ~ "1691",
      partyfacts_id == "2560"&  isoname == "Indonesia" & year == 2004 ~ "Other", #Pour cette année-là, le parti n'est pas inclus dans WVS
      partyfacts_id == "3593"&  isoname == "Jordan" ~ "Other",
      partyfacts_id == "4601" & isoname == "Philippines" & year == 1996  ~ "2388",
      partyfacts_id == "4802" & isoname == "Venezuela" & year == 1998  ~ "Other",
      partyfacts_id == "2335" & isoname == "Zambia" & year == 2007  ~ "",
      partyfacts_id == "730" & isoname == "Slovakia" & year == 2006  ~ "560",
      TRUE ~ partyfacts_id
    )
  )




Elections_global <- Elections_global  %>% 
  rename(election_year = year)

#DINC ----
##Traitement sur données manquantes ----
Elections_global2 <- Elections_global %>%
  group_by(isoname, election_year, election_date) %>%
  mutate(
    seats = if_else(
      !is.na(alliance_seats),
      alliance_seats / sum(!is.na(alliance_seats)),
      seats
    ),
    seats_total = first(seats_total)
  ) %>%
  ungroup()

#Correctifs sur mes bases votes et parlement pour faire join les partis ----
Elections_global2 <- Elections_global2 %>%
  mutate(
    partyfacts_id = case_when(
      party == "other (other-script)" ~ "Other",
      partyfacts_id == "1246" & isoname == "France" ~ "1083",
      TRUE ~ partyfacts_id
    )
  )


Elections_global2 <- Elections_global2 %>%
  mutate(election_date = as.Date(election_date, format = "%Y.%m.%d"))

parlgov_last_elections <- parlgov %>%
  anti_join(Elections_global2,
            by = c("isoname", "election_date"))


Elections_global2 <-
  bind_rows(Elections_global2, parlgov_last_elections)

Elections_global2 <- Elections_global2 %>%
  arrange(isoname, election_date)

Elections_global2 <- Elections_global2  %>% 
  mutate(seats_share = seats / seats_total * 100)

## Join entre les bases ----
Base_all_clivages <- Base_all_clivages %>%
  mutate(election_date = as.Date(election_date))


Base_vote_parlement_global_date <- Base_all_clivages %>%
  filter(!is.na(election_date)) %>%
  left_join(
    Elections_global2 %>%
      select(
        isoname,
        election_date,
        partyfacts_id,
        party,
        seats,
        seats_total,
        seats_share
      ),
    by = c(
      "isoname",
      "election_date",
      "partyfacts_id"
    )
  )

Base_vote_parlement_global_nodate <- Base_all_clivages %>%
  filter(is.na(election_date)) %>%
  left_join(
    Elections_global2 %>%
      select(
        isoname,
        election_year,
        partyfacts_id,
        party,
        seats,
        seats_total,
        seats_share
      ),
    by = c(
      "isoname",
      "election_year",
      "partyfacts_id"
    )
  )

Base_vote_parlement_global <- bind_rows(
  Base_vote_parlement_global_date,
  Base_vote_parlement_global_nodate
) %>%
  arrange(isoname, election_year, election_date)



###Traitement pour avoir le taux de députés par partis sur l'ensemble des députéss
Base_vote_parlement_global <- Base_vote_parlement_global %>%
  mutate(
    seats_share = case_when(
      partyfacts_id == "1691" & election_year == 2002 ~ 42.48,
      partyfacts_id == "1408" & election_year == 2002 ~ 52.331,
      partyfacts_id == "910" & election_year == 2002 ~ 5.18,
      TRUE ~ seats_share
    )
  )

Base_vote_parlement_global <- Base_vote_parlement_global %>%
  mutate(
    seats_share = case_when(
      partyfacts_id == "1083" & isoname == "France" & election_year == 1967  ~ 0.25,
      partyfacts_id == "1083" & isoname == "France" & election_year == 1973  ~ 0.25,
      partyfacts_id == "6241" & isoname == "Italy" & election_year == 2001  ~ 7.30158725,
      partyfacts_id == "1737" & isoname == "Italy" & election_year == 2001  ~ 12.8042326667,
      partyfacts_id == "6241" & isoname == "Italy" & election_year == 2006  ~ 14.867725,
      partyfacts_id == "1372" & isoname == "Italy" & election_year == 2006  ~ 18.4126983333,
      partyfacts_id == "1691" & isoname == "Hungary" & election_year == 2002  ~ 48.70466321,
      partyfacts_id == "1408" & isoname == "Hungary" & election_year == 2002  ~ 46.11398964,
      partyfacts_id == "1408" & isoname == "Hungary" & election_year == 2002  ~ 4.92227979,
      partyfacts_id == "4010" & isoname == "Senegal" & election_year == 2012  ~ 26.44444444,
      
      TRUE ~ seats_share
    )
  )

Base_vote_parlement_global <- Base_vote_parlement_global %>%
  distinct(source,source_recode,isoname,election_year,election_date,partyfacts_id,bias,
    category,.keep_all = TRUE)

Base_vote_parlement_global <- Base_vote_parlement_global %>%
  group_by(source,source_recode,isoname,election_year,election_date,bias,category) %>%
  mutate(
    election_couverture_seats = sum(seats_share, na.rm = TRUE)) %>%
  ungroup()

#Rcenser le nombre de sièges appartnenant aux partis "Other" pour recenser les élections que l'on ne pourra pas traiter
  
  Base_vote_parlement_global <- Base_vote_parlement_global %>%
    group_by(source,source_recode,isoname,election_year,election_date) %>%
    mutate(
      other_seats = seats_share[partyfacts_id == "Other"][1]
    ) %>%
    ungroup()
#Lister les élections problématiques 
View(
  Base_vote_parlement_global %>%
    ungroup() %>%
    filter(election_couverture_seats > 100) %>%
    distinct(election_year, election_date,isoname,source,source_recode,election_couverture_seats,other_seats)
)

#Lister les partis importants qui joinent mal entre la base parlement et la base agrégée

manquants <- Elections_global2 %>%
       filter(seats_share >= 10) %>%
       distinct(isoname,election_year, election_date, partyfacts_id, seats_share) %>%
       anti_join(
         Base_vote_parlement_global %>% distinct(source,source_recode,isoname,election_year,election_date, partyfacts_id,seats_share,election_couverture_seats),
         by = c("isoname","election_year", "election_date", "partyfacts_id"
                ))
View(
 manquants %>%
left_join(Base_vote_parlement_global %>%distinct(isoname, election_year,election_date,source, source_recode),
               by = c("isoname", "election_year", "election_date")))


unique(Base_vote_parlement_global$source_recode)
unique(Base_vote_parlement_global$party[Base_vote_parlement_global$isoname == "Belgium" & 
                                          Base_vote_parlement_global$year == 2003
                                        & Base_vote_parlement_global$source_recode == "ESS"])

#Liste des élections dans Elections Global
elections_dans_elections_global2 <- Elections_global2 %>%
  group_by(isoname,election_year,election_date) %>%
  summarise(.groups = "drop")

elections_dans_elections_global <- Elections_global %>%
  group_by(isoname,election_year,election_date) %>%
  summarise(.groups = "drop")


#Ajout de parline 
parline <- read.csv("data/raw/parline/share of women diputees accross all countries.csv", sep = ";")

parline <- parline %>%
  mutate(election_year = substr(date_from, nchar(date_from) - 3, nchar(date_from)))

parline <- parline %>%
  rename(isoname = Country)
parline <- parline %>%
  rename(Percentage.of.women.diputees = Percentage.of.women)
parline <- parline %>%
  mutate(election_year = as.integer(election_year))
Base_vote_parlement_global <- Base_vote_parlement_global %>%
  left_join(
    parline  %>%
      select(isoname,election_year,Percentage.of.women.diputees),
    by= c("isoname","election_year")
  )

Base_vote_parlement_global <- Base_vote_parlement_global %>%
  distinct(source,source_recode,isoname,election_year,election_date,partyfacts_id,bias,
           category,.keep_all = TRUE)

Base_vote_parlement_global <- Base_vote_parlement_global %>%
  arrange(isoname, election_year,election_date)

unique(parlgov$isoname)

#Export base avec méthode dinc
write.csv(
  Base_vote_parlement_global,
  "data/intermediary/parliament/Elections and parliament global dataset.csv",
  row.names = FALSE
)

write.csv(
  elections_dans_elections_global2,
  "data/intermediary/elections/valid legislative elections.csv",
  row.names = FALSE
)

write.csv(
  elections_dans_elections_global,
  "data/intermediary/elections/list all elections.csv",
  row.names = FALSE)


all_elections_update <- read.csv ("data/intermediary/elections/all elections update.csv", sep = ";")

all_elections_update <- all_elections_update %>%
  mutate(year = as.integer(year)) %>%
  left_join(
    Elections_global %>%
      mutate(year = as.integer(year)) %>%
      dplyr::select(isoname, year, election_date),
    by = c("isoname", "year")
  )

write.csv(
  all_elections_update,
  "data/intermediary/elections/all elections update.csv",
  row.names = FALSE)
            