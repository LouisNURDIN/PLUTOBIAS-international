#Base parlement 
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)

#Import base élections législatives
elections_legislatives_valides <- read.csv("data/intermediary/elections/valid legislative elections.csv", sep = ",")

Base_all_clivages <- read.csv("data/intermediary/elections/dataset with all clivages and elections.csv", sep = ",")

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
      TRUE ~ seats_total
    )
  )
      
      
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
      TRUE ~ partyfacts_id
    )
  )


Elections_global <- Elections_global  %>% 
  mutate(seats_share = seats / seats_total * 100)


#DINC ----
Elections_global2 <- Elections_global %>%
  semi_join(
    Base_all_clivages %>%
      distinct(isoname, year),
    by = c("isoname", "year")
  )

##Traitement sur données manquantes ----
Elections_global2 <- Elections_global2 %>%
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
Elections_global2 <- Elections_global2 %>%
  mutate(
    partyfacts_id = case_when(
      party == "other (other-script)" ~ "Other",
      partyfacts_id == "1246" & isoname == "France" ~ "1083",
      TRUE ~ partyfacts_id
    )
  )

## Join entre les bases ----
Base_vote_parlement_global <- Base_all_clivages %>%
  left_join(
    Elections_global2 %>%
      select(isoname, year,partyfacts_id,election_date,seats,seats_total,seats_share),
    distinct(isoname, year,partyfacts_id),
    by = c("isoname", "year","partyfacts_id")
  )

###Traitement pour avoir le taux de députés par partis sur l'ensemble des députéss


Base_vote_parlement_global <- Base_vote_parlement_global %>%
  mutate(
    seats_share = case_when(
      partyfacts_id == "1691" & year == 2002 ~ 42.48,
      partyfacts_id == "1408" & year == 2002 ~ 52.331,
      partyfacts_id == "910" & year == 2002 ~ 5.18,
      TRUE ~ seats_share
    )
  )

Base_vote_parlement_global <- Base_vote_parlement_global %>%
  mutate(
    seats_share = case_when(
      partyfacts_id == "1083" & isoname == "France" & year == 1967  ~ 0.25,
      partyfacts_id == "1083" & isoname == "France" & year == 1973  ~ 0.25,
      partyfacts_id == "6241" & isoname == "Italy" & year == 2001  ~ 7.30158725,
      partyfacts_id == "1737" & isoname == "Italy" & year == 2001  ~ 12.8042326667,
      partyfacts_id == "6241" & isoname == "Italy" & year == 2006  ~ 14.867725,
      partyfacts_id == "1372" & isoname == "Italy" & year == 2006  ~ 18.4126983333,
      partyfacts_id == "1691" & isoname == "Hungary" & year == 2002  ~ 48.70466321,
      partyfacts_id == "1408" & isoname == "Hungary" & year == 2002  ~ 46.11398964,
      partyfacts_id == "1408" & isoname == "Hungary" & year == 2002  ~ 4.92227979,
      partyfacts_id == "4010" & isoname == "Senegal" & year == 2012  ~ 26.44444444,
      
      TRUE ~ seats_share
    )
  )


Base_vote_parlement_global <- Base_vote_parlement_global  %>% 
  group_by(source_recode,bias,category,isoname,year,)%>%
  mutate(election_couverture_seats = sum(seats_share, na.rm = TRUE))


  Base_vote_parlement_global <- Base_vote_parlement_global %>%
    filter(year <= 2015)

#Lister les élections problématiques 
View(
  Base_vote_parlement_global %>%
    ungroup() %>%
    filter(election_couverture_seats < 80) %>%
    distinct(year,isoname,source_recode,election_couverture_seats)
)

#Lister les partis importants qui joinent mal entre la base parlement et la base agrégée
View(Elections_global2 %>%
       filter(seats_share >= 10) %>%
       filter(year <= 2015) %>%
       distinct(isoname, year, partyfacts_id, seats_share) %>%
       anti_join(
         Base_vote_parlement_global %>% distinct(source_recode,isoname, year, partyfacts_id,seats_share,election_couverture_seats),
         by = c("isoname", "year", "partyfacts_id"
                )
       ))


#Liste des élections dans Elections Global
elections_dans_elections_global <- Elections_global2 %>%
  group_by(isoname,year) %>%
  summarise(.groups = "drop")

#Export base avec méthode dinc
write.csv(
  Base_vote_parlement_global,
  "data/intermediary/parliament/Elections and parliament global dataset.csv",
  row.names = FALSE
)


