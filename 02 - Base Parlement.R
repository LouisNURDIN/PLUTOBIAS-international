#Base parlement 
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)

#Import base élections législatives
Base_legislatives_deciles <- read.csv("data/intermediary/elections/legislative elections with decile dataset.csv", sep = ",")
elections_legislatives_valides <- read.csv("data/intermediary/elections/valid legislative elections.csv", sep = ",")

Base_legislatives_deciles2 <- read.csv("data/intermediary/elections/legislative elections with dinc dataset.csv", sep = ",")

#Elections global ----
Data_elections_global <- read.csv ("data/raw/elections global/elections-global-release.csv" , sep = ";")

pays_gmp_legislatives <- unique(Base_legislatives_deciles$isoname)
annees_gmp_legislatives <- unique(Base_legislatives_deciles$year)

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

check_electionsglobal_wpid_legislatives <- check_electionsglobal_wpid_legislatives %>%
  filter(check_electionsglobal_wpid_legislatives$year <= 2015)


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

Elections_global <- Elections_global %>%
  semi_join(
    Base_legislatives_deciles %>%
      distinct(isoname, year),
    by = c("isoname", "year")
  )

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
      TRUE ~ partyfacts_id
    )
  )


Elections_global <- Elections_global  %>% 
  mutate(seats_share = seats / seats_total)
## Join entre les bases ----
base_vote_parlement_legislatives <- Base_legislatives_deciles %>%
  left_join(
    Elections_global %>%
      select(isoname, year,partyfacts_id,election_date,seats,seats_total,seats_share),
    distinct(isoname, year,partyfacts_id),
    by = c("isoname", "year","partyfacts_id")
  )

###Traitement pour avoir le taux de députés par partis sur l'ensemble des députés




base_vote_parlement_legislatives <- base_vote_parlement_legislatives %>%
  filter(year <= 2015)


base_vote_parlement_legislatives <- base_vote_parlement_legislatives %>%
  mutate(
    seats_share = case_when(
      partyfacts_id == "1691" & year == 2002 ~ 0.487,
      partyfacts_id == "1408" & year == 2002 ~ 0.51295,
      TRUE ~ seats_share
    )
  )

base_vote_parlement_legislatives <- base_vote_parlement_legislatives %>%
  mutate(
    seats = case_when(
      partyfacts_id == "1691" & year == 2002 ~ 188,
      partyfacts_id == "1408" & year == 2002 ~ 198,
      TRUE ~ seats_share
    )
  )

base_vote_parlement_legislatives <- base_vote_parlement_legislatives  %>% 
  group_by(isoname,year,election_date,decile)%>%
  mutate(election_couverture_seats = sum(seats_share))
#Lister les partis sans données sur les sièges au parlement ----
unique(base_vote_parlement_legislatives$vote[is.na(base_vote_parlement_legislatives$seats)])

View(
  base_vote_parlement_legislatives %>%
    ungroup() %>%
    filter(is.na(seats)) %>%
    distinct(year,isoname,vote,partyfacts_id,seats)
)

##Liste des élections où le taux de sièges couverts est inférieur à 1----
View(
  base_vote_parlement_legislatives %>%
    ungroup() %>%
    filter(election_couverture_seats < 0.8) %>%
    distinct(year,isoname,election_couverture_seats)
)   

##Valeurs présentes dans ma base Parlement maps pas élections----
View(Elections_global %>%
       distinct(isoname, year, election_date, partyfacts_id,seats,seats_share) %>%
       anti_join(
         base_vote_parlement_legislatives %>% distinct(isoname, year, election_date, partyfacts_id),
         by = c("isoname", "year", "election_date", "partyfacts_id")
       ))

View(
  Elections_global %>%
    filter(seats_share >= 0.05) %>%
    distinct(isoname, year, election_date, partyfacts_id, seats,seats_share) %>%
    anti_join(
      base_vote_parlement_legislatives %>%
        distinct(isoname, year, election_date, partyfacts_id),
      by = c("isoname", "year", "election_date", "partyfacts_id")
    )
)



#Partis présents dans base élections mais pas Parlement
View(
  base_vote_parlement_legislatives %>%
    ungroup() %>%
    group_by(isoname, year, partyfacts_id) %>%
    summarise(
      mean_pct_votes = mean(pct_votes, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    anti_join(
      Elections_global %>%
        distinct(isoname, year, election_date, partyfacts_id),
      by = c("isoname", "year", "partyfacts_id")
    )
)



