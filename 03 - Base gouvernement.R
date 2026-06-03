library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
#Base avec données gouvernements
base_vote_parlement_legislatives <- read.csv("data/intermediary/parliament/elections and parliament dataset.csv", sep = ",")
whogov <- read.csv("data/raw/whogov/WhoGov_within_V3.1.csv", sep = ";")
elections_legislatives_valides <- read.csv("data/intermediary/elections/valid legislative elections.csv", sep = ",")
pays_gmp_legislatives <- unique(elections_legislatives_valides$isoname)
annees_gmp_legislatives <- unique(elections_legislatives_valides$year)

#Calcul nombre de ministres par année
whogov <- whogov %>%
  rename(isoname = country_name)

whogov_parties <- whogov %>%
  group_by(isoname, year, partyfacts_id) %>%
  summarise(
    ministers_party = n(),
    .groups = "drop_last"
  ) %>%
  mutate(
    total_ministers = sum(ministers_party),
    ministers_share = ministers_party / total_ministers
  ) %>%
  ungroup()

#Join whogov dans ma base vote-parlement ----
whogov_parties <- whogov_parties %>%
  mutate(year = as.integer(year))
whogov_parties <- whogov_parties %>%
  mutate(partyfacts_id = as.character(partyfacts_id))

##calcul bonne date pour le join ----
library(lubridate)
library(stringr)
base_vote_parlement_legislatives2 <- base_vote_parlement_legislatives %>%
  group_by(isoname, year) %>%
  mutate(
    election_date = na_if(election_date, ""),
    election_date = first(na.omit(election_date))
  ) %>%
  ungroup() %>%
  mutate(
    election_date_date = as.Date(str_replace_all(election_date, "\\.", "-")),
    join_year = case_when(
      is.na(election_date_date) ~ year,
      month(election_date_date) < 7 ~ year(election_date_date),
      TRUE ~ year(election_date_date) + 1
    )
  )

###join whogov avec ma base élections/législatives ----
base_vote_parlement_legislatives2 <- base_vote_parlement_legislatives2 %>%
  mutate(
    join_year = as.integer(join_year)
  )

whogov_parties <- whogov_parties %>%
  mutate(
    year = as.integer(year)
  )


library(dplyr)
library(tidyr)
years <- seq(
  min(base_vote_parlement_legislatives2$year, na.rm = TRUE),
  max(whogov_parties$year, na.rm = TRUE),
  by = 1
)

base <- base_vote_parlement_legislatives2 %>%
  distinct(isoname, decile, partyfacts_id)

party_life <- base_vote_parlement_legislatives2 %>%
  group_by(isoname, partyfacts_id) %>%
  summarise(
    first_year = min(year, na.rm = TRUE),
    last_year  = max(year, na.rm = TRUE),
    .groups = "drop"
  )

grid <- tidyr::expand_grid(
  base_vote_parlement_legislatives2 %>%
    distinct(isoname, decile, partyfacts_id),
  year = years
) %>%
  left_join(party_life, by = c("isoname", "partyfacts_id")) %>%
  filter(year >= first_year & year <= last_year)

base_complete <- grid %>%
  left_join(
    base_vote_parlement_legislatives2,
    by = c("isoname", "year", "decile", "partyfacts_id")
  ) %>%
  arrange(isoname,year, decile, partyfacts_id) %>%
  group_by(isoname, decile, partyfacts_id) %>%
  fill(
    survey, votes, pct_votes, nbr_obs, votes_valides,
    taux_participation, election_date_date,
    seats, seats_total, seats_share, election_couverture_seats,
    .direction = "down"
  ) %>%
  ungroup()

base_complete <- base_complete %>%
  left_join(
    whogov_parties,
    by = c("isoname","year","partyfacts_id"),
    relationship = "many-to-many"
  )
#code original ----

###mise au propre de la base ----
base_complete <- base_complete[!is.na(base_complete$year),]

base_complete <- base_complete %>%
  select(isoname, year, election_date_date,join_year, survey, decile, partyfacts_id, votes, pct_votes,
         votes_valides, taux_participation, seats, seats_total, seats_share, ministers_party, total_ministers, ministers_share, election_couverture_seats
         )

#####verif nombre de ministres couverts par élection ----
base_complete <- base_complete %>%
  mutate(
    ministers_party = coalesce(ministers_party, 0),
    total_ministers = coalesce(total_ministers, 0),
    ministers_share = coalesce(ministers_share, 0)
  ) %>%
  group_by(isoname, year, decile) %>%
  mutate(
    election_couverture_ministers = sum(ministers_share, na.rm = TRUE)
  ) %>%
  ungroup()

#Base avec bonnes élections législatives ----
base_complete_legislatives <- base_complete %>%
  filter(isoname%in% pays_gmp_legislatives)

##Pour le moment filtre sur l'année mais on pourra modifier plus tard quand on aura des élections plus récentes ----
base_complete_legislatives_before_2015 <- base_complete_legislatives %>%
  filter(base_complete_legislatives$year <= 2015)


#Liste des pays/années avec données incohérentes ----
View(
  base_complete_legislatives %>%
    ungroup() %>%
    filter(election_couverture_ministers > 1) %>%
    distinct(year,isoname,election_couverture_ministers)
)

##Liste des pays/années où tous les ministres ne sont pas couverts ----
View(
  base_complete_legislatives %>%
    ungroup() %>%
    filter(election_couverture_ministers < 1) %>%
    distinct(year,isoname,election_couverture_seats,election_couverture_ministers)
)




#DINC ----
base_vote_parlement_legislatives_dinc <- read.csv("data/intermediary/parliament/elections and parliament dataset with dinc.csv", sep = ",")

##calcul bonne date pour le join ----
library(lubridate)
library(stringr)
base_vote_parlement_legislatives_dinc2 <- base_vote_parlement_legislatives_dinc %>%
  group_by(isoname, year) %>%
  mutate(
    election_date = na_if(election_date, ""),
    election_date = first(na.omit(election_date))
  ) %>%
  ungroup() %>%
  mutate(
    election_date_date = as.Date(str_replace_all(election_date, "\\.", "-")),
    join_year = case_when(
      is.na(election_date_date) ~ year,
      month(election_date_date) < 7 ~ year(election_date_date),
      TRUE ~ year(election_date_date) + 1
    )
  )

###join whogov avec ma base élections/législatives ----
base_vote_parlement_legislatives_dinc2 <- base_vote_parlement_legislatives_dinc2 %>%
  mutate(
    join_year = as.integer(join_year)
  )

whogov_parties <- whogov_parties %>%
  mutate(
    year = as.integer(year)
  )

tmp2 <- whogov_parties %>%
  left_join(
    base_vote_parlement_legislatives_dinc2,
    by = c("isoname", "partyfacts_id"),
    relationship = "many-to-many"
  )

base_complete2 <- tmp2 %>%
  mutate(
    join_year = as.integer(join_year),
    .year = as.integer(.data$year.x)
  ) %>%
  filter(join_year <= .year | is.na(join_year)) %>%
  group_by(isoname, .year, decile,partyfacts_id) %>%
  slice_max(join_year, n = 1, with_ties = FALSE) %>%
  ungroup()


###mise au propre de la base ----
base_complete2 <- base_complete2 %>%
  rename(year = year.x)

base_complete2 <- base_complete2[!is.na(base_complete2$year),]

base_complete2 <- base_complete2 %>%
  select(isoname, join_year,year, election_date,decile, partyfacts_id, votes, pct_votes,
         votes_valides, taux_participation, seats, seats_total, seats_share, ministers_party, total_ministers, ministers_share, election_couverture_seats
  )

#####verif nombre de ministres couverts par élection ----
base_complete2 <- base_complete2 %>%
  mutate(
    ministers_party = coalesce(ministers_party, 0),
    total_ministers = coalesce(total_ministers, 0),
    ministers_share = coalesce(ministers_share, 0)
  ) %>%
  group_by(isoname, year, decile) %>%
  mutate(
    election_couverture_ministers = sum(ministers_share, na.rm = TRUE)
  ) %>%
  ungroup()



#Base avec bonnes élections législatives ----
base_complete_legislatives_dinc <- base_complete2 %>%
  filter(isoname%in% pays_gmp_legislatives)

##Pour le moment filtre sur l'année mais on pourra modifier plus tard quand on aura des élections plus récentes ----
base_complete_legislatives_before_2015_dinc <- base_complete_legislatives_dinc %>%
  filter(base_complete_legislatives_dinc$year <= 2015)


#Liste des pays/années avec données incohérentes ----
View(
  base_complete_legislatives_dinc2 %>%
    ungroup() %>%
    filter(election_couverture_ministers > 1) %>%
    distinct(year,isoname,election_couverture_ministers)
)

##Liste des pays/années où tous les ministres ne sont pas couverts ----
View(
  base_complete_legislatives_dinc2 %>%
    ungroup() %>%
    filter(election_couverture_ministers < 1) %>%
    distinct(year,isoname,election_couverture_seats,election_couverture_ministers)
)


#Export des bases ----
#Sans dinc
write.csv(
  base_complete_legislatives,
  "data/final/final dataset legislative elections.csv",
  row.names = FALSE
)

write.csv(
  base_complete,
  "data/final/final dataset all countries.csv",
  row.names = FALSE
)
#avec dinc
write.csv(
  base_complete_legislatives_dinc,
  "data/final/final dataset legislative elections dinc method.csv",
  row.names = FALSE
)

write.csv(
  base_complete2,
  "data/final/final dataset all countries dinc method.csv",
  row.names = FALSE
)



