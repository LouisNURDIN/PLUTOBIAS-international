library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(lubridate)
#Base avec données gouvernements
whogov <- read.csv("data/raw/whogov/WhoGov_within_V3.1.csv", sep = ";")
elections_legislatives_valides <- read.csv("data/intermediary/elections/valid elections.csv", sep = ",")
pays_gmp_legislatives <- unique(elections_legislatives_valides$isoname)
annees_gmp_legislatives <- unique(elections_legislatives_valides$year)

#Calcul nombre de ministres par année
whogov <- whogov %>%
  rename(isoname = country_name)

whogov_parties <- whogov %>%
  group_by(isoname, year, partyfacts_id) %>%
  summarise(
    ministers_party = n(),
    prime_minister = first(name[position == "Prime Min."]),
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

#voir les partis présents dans ma base whogov mais pas vote-parlement
whogov_parties_bonnes_elections <- whogov_parties %>%
  semi_join(
    elections_legislatives_valides %>%
      distinct(isoname, year),
    by = c("isoname", "year")
  )


#Correctifs de mes partis dans whogov pouyr faire les bons joins ----

whogov_parties <- whogov_parties %>%
  mutate(
    isoname = case_when(
      isoname == "Czechia" ~ "Czech Republic",
      TRUE ~ isoname
    )
  )
      
      
whogov_parties <- whogov_parties %>%
  mutate(
    partyfacts_id = case_when(
      partyfacts_id == "480" & isoname == "Belgium" & year > 1977  ~ "500",
      partyfacts_id == "554" & isoname == "Belgium" & year >= 2003  ~ "789",
      partyfacts_id == "1680"&  isoname == "Belgium" & year == 2003  ~ "1586",
      partyfacts_id == "1586"&  isoname == "Belgium" & year == 2007   ~ "1680",
      partyfacts_id == "604"&  isoname == "Belgium" & year == 2010   ~ "622",
      partyfacts_id == "604"&  isoname == "Belgium" & year == 2014   ~ "622",
      partyfacts_id == "2685"&  isoname == "Finland" ~ "Other",
      partyfacts_id == "5514"&  isoname == "France" ~ "1083",
      partyfacts_id == "1246"&  isoname == "France" ~ "1083",
      partyfacts_id == "2688"&  isoname == "France" & year == 1973   ~ "Other",
      partyfacts_id == "8041"&  isoname == "France" ~ "1083",
      partyfacts_id == "2688"&  isoname == "France" & year == 1978   ~ "Other",
      partyfacts_id == "2719"&  isoname == "Hungary" & year == 1998   ~ "Other",
      partyfacts_id == "2719"&  isoname == "Hungary" & year == 2002   ~ "Other",
      partyfacts_id == "2722"&  isoname == "Iceland" & year ==2009 ~ "Other",
      partyfacts_id == "2726"&  isoname == "India" & year == 1967 ~ "Other",
      partyfacts_id == "1207"&  isoname == "India" & year == 1996 ~ "6321",   #attention pour le cas de l'INde
      partyfacts_id == "2491"&  isoname == "India" & year == 1996 ~ "6321", #attention pour le cas de l'INde
      partyfacts_id == "2731"&  isoname == "Indonesia"  ~ "Other",
      partyfacts_id == "3433"&  isoname == "Iraq" ~ "Other",
      partyfacts_id == "5619"&  isoname == "Iraq" & year == 2010 ~ "5897",
      partyfacts_id == "5616"&  isoname == "Iraq" & year == 2004 ~ "5917",
      partyfacts_id == "5616"&  isoname == "Iraq" & year == 2005 ~ "5917",
      partyfacts_id == "2735"&  isoname == "Ireland" & year == 1992 ~ "Other",
      partyfacts_id == "2741"&  isoname == "Italy" ~ "Other",
      partyfacts_id == "813"&  isoname == "Italy" & year == 2001 ~ "6241",
      partyfacts_id == "1626"&  isoname == "Italy" & year ==  2001 ~ "6241",
      partyfacts_id == "279"&  isoname == "Italy" & year == 2006 ~ "1372",
      partyfacts_id == "878"&  isoname == "Italy" & year == 2006 ~ "1372",
      partyfacts_id == "813"&  isoname == "Italy" & year == 2008 ~ "6303",
      partyfacts_id == "1626"&  isoname == "Italy" & year == 2008 ~ "6303",
      partyfacts_id == "365"&  isoname == "Italy" & year ==  2013 ~ "1626",
      partyfacts_id == "6303"&  isoname == "Italy" & year ==  2013 ~ "1626",
      partyfacts_id == "2484"&  isoname == "Malaysia" ~ "3637",
      partyfacts_id == "2318"&  isoname == "Malaysia" ~ "3637",
      partyfacts_id == "2789"&  isoname == "Malaysia" ~ "Other",
      partyfacts_id == "5599"&  isoname == "Malaysia" & year == 2013 ~ "3637",
      partyfacts_id == "921"&  isoname == "Netherlands" & year == 1971 ~ "45",
      partyfacts_id == "921"&  isoname == "Netherlands" & year == 1972 ~ "45",
      partyfacts_id == "163"&  isoname == "Netherlands" & year == 1977 ~ "1157",
      partyfacts_id == "1390"&  isoname == "Netherlands" & year == 1977 ~ "1157",
      partyfacts_id == "1390"&  isoname == "Netherlands" & year ==  1981 ~ "1157",
      partyfacts_id == "2854"&  isoname == "Nigeria" ~ "Other",
      partyfacts_id == "2888"&  isoname == "Poland" ~ "Other",
      partyfacts_id == "727"&  isoname == "Poland" & year ==  2007 ~ "Other",  #ou sinon on peut le mettre avec PSL mais ce n'est pas exactement pareil
      partyfacts_id == "2891"&  isoname == "Portugal" ~ "Other",
      partyfacts_id == "1308"&  isoname == "Portugal" & year ==  2015 ~ "1359",
      partyfacts_id == "2907"&  isoname == "Senegal" ~ "Other",
      partyfacts_id == "2757"&  isoname == "South Korea" ~ "Other",
      partyfacts_id == "2927"&  isoname == "Spain" ~ "Other",
      partyfacts_id == "2934"&  isoname == "Sweden" ~ "Other",
      partyfacts_id == "1231"&  isoname == "Switzerland" ~ "360",
      partyfacts_id == "2941"&  isoname == "Taiwan" ~ "Other",
      partyfacts_id == "2956"&  isoname == "Turkey" ~ "Other",
      partyfacts_id == "1388"&  isoname == "United Kingdom" & year == 2010 ~ "540",
      partyfacts_id == "5766"&  isoname == "Senegal" & year == 2012 ~ "4010",
      TRUE ~ partyfacts_id
    )
  )

View(whogov_parties %>%
       filter(ministers_share >= 0.10) %>%
       filter(year <= 2015) %>%
       distinct(isoname, year, partyfacts_id,ministers_share) %>%
       anti_join(
         base_vote_parlement_legislatives %>% distinct(isoname, year, partyfacts_id),
         by = c("isoname", "year", "partyfacts_id")
       ))

whogov_parties <- whogov_parties %>%
  group_by(isoname, year) %>%
  mutate(
    other_ministers = ministers_share[partyfacts_id == "Other"][1]
  ) %>%
  ungroup()


#DINC ----
base_vote_parlement_legislatives_dinc <- read.csv("data/intermediary/parliament/elections and parliament dataset with dinc.csv", sep = ",")
##calcul bonne date pour le join ----

base_vote_parlement_legislatives_dinc2 <- base_vote_parlement_legislatives_dinc %>%
  group_by(isoname, year,source, source_recode) %>%
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


library(dplyr)
library(tidyr)
years <- seq(
  min(base_vote_parlement_legislatives_dinc2$year, na.rm = TRUE),
  max(whogov_parties$year, na.rm = TRUE),
  by = 1
)

base <- base_vote_parlement_legislatives_dinc2 %>%
  distinct(isoname, decile, partyfacts_id,source, source_recode)

party_life <- base_vote_parlement_legislatives_dinc2 %>%
  group_by(isoname, partyfacts_id,source, source_recode) %>%
  summarise(
    first_year = min(year, na.rm = TRUE),
    last_year  = max(year, na.rm = TRUE),
    .groups = "drop"
  )

grid <- tidyr::expand_grid(
  base_vote_parlement_legislatives_dinc2 %>%
    distinct(isoname, decile, partyfacts_id,source, source_recode),
  year = years
) %>%
  left_join(party_life, by = c("isoname", "partyfacts_id","source", "source_recode")) %>%
  filter(year >= first_year & year <= last_year)

base_complete_dinc <- grid %>%
  left_join(
    base_vote_parlement_legislatives_dinc2,
    by = c("isoname", "year", "decile", "partyfacts_id", "source", "source_recode")
  ) %>%
  arrange(isoname,year, decile, partyfacts_id) %>%
  group_by(isoname, decile, partyfacts_id,source, source_recode) %>%
  fill(
    survey, votes, pct_votes, nbr_obs, votes_valides,
    taux_participation, election_date_date,
    seats, seats_total, seats_share, election_couverture_seats,source, source_recode,
    .direction = "down"
  ) %>%
  ungroup()

base_complete_dinc <- base_complete_dinc %>%
  left_join(
    whogov_parties,
    by = c("isoname","year","partyfacts_id"),
    relationship = "many-to-many"
  )
#code original ----

###mise au propre de la base ----
base_complete_dinc <- base_complete_dinc[!is.na(base_complete_dinc$year),]

base_complete_dinc <- base_complete_dinc %>%
  select(isoname, year, election_date_date,join_year, survey, source, source_recode,decile, partyfacts_id, votes, pct_votes,
         votes_valides, taux_participation, seats, seats_total, seats_share, ministers_party, total_ministers, ministers_share, election_couverture_seats
  )

#Traiter les cas où les ministres se dupliquent car plusieurs fois le même PF dans une élection ----
base_complete_dinc <- base_complete_dinc %>%
  mutate(
    ministers_share = case_when(
      partyfacts_id == "1083" & isoname == "France" & year == 1967  ~ 0.25,
      partyfacts_id == "1083" & isoname == "France" & year == 1973  ~ 0.224101475,
      partyfacts_id == "6241" & isoname == "Italy" & year == 2001  ~ 0.05859375,
      partyfacts_id == "1372" & isoname == "Italy" & year == 2006  ~ 0.0392857138,
      
      TRUE ~ ministers_share
    )
  )
#####verif nombre de ministres couverts par élection ----
base_complete_dinc <- base_complete_dinc %>%
  mutate(
    ministers_party = coalesce(ministers_party, 0),
    total_ministers = coalesce(total_ministers, 0),
    ministers_share = coalesce(ministers_share, 0)
  ) %>%
  group_by(isoname, year, decile,source,source_recode) %>%
  mutate(
    election_couverture_ministers = sum(ministers_share, na.rm = TRUE)
  ) %>%
  ungroup()

unique(base_complete_dinc$source_recode)







#Liste des pays/années avec données incohérentes ----
View(
  base_complete_dinc %>%
    ungroup() %>%
    filter(election_couverture_ministers > 1) %>%
    distinct(year,isoname,election_couverture_ministers,source_recode)
)

View(
  base_complete_dinc %>%
    ungroup() %>%
    filter(election_couverture_seats > 100) %>%
    distinct(year,isoname,election_couverture_seats,source_recode)
)
##Liste des pays/années où tous les ministres ne sont pas couverts ----
View(
  base_complete_dinc %>%
    ungroup() %>%
    filter(election_couverture_ministers < 100) %>%
    distinct(year,isoname,election_couverture_seats,election_couverture_ministers,source_recode)
)


#Export des bases ----
write.csv(
  base_complete_dinc,
  "data/final/final dataset all countries dinc method.csv",
  row.names = FALSE
)

