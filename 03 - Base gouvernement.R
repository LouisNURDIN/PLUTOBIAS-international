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
      
      
whogov_parties_bonnes_elections <- whogov_parties_bonnes_elections %>%
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
      partyfacts_id == "2603"&  isoname == "Belarus" ~ "independent",
      partyfacts_id == "2577"&  isoname == "Argentina" ~ "independent",
      partyfacts_id == "2582"&  isoname == "Armenia" ~ "independent",
      partyfacts_id == "2629"&  isoname == "Brazil" ~ "independent",
      partyfacts_id == "2633"&  isoname == "Bulgaria" ~ "independent",
      partyfacts_id == "482"&  isoname == "Bulgaria" & year == 2001 ~ "1183",
      partyfacts_id == "6749"&  isoname == "Bulgaria" & year == 2001 ~ "1183",
      partyfacts_id == "623"&  isoname == "Argentina" & year == 2015 ~ "",
      partyfacts_id == "2639"&  isoname == "Bulgaria" ~ "independent",
      partyfacts_id == "2642"&  isoname == "Colombia" ~ "independent",
      partyfacts_id == "2670"&  isoname == "Egypt" ~ "independent",
      partyfacts_id == "2674"&  isoname == "El Salvador" ~ "independent",
      partyfacts_id == "2567"&  isoname == "Estonia" ~ "independent",
      partyfacts_id == "2691"&  isoname == "Georgia" ~ "independent",
      partyfacts_id == "1731"&  isoname == "Germany" ~ "211",
      partyfacts_id == "1375"&  isoname == "Germany" ~ "211",
      TRUE ~ partyfacts_id
    )
  )



whogov_parties <- whogov_parties %>%
  group_by(isoname, year) %>%
  mutate(
    other_ministers = ministers_share[partyfacts_id == "Other"][1]
  ) %>%
  ungroup()


#DINC ----
Base_vote_parlement_global <- read.csv("data/intermediary/parliament/Elections and parliament global dataset.csv", sep = ",")
##calcul bonne date pour le join ----

#Lister les partis présents dans whogov mais pas la base complète 
unique(Base_vote_parlement_global$vote[Base_vote_parlement_global$isoname == "Georgia" & Base_vote_parlement_global$year ==2014 ] )
unique(Base_vote_parlement_global$year[Base_vote_parlement_global$isoname == "Brazil" ] )
View(whogov_parties_bonnes_elections %>%
       filter(ministers_share >= 0.10) %>%
       filter(year <= 2015) %>%
       distinct(isoname, year, partyfacts_id,ministers_share) %>%
       anti_join(
         Base_vote_parlement_global %>% distinct(isoname, year, partyfacts_id),
         by = c("isoname", "year", "partyfacts_id")
       ))

Base_vote_parlement_global2 <- Base_vote_parlement_global %>%
  group_by( source_recode, isoname, year,source) %>%
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
Base_vote_parlement_global2 <- Base_vote_parlement_global2 %>%
  mutate(
    join_year = as.integer(join_year)
  )

whogov_parties_bonnes_elections <- whogov_parties_bonnes_elections %>%
  mutate(
    year = as.integer(year)
  )


library(dplyr)
library(tidyr)
years <- seq(
  min(Base_vote_parlement_global2$year, na.rm = TRUE),
  max(whogov_parties$year, na.rm = TRUE),
  by = 1
)

base <- Base_vote_parlement_global2 %>%
  distinct(source_recode,isoname,bias,category, partyfacts_id)

party_life <- Base_vote_parlement_global2 %>%
  group_by(source_recode,isoname, partyfacts_id) %>%
  summarise(
    first_year = min(year, na.rm = TRUE),
    last_year  = max(year, na.rm = TRUE),
    .groups = "drop"
  )

grid <- tidyr::expand_grid(
  Base_vote_parlement_global2 %>%
    distinct(source_recode,isoname,bias,category, partyfacts_id),
  year = years
) %>%
  left_join(party_life, by = c("source_recode","isoname", "partyfacts_id")) %>%
  filter(year >= first_year & year <= last_year)

Base_complete <- grid %>%
  left_join(
    Base_vote_parlement_global2,
    by = c("source_recode","isoname", "year", "bias", "category","partyfacts_id")
  ) %>%
  arrange(source_recode,isoname,year,bias, category, partyfacts_id) %>%
  group_by(source_recode,isoname,bias,category, partyfacts_id) %>%
  fill(
    survey, votes, pct_votes, nbr_obs, votes_valides,
    taux_participation,
    seats, seats_total, seats_share, election_couverture_seats,source_recode,
    .direction = "down"
  ) %>%
  ungroup()

Base_complete <- Base_complete %>%
  group_by(source_recode, isoname, year) %>%
  fill(election_date_date, .direction = "downup") %>%
  ungroup()


Base_complete <- Base_complete %>%
  arrange(source_recode, isoname, year) %>%
  group_by(source_recode, isoname) %>%
  fill(election_date_date, .direction = "down") %>%
  ungroup()

View(
  Base_complete %>%
    count(source_recode, isoname,election_date_date, year,bias))


Base_complete <- Base_complete %>%
  left_join(
    whogov_parties_bonnes_elections,
    by = c("isoname","year","partyfacts_id"),
    relationship = "many-to-many"
  )
#code original ----

###mise au propre de la base ----
Base_complete <- Base_complete[!is.na(Base_complete$year),]

Base_complete <- Base_complete %>%
  select(source, source_recode,survey,isoname, year, election_date_date,join_year,bias,category, partyfacts_id, votes, pct_votes,
         votes_valides, taux_participation, seats, seats_total, seats_share, ministers_party, total_ministers, ministers_share, election_couverture_seats
  )

#Traiter les cas où les ministres se dupliquent car plusieurs fois le même PF dans une élection ----
Base_complete <- Base_complete %>%
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
Base_complete <- Base_complete %>%
  mutate(
    ministers_party = coalesce(ministers_party, 0),
    total_ministers = coalesce(total_ministers, 0),
    ministers_share = coalesce(ministers_share, 0)
  ) %>%
  group_by(source_recode,isoname, year,bias, category) %>%
  mutate(
    election_couverture_ministers = sum(ministers_share, na.rm = TRUE)
  ) %>%
  ungroup()


#Liste des pays/années avec données incohérentes ----
View(
  Base_complete %>%
    ungroup() %>%
    filter(election_couverture_ministers > 1) %>%
    distinct(year,isoname,election_couverture_ministers,source_recode,bias)
)

View(
  Base_complete %>%
    ungroup() %>%
    filter(election_couverture_seats > 100) %>%
    distinct(year,isoname,election_couverture_seats,source_recode,bias)
)
##Liste des pays/années où tous les ministres ne sont pas couverts ----
View(
  Base_complete %>%
    ungroup() %>%
    filter(election_couverture_ministers < 0.8) %>%
    distinct(year,isoname,election_couverture_seats,election_couverture_ministers,source_recode)
)


#Export des bases ----
write.csv(
  Base_complete,
  "data/final/final dataset all countries and clivages.csv",
  row.names = FALSE
)

