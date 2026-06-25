library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(lubridate)
#Base avec données gouvernements
whogov <- read.csv("data/raw/whogov/WhoGov_within_V3.1.csv", sep = ";")
elections_legislatives_valides <- read.csv("data/intermediary/elections/valid elections.csv", sep = ",")
elections_legislatives_valides2 <- read.csv("data/intermediary/elections/valid legislative elections.csv", sep = ",")
pays_gmp_legislatives <- unique(elections_legislatives_valides2$isoname)
annees_gmp_legislatives <- unique(elections_legislatives_valides2$year)

#Calcul nombre de ministres par année
whogov <- whogov %>%
  rename(isoname = country_name)

whogov <- whogov %>%
  filter(core >= 1)
unique(whogov$core)

whogov_parties <- whogov %>%
  group_by(isoname, year, partyfacts_id) %>%
  summarise(
    ministers_party = n(),
    women_party = sum(gender == "Female", na.rm = TRUE),
    
    prime_minister = first(name[position == "Prime Min."]),
    .groups = "drop_last"
  ) %>%
  mutate(
    total_ministers = sum(ministers_party),
    total_women = sum(women_party),
    ministers_share = ministers_party / total_ministers,
    women_share_party = women_party / ministers_party,
    women_share_government = total_women / total_ministers
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
    elections_legislatives_valides2 %>%
      distinct(isoname, year),
    by = c("isoname", "year")
  )


#Correctifs de mes partis dans whogov pouyr faire les bons joins ----

whogov_parties_bonnes_elections <- whogov_parties_bonnes_elections %>%
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
      partyfacts_id == "2719"&  isoname == "Hungary" ~ "Other",
      partyfacts_id == "2722"&  isoname == "Iceland" & year ==2009 ~ "Other",
      partyfacts_id == "2726"&  isoname == "India" & year == 1967 ~ "Other",
      partyfacts_id == "1207"&  isoname == "India" & year == 1996 ~ "Other",   #attention pour le cas de l'INde
      partyfacts_id == "2491"&  isoname == "India" & year == 1996 ~ "Other", #attention pour le cas de l'INde
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
      partyfacts_id == "2603"&  isoname == "Belarus" ~ "Other",
      partyfacts_id == "2577"&  isoname == "Argentina" ~ "Other",
      partyfacts_id == "2582"&  isoname == "Armenia" ~ "Other",
      partyfacts_id == "2629"&  isoname == "Brazil" ~ "Other",
      partyfacts_id == "2633"&  isoname == "Bulgaria" ~ "Other",
      partyfacts_id == "482"&  isoname == "Bulgaria" & year == 2001 ~ "1183",
      partyfacts_id == "6749"&  isoname == "Bulgaria" & year == 2001 ~ "1183",
      partyfacts_id == "623"&  isoname == "Argentina" & year == 2015 ~ "",
      partyfacts_id == "2639"&  isoname == "Bulgaria" ~ "Other",
      partyfacts_id == "2642"&  isoname == "Colombia" ~ "Other",
      partyfacts_id == "2670"&  isoname == "Egypt" ~ "Other",
      partyfacts_id == "2674"&  isoname == "El Salvador" ~ "Other",
      partyfacts_id == "2567"&  isoname == "Estonia" ~ "Other",
      partyfacts_id == "2691"&  isoname == "Georgia" ~ "Other",
      partyfacts_id == "1731"&  isoname == "Germany" ~ "211",
      partyfacts_id == "1375"&  isoname == "Germany" ~ "211",
      partyfacts_id == "2007"&  isoname == "Armenia" & year == 2012  ~ "Other",
      partyfacts_id == "599"&  isoname == "Austria" & year == 2006 ~ "Other",
      partyfacts_id == "2598"&  isoname == "Bangladesh" ~ "Other",
      partyfacts_id == "1680"&  isoname == "Belgium" & year == 2007 ~ "756",
      partyfacts_id == "2639"&  isoname == "Chile" ~ "Other",
      partyfacts_id == "2650"&  isoname == "Croatia" ~ "Other",
      partyfacts_id == "2652"&  isoname == "Cyprus" ~ "Other",
      partyfacts_id == "623"&  isoname == "Argentina" & year >= 2006 ~ "2530",
      partyfacts_id == "757"&  isoname == "Bulgaria" ~ "1665",
      partyfacts_id == "757"&  isoname == "Bulgaria" ~ "1665",
      partyfacts_id == "54"&  isoname == "Chile" & year >= 2005 & year <= 2009 ~ "4550",
      partyfacts_id == "390"&  isoname == "Chile" & year >= 2005 & year <= 2009 ~ "4550", #Attention aux lignes qui se dupliquent
      partyfacts_id == "1507"&  isoname == "Denmark" & year >= 1990 & year <= 2001 ~ "1204",
      partyfacts_id == "1507"&  isoname == "Denmark" & year ==2015 ~ "1204",
      partyfacts_id == "536"&  isoname == "Denmark" & year ==2011 ~ "1204", #Il s'agit d'une alliance entre le 536 et le 1204 pour cette année là
      partyfacts_id == "2708"&  isoname == "Greece" ~ "Other",
      partyfacts_id == "2560"&  isoname == "Indonesia" & year ==2004 ~ "Other", #vérifier si le parti est présent dans WPID
      partyfacts_id == "3593"&  isoname == "Jordan" ~ "Other",
      partyfacts_id == "3605"&  isoname == "Libya" ~ "Other",
      partyfacts_id == "671"&  isoname == "Latvia" & year ==1998 ~ "1704",
      partyfacts_id == "1531"&  isoname == "Latvia" & year == 2010 ~ "852",
      partyfacts_id == "7619"&  isoname == "Latvia" & year == 2010 ~ "1704",
      partyfacts_id == "193"&  isoname == "Lithuania" & year == 2000 ~ "1357",
      partyfacts_id == "2778"&  isoname == "Lithuania" ~ "Other",
      partyfacts_id == "2794"&  isoname == "Mali" ~ "Other",
      partyfacts_id == "2806"&  isoname == "Mexico" ~ "Other",
      partyfacts_id == "3185"&  isoname == "Montenegro" & year >= 2001 & year <= 2006 ~ "3162", #Attention aux lignes qui se dupliquent il s'agit d'une alliance
      partyfacts_id == "3162"&  isoname == "Montenegro" & year == 2012  ~ "4767",  #Ãttention lignes qui se dupliquent
      partyfacts_id == "3185"&  isoname == "Montenegro" & year == 2012  ~ "4767",  #Attention lignes qui se dupliquent
      partyfacts_id == "2825"&  isoname == "Morocco" ~ "Other",
      
      TRUE ~ partyfacts_id
    )
  )

unique(Base_vote_parlement_global$partyfacts_id[Base_vote_parlement_global$isoname == "Netherlands" & 
                                                  Base_vote_parlement_global$year == 2010] )


whogov_parties_bonnes_elections <- whogov_parties_bonnes_elections %>%
  group_by(isoname, year) %>%
  mutate(
    other_ministers = ministers_share[partyfacts_id == "Other"][1]
  ) %>%
  ungroup()


#DINC ----
Base_vote_parlement_global <- read.csv("data/intermediary/parliament/Elections and parliament global dataset.csv", sep = ",")

unique(Base_vote_parlement_global$party[Base_vote_parlement_global$isoname == "Denmark" & Base_vote_parlement_global$year == 2011 ] )
unique(whogov$party[whogov$isoname == "Bangladesh" & whogov$year ==2007 ] )
##calcul bonne date pour le join ----
View(Base_vote_parlement_global %>%
  count(
    source,
    source_recode,
    isoname,
    year,
    bias,
    category,
    partyfacts_id,
    name = "n"
  ) %>%
  filter(n > 1) %>%
  arrange(desc(n)))


#Lister les partis présents dans whogov mais pas la base complète 
missing_parties <- whogov_parties_bonnes_elections %>%
  filter(ministers_share >= 0.20,
         year <= 2015) %>%
  distinct(isoname, year, partyfacts_id, ministers_share) %>%
  anti_join(
    Base_vote_parlement_global %>%
      distinct(isoname, year, partyfacts_id),
    by = c("isoname", "year", "partyfacts_id"))

View(missing_parties %>%
    left_join(
      Base_vote_parlement_global %>%
        distinct(isoname, year, source_recode, source),
      by = c("isoname", "year")))


View(
  Base_complete %>%
    count(source,source_recode, isoname,election_date_date, survey_year,year,bias))


Base_complete <- Base_vote_parlement_global %>%
  full_join(whogov_parties,
    by = c("isoname","year","partyfacts_id"),
    relationship = "many-to-many")


#code original ----

###mise au propre de la base ----
Base_complete <- Base_complete[!is.na(Base_complete$year),]

Base_complete <- Base_complete %>%
  select(isoname,year, survey_year, source, source_recode,survey, election_date,bias,category, partyfacts_id, votes, pct_votes,
         votes_valides, taux_participation, seats, seats_total, seats_share, ministers_party, total_ministers, ministers_share,women_party,women_share_party,Percentage.of.women.diputees,women_share_government, election_couverture_seats
  )
Base_complete <- Base_complete %>%
  arrange(isoname, year)

Base_complete <- Base_complete %>%
  distinct(source,source_recode,isoname,survey_year,year,partyfacts_id,bias,
           category,.keep_all = TRUE)


cols_check <- c(
  "election_date", "bias", "category",
  "votes", "pct_votes", "votes_valides",
  "taux_participation", "seats_share",
  "seats_total", "election_couverture_seats"
)

# 1. Années valides (IMPORTANT: pas de source ici)
valid_years <- Base_complete %>%
  group_by(isoname, year) %>%
  summarise(
    ok = !all(is.na(across(all_of(cols_check)))),
    .groups = "drop"
  ) %>%
  filter(ok)

# 2. Dernière année valide par pays
last_year <- valid_years %>%
  group_by(isoname) %>%
  summarise(last_year = max(year), .groups = "drop")

# 3. Template = dernière élection complète (toutes sources conservées)
templates <- Base_complete %>%
  inner_join(last_year, by = "isoname") %>%
  filter(year == last_year)

# 4. Années manquantes
missing_years <- Base_complete %>%
  distinct(isoname, year) %>%
  anti_join(valid_years, by = c("isoname", "year"))

# 5. Réplication propre des sources
to_add <- missing_years %>%
  inner_join(templates, by = "isoname") %>%
  mutate(year = year.x) %>%
  select(-year.x)

# 6. Dataset final
Base_complete_filled <- bind_rows(Base_complete, to_add)


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
  group_by(source,source_recode,isoname,survey_year, year,bias, category) %>%
  mutate(
    election_couverture_ministers = sum(ministers_share, na.rm = TRUE)
  ) %>%
  ungroup()

Base_complete <- Base_complete %>%
  group_by(source,source_recode,isoname,survey_year,year,bias) %>%
  mutate(
    other_ministers = ministers_share[partyfacts_id == "Other"][1]
  ) %>%
  ungroup()
#Liste des pays/années avec données incohérentes ----
View(
  Base_complete %>%
    ungroup() %>%
    filter(election_couverture_ministers > 1) %>%
    distinct(survey_year,year,isoname,election_couverture_ministers,source,source_recode,bias)
)

View(
  Base_complete %>%
    ungroup() %>%
    filter(election_couverture_seats > 100) %>%
    distinct(survey_year,year,isoname,election_couverture_seats,source,source_recode,bias))


View(
  Base_complete %>%
    ungroup() %>%
    filter(election_couverture_seats > 100) %>%
    distinct(survey_year,year,isoname,election_couverture_seats,source,source_recode,bias))
##Liste des pays/années où tous les ministres ne sont pas couverts ----

View(
  Base_complete %>%
    ungroup() %>%
    filter(election_couverture_ministers < 0.8) %>%
    distinct(survey_year,year,isoname,bias,election_couverture_seats,election_couverture_ministers,other_ministers,source,source_recode)
)




#Export des bases ----
write.csv(
  Base_complete,
  "data/final/final dataset all countries and clivages.csv",
  row.names = FALSE
)



