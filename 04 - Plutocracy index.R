#Calcul des indices de ploutocratie 
base_complete_legislative <-  read.csv("data/final/final dataset legislative elections.csv", sep = ",")
base_complete_legislative_dinc <-  read.csv("data/final/final dataset legislative elections dinc method.csv", sep = ",")

base_complete_legislative <- base_complete_legislative[!is.na(base_complete_legislative$decile),]
base_complete_legislative_dinc <- base_complete_legislative_dinc[!is.na(base_complete_legislative_dinc$decile),]

#Pour le moment filtre sur l'année 2015 mais on pourraz le modifier quand on aura des données d'enquêtes plus récentes
base_complete_legislative <- base_complete_legislative %>%
  filter(year <= 2015)

base_complete_legislative_dinc <- base_complete_legislative %>%
  filter(year <= 2015)

#Calcul des indices pour la ploutocratie ---- 
library(dplyr)
base_complete_legislative <- base_complete_legislative %>%
  mutate(
    across(c(ministers_share, seats_share),
           ~ as.numeric(gsub(",", ".", .)) * 100),
    
    votes_en_siege = pmin(coalesce(seats_share, 0), pct_votes),
    votes_en_ministres = pmin(coalesce(ministers_share, 0), pct_votes)
  )

# 3. Agrégation PROPRE au niveau décile

deciles_data <- base_complete_legislative %>%
  group_by(isoname, year, decile) %>%
  summarise(
    taux_participation = first(na.omit(taux_participation)),
    total_sieges = sum(votes_en_siege, na.rm = TRUE),
    total_ministres = sum(votes_en_ministres, na.rm = TRUE),
    votes_valides_en_sieges = sum(votes_en_siege[!is.na(votes_en_siege)]),
    .groups = "drop"
  )

# 4. Ratios 1 vs 10
ratios_1_10 <- deciles_data %>%
  group_by(isoname, year) %>%
  summarise(
    
    ratio_participation_1_10 =
      first(taux_participation[decile == 10]) /
      first(taux_participation[decile == 1]),
    
    ratio_sieges_1_10 =
      first(total_sieges[decile == 10]) /
      first(total_sieges[decile == 1]),
    
    ratio_gouvernement_1_10 =
      first(total_ministres[decile == 10]) /
      first(total_ministres[decile == 1]),
    
    ratio_votes_valides_en_sieges_1_10 =
      first(votes_valides_en_sieges[decile == 10]) /
      first(votes_valides_en_sieges[decile == 1]),
    
    ratio_sieges_ministres_1_10 =
      (first(total_ministres[decile == 10]) /
         first(total_sieges[decile == 10])) /
      (first(total_ministres[decile == 1]) /
         first(total_sieges[decile == 1])),
    
    .groups = "drop"
  )


# 5. Ratios 50 / 50

ratios_50 <- deciles_data %>%
  group_by(isoname,year) %>%
  summarise(
    ratio_participation_50_50 =
      sum(taux_participation[decile %in% 6:10]) /
      sum(taux_participation[decile %in% 1:5]),
    
    ratio_sieges_50_50 =
      sum(total_sieges[decile %in% 6:10]) /
      sum(total_sieges[decile %in% 1:5]),
    
    ratio_gouvernement_50_50 =
      sum(total_ministres[decile %in% 6:10]) /
      sum(total_ministres[decile %in% 1:5]),
    
    ratio_votes_valides_en_sieges_50_50 =
      sum(votes_valides_en_sieges[decile %in% 6:10]) /
      sum(votes_valides_en_sieges[decile %in% 1:5]),
    
    ratio_sieges_ministres_50_50 =
      sum((total_ministres / total_sieges)[decile %in% 6:10]) /
      sum((total_ministres / total_sieges)[decile %in% 1:5]),
    
    .groups = "drop"
  )


# 6. Réintégration dans la base principale

base_complete_legislative <- base_complete_legislative %>%
  left_join(ratios_1_10, by = c("isoname", "year")) %>%
   left_join(ratios_50, by = c("isoname", "year"))

base_complete_legislative <- base_complete_legislative %>%
  left_join(
    deciles_data %>%
      select(isoname,year, decile, total_sieges, total_ministres),
    by = c("isoname","year", "decile")
  )

names(base_complete_legislative)
# Base finale
base_complete_legislative <- base_complete_legislative %>%
  mutate(
    verif_ratio_10_10 =
      ratio_participation_1_10 *
      ratio_votes_valides_en_sieges_1_10 *
      ratio_sieges_ministres_1_10,
    
    verif_ratio_50_50 =
      ratio_participation_50_50 *
      ratio_votes_valides_en_sieges_50_50 *
      ratio_sieges_ministres_50_50,
    
  ) %>%
  select(
    isoname,join_year,year,survey,
    election_date_date,decile,
    partyfacts_id,
    pct_votes, taux_participation,
    seats_share, votes_en_siege,
    ministers_share, votes_en_ministres,
    total_sieges, total_ministres,
    ratio_participation_1_10,ratio_votes_valides_en_sieges_1_10,
    ratio_sieges_ministres_1_10,ratio_gouvernement_1_10,
    verif_ratio_10_10, ratio_participation_50_50, ratio_votes_valides_en_sieges_50_50,
    ratio_sieges_ministres_50_50, ratio_gouvernement_50_50, verif_ratio_50_50,election_couverture_seats,
    election_couverture_ministers
  ) 

#base propre
base_complete_legislative_index <- base_complete_legislative %>%
  group_by(isoname, year, decile) %>%
  summarise(
    
    # identifiant survey (supposé constant)
    survey = first(na.omit(survey)),
    election_date_date = first(na.omit(election_date_date)),
    # ratios → on sécurise avec mean (ou first si tu es sûr qu'ils sont constants)
    taux_participation = mean(taux_participation, na.rm = TRUE),
    total_sieges_decile = mean(total_sieges, na.rm = TRUE),
    total_ministres_decile = mean(total_ministres, na.rm = TRUE),
    
    ratio_participation_1_10 = mean(ratio_participation_1_10, na.rm = TRUE),
    ratio_votes_valides_en_sieges_1_10 = mean(ratio_votes_valides_en_sieges_1_10, na.rm = TRUE),
    ratio_sieges_ministres_1_10 = mean(ratio_sieges_ministres_1_10, na.rm = TRUE),
    ratio_gouvernement_1_10 = mean(ratio_gouvernement_1_10, na.rm = TRUE),
    verif_ratio_10_10 = mean(verif_ratio_10_10, na.rm = TRUE),
    
    ratio_participation_50_50 = mean(ratio_participation_50_50, na.rm = TRUE),
    ratio_votes_valides_en_sieges_50_50 = mean(ratio_votes_valides_en_sieges_50_50, na.rm = TRUE),
    ratio_sieges_ministres_50_50 = mean(ratio_sieges_ministres_50_50, na.rm = TRUE),
    ratio_gouvernement_50_50 = mean(ratio_gouvernement_50_50, na.rm = TRUE),
    verif_ratio_50_50 = mean(verif_ratio_50_50, na.rm = TRUE),
    
    # sécurité diagnostic

    
    .groups = "drop"
  )



#DINC ----
library(dplyr)
base_complete_legislative_dinc <- base_complete_legislative_dinc %>%
  mutate(
    across(c(ministers_share, seats_share),
           ~ as.numeric(gsub(",", ".", .)) * 100),
    
    votes_en_siege = pmin(coalesce(seats_share, 0), pct_votes),
    votes_en_ministres = pmin(coalesce(ministers_share, 0), pct_votes)
  )


# 3. Agrégation PROPRE au niveau décile

deciles_data_dinc <- base_complete_legislative_dinc %>%
  group_by(isoname, year, decile) %>%
  summarise(
    taux_participation = first(na.omit(taux_participation)),
    total_sieges = sum(votes_en_siege, na.rm = TRUE),
    total_ministres = sum(votes_en_ministres, na.rm = TRUE),
    votes_valides_en_sieges = sum(votes_en_siege[!is.na(votes_en_siege)]),
    .groups = "drop"
  )

# 4. Ratios 1 vs 10

ratios_1_10_dinc <- deciles_data_dinc %>%
  group_by(isoname, year) %>%
  summarise(
    
    ratio_participation_1_10 =
      first(taux_participation[decile == 10]) /
      first(taux_participation[decile == 1]),
    
    ratio_sieges_1_10 =
      first(total_sieges[decile == 10]) /
      first(total_sieges[decile == 1]),
    
    ratio_gouvernement_1_10 =
      first(total_ministres[decile == 10]) /
      first(total_ministres[decile == 1]),
    
    ratio_votes_valides_en_sieges_1_10 =
      first(votes_valides_en_sieges[decile == 10]) /
      first(votes_valides_en_sieges[decile == 1]),
    
    ratio_sieges_ministres_1_10 =
      (first(total_ministres[decile == 10]) /
         first(total_sieges[decile == 10])) /
      (first(total_ministres[decile == 1]) /
         first(total_sieges[decile == 1])),
    
    .groups = "drop"
  )

# 5. Ratios 50 / 50
ratios_50_dinc <- deciles_data_dinc %>%
  group_by(isoname,year) %>%
  summarise(
    ratio_participation_50_50 =
      sum(taux_participation[decile %in% 6:10]) /
      sum(taux_participation[decile %in% 1:5]),
    
    ratio_sieges_50_50 =
      sum(total_sieges[decile %in% 6:10]) /
      sum(total_sieges[decile %in% 1:5]),
    
    ratio_gouvernement_50_50 =
      sum(total_ministres[decile %in% 6:10]) /
      sum(total_ministres[decile %in% 1:5]),
    
    ratio_votes_valides_en_sieges_50_50 =
      sum(votes_valides_en_sieges[decile %in% 6:10]) /
      sum(votes_valides_en_sieges[decile %in% 1:5]),
    
    ratio_sieges_ministres_50_50 =
      sum((total_ministres / total_sieges)[decile %in% 6:10]) /
      sum((total_ministres / total_sieges)[decile %in% 1:5]),
    
    .groups = "drop"
  )

# 6. Réintégration dans la base principale
base_complete_legislative_dinc <- base_complete_legislative_dinc %>%
  left_join(ratios_1_10_dinc, by = c("isoname", "year")) %>%
  left_join(ratios_50_dinc, by = c("isoname", "year"))

base_complete_legislative_dinc <- base_complete_legislative_dinc %>%
  left_join(
    deciles_data_dinc %>%
      select(isoname,year, decile, total_sieges, total_ministres),
    by = c("isoname","year", "decile")
  )

names(base_complete_legislative)
# Base finale
base_complete_legislative_dinc_index <- base_complete_legislative_dinc %>%
  mutate(
    verif_ratio_10_10 =
      ratio_participation_1_10 *
      ratio_votes_valides_en_sieges_1_10 *
      ratio_sieges_ministres_1_10,
    
    verif_ratio_50_50 =
      ratio_participation_50_50 *
      ratio_votes_valides_en_sieges_50_50 *
      ratio_sieges_ministres_50_50,
    
  ) %>%
  select(
    isoname,join_year,year,
    election_date,decile,
    partyfacts_id,
    pct_votes, taux_participation,
    seats_share, votes_en_siege,
    ministers_share, votes_en_ministres,
    total_sieges, total_ministres,
    ratio_participation_1_10,ratio_votes_valides_en_sieges_1_10,
    ratio_sieges_ministres_1_10,ratio_gouvernement_1_10,
    verif_ratio_10_10, ratio_participation_50_50, ratio_votes_valides_en_sieges_50_50,
    ratio_sieges_ministres_50_50, ratio_gouvernement_50_50, verif_ratio_50_50,election_couverture_seats,
    election_couverture_ministers
  ) 

#base propre
base_complete_legislative_dinc_index <- base_complete_legislative_dinc_index %>%
  group_by(isoname,join_year, year,election_date, decile) %>%
  summarise(
    
    # identifiant survey (supposé constant)

    
    # ratios → on sécurise avec mean (ou first si tu es sûr qu'ils sont constants)
    taux_participation = mean(taux_participation, na.rm = TRUE),
    total_sieges_decile = mean(total_sieges, na.rm = TRUE),
    total_ministres_decile = mean(total_ministres, na.rm = TRUE),
    
    ratio_participation_1_10 = mean(ratio_participation_1_10, na.rm = TRUE),
    ratio_votes_valides_en_sieges_1_10 = mean(ratio_votes_valides_en_sieges_1_10, na.rm = TRUE),
    ratio_sieges_ministres_1_10 = mean(ratio_sieges_ministres_1_10, na.rm = TRUE),
    ratio_gouvernement_1_10 = mean(ratio_gouvernement_1_10, na.rm = TRUE),
    verif_ratio_10_10 = mean(verif_ratio_10_10, na.rm = TRUE),
    
    ratio_participation_50_50 = mean(ratio_participation_50_50, na.rm = TRUE),
    ratio_votes_valides_en_sieges_50_50 = mean(ratio_votes_valides_en_sieges_50_50, na.rm = TRUE),
    ratio_sieges_ministres_50_50 = mean(ratio_sieges_ministres_50_50, na.rm = TRUE),
    ratio_gouvernement_50_50 = mean(ratio_gouvernement_50_50, na.rm = TRUE),
    verif_ratio_50_50 = mean(verif_ratio_50_50, na.rm = TRUE),
    
    # sécurité diagnostic
    
    
    .groups = "drop"
  )

