#Calcul des indices de ploutocratie 
library(dplyr)
Base_complete <-  read.csv("data/final/final dataset all countries and clivages.csv", sep = ",")


Base_complete <- Base_complete[!is.na(Base_complete$category),]
Base_complete <- Base_complete[!is.na(Base_complete$partyfacts_id),]
Base_complete<- Base_complete[!is.na(Base_complete$election_date_date),]
Base_complete<- Base_complete %>%
  filter(Base_complete$partyfacts_id != "Other")
Base_complete <- Base_complete %>%
  filter(Base_complete$year >= 1966)

Base_complete$category_recode1 <- NA 
Base_complete <- Base_complete %>%
  mutate(
    category_recode1 = case_when(
      category == "inc-1" ~ "bot",
      category == "inc-10" ~ "top",
      category == "men" ~ "top",
      category == "women" ~ "bot",
      category == "top-educ" ~ "top",
      category == "bot-educ" ~ "bot",
      category == "top-age" ~ "top",
      category == "bot-age" ~ "bot",
      TRUE ~ NA_character_
    )
  )
unique(Base_complete$category_recode1)

Base_complete$category_recode2 <- NA 
Base_complete <- Base_complete %>%
  mutate(
    category_recode2 = case_when(
      category_recode1 == "bot" ~ "bot",
      category_recode1 == "top" ~ "top",
      category == "inc-2" ~ "bot",
      category == "inc-3" ~ "bot",
      category == "inc-4" ~ "bot",
      category == "inc-5" ~ "bot",
      category == "inc-6" ~ "top",
      category == "inc-7" ~ "top",
      category == "inc-8" ~ "top",
      category == "inc-9" ~ "top",
      TRUE ~ NA_character_
    )
  )
unique(Base_complete$category_recode2)

#DINC ----
library(dplyr)
Base_complete <- Base_complete %>%
  mutate(
    ministers_share = as.numeric(gsub(",", ".", ministers_share)) * 100,
    seats_share = as.numeric(gsub(",", ".", seats_share)),
    
    votes_en_siege = pmin(coalesce(seats_share, 0), pct_votes),
    votes_en_ministres = pmin(coalesce(ministers_share, 0), pct_votes)
  )


# 3. Agrégation PROPRE au niveau décile

categories_index <- Base_complete %>%
  group_by(source,source_recode, isoname,survey_year, year, bias, category,category_recode1,category_recode2) %>%
  summarise(
    taux_participation = first(na.omit(taux_participation)),
    total_sieges = sum(votes_en_siege, na.rm = TRUE),
    total_ministres = sum(votes_en_ministres, na.rm = TRUE),
    votes_valides_en_sieges =
      total_sieges / taux_participation * 100,
    .groups = "drop"
  )

# 4. Ratios 1 vs 10

first_index <- categories_index %>%
  group_by(source,source_recode,isoname,survey_year, year,bias) %>%
  summarise(
    
    ratio_participation_top_bot =
      first(taux_participation[category_recode1 == "top"]) /
      first(taux_participation[category_recode1 == "bot"]),
    
    ratio_sieges_top_bot =
      first(total_sieges[category_recode1 == "top"]) /
      first(total_sieges[category_recode1 == "bot"]),
    
    ratio_gouvernement_top_bot =
      first(total_ministres[category_recode1 == "top"]) /
      first(total_ministres[category_recode1 == "bot"]),
    
    ratio_votes_valides_en_sieges_top_bot =
      first(votes_valides_en_sieges[category_recode1 == "top"]) /
      first(votes_valides_en_sieges[category_recode1 == "bot"]),
    
    ratio_sieges_ministres_top_bot =
      (first(total_ministres[category_recode1 == "top"]) /
         first(total_sieges[category_recode1 == "top"])) /
      (first(total_ministres[category_recode1 == "bot"]) /
         first(total_sieges[category_recode1 == "bot"])),
    
    .groups = "drop"
  )

# 5. Ratios 50 / 50
second_index <- categories_index %>%
  group_by(source,source_recode,isoname,survey_year, year,bias) %>%
  summarise(
    
    ratio_participation_top_bot2 =
      first(taux_participation[category_recode2 == "top"]) /
      first(taux_participation[category_recode2 == "bot"]),
    
    ratio_sieges_top_bot2 =
      first(total_sieges[category_recode2 == "top"]) /
      first(total_sieges[category_recode2 == "bot"]),
    
    ratio_gouvernement_top_bot2 =
      first(total_ministres[category_recode2 == "top"]) /
      first(total_ministres[category_recode2 == "bot"]),
    
    ratio_votes_valides_en_sieges_top_bot2 =
      first(votes_valides_en_sieges[category_recode2 == "top"]) /
      first(votes_valides_en_sieges[category_recode2 == "bot"]),
    
    ratio_sieges_ministres_top_bot2 =
      (first(total_ministres[category_recode2 == "top"]) /
         first(total_sieges[category_recode2 == "top"])) /
      (first(total_ministres[category_recode2 == "bot"]) /
         first(total_sieges[category_recode2 == "bot"])),
    
    .groups = "drop"
  )

# 6. Réintégration dans la base principale
Base_complete <- Base_complete %>%
  left_join(
    categories_index %>%
      select(source,source_recode,isoname,survey_year,year,bias, category,category_recode1,category_recode2,total_sieges, total_ministres),
    by = c("source","source_recode","isoname","survey_year","year", "bias","category","category_recode1","category_recode2")
  )

Base_complete <- Base_complete %>%
  left_join(first_index, by = c("source","source_recode","isoname", "survey_year","year","bias")) %>%
  left_join(second_index, by = c("source","source_recode","isoname","survey_year", "year","bias"))



# Base finale
Base_complete_clean <- Base_complete %>%
  mutate(
    verif_ratio_top_bot =
      ratio_participation_top_bot *
      ratio_votes_valides_en_sieges_top_bot *
      ratio_sieges_ministres_top_bot,
    
    verif_ratio_top_bot2 =
      ratio_participation_top_bot2 *
      ratio_votes_valides_en_sieges_top_bot2 *
      ratio_sieges_ministres_top_bot2,
    
  ) %>%
  select(
    source,source_recode,survey, isoname,survey_year,year,
    election_date_date,bias,category,
    partyfacts_id,
    pct_votes, taux_participation,
    seats_share, votes_en_siege,
    ministers_share, votes_en_ministres,
    total_sieges, total_ministres,
    ratio_participation_top_bot,ratio_votes_valides_en_sieges_top_bot,
    ratio_sieges_ministres_top_bot,ratio_gouvernement_top_bot,
    verif_ratio_top_bot, ratio_participation_top_bot2, ratio_votes_valides_en_sieges_top_bot2,
    ratio_sieges_ministres_top_bot2, ratio_gouvernement_top_bot2, verif_ratio_top_bot2,election_couverture_seats,
    election_couverture_ministers, 
  ) 

cor(Base_complete_clean$ratio_gouvernement_top_bot, Base_complete_clean$verif_ratio_top_bot, 
    use = "complete.obs")

cor(Base_complete_clean$ratio_gouvernement_top_bot2, Base_complete_clean$verif_ratio_top_bot2, 
    use = "complete.obs")


#base propre
Base_complete_index <- Base_complete_clean  %>%
  group_by(source,source_recode,isoname,survey_year,year,election_date_date,bias) %>%
  summarise(
    
    # identifiant survey (supposé constant)
    
    ratio_participation_top_bot = mean(ratio_participation_top_bot, na.rm = TRUE),
    ratio_votes_valides_en_sieges_top_bot = mean(ratio_votes_valides_en_sieges_top_bot, na.rm = TRUE),
    ratio_sieges_ministres_top_bot = mean(ratio_sieges_ministres_top_bot, na.rm = TRUE),
    ratio_gouvernement_top_bot = mean(ratio_gouvernement_top_bot, na.rm = TRUE),
    verif_ratio_top_bot = mean(verif_ratio_top_bot, na.rm = TRUE),
    
    ratio_participation_top_bot2 = mean(ratio_participation_top_bot2, na.rm = TRUE),
    ratio_votes_valides_en_sieges_top_bot2 = mean(ratio_votes_valides_en_sieges_top_bot2, na.rm = TRUE),
    ratio_sieges_ministres_top_bot2 = mean(ratio_sieges_ministres_top_bot2, na.rm = TRUE),
    ratio_gouvernement_top_bot2 = mean(ratio_gouvernement_top_bot2, na.rm = TRUE),
    verif_ratio_top_bot2 = mean(verif_ratio_top_bot2, na.rm = TRUE),
    election_couverture_seats = mean(election_couverture_seats, na.rm = TRUE),
    election_couverture_ministers = mean(election_couverture_ministers, na.rm = TRUE),
    # sécurité diagnostic
    
    
    .groups = "drop"
  )

#Lister pays/années où mes indices se dupliquent (on est censé en avoir 4)
View(
Base_complete_index %>%
  count(source,source_recode, isoname, survey_year,year)%>%
  filter(n > 4))

#L'objectif est d'arriver à une table où on a 4 lignes par 
Base_complete_test_index <- Base_complete_index %>%
  group_by(source,source_recode,isoname,survey_year, year,bias) %>%
  slice(1) %>%
  ungroup()  #on a bien atteint l'objectif avec une ligne = un pays, une enquête, une élection, un biais, et tous les ratios correspondants


##Liste des pays/années où tous les ministres ne sont pas couverts ----
View(
  Base_complete_index %>%
    ungroup() %>%
    filter(election_couverture_ministers < 80) %>%
    distinct(source_recode,isoname,year,election_couverture_seats,election_couverture_ministers,source_recode)
)

#Export des bases ----
write.csv(
  Base_complete_index,
  "data/final/dataset complete with index.csv",
  row.names = FALSE
)

#Quelques tests de vérification


library(ggplot2)
library(ggrepel)

ggplot(Base_complete_index,
       aes(x = ratio_gouvernement_top_bot,
           y = ratio_gouvernement_top_bot2,
           color = bias,
           label = paste(isoname, year))) +
  geom_point(alpha = 0.7) +
  geom_text_repel(size = 3, max.overlaps = 50) +
  theme_minimal() +
  labs(
    x = "Ratio gouvernement 1 vs 10",
    y = "Ratio gouvernement 50 vs 50",
    title = "Comparaison des ratios gouvernementaux"
  )


#Regarder dans quels pays/années se situent les indices les plus élevés
Base_complete_index %>%
  filter(bias == "plutocracy") %>%
  filter(ratio_gouvernement_top_bot == max(ratio_gouvernement_top_bot, na.rm = TRUE)) %>%
  select(source_recode, isoname, year, ratio_gouvernement_top_bot)

Base_complete_index %>%
  filter(bias == "androcracy") %>%
  filter(ratio_gouvernement_top_bot == max(ratio_gouvernement_top_bot, na.rm = TRUE)) %>%
  select(source_recode, isoname, year, ratio_gouvernement_top_bot)

Base_complete_index %>%
  filter(bias == "epistocracy") %>%
  filter(ratio_gouvernement_top_bot == max(ratio_gouvernement_top_bot, na.rm = TRUE)) %>%
  select(source_recode, isoname, year, ratio_gouvernement_top_bot)

Base_complete_index %>%
  filter(bias == "gerontocracy") %>%
  filter(ratio_gouvernement_top_bot == max(ratio_gouvernement_top_bot, na.rm = TRUE)) %>%
  select(source_recode, isoname, year, ratio_gouvernement_top_bot)
