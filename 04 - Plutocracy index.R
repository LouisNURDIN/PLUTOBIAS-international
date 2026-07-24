#Calcul des indices de ploutocratie 
library(dplyr)
library(stringr)
Base_complete <-  read.csv("data/final/final dataset all countries and clivages.csv", sep = ",")
unique(Base_complete$bias[Base_complete$source_recode == "WPID"])

table(Base_complete$source_recode)
pays_regimes_presidentiels <- read.csv("data/raw/Liste régimes présidentiels.csv", sep = ",")
pays_regimes_presidentiels <- unique(pays_regimes_presidentiels$isoname)

Base_complete <- Base_complete[!is.na(Base_complete$category),]
Base_complete <- Base_complete[!is.na(Base_complete$partyfacts_id),]

Base_complete<- Base_complete %>% filter(Base_complete$partyfacts_id != "Other")
Base_complete <- Base_complete %>%filter(Base_complete$year >= 1966)
Base_complete <- Base_complete %>%filter(Base_complete$election_couverture_seats <= 100)
Base_complete <- Base_complete %>%filter(Base_complete$election_couverture_ministers <= 100)


unique(Base_complete$category)

#Category 1 = Top 10 VS bot 10
Base_complete$category_recode1 <- NA 
Base_complete <- Base_complete %>%
  mutate(
    category_recode1 = case_when(
      category == "bot-income-10" ~ "bot",
      category == "top-income-10" ~ "top",
      category == "men" ~ "top",
      category == "women" ~ "bot",
      category == "top-educ-10" ~ "top",
      category == "bot-educ-10" ~ "bot",
      category == "top-age-10" ~ "top",
      category == "bot-age-10" ~ "bot",
      TRUE ~ NA_character_
    )
  )
unique(Base_complete$category_recode1)

#Category 2 = Top 50 VS bot 50
Base_complete$category_recode2 <- NA 
Base_complete <- Base_complete %>%
  mutate(
    category_recode2 = case_when(
      category == "bot-income-50" ~ "bot",
      category == "top-income-50" ~ "top",
      category == "men" ~ "top",
      category == "women" ~ "bot",
      category == "top-educ-50" ~ "top",
      category == "bot-educ-50" ~ "bot",
      category == "top-age-50" ~ "top",
      category == "bot-age-50" ~ "bot",
      TRUE ~ NA_character_
    )
  )
unique(Base_complete$category_recode1)
unique(Base_complete$category_recode2)

unique(Base_complete$category_recode2[Base_complete$category == "top-income-10"])



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

categories_index_10 <- Base_complete %>%
  filter(
    category %in% c("men", "women") |
      str_detect(category, "-10$")
  ) %>%
  group_by(
    source,
    source_recode,
    isoname,
    year,
    type,
    bias,
    category,
    category_recode1
  ) %>%
  summarise(
    taux_participation = first(na.omit(taux_participation)),
    total_sieges = sum(votes_en_siege, na.rm = TRUE),
    total_ministres = sum(votes_en_ministres, na.rm = TRUE),
    votes_valides_en_sieges =
      total_sieges / taux_participation * 100,
    votes_valides_en_ministres =
      total_ministres / taux_participation * 100,
    .groups = "drop")


# 4. Ratios 1 vs 10

first_index <- categories_index_10 %>%
  group_by(source,source_recode,isoname,type, year,bias) %>%
  summarise(
    
    ratio_participation_top_bot =
      first(taux_participation[category_recode1 == "top"]) /
      first(taux_participation[category_recode1 == "bot"]),
    
    ratio_sieges_top_bot = case_when(
      !str_detect(first(type), "Presidential") ~
        first(total_sieges[category_recode1 == "top"]) /
        first(total_sieges[category_recode1 == "bot"]),
      TRUE ~ NA_real_
    ),
    
    ratio_gouvernement_top_bot =
      first(total_ministres[category_recode1 == "top"]) /
      first(total_ministres[category_recode1 == "bot"]),
    
    ratio_votes_valides_en_sieges_top_bot = case_when(
      !str_detect(first(type), "Presidential") ~
        first(votes_valides_en_sieges[category_recode1 == "top"]) /
        first(votes_valides_en_sieges[category_recode1 == "bot"]),
      TRUE ~ NA_real_
    ),
    
    ratio_votes_valides_en_ministres_top_bot = case_when(
      str_detect(first(type), "Presidential") | first(type) == "General election" ~
        first(votes_valides_en_ministres[category_recode1 == "top"]) /
        first(votes_valides_en_ministres[category_recode1 == "bot"]),
      TRUE ~ NA_real_
    ),
    
    ratio_sieges_ministres_top_bot = case_when(
      !str_detect(first(type), "Presidential") ~
        (first(total_ministres[category_recode1 == "top"]) /
           first(total_sieges[category_recode1 == "top"])) /
        (first(total_ministres[category_recode1 == "bot"]) /
           first(total_sieges[category_recode1 == "bot"])),
      TRUE ~ NA_real_
    ),
    
    .groups = "drop"
  )


# Agrégation PROPRE au niveau décile
categories_index_50 <- Base_complete %>%
  filter(
    category %in% c("men", "women") |
      str_detect(category, "-50$")
  ) %>%
  group_by(
    source,
    source_recode,
    isoname,
    year,
    type,
    bias,
    category,
    category_recode2
  ) %>%
  summarise(
    taux_participation = first(na.omit(taux_participation)),
    total_sieges = sum(votes_en_siege, na.rm = TRUE),
    total_ministres = sum(votes_en_ministres, na.rm = TRUE),
    votes_valides_en_sieges =
      total_sieges / taux_participation * 100,
    votes_valides_en_ministres =
      total_ministres / taux_participation * 100,
    .groups = "drop"
  )

# 5. Ratios 50 / 50
second_index <- categories_index_50 %>%
  group_by(source,source_recode,isoname,type,year,bias) %>%
  summarise(
    
    ratio_participation_top_bot2 =
      first(taux_participation[category_recode2 == "top"]) /
      first(taux_participation[category_recode2 == "bot"]),

    ratio_sieges_top_bot2 = case_when(
      !str_detect(first(type), "Presidential") ~
        first(total_sieges[category_recode2 == "top"]) /
        first(total_sieges[category_recode2 == "bot"]),
      TRUE ~ NA_real_
    ),
    
    
    ratio_gouvernement_top_bot2 =
      first(total_ministres[category_recode2 == "top"]) /
      first(total_ministres[category_recode2 == "bot"]),
    
    ratio_votes_valides_en_sieges_top_bot2 = case_when(
      !str_detect(first(type), "Presidential") ~
        first(votes_valides_en_sieges[category_recode2 == "top"]) /
        first(votes_valides_en_sieges[category_recode2 == "bot"]),
      TRUE ~ NA_real_
    ),
    
    ratio_votes_valides_en_ministres_top_bot2 = case_when(
      str_detect(first(type), "Presidential") | first(type) == "General election" ~
        first(votes_valides_en_ministres[category_recode2 == "top"]) /
        first(votes_valides_en_ministres[category_recode2 == "bot"]),
      TRUE ~ NA_real_
    ),
    
    ratio_sieges_ministres_top_bot2 = case_when(
      !str_detect(first(type), "Presidential") ~
        (first(total_ministres[category_recode2 == "top"]) /
           first(total_sieges[category_recode2 == "top"])) /
        (first(total_ministres[category_recode2 == "bot"]) /
           first(total_sieges[category_recode2 == "bot"])),
      TRUE ~ NA_real_
    ),
    
    .groups = "drop"
  )

categories_index <- bind_rows(categories_index_10,categories_index_50)
# 6. Réintégration dans la base principale
Base_complete <- Base_complete %>%
  left_join(
    categories_index %>%
      select(source,source_recode,isoname,year,type,bias, category,category_recode1,category_recode2,total_sieges, total_ministres),
    by = c("source","source_recode","isoname","year","type", "bias","category","category_recode1","category_recode2")
  )

Base_complete <- Base_complete %>%
  left_join(first_index, by = c("source","source_recode","isoname", "year","type","bias")) %>%
  left_join(second_index, by = c("source","source_recode","isoname","year","type","bias"))

unique(Base_complete$bias[Base_complete$source_recode == "WPID"])

unique(Base_complete$type)
# Base finale
Base_complete_clean <- Base_complete %>%
  mutate(
    verif_ratio_top_bot = case_when(
      !str_detect(type, "Presidential") ~
        ratio_participation_top_bot *
        ratio_votes_valides_en_sieges_top_bot *
        ratio_sieges_ministres_top_bot,
      TRUE ~ NA_real_
    ),
    
    verif_ratio_top_bot2 = case_when(
      !str_detect(type, "Presidential") ~
        ratio_participation_top_bot2 *
        ratio_votes_valides_en_sieges_top_bot2 *
        ratio_sieges_ministres_top_bot2,
      TRUE ~ NA_real_
    ),
    
    verif_ratio_parlement_top_bot = case_when(
      !str_detect(type, "Presidential") ~
        ratio_participation_top_bot *
        ratio_votes_valides_en_sieges_top_bot,
      TRUE ~ NA_real_
    ),
    
    verif_ratio_parlement_top_bot2 = case_when(
      !str_detect(type, "Presidential") ~
        ratio_participation_top_bot2 *
        ratio_votes_valides_en_sieges_top_bot2,
      TRUE ~ NA_real_
    ),
    
    
    verif_ratio_presidentiel_top_bot = case_when(
      str_detect(type, "Presidential")  ~
        ratio_participation_top_bot *
        ratio_votes_valides_en_ministres_top_bot,
      TRUE ~ NA_real_
    ),
    
    verif_ratio_presidentiel_top_bot2 = case_when(
      str_detect(type, "Presidential")   ~
        ratio_participation_top_bot2 *
        ratio_votes_valides_en_ministres_top_bot2,
      TRUE ~ NA_real_
    )
  ) %>%
  select(
    source,source_recode,survey, isoname,year,election_year,
    election_date,type,bias,category,
    partyfacts_id,
    pct_votes, taux_participation,
    seats_share, votes_en_siege,
    ministers_share, votes_en_ministres,
    total_sieges, total_ministres,
    ratio_participation_top_bot,ratio_votes_valides_en_sieges_top_bot,
    ratio_sieges_ministres_top_bot,ratio_votes_valides_en_ministres_top_bot,ratio_gouvernement_top_bot,
    verif_ratio_top_bot,verif_ratio_presidentiel_top_bot,ratio_sieges_top_bot,verif_ratio_parlement_top_bot, ratio_participation_top_bot2, ratio_votes_valides_en_sieges_top_bot2,
    ratio_sieges_ministres_top_bot2,ratio_votes_valides_en_ministres_top_bot2, ratio_gouvernement_top_bot2,verif_ratio_top_bot2,verif_ratio_presidentiel_top_bot2,
    ratio_sieges_top_bot2,verif_ratio_parlement_top_bot2,election_couverture_seats,
    election_couverture_ministers,Percentage.of.women.diputees,women_share_party,women_share_government
  ) 







#Isoler les régimes présidentiels dans une autre base ----
Base_regimes_presidentiels <- Base_complete_clean %>%
  filter(isoname %in% pays_regimes_presidentiels)


#Garder les pays qui ne sont pas des régimes présidentiels ----
Base_complete_clean <- Base_complete_clean %>% filter(!isoname %in% pays_regimes_presidentiels)


#base propre = une ligne par source/pays/année ----
Base_complete_legislative_index <- Base_complete_clean  %>%
  group_by(source,source_recode,isoname,survey,year,election_date,bias) %>%
  summarise(ratio_participation_top_bot = mean(ratio_participation_top_bot, na.rm = TRUE),
    ratio_votes_valides_en_sieges_top_bot = mean(ratio_votes_valides_en_sieges_top_bot, na.rm = TRUE),
    ratio_sieges_ministres_top_bot = mean(ratio_sieges_ministres_top_bot, na.rm = TRUE),
    ratio_gouvernement_top_bot = mean(ratio_gouvernement_top_bot, na.rm = TRUE),
    verif_ratio_top_bot = mean(verif_ratio_top_bot, na.rm = TRUE),
    ratio_sieges_top_bot = mean(ratio_sieges_top_bot, na.rm = TRUE),
    verif_ratio_parlement_top_bot = mean(verif_ratio_parlement_top_bot, na.rm = TRUE),
    
    ratio_participation_top_bot2 = mean(ratio_participation_top_bot2, na.rm = TRUE),
    ratio_votes_valides_en_sieges_top_bot2 = mean(ratio_votes_valides_en_sieges_top_bot2, na.rm = TRUE),
    ratio_sieges_ministres_top_bot2 = mean(ratio_sieges_ministres_top_bot2, na.rm = TRUE),
    ratio_gouvernement_top_bot2 = mean(ratio_gouvernement_top_bot2, na.rm = TRUE),
    verif_ratio_top_bot2 = mean(verif_ratio_top_bot2, na.rm = TRUE),
    ratio_sieges_top_bot2 = mean(ratio_sieges_top_bot2, na.rm = TRUE),
    verif_ratio_parlement_top_bot2 = mean(verif_ratio_parlement_top_bot2, na.rm = TRUE),
    election_couverture_seats = mean(election_couverture_seats, na.rm = TRUE),
    election_couverture_ministers = mean(election_couverture_ministers, na.rm = TRUE),
    Percentage.of.women.diputees = mean(Percentage.of.women.diputees, na.rm = TRUE),
    women_share_government = mean(women_share_government, na.rm = TRUE),
    # sécurité diagnostic
    .groups = "drop")

Base_complete_legislative_index <- Base_complete_legislative_index %>%
  arrange(isoname, year)

cor(
  Base_complete_legislative_index$ratio_sieges_top_bot2,
  Base_complete_legislative_index$verif_ratio_parlement_top_bot2,
  use = "complete.obs")

unique(Base_complete_legislative_index$year[Base_complete_legislative_index$source_recode == "ESS" & Base_complete_legislative_index$isoname == "France"])
#Lister pays/années où mes indices se dupliquent (on est censé en avoir 4)
View(
Base_complete_legislative_index %>%
  count(source,source_recode, isoname,year)%>%
  filter(n > 4))

#L'objectif est d'arriver à une table où on a 4 lignes par 
Base_complete_test_index <- Base_complete_legislative_index %>%
  group_by(source,source_recode,isoname,year,bias) %>%
  slice(1) %>%
  ungroup()  #


##Liste des pays/années où tous les ministres ne sont pas couverts ----
View(
  Base_complete_legislative_index %>%
    ungroup() %>%
    filter(election_couverture_ministers < 0.80) %>%
    distinct(source,source_recode,isoname,year,election_couverture_seats,election_couverture_ministers)
)

#Export des bases ----
write.csv(
  Base_complete_legislative_index,
  "data/final/legislative dataset complete with index.csv",
  row.names = FALSE
)


#Même chose avec les régimes présidentiels ----
Base_regimes_presidentiels_index <- Base_regimes_presidentiels  %>%
  group_by(source,source_recode,isoname,survey,year,election_date,bias) %>%
  summarise(ratio_participation_top_bot = mean(ratio_participation_top_bot, na.rm = TRUE),
            ratio_votes_valides_en_ministres_top_bot = mean(ratio_votes_valides_en_ministres_top_bot, na.rm = TRUE),
            ratio_gouvernement_top_bot = mean(ratio_gouvernement_top_bot, na.rm = TRUE),
            verif_ratio_presidentiel_top_bot = mean(verif_ratio_presidentiel_top_bot, na.rm = TRUE),
            
            ratio_participation_top_bot2 = mean(ratio_participation_top_bot2, na.rm = TRUE),
            ratio_votes_valides_en_ministres_top_bot2 = mean(ratio_votes_valides_en_ministres_top_bot2, na.rm = TRUE),
            ratio_gouvernement_top_bot2 = mean(ratio_gouvernement_top_bot2, na.rm = TRUE),
            verif_ratio_presidentiel_top_bot2 = mean(verif_ratio_presidentiel_top_bot2, na.rm = TRUE),
            election_couverture_seats = mean(election_couverture_seats, na.rm = TRUE),
            election_couverture_ministers = mean(election_couverture_ministers, na.rm = TRUE),
            Percentage.of.women.diputees = mean(Percentage.of.women.diputees, na.rm = TRUE),
            women_share_government = mean(women_share_government, na.rm = TRUE),
            .groups = "drop")

Base_regimes_presidentiels_index <- Base_regimes_presidentiels_index %>%
  arrange(isoname, year)

#Lister pays/années où mes indices se dupliquent (on est censé en avoir 4)
View(
  Base_regimes_presidentiels_index %>%
    count(source,source_recode, isoname,year)%>%
    filter(n > 4))

#L'objectif est d'arriver à une table où on a 4 lignes par 
Base_regimes_presidentiels_index_test <- Base_regimes_presidentiels_index %>%
  group_by(source,source_recode,isoname, year,bias) %>%
  slice(1) %>%
  ungroup()  #


##Export des bases ----
write.csv(
  Base_regimes_presidentiels_index,
  "data/final/dataset complete regimes presidentiels.csv",
  row.names = FALSE
)


#Commande pour vérifier que nos indices 10 / 10 et 50 / 50 soient corrélées au sein des biais
#(on peut adapter le code en fonction du biais que l'on veut vérifier)
#=> pour androcracy cela doit être égal à 1
Base_complete_legislative_index %>%
  filter(
    bias == "gerontocracy",
    is.finite(ratio_gouvernement_top_bot),
    is.finite(ratio_gouvernement_top_bot2)
  ) %>%
  summarise(
    cor = cor(ratio_gouvernement_top_bot, ratio_gouvernement_top_bot2)
  )

#Commande pour vérifier que nos indices finaux soient corrects (on peut adapter le code en fonction du code que l'on veut vérifier)
# Les corrélations doivent être égales à 1 à chaque fois
Base_complete_legislative_index %>%
  filter(
    is.finite(verif_ratio_top_bot),
    is.finite(ratio_gouvernement_top_bot)
  ) %>%
  summarise(
    cor = cor(
      verif_ratio_top_bot,
      ratio_gouvernement_top_bot
    )
  )







