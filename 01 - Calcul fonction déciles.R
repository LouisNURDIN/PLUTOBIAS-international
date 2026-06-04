#Calcul fonction déciles
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
Base_elections_legislatives <- read.csv("data/intermediary/elections/legislative elections dataset.csv", sep = ",")

sum(is.na(Base_elections_legislatives$weight))

Base_elections_legislatives <- Base_elections_legislatives %>%
  filter(Base_elections_legislatives$partyfacts_id != "Other")
##Test pour recalculer les déciles ----
#Fonction pour calculer vote par décile

build_gmp_inc_base_long_2 <- function(df, annee, country){
  
  # sécurité
  year <- unique(df$year)[1]
  survey <- unique(df$survey)[1]
  source <- unique(df$source)[1]
  source_recode <- unique(df$source_recode)[1]
  # 1. distribution revenu (par pays + année)
  
  dist_rev <- df %>%
    count(inc) %>%
    arrange(inc) %>%
    mutate(
      pct = n / sum(n) * 100,
      cum_pct = cumsum(pct),
      cum_prev = lag(cum_pct, default = 0)
    )
  
  deciles <- data.frame(
    decile = 1:10,
    lower = seq(0, 90, by = 10),
    upper = seq(10, 100, by = 10)
  )
  
  weights_matrix <- dist_rev %>%
    crossing(deciles) %>%
    mutate(
      overlap = pmax(0, pmin(cum_pct, upper) - pmax(cum_prev, lower)),
      weight_decile = overlap / (cum_pct - cum_prev)
    ) %>%
    filter(weight_decile > 0)
  
  
  # 2. assignation déciles
  
  df_deciles <- df %>%
    left_join(
      weights_matrix %>% select(inc, decile, weight_decile),
      by = "inc",
      relationship = "many-to-many"
    ) %>%
    filter(!is.na(weight_decile))
  
  
  # 3. participation
  
  nbr_obs <- df_deciles %>%
    group_by(decile) %>%
    summarise(nbr_obs = sum(weight_decile), .groups = "drop")
  
  votes_valides <- df_deciles %>%
    filter(!str_detect(dataset_party_id, "Abstention$")) %>%
    group_by(decile) %>%
    summarise(votes_valides = sum(weight_decile), .groups = "drop")
  
  participation <- nbr_obs %>%
    left_join(votes_valides, by = "decile") %>%
    mutate(taux_participation = votes_valides / nbr_obs * 100)
  
  
  # 4. votes
  
  votes <- df_deciles %>%
    mutate(
      vote = case_when(
        is.na(dataset_party_id) ~ NA_character_,
        str_detect(dataset_party_id, "Abstention$") ~ "Abstention",
        TRUE ~ dataset_party_id
      )
    ) %>%
    group_by(decile, vote, partyfacts_id) %>%
    summarise(votes = sum(weight_decile), .groups = "drop") %>%
    group_by(decile) %>%
    mutate(pct_votes = votes / sum(votes) * 100)
  
  
  # 5. output
  
  votes %>%
    left_join(participation, by = "decile") %>%
    mutate(
      annee = annee,
      year = year,
      isoname = country,
      survey = survey,
      source = source,
      source_recode = source_recode
    ) %>%
    relocate(isoname, annee, year, vote, decile, survey)
}


# PIPELINE MULTI-PAYS


Base_legislatives_deciles <- Base_elections_legislatives %>%
  group_by(isoname, year) %>%
  group_split() %>%
  map_dfr(~ build_gmp_inc_base_long_2(.x, unique(.x$year), unique(.x$isoname)))



#Voir PF manquants dans ma base législatives avec 
View(
  Base_legislatives_deciles %>%
    filter(is.na(partyfacts_id) & pct_votes >= 5 & vote != "")& vote %>%
    distinct(isoname,vote,year, pct_votes)
)

unique(
  Base_legislatives_deciles$vote[
    Base_legislatives_deciles$partyfacts_id %>% is.na() &
      Base_legislatives_deciles$pct_votes >= 5 &
      Base_legislatives_deciles$vote != ""])


#Elections problématiques liste ----
#voir élections  où taux participation = 100
elections_legislatives_problematiques <- Base_legislatives_deciles %>%
  filter(taux_participation == 100) %>%
  ungroup() %>%
  group_by(isoname, year) %>%
  summarise(.groups = "drop")

sum(Base_legislatives_deciles$taux_participation == 100)


#Bases sans élections problématiques ----
Base_legislatives_deciles <- Base_legislatives_deciles %>%
  filter(taux_participation != 100)


#Liste des élections législatives valides
elections_legislatives_valides <- Base_legislatives_deciles %>%
  group_by(isoname,year) %>%
  summarise(.groups = "drop")

#Export de la base avec les déciles
write.csv(
  Base_legislatives_deciles,
  "data/intermediary/elections/legislative elections with decile dataset.csv",
  row.names = FALSE
)

write.csv(
  elections_legislatives_valides,
  "data/intermediary/elections/valid legislative elections.csv",
  row.names = FALSE
)

sum(is.na(Base_elections_legislatives$inc))

#Deuxième méthode de calcul avec dinc ----

meta_info <- Base_elections_legislatives %>%
  distinct(isoname, year, survey, source, source_recode)
build_votes_by_decile <- function(df){
  
  df <- df %>%
    mutate(
      vote = case_when(
        is.na(dataset_party_id) ~ NA_character_,
        str_detect(dataset_party_id, "Abstention$") ~ "Abstention",
        TRUE ~ dataset_party_id
      )
    )
  
  # 1. Votes par décile × parti (comptage simple)
  votes <- df %>%
    group_by(isoname, year,dinc, vote, partyfacts_id) %>%
    summarise(
      votes = n(),
      .groups = "drop"
    ) %>%
    group_by(dinc) %>%
    mutate(
      pct_votes = votes / sum(votes) * 100
    ) %>%
    ungroup()
  
  # 2. Participation par décile
  participation <- df %>%
    group_by(dinc) %>%
    summarise(
      nbr_obs = n(),
      votes_valides = sum(!str_detect(dataset_party_id, "Abstention$"), na.rm = TRUE),
      taux_participation = votes_valides / nbr_obs * 100,
      .groups = "drop"
    )
  
  # 3. Output final
  result <- votes %>%
    left_join(participation, by = "dinc")
  
  return(result)
}

Base_legislatives_deciles2 <- Base_elections_legislatives %>%
  group_by(isoname, year) %>%
  group_split() %>%
  map_dfr(~ build_votes_by_decile(.x)) %>%
  left_join(meta_info, by = c("isoname", "year"))

Base_legislatives_deciles2 <- Base_legislatives_deciles2 %>%
  filter(taux_participation != 100)
Base_legislatives_deciles2 <- Base_legislatives_deciles2 %>%
  rename(decile = dinc)

Base_legislatives_deciles2 <- Base_legislatives_deciles2[!is.na(Base_legislatives_deciles2$decile),]

Base_legislatives_deciles2 <- Base_legislatives_deciles2 %>%
  group_by(isoname, year, decile, partyfacts_id) %>%
  slice(1) %>%
  ungroup()

#Liste des élections législatives valides
elections_legislatives_valides2 <- Base_legislatives_deciles2 %>%
  group_by(isoname,year) %>%
  summarise(.groups = "drop")

write.csv(
  Base_legislatives_deciles2,
  "data/intermediary/elections/legislative elections with dinc dataset.csv",
  row.names = FALSE
)

