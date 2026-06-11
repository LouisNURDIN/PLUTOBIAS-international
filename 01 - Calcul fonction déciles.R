#Calcul fonction déciles
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
Base_elections_legislatives <- read.csv("data/intermediary/elections/legislative elections dataset.csv", sep = ",")
unique(Base_elections_legislatives$gender)
unique(Base_elections_legislatives$isoname[Base_elections_legislatives$source == "CSES"])
sum(is.na(Base_elections_legislatives$weight))

Base_elections_legislatives <- Base_elections_legislatives %>%
  filter(Base_elections_legislatives$partyfacts_id != "Other")

#Calcul fonction déciles WPID ----

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
  rename(category = dinc)

Base_legislatives_deciles2 <- Base_legislatives_deciles2[!is.na(Base_legislatives_deciles2$category),]

Base_legislatives_deciles2 <- Base_legislatives_deciles2 %>%
  group_by(isoname, year, category, partyfacts_id) %>%
  slice(1) %>%
  ungroup()

Base_legislatives_deciles2 <- Base_legislatives_deciles2 %>%
  mutate(bias = "plutocracy")

##wpid gender ----
Base_elections_legislatives_sexe <- Base_elections_legislatives[!is.na(Base_elections_legislatives$gender),]
###Calcul fonction sexe wpid ----
build_votes_by_gender <- function(df){
  
  df <- df %>%
    mutate(
      vote = case_when(
        is.na(dataset_party_id) ~ NA_character_,
        str_detect(dataset_party_id, "Abstention$") ~ "Abstention",
        TRUE ~ as.character(dataset_party_id)
      )
    )
  
  # Votes par sexe × parti
  votes <- df %>%
    group_by(isoname, year, gender, vote, partyfacts_id) %>%
    summarise(
      votes = n(),
      .groups = "drop"
    ) %>%
    group_by(isoname, year, gender) %>%
    mutate(
      pct_votes = votes / sum(votes) * 100
    ) %>%
    ungroup()
  
  # Participation par sexe
  participation <- df %>%
    group_by(isoname, year, gender) %>%
    summarise(
      nbr_obs = n(),
      votes_valides = sum(
        !str_detect(dataset_party_id, "Abstention$"),
        na.rm = TRUE
      ),
      taux_participation = votes_valides / nbr_obs * 100,
      .groups = "drop"
    )
  
  votes %>%
    left_join(
      participation,
      by = c("isoname", "year", "gender")
    )
}
meta_info <- Base_elections_legislatives %>%
  distinct(isoname, year, survey, source, source_recode)

Base_legislatives_gender <- Base_elections_legislatives_sexe %>%
  group_by(isoname, year) %>%
  group_split() %>%
  map_dfr(~ build_votes_by_gender(.x)) %>%
  left_join(meta_info, by = c("isoname", "year"))

Base_legislatives_gender <- Base_legislatives_gender %>%
  mutate(bias = "phallocracy")
Base_legislatives_gender <- Base_legislatives_gender %>%
  rename(category = gender)

#Calcul fonction educ wpid ----
unique(Base_elections_legislatives$educ)
Base_elections_legislatives_educ <- Base_elections_legislatives[!is.na(Base_elections_legislatives$educ),]

build_votes_by_educ_fractional <- function(df){
  
  # 1. distribution + weights (comme ta fonction revenu)
  dist_educ <- df %>%
    count(educ) %>%
    arrange(educ) %>%
    mutate(
      pct = n / sum(n) * 100,
      cum_pct = cumsum(pct),
      cum_prev = lag(cum_pct, default = 0)
    )
  
  educ_groups <- data.frame(
    educ_group = c(1, 2),
    lower = c(0, 50),
    upper = c(50, 100)
  )
  
  weights_matrix <- dist_educ %>%
    crossing(educ_groups) %>%
    mutate(
      overlap = pmax(0, pmin(cum_pct, upper) - pmax(cum_prev, lower)),
      weight = overlap / (cum_pct - cum_prev)
    ) %>%
    filter(weight > 0)
  
  # 2. expansion micro → groupes pondérés
  df_groups <- df %>%
    left_join(
      weights_matrix %>%
        dplyr::select(educ, educ_group, weight),
      by = "educ",
      relationship = "many-to-many"
    ) %>%
    filter(!is.na(weight))
  
  # 3. vote variable (comme ta fonction standard)
  df_groups <- df_groups %>%
    mutate(
      vote = case_when(
        is.na(dataset_party_id) ~ NA_character_,
        str_detect(dataset_party_id, "Abstention$") ~ "Abstention",
        TRUE ~ dataset_party_id
      )
    )
  
  # 4. votes
  votes <- df_groups %>%
    group_by(isoname, year, educ_group, vote, partyfacts_id) %>%
    summarise(
      votes = sum(weight),
      .groups = "drop"
    ) %>%
    group_by(educ_group) %>%
    mutate(
      pct_votes = votes / sum(votes) * 100
    ) %>%
    ungroup()
  
  # 5. participation
  participation <- df_groups %>%
    group_by(educ_group) %>%
    summarise(
      nbr_obs = sum(weight),
      votes_valides = sum(
        weight * (!str_detect(dataset_party_id, "Abstention$")),
        na.rm = TRUE
      ),
      taux_participation = votes_valides / nbr_obs * 100,
      .groups = "drop"
    )
  
  # 6. output final (même structure que deciles / sexe)
  votes %>%
    left_join(participation, by = "educ_group")
}

Base_legislatives_educ <- Base_elections_legislatives %>%
  group_by(isoname, year) %>%
  group_split() %>%
  map_dfr(~ build_votes_by_educ_fractional(.x)) %>%
  left_join(meta_info, by = c("isoname", "year"))

Base_legislatives_educ <- Base_legislatives_educ %>%
  mutate(bias = "epistocracy")
Base_legislatives_educ <- Base_legislatives_educ %>%
  rename(category = educ_group)
Base_legislatives_educ <- Base_legislatives_educ %>%
  mutate(
    category = case_when(
      category == "1" ~ "bot-educ", 
      category== "2" ~ "top-educ",
      TRUE ~ as.character(category)
    )
  )
#Calcul fonction age ----
unique(Base_elections_legislatives$age)


#Autres bases ----
#CSES ----
cses_data_clean <- read.csv("data/intermediary/elections/cses elections dataset.csv")
cses_data_clean_income <- cses_data_clean %>%
  filter(inc <= 5)

#calcul décile cses ----
build_cses_base_long <- function(df, annee, country){
  
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


cses_dataset_income <- cses_data_clean_income %>%
  group_by(isoname, year) %>%
  group_split() %>%
  map_dfr(~ build_cses_base_long(.x, unique(.x$year), unique(.x$isoname)))
cses_dataset_income <- cses_dataset_income %>%
  mutate(bias = "plutocracy")
cses_dataset_income <- cses_dataset_income %>%
  rename(category = decile)


##CSES gender ----
cses_data_clean_sexe <- cses_data_clean %>%
  filter(gender %in% c("men", "women"))

###Calcul CSES gender ----
build_votes_by_gender <- function(df){
  
  df <- df %>%
    mutate(
      vote = case_when(
        is.na(dataset_party_id) ~ NA_character_,
        str_detect(dataset_party_id, "Abstention$") ~ "Abstention",
        TRUE ~ as.character(dataset_party_id)
      )
    )
  
  # Votes par sexe × parti
  votes <- df %>%
    group_by(isoname, year, gender, vote, partyfacts_id) %>%
    summarise(
      votes = n(),
      .groups = "drop"
    ) %>%
    group_by(isoname, year, gender) %>%
    mutate(
      pct_votes = votes / sum(votes) * 100
    ) %>%
    ungroup()
  
  # Participation par sexe
  participation <- df %>%
    group_by(isoname, year, gender) %>%
    summarise(
      nbr_obs = n(),
      votes_valides = sum(
        !str_detect(dataset_party_id, "Abstention$"),
        na.rm = TRUE
      ),
      taux_participation = votes_valides / nbr_obs * 100,
      .groups = "drop"
    )
  
  votes %>%
    left_join(
      participation,
      by = c("isoname", "year", "gender")
    )
}
meta_info_cses <- cses_data_clean %>%
  distinct(isoname, year, survey, source, source_recode)

cses_dataset_gender <- cses_data_clean_sexe %>%
  group_by(isoname, year) %>%
  group_split() %>%
  map_dfr(~ build_votes_by_gender(.x)) %>%
  left_join(meta_info_cses, by = c("isoname", "year"))

cses_dataset_gender <- cses_dataset_gender %>%
  mutate(bias = "phallocracy")
cses_dataset_gender <- cses_dataset_gender %>%
  rename(category = gender)

##CSES educ ----
cses_data_clean_educ <- cses_data_clean %>%
  filter(educ < 6)

build_votes_by_educ_fractional <- function(df){
  
  # 1. distribution + weights (comme ta fonction revenu)
  dist_educ <- df %>%
    count(educ) %>%
    arrange(educ) %>%
    mutate(
      pct = n / sum(n) * 100,
      cum_pct = cumsum(pct),
      cum_prev = lag(cum_pct, default = 0)
    )
  
  educ_groups <- data.frame(
    educ_group = c(1, 2),
    lower = c(0, 50),
    upper = c(50, 100)
  )
  
  weights_matrix <- dist_educ %>%
    crossing(educ_groups) %>%
    mutate(
      overlap = pmax(0, pmin(cum_pct, upper) - pmax(cum_prev, lower)),
      weight = overlap / (cum_pct - cum_prev)
    ) %>%
    filter(weight > 0)
  
  # 2. expansion micro → groupes pondérés
  df_groups <- df %>%
    left_join(
      weights_matrix %>%
        dplyr::select(educ, educ_group, weight),
      by = "educ",
      relationship = "many-to-many"
    ) %>%
    filter(!is.na(weight))
  
  # 3. vote variable (comme ta fonction standard)
  df_groups <- df_groups %>%
    mutate(
      vote = case_when(
        is.na(dataset_party_id) ~ NA_character_,
        str_detect(dataset_party_id, "Abstention$") ~ "Abstention",
        TRUE ~ dataset_party_id
      )
    )
  
  # 4. votes
  votes <- df_groups %>%
    group_by(isoname, year, educ_group, vote, partyfacts_id) %>%
    summarise(
      votes = sum(weight),
      .groups = "drop"
    ) %>%
    group_by(educ_group) %>%
    mutate(
      pct_votes = votes / sum(votes) * 100
    ) %>%
    ungroup()
  
  # 5. participation
  participation <- df_groups %>%
    group_by(educ_group) %>%
    summarise(
      nbr_obs = sum(weight),
      votes_valides = sum(
        weight * (!str_detect(dataset_party_id, "Abstention$")),
        na.rm = TRUE
      ),
      taux_participation = votes_valides / nbr_obs * 100,
      .groups = "drop"
    )
  
  # 6. output final (même structure que deciles / sexe)
  votes %>%
    left_join(participation, by = "educ_group")
}

cses_dataset_educ <- cses_data_clean_educ %>%
  group_by(isoname, year) %>%
  group_split() %>%
  map_dfr(~ build_votes_by_educ_fractional(.x)) %>%
  left_join(meta_info_cses, by = c("isoname", "year"))

cses_dataset_educ <- cses_dataset_educ %>%
  mutate(bias = "epistocracy")
cses_dataset_educ <- cses_dataset_educ %>%
  rename(category = educ_group)
cses_dataset_educ <- cses_dataset_educ %>%
  mutate(
    category = case_when(
      category == "1" ~ "bot-educ", 
      category== "2" ~ "top-educ",
      TRUE ~ as.character(category)
    )
  )
##CSES age ----
cses_data_clean_age <- cses_data_clean %>%
  filter(age < 9997)

#Espace pour empiler les bases votes entre elles ----
Base_legislatives_deciles2 <- Base_legislatives_deciles2 %>%
  bind_rows(cses_dataset_income) 

#WVS ----
wvs_data_clean <- read.csv("data/intermediary/elections/wvs elections dataset.csv")
wvs_data_clean_income <- wvs_data_clean %>%
  filter(inc >= 1)

#calcul décile cses ----
build_wvs_base_long <- function(df, annee, country){
  
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


wvs_dataset_income <- wvs_data_clean_income %>%
  group_by(isoname, year) %>%
  group_split() %>%
  map_dfr(~ build_wvs_base_long(.x, unique(.x$year), unique(.x$isoname)))
wvs_dataset_income <- wvs_dataset_income %>%
  mutate(bias = "plutocracy")
wvs_dataset_income <- wvs_dataset_income %>%
  rename(category = decile)


##WVS gender ---
wvs_data_clean_sexe <- wvs_data_clean %>%
  filter(gender %in% c("men", "women"))

###WVS calcul fonction gender ----
build_votes_by_gender <- function(df){
  
  df <- df %>%
    mutate(
      vote = case_when(
        is.na(dataset_party_id) ~ NA_character_,
        str_detect(dataset_party_id, "Abstention$") ~ "Abstention",
        TRUE ~ as.character(dataset_party_id)
      )
    )
  
  # Votes par sexe × parti
  votes <- df %>%
    group_by(isoname, year, gender, vote, partyfacts_id) %>%
    summarise(
      votes = n(),
      .groups = "drop"
    ) %>%
    group_by(isoname, year, gender) %>%
    mutate(
      pct_votes = votes / sum(votes) * 100
    ) %>%
    ungroup()
  
  # Participation par sexe
  participation <- df %>%
    group_by(isoname, year, gender) %>%
    summarise(
      nbr_obs = n(),
      votes_valides = sum(
        !str_detect(dataset_party_id, "Abstention$"),
        na.rm = TRUE
      ),
      taux_participation = votes_valides / nbr_obs * 100,
      .groups = "drop"
    )
  
  votes %>%
    left_join(
      participation,
      by = c("isoname", "year", "gender")
    )
}
meta_info_wvs <- wvs_data_clean %>%
  distinct(isoname, year, survey, source, source_recode)

wvs_dataset_gender <- wvs_data_clean_sexe %>%
  group_by(isoname, year) %>%
  group_split() %>%
  map_dfr(~ build_votes_by_gender(.x)) %>%
  left_join(meta_info_wvs, by = c("isoname", "year"))

wvs_dataset_gender <- wvs_dataset_gender %>%
  mutate(bias = "phallocracy")
wvs_dataset_gender <- wvs_dataset_gender %>%
  rename(category = gender)

names(wvs_dataset_gender)
names(wvs_dataset_income)

##WVS educ ----
wvs_data_clean_educ <- wvs_data_clean %>%
  filter(educ >= 1)

###WVS calcul fonction educ ----
build_votes_by_educ_fractional <- function(df){
  
  # 1. distribution + weights (comme ta fonction revenu)
  dist_educ <- df %>%
    count(educ) %>%
    arrange(educ) %>%
    mutate(
      pct = n / sum(n) * 100,
      cum_pct = cumsum(pct),
      cum_prev = lag(cum_pct, default = 0)
    )
  
  educ_groups <- data.frame(
    educ_group = c(1, 2),
    lower = c(0, 50),
    upper = c(50, 100)
  )
  
  weights_matrix <- dist_educ %>%
    crossing(educ_groups) %>%
    mutate(
      overlap = pmax(0, pmin(cum_pct, upper) - pmax(cum_prev, lower)),
      weight = overlap / (cum_pct - cum_prev)
    ) %>%
    filter(weight > 0)
  
  # 2. expansion micro → groupes pondérés
  df_groups <- df %>%
    left_join(
      weights_matrix %>%
        dplyr::select(educ, educ_group, weight),
      by = "educ",
      relationship = "many-to-many"
    ) %>%
    filter(!is.na(weight))
  
  # 3. vote variable (comme ta fonction standard)
  df_groups <- df_groups %>%
    mutate(
      vote = case_when(
        is.na(dataset_party_id) ~ NA_character_,
        str_detect(dataset_party_id, "Abstention$") ~ "Abstention",
        TRUE ~ dataset_party_id
      )
    )
  
  # 4. votes
  votes <- df_groups %>%
    group_by(isoname, year, educ_group, vote, partyfacts_id) %>%
    summarise(
      votes = sum(weight),
      .groups = "drop"
    ) %>%
    group_by(educ_group) %>%
    mutate(
      pct_votes = votes / sum(votes) * 100
    ) %>%
    ungroup()
  
  # 5. participation
  participation <- df_groups %>%
    group_by(educ_group) %>%
    summarise(
      nbr_obs = sum(weight),
      votes_valides = sum(
        weight * (!str_detect(dataset_party_id, "Abstention$")),
        na.rm = TRUE
      ),
      taux_participation = votes_valides / nbr_obs * 100,
      .groups = "drop"
    )
  
  # 6. output final (même structure que deciles / sexe)
  votes %>%
    left_join(participation, by = "educ_group")
}

wvs_dataset_educ <- wvs_data_clean_educ %>%
  group_by(isoname, year) %>%
  group_split() %>%
  map_dfr(~ build_votes_by_educ_fractional(.x)) %>%
  left_join(meta_info_wvs, by = c("isoname", "year"))

wvs_dataset_educ <- wvs_dataset_educ %>%
  mutate(bias = "epistocracy")
wvs_dataset_educ <- wvs_dataset_educ %>%
  rename(category = educ_group)
wvs_dataset_educ <- wvs_dataset_educ %>%
  mutate(
    category = case_when(
      category == "1" ~ "bot-educ", 
      category== "2" ~ "top-educ",
      TRUE ~ as.character(category)
    )
  )
##WVS age ----
wvs_data_clean_age <- cses_data_clean %>%
  filter(age >= 1)

###WVS calcul fonction age ----

#Espace pour empiler les bases votes entre elles ----
Base_legislatives_deciles2 <- Base_legislatives_deciles2 %>%
  bind_rows(wvs_dataset_income)



elections_legislatives_valides2 <- Base_legislatives_deciles2 %>%
  group_by(isoname,year,source, source_recode) %>%
  summarise(.groups = "drop")

write.csv(
  elections_legislatives_valides2,
  "data/intermediary/elections/valid elections.csv",
  row.names = FALSE
)

write.csv(
  Base_legislatives_deciles2,
  "data/intermediary/elections/legislative elections with dinc dataset.csv",
  row.names = FALSE
)