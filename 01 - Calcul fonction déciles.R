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
    group_by(source, source_recode,isoname, year,dinc, vote, partyfacts_id) %>%
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
  group_by(source, source_recode,isoname, year) %>%
  group_split() %>%
  map_dfr(~ build_votes_by_decile(.x)) %>%
  left_join(meta_info, by = c("source", "source_recode", "isoname", "year"))

Base_legislatives_deciles2 <- Base_legislatives_deciles2 %>%
  filter(taux_participation != 100)
Base_legislatives_deciles2 <- Base_legislatives_deciles2 %>%
  rename(category = dinc)


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
    group_by(source,source_recode,isoname, year, gender, vote, partyfacts_id) %>%
    summarise(
      votes = n(),
      .groups = "drop"
    ) %>%
    group_by(source, source_recode,isoname, year, gender) %>%
    mutate(
      pct_votes = votes / sum(votes) * 100
    ) %>%
    ungroup()
  
  # Participation par sexe
  participation <- df %>%
    group_by(source, source_recode,isoname, year, gender) %>%
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
      by = c("source","source_recode", "isoname", "year", "gender")
    )
}
meta_info <- Base_elections_legislatives %>%
  distinct(isoname, year, survey, source, source_recode)

Base_legislatives_gender <- Base_elections_legislatives_sexe %>%
  group_by(source,source_recode,isoname, year) %>%
  group_split() %>%
  map_dfr(~ build_votes_by_gender(.x)) %>%
  left_join(meta_info, by = c("source","source_recode", "isoname", "year"))

Base_legislatives_gender <- Base_legislatives_gender %>%
  mutate(bias = "androcracy")
Base_legislatives_gender <- Base_legislatives_gender %>%
  rename(category = gender)

##Calcul fonction educ wpid ----
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
    group_by(source, source_recode,isoname, year, educ_group, vote, partyfacts_id) %>%
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
  group_by(source, source_recode,isoname, year) %>%
  group_split() %>%
  map_dfr(~ build_votes_by_educ_fractional(.x)) %>%
  left_join(meta_info, by = c("source","source_recode", "isoname", "year"))

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


##Calcul fonction age ----
unique(Base_elections_legislatives$age)

Base_elections_legislatives_age <- Base_elections_legislatives[!is.na(Base_elections_legislatives$age),]

build_votes_by_age_fractional <- function(df){
  
  # 1. distribution + weights (comme ta fonction revenu)
  dist_age <- df %>%
    count(age) %>%
    arrange(age) %>%
    mutate(
      pct = n / sum(n) * 100,
      cum_pct = cumsum(pct),
      cum_prev = lag(cum_pct, default = 0)
    )
  
  age_groups <- data.frame(
    age_group = c(1, 2),
    lower = c(0, 50),
    upper = c(50, 100)
  )
  
  weights_matrix <- dist_age %>%
    crossing(age_groups) %>%
    mutate(
      overlap = pmax(0, pmin(cum_pct, upper) - pmax(cum_prev, lower)),
      weight = overlap / (cum_pct - cum_prev)
    ) %>%
    filter(weight > 0)
  
  # 2. expansion micro → groupes pondérés
  df_groups <- df %>%
    left_join(
      weights_matrix %>%
        dplyr::select(age, age_group, weight),
      by = "age",
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
    group_by(source, source_recode,isoname, year, age_group, vote, partyfacts_id) %>%
    summarise(
      votes = sum(weight),
      .groups = "drop"
    ) %>%
    group_by(age_group) %>%
    mutate(
      pct_votes = votes / sum(votes) * 100
    ) %>%
    ungroup()
  
  # 5. participation
  participation <- df_groups %>%
    group_by(age_group) %>%
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
    left_join(participation, by = "age_group")
}

Base_legislatives_age <- Base_elections_legislatives %>%
  group_by(source,source_recode,isoname, year) %>%
  group_split() %>%
  map_dfr(~ build_votes_by_age_fractional(.x)) %>%
  left_join(meta_info, by = c("source","source_recode", "isoname", "year"))

Base_legislatives_age <- Base_legislatives_age %>%
  mutate(bias = "gerontocracy")
Base_legislatives_age <- Base_legislatives_age %>%
  rename(category = age_group)
Base_legislatives_age <- Base_legislatives_age %>%
  mutate(
    category = case_when(
      category == "1" ~ "bot-age", 
      category== "2" ~ "top-age",
      TRUE ~ as.character(category)
    )
  )
#Autres bases ----
#CSES ----
cses_data_clean <- read.csv("data/intermediary/elections/cses elections dataset.csv")
sum(cses_data_clean$partyfacts_id == "Abstention", na.rm = TRUE)
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
  mutate(bias = "androcracy")
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

build_votes_by_age_fractional <- function(df){
  
  # 1. distribution + weights (comme ta fonction revenu)
  dist_age <- df %>%
    count(age) %>%
    arrange(age) %>%
    mutate(
      pct = n / sum(n) * 100,
      cum_pct = cumsum(pct),
      cum_prev = lag(cum_pct, default = 0)
    )
  
  age_groups <- data.frame(
    age_group = c(1, 2),
    lower = c(0, 50),
    upper = c(50, 100)
  )
  
  weights_matrix <- dist_age %>%
    crossing(age_groups) %>%
    mutate(
      overlap = pmax(0, pmin(cum_pct, upper) - pmax(cum_prev, lower)),
      weight = overlap / (cum_pct - cum_prev)
    ) %>%
    filter(weight > 0)
  
  # 2. expansion micro → groupes pondérés
  df_groups <- df %>%
    left_join(
      weights_matrix %>%
        dplyr::select(age, age_group, weight),
      by = "age",
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
    group_by(isoname, year, age_group, vote, partyfacts_id) %>%
    summarise(
      votes = sum(weight),
      .groups = "drop"
    ) %>%
    group_by(age_group) %>%
    mutate(
      pct_votes = votes / sum(votes) * 100
    ) %>%
    ungroup()
  
  # 5. participation
  participation <- df_groups %>%
    group_by(age_group) %>%
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
    left_join(participation, by = "age_group")
}

cses_dataset_age <- cses_data_clean_age %>%
  group_by(isoname, year) %>%
  group_split() %>%
  map_dfr(~ build_votes_by_age_fractional(.x)) %>%
  left_join(meta_info_cses, by = c("isoname", "year"))

cses_dataset_age <- cses_dataset_age %>%
  mutate(bias = "gerontocracy")
cses_dataset_age <- cses_dataset_age %>%
  rename(category = age_group)
cses_dataset_age <- cses_dataset_age %>%
  mutate(
    category = case_when(
      category == "1" ~ "bot-age", 
      category== "2" ~ "top-age",
      TRUE ~ as.character(category)
    )
  )


#WVS ----
wvs_data_clean <- read.csv("data/intermediary/elections/wvs elections dataset.csv")
wvs_data_clean_income <- wvs_data_clean %>%
  filter(inc >= 1)

#calcul décile wvs ----
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
  mutate(bias = "androcracy")
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
wvs_data_clean_age <- wvs_data_clean %>%
  filter(age >= 1)

build_votes_by_age_fractional <- function(df){
  
  # 1. distribution + weights (comme ta fonction revenu)
  dist_age <- df %>%
    count(age) %>%
    arrange(age) %>%
    mutate(
      pct = n / sum(n) * 100,
      cum_pct = cumsum(pct),
      cum_prev = lag(cum_pct, default = 0)
    )
  
  age_groups <- data.frame(
    age_group = c(1, 2),
    lower = c(0, 50),
    upper = c(50, 100)
  )
  
  weights_matrix <- dist_age %>%
    crossing(age_groups) %>%
    mutate(
      overlap = pmax(0, pmin(cum_pct, upper) - pmax(cum_prev, lower)),
      weight = overlap / (cum_pct - cum_prev)
    ) %>%
    filter(weight > 0)
  
  # 2. expansion micro → groupes pondérés
  df_groups <- df %>%
    left_join(
      weights_matrix %>%
        dplyr::select(age, age_group, weight),
      by = "age",
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
    group_by(isoname, year, age_group, vote, partyfacts_id) %>%
    summarise(
      votes = sum(weight),
      .groups = "drop"
    ) %>%
    group_by(age_group) %>%
    mutate(
      pct_votes = votes / sum(votes) * 100
    ) %>%
    ungroup()
  
  # 5. participation
  participation <- df_groups %>%
    group_by(age_group) %>%
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
    left_join(participation, by = "age_group")
}

wvs_dataset_age <- wvs_data_clean_age %>%
  group_by(isoname, year) %>%
  group_split() %>%
  map_dfr(~ build_votes_by_age_fractional(.x)) %>%
  left_join(meta_info_wvs, by = c("isoname", "year"))

wvs_dataset_age <- wvs_dataset_age %>%
  mutate(bias = "gerontocracy")
wvs_dataset_age <- wvs_dataset_age %>%
  rename(category = age_group)
wvs_dataset_age <- wvs_dataset_age %>%
  mutate(
    category = case_when(
      category == "1" ~ "bot-age", 
      category== "2" ~ "top-age",
      TRUE ~ as.character(category)
    )
  )

#ESS ----
ess_data_clean <- read.csv("data/intermediary/elections/ess elections dataset.csv")
sum(ess_data_clean$partyfacts_id == "Abstention", na.rm = TRUE)
ess_data_clean_income <- ess_data_clean %>%
  filter(inc < 77)

#calcul décile cses ----
build_ess_base_long <- function(df, annee, country){
  
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

ess_dataset_income <- ess_data_clean_income %>%
  group_by(isoname, year) %>%
  group_split() %>%
  map_dfr(~ build_ess_base_long(.x, unique(.x$year), unique(.x$isoname)))
ess_dataset_income <- ess_dataset_income %>%
  mutate(bias = "plutocracy")
ess_dataset_income <- ess_dataset_income %>%
  rename(category = decile)


##ESS gender ----
ess_data_clean_sexe <- ess_data_clean %>%
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
meta_info_ess <- ess_data_clean %>%
  distinct(isoname, year, survey, source, source_recode)

ess_dataset_gender <- ess_data_clean_sexe %>%
  group_by(isoname, year) %>%
  group_split() %>%
  map_dfr(~ build_votes_by_gender(.x)) %>%
  left_join(meta_info_ess, by = c("isoname", "year"))

ess_dataset_gender <- ess_dataset_gender %>%
  mutate(bias = "androcracy")
ess_dataset_gender <- ess_dataset_gender %>%
  rename(category = gender)
unique(ess_data_clean$educ)

##ESS educ ----
ess_data_clean_educ <- ess_data_clean %>%
  filter(educ < 55) #véroifier à quoi correspond educ = 55

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

ess_dataset_educ <- ess_data_clean_educ %>%
  group_by(isoname, year) %>%
  group_split() %>%
  map_dfr(~ build_votes_by_educ_fractional(.x)) %>%
  left_join(meta_info_ess, by = c("isoname", "year"))

ess_dataset_educ <- ess_dataset_educ %>%
  mutate(bias = "epistocracy")
ess_dataset_educ <- ess_dataset_educ %>%
  rename(category = educ_group)
ess_dataset_educ <- ess_dataset_educ %>%
  mutate(
    category = case_when(
      category == "1" ~ "bot-educ", 
      category== "2" ~ "top-educ",
      TRUE ~ as.character(category)
    )
  )

##ESS age ----
ess_data_clean_age <- ess_data_clean %>%
  filter(age < 99)

build_votes_by_age_fractional <- function(df){
  
  # 1. distribution + weights (comme ta fonction revenu)
  dist_age <- df %>%
    count(age) %>%
    arrange(age) %>%
    mutate(
      pct = n / sum(n) * 100,
      cum_pct = cumsum(pct),
      cum_prev = lag(cum_pct, default = 0)
    )
  
  age_groups <- data.frame(
    age_group = c(1, 2),
    lower = c(0, 50),
    upper = c(50, 100)
  )
  
  weights_matrix <- dist_age %>%
    crossing(age_groups) %>%
    mutate(
      overlap = pmax(0, pmin(cum_pct, upper) - pmax(cum_prev, lower)),
      weight = overlap / (cum_pct - cum_prev)
    ) %>%
    filter(weight > 0)
  
  # 2. expansion micro → groupes pondérés
  df_groups <- df %>%
    left_join(
      weights_matrix %>%
        dplyr::select(age, age_group, weight),
      by = "age",
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
    group_by(isoname, year, age_group, vote, partyfacts_id) %>%
    summarise(
      votes = sum(weight),
      .groups = "drop"
    ) %>%
    group_by(age_group) %>%
    mutate(
      pct_votes = votes / sum(votes) * 100
    ) %>%
    ungroup()
  
  # 5. participation
  participation <- df_groups %>%
    group_by(age_group) %>%
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
    left_join(participation, by = "age_group")
}

ess_dataset_age <- ess_data_clean_age %>%
  group_by(isoname, year) %>%
  group_split() %>%
  map_dfr(~ build_votes_by_age_fractional(.x)) %>%
  left_join(meta_info_ess, by = c("isoname", "year"))

ess_dataset_age <- ess_dataset_age %>%
  mutate(bias = "gerontocracy")
ess_dataset_age <- ess_dataset_age %>%
  rename(category = age_group)
ess_dataset_age <- ess_dataset_age %>%
  mutate(
    category = case_when(
      category == "1" ~ "bot-age", 
      category== "2" ~ "top-age",
      TRUE ~ as.character(category)
    )
  )


#Traitement des df income ----
Base_legislatives_deciles2 <- Base_legislatives_deciles2 %>%
  mutate(
    category = paste0("inc-", category))
cses_dataset_income <- cses_dataset_income %>%
  mutate(
    category = paste0("inc-", category))
wvs_dataset_income <- wvs_dataset_income %>%
  mutate(
    category = paste0("inc-", category))
ess_dataset_income <- ess_dataset_income %>%
  mutate(
    category = paste0("inc-", category))

#Espace pour empiler les bases votes entre elles ----
bases <- list(
  Base_legislatives_deciles2,
  Base_legislatives_educ,
  Base_legislatives_age,
  Base_legislatives_gender,
  cses_dataset_income,
  cses_dataset_educ,
  cses_dataset_age,
  cses_dataset_gender,
  wvs_dataset_income,
  wvs_dataset_educ,
  wvs_dataset_age,
  wvs_dataset_gender,
  ess_dataset_income,
  ess_dataset_educ,
  ess_dataset_age,
  ess_dataset_gender
)

bases <- lapply(
  bases,
  \(x) x %>% mutate(category = as.character(category),
                    partyfacts_id = as.character(partyfacts_id))
)

Base_all_clivages <- bind_rows(bases)

#vérif qu'on a bien tout 
unique(Base_all_clivages$bias)
unique(Base_all_clivages$category)
unique(Base_all_clivages$source_recode)

Base_all_clivages <- Base_all_clivages %>%
  select(source,source_recode,survey,bias,isoname,year, 
         category,vote,partyfacts_id,votes, pct_votes,nbr_obs,votes_valides,taux_participation)


#Liste de toutes nos élections par années et datasets
elections_legislatives_valides2 <- Base_all_clivages %>%
  group_by(isoname,year,source, source_recode) %>%
  summarise(.groups = "drop")

write.csv(
  elections_legislatives_valides2,
  "data/intermediary/elections/valid elections.csv",
  row.names = FALSE
)

#Rename des paryfacts pour les gros partis qui joinent mal entre bases
unique(Base_all_clivages$vote[Base_all_clivages$isoname == "Zimbabwe" & Base_all_clivages$survey_year == 2012  ] )
unique(Base_all_clivages$partyfacts_id[Base_all_clivages$isoname == "Latvia" & Base_all_clivages$survey_year == 1996 ] )
Base_all_clivages <- Base_all_clivages %>%
  mutate(
    partyfacts_id = case_when(
      vote == "12004" & isoname == "Algeria" & year >= 2002  ~ "5222",
      vote == "32013" & isoname == "Argentina" & year == 1999  ~ "6116",
      vote == "32012" & isoname == "Argentina" & year >= 2006 & year <= 2013  ~ "2530",
      vote == "112001" & isoname == "Belarus" & year == 1990 ~ "2030",
      vote == "70029" & isoname == "Bosnia and Herzegovina" & year == 1998 ~ "1340",
      vote == "152020" & isoname == "Chile" & year == 2005 ~ "6061",   #Attention pour le cas du Chili ce n'est peut-être pas le bon, parti
      vote == "218001" & isoname == "Ecuador" ~ "4044",
      vote == "288001" & isoname == "Ghana" & year == 2012 ~ "2311",
      vote == "288002" & isoname == "Ghana" & year == 2012 ~ "2312",
      vote == "HU-Fidesz" & isoname == "Hungary" & year == 2010 ~ "6366",
      vote == "HU-Fidesz" & isoname == "Hungary" & year == 2014 ~ "6366",
      vote == "HU-Fidesz" & isoname == "Iran" & year == 2000 ~ "5359",
      vote == "HU-Fidesz" & isoname == "Iran" & year == 2000 ~ "6875",
      vote == "4280043" & isoname == "Latvia" & year == 2011 ~ "1704",
      vote == "4280043" & isoname == "Latvia" & year == 2014 ~ "1704",
      vote == "484003" & isoname == "Mexico" & year == 2000 ~ "1988",
      vote == "499001" & isoname == "Montenegro" & year == 1996 ~ "3162",
      vote == "499103" & isoname == "Montenegro" & year == 1996 ~ "3164",
      vote == "499001" & isoname == "Montenegro" & year == 2001 ~ "3162",
      vote == "499006" & isoname == "Montenegro" & year == 2001 ~ "3645",
      vote == "499005" & isoname == "Montenegro" & year == 2001 ~ "3104",
      vote == "504004" & isoname == "Morocco" & year == 2007 ~ "2480",
      vote == "504004" & isoname == "Morocco" & year == 2011 ~ "2480",
      vote == "NG-People's Democratic Party" & isoname == "Nigeria" & year == 1999 ~ "2354",
      vote == "NO-Centrists-Liberals" & isoname == "Norway" & year == 1965 ~ "1072",
      vote == "608004" & isoname == "Philippines" & year == 2001 ~ "2466",
      vote == "616025" & isoname == "Poland" & year == 1989 ~ "1286",
      vote == "616028" & isoname == "Poland" & year == 1989 ~ "767",
      vote == "616007" & isoname == "Poland" & year == 1997 ~ "1566",
      vote == "642055" & isoname == "Romania" & year == 2012 ~ "2474",
      vote == "642063" & isoname == "Romania" & year == 2012 ~ "5940",
      vote == "642008" & isoname == "Romania" & year == 2012 ~ "5941",
      vote == "642052" & isoname == "Romania" & year == 2012 ~ "5941",
      vote == "643018" & isoname == "Russia" & year == 1995 ~ "2247",
      vote == "703018" & isoname == "Slovakia" & year == 1990 ~ "5",
      vote == "152004" & isoname == "Chile"  ~ "390",
      vote == "152005" & isoname == "Chile" & year == 1990 ~ "256",
      vote == "152005" & isoname == "Chile" & year == 2000 ~ "256",
      vote == "233003" & isoname == "Estonia" ~ "1556",
      vote == "233005" & isoname == "Estonia" ~ "174",
      vote == "268123"&  isoname == "Georgia" ~ "2988",
      vote == "268107"&  isoname == "Georgia" ~ "5885",
      vote == "8002" & isoname == "Albania" & year >= 1998 ~ "7075",
      vote == "76001" & isoname == "Brazil" & year == 1991 ~ "654",
      vote == "76001" & isoname == "Brazil" & year == 1997 ~ "654",
      vote == "76003" & isoname == "Brazil" & year == 1991 ~ "4402",  #76003 ou 76021 pour celui-là, à vérifier
      vote == "203019" & isoname == "Czech Republic" & year == 1991 ~ "3921",
      vote == "203009" & isoname == "Czech Republic" & year == 1991 ~ "3921", #alliance civic democratic et christian democratic party
      vote == "818128" & isoname == "Egypt" & year == 2013 ~ "5871",
      vote == "233010" & isoname == "Estonia" & year == 1996 ~ "779",
      vote == "233031" & isoname == "Estonia" & year == 1996 ~ "1150",
      vote == "268002" & isoname == "Georgia" & year == 1996 ~ "2168",
      vote == "276005" & isoname == "Germany" & year == 2006 ~ "1545",#le parti n'existait pas encore au momet de l'enquête donc je le rattache au parti qui l'a précédé
      vote == "278001" & isoname == "Ghana" & year == 2007 ~ "2311",
      vote == "288002" & isoname == "Ghana" & year == 2012 ~ "2312",
      vote == "348018" & isoname == "Hungary" & year == 2009 ~ "42",
      vote == "356068" & isoname == "India" & year == 1990 ~ "1207",
      vote == "360007" & isoname == "Indonesia" & year == 2006 ~ "2560",
      vote == "364012" & isoname == "Iran" & year == 2007 ~ "6322",
      vote == "364011" & isoname == "Iran" & year == 2007 ~ "5358",
      vote == "368003" & isoname == "Iraq" & year == 2006 ~ "5919",
      vote == "368002" & isoname == "Iraq" & year == 2006 ~ "5897",
      vote == "368018" & isoname == "Iraq" & year == 2013 ~ "5927",
      vote == "376002" & isoname == "Israel"  ~ "615",
      vote == "428032" & isoname == "Latvia" & year == 1996 ~ "1043",
      vote == "428023" & isoname == "Latvia" & year == 1996 ~ "1704", #attention le parti à l'élection 1998 était dans une alliance qui n'existe pas encore au moment de l'enquête
      vote == "428002" & isoname == "Latvia" & year == 1996 ~ "1719",
      vote == "440013" & isoname == "Lithuania" & year == 1997 ~ "1357",
      vote == "440005" & isoname == "Lithuania" & year == 1997 ~ "738",
      vote == "458001" & isoname == "Malaysia" & year == 2012 ~ "2485",
      vote == "458005" & isoname == "Malaysia" & year == 2012 ~ "3637",
      vote == "566002" & isoname == "Nigeria" & year == 2012 ~ "5538",#le parti n'existe pas encore au moment de l'enquête, donc je l'ai associé à son prédécesseur
      vote == "586002" & isoname == "Pakistan" & year == 2001  ~ "2385",
      vote == "604008" & isoname == "Peru" & year == 1996  ~ "5130",
      vote == "604008" & isoname == "Peru" & year == 1996  ~ "5130",
      vote == "608004" & isoname == "Philippines" & year == 2012  ~ "2466",
      vote == "642003" & isoname == "Romania" & year == 2005  ~ "660",
      vote == "642060" & isoname == "Romania" & year == 2005  ~ "481",
      vote == "646109" & isoname == "Rwanda" & year == 2012  ~ "3658",
      vote == "688001" & isoname == "Serbia" & year == 2001  ~ "2190", #je ne sais pas si il faut le relier au 2189 ou 2190, les noms sont très proches
      vote == "68804" & isoname == "Serbia" & year == 2001  ~ "2175",
      vote == "688001" & isoname == "Serbia" & year == 2006  ~ "2190", #je ne sais pas si il faut le relier au 2189 ou 2190, les noms sont très proches
      vote == "68804" & isoname == "Serbia" & year == 2006 ~ "2175",
      vote == "705006" & isoname == "Slovenia" & year == 1995 ~ "472",
      vote == "705006" & isoname == "Slovenia" & year == 2005 ~ "472",
      vote == "705003" & isoname == "Slovenia" & year == 2005 ~ "474",
      vote == "710008" & isoname == "South Africa" & year == 1990 ~ "1630",
      vote == "410002" & isoname == "South Korea" & year == 2005 ~ "2305",
      vote == "410001" & isoname == "South Korea" & year == 2005 ~ "2307",
      vote == "410002" & isoname == "South Korea" & year == 2010 ~ "2305",
      vote == "410001" & isoname == "South Korea" & year == 2010 ~ "2307",
      vote == "756022" & isoname == "Switzerland" & year == 1989 ~ "1808",
      vote == "756026" & isoname == "Switzerland" & year == 1989 ~ "360",
      vote == "756024" & isoname == "Switzerland" & year == 1989 ~ "308",
      vote == "756023" & isoname == "Switzerland" & year == 1989 ~ "29",
      vote == "788023" & isoname == "Tunisia" & year == 2003 ~ "5832",
      vote == "788022" & isoname == "Tunisia" & year == 2003 ~ "4530",
      vote == "792042" & isoname == "Turkey" & year == 1990 ~ "1253",
      vote == "792015" & isoname == "Turkey" & year == 1996 ~ "1463", #le parti n'existait pas au moment de l'enquête je l'ai rattaché à son prédécesseur 
      vote == "792025" & isoname == "Turkey" & year == 2001 ~ "306", #le parti n'existait pas au moment de l'enquête je l'ai rattaché à son prédécesseur 
      vote == "716007" & isoname == "Zimbabwe" & year == 2001 ~ "3305",
      vote == "716002" & isoname == "Zimbabwe" & year == 2012 ~ "3559",
      vote == "716007" & isoname == "Zimbabwe" & year == 2001 ~ "3305",
      
      TRUE ~ partyfacts_id
    )
  )


Base_all_clivages <- Base_all_clivages %>%
  group_by(source, source_recode,survey, isoname, year, bias, category, vote,partyfacts_id) %>%
  summarise(
    votes = sum(votes, na.rm = TRUE),
    nbr_obs = first(nbr_obs),
    votes_valides = first(votes_valides),
    taux_participation = first(taux_participation),
    .groups = "drop"
  ) %>%
  group_by(source, source_recode, isoname, year, bias) %>%
  mutate(
    pct_votes = votes / sum(votes, na.rm = TRUE) * 100
  ) %>%
  ungroup()
unique(Base_all_clivages$source_recode)

View(Base_all_clivages %>%
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
#Traitement sur Base_all_clivages pour traiter tous les joins manquants entre bases 


write.csv(
  Base_all_clivages,
  "data/intermediary/elections/dataset with all clivages and elections.csv",
  row.names = FALSE
)



