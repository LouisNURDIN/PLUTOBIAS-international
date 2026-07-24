#Calcul fonction déciles
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
Base_all_sources <- read.csv("data/intermediary/elections/dataset all sources.csv", sep = ",")


#WPID ----
Base_wpid_clean <- Base_all_sources %>%
  filter(Base_all_sources$source_recode == "WPID")

Base_wpid_clean <- Base_wpid_clean %>%
  filter(Base_wpid_clean$partyfacts_id != "Other")

#WPID income 10 ----

meta_info <- Base_wpid_clean %>%
  distinct(isoname, election_year, survey, source, source_recode,type)

Base_wpid_inc_raw_10 <- Base_wpid_clean %>%
  filter(!is.na(dinc))

build_votes_by_decile <- function(df){
  
  df <- df %>%
    mutate(
      weight_final = weight,
      vote = case_when(
        is.na(dataset_party_id) ~ NA_character_,
        str_detect(dataset_party_id, "Abstention$") ~ "Abstention",
        TRUE ~ dataset_party_id
      )
    )
  
  # Votes pondérés
  votes <- df %>%
    group_by(source, source_recode, isoname, election_year, dinc, vote, partyfacts_id) %>%
    summarise(
      votes = sum(weight_final, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    group_by(source, source_recode, isoname, election_year, dinc) %>%
    mutate(
      pct_votes = votes / sum(votes) * 100
    ) %>%
    ungroup()
  
  # Participation pondérée
  participation <- df %>%
    group_by(source, source_recode, isoname, election_year, dinc) %>%
    summarise(
      nbr_obs = sum(weight_final, na.rm = TRUE),
      votes_valides = sum(
        weight_final * (!str_detect(dataset_party_id, "Abstention$")),
        na.rm = TRUE
      ),
      taux_participation = votes_valides / nbr_obs * 100,
      .groups = "drop"
    )
  
  votes %>%
    left_join(
      participation,
      by = c("source", "source_recode", "isoname", "election_year", "dinc")
    )
}

Base_wpid_inc_10 <- Base_wpid_inc_raw_10 %>%
  group_by(source, source_recode,isoname, election_year) %>%
  group_split() %>%
  map_dfr(~ build_votes_by_decile(.x)) %>%
  left_join(meta_info, by = c("source", "source_recode", "isoname", "election_year"))

Base_wpid_inc_10 <- Base_wpid_inc_10%>%
  filter(taux_participation != 100)
Base_wpid_inc_10 <- Base_wpid_inc_10 %>%
  rename(category = dinc)

Base_wpid_inc_10 <- Base_wpid_inc_10 %>%
  mutate(bias = "plutocracy")

Base_wpid_inc_10 <- Base_wpid_inc_10 %>%
  mutate(
    category = case_when(
      category == 1 ~ "bot-income-10",
      category == 10 ~ "top-income-10",
      TRUE ~ NA_character_))

unique(Base_wpid_inc_10$category)


##WPID income 50 ----
Base_wpid_inc_raw_50 <- Base_wpid_clean %>%
  filter(!is.na(dinc))

build_wpid_base_long_50 <- function(df, annee, country){
  
  # sécurité
  election_year <- unique(df$election_year)[1]
  survey <- unique(df$survey)[1]
  type <- unique(df$type)[1]
  source <- unique(df$source)[1]
  source_recode <- unique(df$source_recode)[1]
  # 1. distribution revenu (par pays + année)
  
  dist_rev <- df %>%
    count(dinc) %>%
    arrange(dinc) %>%
    mutate(
      pct = n / sum(n) * 100,
      cum_pct = cumsum(pct),
      cum_prev = lag(cum_pct, default = 0)
    )
  
  deciles <- data.frame(
    decile = c(1, 2),
    lower = c(0, 50),
    upper = c(50, 100)
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
      weights_matrix %>% select(dinc, decile, weight_decile),
      by = "dinc",
      relationship = "many-to-many"
    ) %>%
    filter(!is.na(weight_decile)) %>%
    mutate(weight_final = weight * weight_decile)
  
  
  # 3. participation
  nbr_obs <- df_deciles %>%
    group_by(isoname, election_year,decile) %>%
    summarise(
      nbr_obs = sum(weight_final, na.rm = TRUE),
      .groups = "drop"
    )
  
  votes_valides <- df_deciles %>%
    filter(!str_detect(dataset_party_id, "Abstention$")) %>%
    group_by(isoname,election_year, decile) %>%
    summarise(
      votes_valides = sum(weight_final, na.rm = TRUE),
      .groups = "drop"
    )
  
  participation <- nbr_obs %>%
    left_join(votes_valides, by = c("isoname","election_year","decile")) %>%
    mutate(
      taux_participation = votes_valides / nbr_obs * 100
    )
  
  
  # 4. votes
  
  votes <- df_deciles %>%
    mutate(
      vote = case_when(
        is.na(dataset_party_id) ~ NA_character_,
        str_detect(dataset_party_id, "Abstention$") ~ "Abstention",
        TRUE ~ dataset_party_id
      )
    ) %>%
    group_by(isoname, election_year, decile, vote, partyfacts_id) %>%
    summarise(votes = sum(weight_final,na.rm = TRUE), .groups = "drop") %>%
    group_by(decile) %>%
    mutate(pct_votes = votes / sum(votes) * 100)
  
  
  # 5. output
  
  votes %>%
    left_join(participation, by = c("isoname", "election_year", "decile")) %>%
    mutate(
      annee = annee,
      election_year = election_year,
      type = type,
      isoname = country,
      survey = survey,
      source = source,
      source_recode = source_recode
    ) %>%
    relocate(isoname, annee, election_year, vote, decile, survey,type)
}


# PIPELINE MULTI-PAYS


Base_wpid_inc_50 <- Base_wpid_inc_raw_50 %>%
  group_by(isoname, election_year) %>%
  group_split() %>%
  map_dfr(~ build_wpid_base_long_50(.x, unique(.x$election_year), unique(.x$isoname)))
Base_wpid_inc_50 <- Base_wpid_inc_50 %>%
  mutate(bias = "plutocracy")
Base_wpid_inc_50 <- Base_wpid_inc_50 %>%
  rename(category = decile)
Base_wpid_inc_50 <- Base_wpid_inc_50 %>%
  mutate(
    category = case_when(
      category == 1 ~ "bot-income-50",
      category == 2 ~ "top-income-50",
      TRUE ~ NA_character_))

unique(Base_wpid_inc_50$category)

Base_wpid_income <- bind_rows(Base_wpid_inc_50, Base_wpid_inc_10)
Base_wpid_income <- Base_wpid_income %>% filter(!is.na(category))
Base_wpid_income <- Base_wpid_income %>% arrange(isoname,election_year,source)
Base_wpid_income <- Base_wpid_income %>% select(-annee)
unique(Base_wpid_income$category)
unique(Base_wpid_income$bias)



##wpid gender ----
Base_wpid_raw_gender <- Base_wpid_clean[!is.na(Base_wpid_clean$gender),]
###Calcul fonction sexe wpid ----
build_votes_by_gender <- function(df){
  
  df <- df %>%
    mutate(
      weight_final = weight,
      vote = case_when(
        is.na(dataset_party_id) ~ NA_character_,
        str_detect(dataset_party_id, "Abstention$") ~ "Abstention",
        TRUE ~ as.character(dataset_party_id)
      )
    )
  
  # Votes pondérés par sexe × parti
  votes <- df %>%
    group_by(source, source_recode, isoname, election_year, gender, vote, partyfacts_id,type) %>%
    summarise(
      votes = sum(weight_final, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    group_by(source, source_recode, isoname, election_year, gender) %>%
    mutate(
      pct_votes = votes / sum(votes) * 100
    ) %>%
    ungroup()
  
  # Participation pondérée
  participation <- df %>%
    group_by(source, source_recode, isoname, election_year, gender) %>%
    summarise(
      nbr_obs = sum(weight_final, na.rm = TRUE),
      votes_valides = sum(
        weight_final * (!str_detect(dataset_party_id, "Abstention$")),
        na.rm = TRUE
      ),
      taux_participation = votes_valides / nbr_obs * 100,
      .groups = "drop"
    )
  
  votes %>%
    left_join(
      participation,
      by = c("source", "source_recode", "isoname", "election_year", "gender")
    )
}


Base_wpid_gender <- Base_wpid_raw_gender %>%
  group_by(source, source_recode, isoname, election_year) %>%
  group_split() %>%
  map_dfr(~ build_votes_by_gender(.x)) %>%
  left_join(meta_info, by = c("source", "source_recode", "isoname", "election_year"))

Base_wpid_gender <- Base_wpid_gender %>%
  mutate(bias = "androcracy") %>%
  rename(category = gender)




##educ wpid 50----
unique(Base_wpid_clean$educ)
Base_raw_educ_50 <- Base_wpid_clean %>%
  filter(!is.na(educ))

build_votes_by_educ_fractional_50 <- function(df){
  
  # 1. Distribution pondérée de l'éducation
  dist_educ <- df %>%
    group_by(educ) %>%
    summarise(
      n = sum(weight, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(educ) %>%
    mutate(
      pct = n / sum(n) * 100,
      cum_pct = cumsum(pct),
      cum_prev = lag(cum_pct, default = 0)
    )
  
  # Deux groupes : bas / haut niveau d'éducation
  educ_groups <- data.frame(
    educ_group = c(1, 2),
    lower = c(0, 50),
    upper = c(50, 100)
  )
  
  # 2. Calcul des poids de répartition
  weights_matrix <- dist_educ %>%
    crossing(educ_groups) %>%
    mutate(
      overlap = pmax(0, pmin(cum_pct, upper) - pmax(cum_prev, lower)),
      weight_group = overlap / (cum_pct - cum_prev)
    ) %>%
    filter(weight_group > 0)
  
  # 3. Attribution des groupes
  df_groups <- df %>%
    left_join(
      weights_matrix %>%
        select(educ, educ_group, weight_group),
      by = "educ",
      relationship = "many-to-many"
    ) %>%
    filter(!is.na(weight_group)) %>%
    mutate(
      weight_final = weight * weight_group,
      vote = case_when(
        is.na(dataset_party_id) ~ NA_character_,
        str_detect(dataset_party_id, "Abstention$") ~ "Abstention",
        TRUE ~ dataset_party_id
      )
    )
  
  # 4. Votes pondérés
  votes <- df_groups %>%
    group_by(
      source,
      source_recode,
      isoname,
      election_year,
      educ_group,
      vote,
      partyfacts_id
    ) %>%
    summarise(
      votes = sum(weight_final, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    group_by(
      source,
      source_recode,
      isoname,
      election_year,
      educ_group
    ) %>%
    mutate(
      pct_votes = votes / sum(votes) * 100
    ) %>%
    ungroup()
  
  # 5. Participation pondérée
  participation <- df_groups %>%
    group_by(
      source,
      source_recode,
      isoname,
      election_year,
      educ_group
    ) %>%
    summarise(
      nbr_obs = sum(weight_final, na.rm = TRUE),
      votes_valides = sum(
        weight_final * (!str_detect(dataset_party_id, "Abstention$")),
        na.rm = TRUE
      ),
      taux_participation = votes_valides / nbr_obs * 100,
      .groups = "drop"
    )
  
  # 6. Output
  votes %>%
    left_join(
      participation,
      by = c(
        "source",
        "source_recode",
        "isoname",
        "election_year",
        "educ_group"
      )
    )
}



# Construction de la base finale
Base_wpid_educ_50 <- Base_raw_educ_50 %>%
  group_by(source, source_recode, isoname, election_year) %>%
  group_split() %>%
  map_dfr(~ build_votes_by_educ_fractional_50(.x)) %>%
  left_join(
    meta_info,
    by = c("source", "source_recode", "isoname", "election_year")
  ) %>%
  mutate(
    bias = "epistocracy",
    category = case_when(
      educ_group == 1 ~ "bot-educ-50",
      educ_group == 2 ~ "top-educ-50"
    )
  ) %>%
  select(-educ_group)


#educ 10 10 ----
Base_raw_educ_10 <- Base_wpid_clean %>%
  filter(!is.na(educ))

build_votes_by_educ_fractional_10 <- function(df){
  
  # 1. Distribution pondérée de l'éducation
  dist_educ <- df %>%
    group_by(educ) %>%
    summarise(
      n = sum(weight, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(educ) %>%
    mutate(
      pct = n / sum(n) * 100,
      cum_pct = cumsum(pct),
      cum_prev = lag(cum_pct, default = 0)
    )
  
  # Deux groupes : bas / haut niveau d'éducation
  
  educ_groups <- data.frame(
    educ_group = 1:10,
    lower = seq(0, 90, by = 10),
    upper = seq(10, 100, by = 10)
  )
  
  # 2. Calcul des poids de répartition
  weights_matrix <- dist_educ %>%
    crossing(educ_groups) %>%
    mutate(
      overlap = pmax(0, pmin(cum_pct, upper) - pmax(cum_prev, lower)),
      weight_group = overlap / (cum_pct - cum_prev)
    ) %>%
    filter(weight_group > 0)
  
  # 3. Attribution des groupes
  df_groups <- df %>%
    left_join(
      weights_matrix %>%
        select(educ, educ_group, weight_group),
      by = "educ",
      relationship = "many-to-many"
    ) %>%
    filter(!is.na(weight_group)) %>%
    mutate(
      weight_final = weight * weight_group,
      vote = case_when(
        is.na(dataset_party_id) ~ NA_character_,
        str_detect(dataset_party_id, "Abstention$") ~ "Abstention",
        TRUE ~ dataset_party_id
      )
    )
  
  # 4. Votes pondérés
  votes <- df_groups %>%
    group_by(
      source,
      source_recode,
      isoname,
      election_year,
      educ_group,
      vote,
      partyfacts_id
    ) %>%
    summarise(
      votes = sum(weight_final, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    group_by(
      source,
      source_recode,
      isoname,
      election_year,
      educ_group
    ) %>%
    mutate(
      pct_votes = votes / sum(votes) * 100
    ) %>%
    ungroup()
  
  # 5. Participation pondérée
  participation <- df_groups %>%
    group_by(
      source,
      source_recode,
      isoname,
      election_year,
      educ_group
    ) %>%
    summarise(
      nbr_obs = sum(weight_final, na.rm = TRUE),
      votes_valides = sum(
        weight_final * (!str_detect(dataset_party_id, "Abstention$")),
        na.rm = TRUE
      ),
      taux_participation = votes_valides / nbr_obs * 100,
      .groups = "drop"
    )
  
  # 6. Output
  votes %>%
    left_join(
      participation,
      by = c(
        "source",
        "source_recode",
        "isoname",
        "election_year",
        "educ_group"
      )
    )
}


# Construction de la base finale
Base_wpid_educ_10 <- Base_raw_educ_10 %>%
  group_by(source, source_recode, isoname, election_year) %>%
  group_split() %>%
  map_dfr(~ build_votes_by_educ_fractional_10(.x)) %>%
  left_join(
    meta_info,
    by = c("source", "source_recode", "isoname", "election_year")
  ) %>%
  mutate(
    bias = "epistocracy",
    category = case_when(
      educ_group == 1 ~ "bot-educ-10",
      educ_group == 10 ~ "top-educ-10"
    )
  ) %>%
  select(-educ_group)

Base_wpid_educ <- bind_rows(Base_wpid_educ_10,Base_wpid_educ_50)
Base_wpid_educ <- Base_wpid_educ %>% filter(!is.na(category))
Base_wpid_educ <- Base_wpid_educ %>% arrange(isoname,election_year,source)
unique(Base_wpid_educ$category)


##WPID age 50 ----
Base_wpid_raw_age_50 <- Base_wpid_clean[!is.na(Base_wpid_clean$age),]

build_votes_by_age_fractional_50 <- function(df){
  
  # 1. Distribution pondérée de l'éducation
  dist_age <- df %>%
    group_by(age) %>%
    summarise(
      n = sum(weight, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(age) %>%
    mutate(
      pct = n / sum(n) * 100,
      cum_pct = cumsum(pct),
      cum_prev = lag(cum_pct, default = 0)
    )
  
  # Deux groupes : bas / haut niveau d'éducation
  age_groups <- data.frame(
    age_group = c(1, 2),
    lower = c(0, 50),
    upper = c(50, 100)
  )
  
  # 2. Calcul des poids de répartition
  weights_matrix <- dist_age %>%
    crossing(age_groups) %>%
    mutate(
      overlap = pmax(0, pmin(cum_pct, upper) - pmax(cum_prev, lower)),
      weight_group = overlap / (cum_pct - cum_prev)
    ) %>%
    filter(weight_group > 0)
  
  # 3. Attribution des groupes
  df_groups <- df %>%
    left_join(
      weights_matrix %>%
        select(age, age_group, weight_group),
      by = "age",
      relationship = "many-to-many"
    ) %>%
    filter(!is.na(weight_group)) %>%
    mutate(
      weight_final = coalesce(weight, 0) * weight_group,
      vote = case_when(
        is.na(dataset_party_id) ~ NA_character_,
        str_detect(dataset_party_id, "Abstention$") ~ "Abstention",
        TRUE ~ dataset_party_id
      )
    )
  
  # 4. Votes pondérés
  votes <- df_groups %>%
    group_by(
      source,
      source_recode,
      isoname,
      election_year,
      age_group,
      vote,
      partyfacts_id
    ) %>%
    summarise(
      votes = sum(weight_final, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    group_by(
      source,
      source_recode,
      isoname,
      election_year,
      age_group
    ) %>%
    mutate(
      pct_votes = votes / sum(votes) * 100
    ) %>%
    ungroup()
  
  # 5. Participation pondérée
  participation <- df_groups %>%
    group_by(
      source,
      source_recode,
      isoname,
      election_year,
      age_group
    ) %>%
    summarise(
      nbr_obs = sum(weight_final, na.rm = TRUE),
      votes_valides = sum(
        weight_final * (!str_detect(dataset_party_id, "Abstention$")),
        na.rm = TRUE
      ),
      taux_participation = votes_valides / nbr_obs * 100,
      .groups = "drop"
    )
  
  # 6. Output
  votes %>%
    left_join(
      participation,
      by = c(
        "source",
        "source_recode",
        "isoname",
        "election_year",
        "age_group"
      )
    )
}

# Construction de la base finale
Base_wpid_age_50 <- Base_wpid_raw_age_50 %>%
  group_by(source, source_recode, isoname, election_year) %>%
  group_split() %>%
  map_dfr(~ build_votes_by_age_fractional_50(.x)) %>%
  left_join(
    meta_info,
    by = c("source", "source_recode", "isoname", "election_year")
  ) %>%
  mutate(
    bias = "gerontocracy",
    category = case_when(
      age_group == 1 ~ "bot-age-50",
      age_group == 2 ~ "top-age-50"
    )
  ) %>%
  select(-age_group)



verification_pct_votes <- Base_wpid_age_50 %>%
  group_by(isoname,source, election_year, category) %>%
  summarise(
    somme_pct_votes = sum(pct_votes, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(abs(somme_pct_votes - 100) > 0.01)


#WPID age 10 ----
Base_wpid_raw_age_10 <- Base_wpid_clean[!is.na(Base_wpid_clean$age),]

build_votes_by_age_fractional_10 <- function(df){
  
  # 1. Distribution pondérée de l'éducation
  dist_age <- df %>%
    group_by(age) %>%
    summarise(
      n = sum(weight, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(age) %>%
    mutate(
      pct = n / sum(n) * 100,
      cum_pct = cumsum(pct),
      cum_prev = lag(cum_pct, default = 0)
    )
  
  # Deux groupes : bas / haut niveau d'éducation
  age_groups <- data.frame(
    age_group = 1:10,
    lower = seq(0, 90, by = 10),
    upper = seq(10, 100, by = 10)
  )
  
  # 2. Calcul des poids de répartition
  weights_matrix <- dist_age %>%
    crossing(age_groups) %>%
    mutate(
      overlap = pmax(0, pmin(cum_pct, upper) - pmax(cum_prev, lower)),
      weight_group = overlap / (cum_pct - cum_prev)
    ) %>%
    filter(weight_group > 0)
  
  # 3. Attribution des groupes
  df_groups <- df %>%
    left_join(
      weights_matrix %>%
        select(age, age_group, weight_group),
      by = "age",
      relationship = "many-to-many"
    ) %>%
    filter(!is.na(weight_group)) %>%
    mutate(
      weight_final = coalesce(weight, 0) * weight_group,
      vote = case_when(
        is.na(dataset_party_id) ~ NA_character_,
        str_detect(dataset_party_id, "Abstention$") ~ "Abstention",
        TRUE ~ dataset_party_id
      )
    )
  
  # 4. Votes pondérés
  votes <- df_groups %>%
    group_by(
      source,
      source_recode,
      isoname,
      election_year,
      age_group,
      vote,
      partyfacts_id
    ) %>%
    summarise(
      votes = sum(weight_final, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    group_by(
      source,
      source_recode,
      isoname,
      election_year,
      age_group
    ) %>%
    mutate(
      pct_votes = votes / sum(votes) * 100
    ) %>%
    ungroup()
  
  # 5. Participation pondérée
  participation <- df_groups %>%
    group_by(
      source,
      source_recode,
      isoname,
      election_year,
      age_group
    ) %>%
    summarise(
      nbr_obs = sum(weight_final, na.rm = TRUE),
      votes_valides = sum(
        weight_final * (!str_detect(dataset_party_id, "Abstention$")),
        na.rm = TRUE
      ),
      taux_participation = votes_valides / nbr_obs * 100,
      .groups = "drop"
    )
  
  # 6. Output
  votes %>%
    left_join(
      participation,
      by = c(
        "source",
        "source_recode",
        "isoname",
        "election_year",
        "age_group"
      )
    )
}


# Construction de la base finale
Base_wpid_age_10 <- Base_wpid_raw_age_10 %>%
  group_by(source, source_recode, isoname, election_year) %>%
  group_split() %>%
  map_dfr(~ build_votes_by_age_fractional_10(.x)) %>%
  left_join(
    meta_info,
    by = c("source", "source_recode", "isoname", "election_year")
  ) %>%
  mutate(
    bias = "gerontocracy",
    category = case_when(
      age_group == 1 ~ "bot-age-10",
      age_group == 10 ~ "top-age-10"
    )
  ) %>%
  select(-age_group)

Base_wpid_age <- bind_rows(Base_wpid_age_10,Base_wpid_age_50)
Base_wpid_age <- Base_wpid_age %>% filter(!is.na(category))
Base_wpid_age <- Base_wpid_age %>% arrange(isoname,election_year,source)
unique(Base_wpid_age$category)



#Autres bases ----
#CSES ----
cses_data_clean <- Base_all_sources %>%
  filter(Base_all_sources$source_recode == "CSES")

cses_data_clean <- cses_data_clean %>%
  filter(cses_data_clean$partyfacts_id != "Other")

#CSES income 10 ----
cses_data_clean_income_10 <- cses_data_clean %>%
  filter(inc <= 5)
build_cses_base_long_10 <- function(df, annee, country){
  
  # sécurité
  election_year <- unique(df$election_year)[1]
  survey <- unique(df$survey)[1]
  type <- unique(df$type)[1]
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
    filter(!is.na(weight_decile)) %>%
    mutate(weight_final = weight * weight_decile)
  
  
  # 3. participation
  nbr_obs <- df_deciles %>%
    group_by(isoname, election_year, election_date,decile) %>%
    summarise(
      nbr_obs = sum(weight_final, na.rm = TRUE),
      .groups = "drop"
    )
  
  votes_valides <- df_deciles %>%
    filter(!str_detect(dataset_party_id, "Abstention$")) %>%
    group_by(isoname,election_year, election_date,decile) %>%
    summarise(
      votes_valides = sum(weight_final, na.rm = TRUE),
      .groups = "drop"
    )
  
  participation <- nbr_obs %>%
    left_join(votes_valides, by = c("isoname","election_year","election_date","decile")) %>%
    mutate(
      taux_participation = votes_valides / nbr_obs * 100
    )
  
  
  # 4. votes
  
  votes <- df_deciles %>%
    mutate(
      vote = case_when(
        is.na(dataset_party_id) ~ NA_character_,
        str_detect(dataset_party_id, "Abstention$") ~ "Abstention",
        TRUE ~ dataset_party_id
      )
    ) %>%
    group_by(isoname, election_year, election_date, decile, vote, partyfacts_id) %>%
    summarise(votes = sum(weight_final,na.rm = TRUE), .groups = "drop") %>%
    group_by(decile) %>%
    mutate(pct_votes = votes / sum(votes) * 100)
  
  
  # 5. output
  
  votes %>%
    left_join(participation, by = c("isoname", "election_year", "election_date", "decile")) %>%
    mutate(
      annee = annee,
      election_year = election_year,
      election_date = election_date,
      type = type,
      isoname = country,
      survey = survey,
      source = source,
      source_recode = source_recode
    ) %>%
    relocate(isoname, annee, election_year, vote, decile, survey,type)
}


# PIPELINE MULTI-PAYS


cses_dataset_income_10 <- cses_data_clean_income_10 %>%
  group_by(isoname, election_year,election_date) %>%
  group_split() %>%
  map_dfr(~ build_cses_base_long_10(.x, unique(.x$election_year), unique(.x$isoname)))
cses_dataset_income_10 <- cses_dataset_income_10 %>%
  mutate(bias = "plutocracy")
cses_dataset_income_10 <- cses_dataset_income_10 %>%
  rename(category = decile)
cses_dataset_income_10 <- cses_dataset_income_10 %>%
  mutate(
    category = case_when(
      category == 1 ~ "bot-income-10",
      category == 10 ~ "top-income-10",
      TRUE ~ NA_character_))

unique(cses_dataset_income_10$category)

##CSES income 50 ----
cses_data_clean_income_50 <- cses_data_clean %>%
  filter(inc <= 5)
build_cses_base_long_50 <- function(df, annee, country){
  
  # sécurité
  election_year <- unique(df$election_year)[1]
  survey <- unique(df$survey)[1]
  type <- unique(df$type)[1]
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
    decile = c(1, 2),
    lower = c(0, 50),
    upper = c(50, 100)
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
    filter(!is.na(weight_decile)) %>%
    mutate(weight_final = weight * weight_decile)
  
  
  # 3. participation
  nbr_obs <- df_deciles %>%
    group_by(isoname, election_year, election_date,decile) %>%
    summarise(
      nbr_obs = sum(weight_final, na.rm = TRUE),
      .groups = "drop"
    )
  
  votes_valides <- df_deciles %>%
    filter(!str_detect(dataset_party_id, "Abstention$")) %>%
    group_by(isoname,election_year, election_date,decile) %>%
    summarise(
      votes_valides = sum(weight_final, na.rm = TRUE),
      .groups = "drop"
    )
  
  participation <- nbr_obs %>%
    left_join(votes_valides, by = c("isoname","election_year","election_date","decile")) %>%
    mutate(
      taux_participation = votes_valides / nbr_obs * 100
    )
  
  
  # 4. votes
  
  votes <- df_deciles %>%
    mutate(
      vote = case_when(
        is.na(dataset_party_id) ~ NA_character_,
        str_detect(dataset_party_id, "Abstention$") ~ "Abstention",
        TRUE ~ dataset_party_id
      )
    ) %>%
    group_by(isoname, election_year, election_date, decile, vote, partyfacts_id) %>%
    summarise(votes = sum(weight_final,na.rm = TRUE), .groups = "drop") %>%
    group_by(decile) %>%
    mutate(pct_votes = votes / sum(votes) * 100)
  
  
  # 5. output
  
  votes %>%
    left_join(participation, by = c("isoname", "election_year", "election_date", "decile")) %>%
    mutate(
      annee = annee,
      election_year = election_year,
      election_date = election_date,
      type = type,
      isoname = country,
      survey = survey,
      source = source,
      source_recode = source_recode
    ) %>%
    relocate(isoname, annee, election_year, vote, decile, survey,type)
}


# PIPELINE MULTI-PAYS


cses_dataset_income_50 <- cses_data_clean_income_50 %>%
  group_by(isoname, election_year,election_date) %>%
  group_split() %>%
  map_dfr(~ build_cses_base_long_50(.x, unique(.x$election_year), unique(.x$isoname)))
cses_dataset_income_50 <- cses_dataset_income_50 %>%
  mutate(bias = "plutocracy")
cses_dataset_income_50 <- cses_dataset_income_50 %>%
  rename(category = decile)
cses_dataset_income_50 <- cses_dataset_income_50 %>%
  mutate(
    category = case_when(
      category == 1 ~ "bot-income-50",
      category == 2 ~ "top-income-50",
      TRUE ~ NA_character_))

unique(cses_dataset_income_50$category)

Base_cses_income <- bind_rows(cses_dataset_income_10, cses_dataset_income_50)
Base_cses_income <- Base_cses_income %>% filter(!is.na(category))
Base_cses_income <- Base_cses_income %>% arrange(isoname,election_year,source)
Base_cses_income <- Base_cses_income %>% select(-annee)
unique(Base_cses_income$category)


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
  
  # Votes pondérés par sexe × parti
  votes <- df %>%
    group_by(isoname, election_year,election_date, gender, vote, partyfacts_id) %>%
    summarise(
      votes = sum(weight, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    group_by(isoname, election_year,election_date, gender) %>%
    mutate(
      pct_votes = votes / sum(votes) * 100
    ) %>%
    ungroup()
  
  # Participation pondérée
  participation <- df %>%
    group_by(isoname, election_year,election_date, gender) %>%
    summarise(
      nbr_obs = sum(weight, na.rm = TRUE),
      votes_valides = sum(
        weight * (!str_detect(dataset_party_id, "Abstention$")),
        na.rm = TRUE
      ),
      taux_participation = votes_valides / nbr_obs * 100,
      .groups = "drop"
    )
  
  votes %>%
    left_join(
      participation,
      by = c("isoname", "election_year","election_date", "gender")
    )
}

# Informations de contexte
meta_info_cses <- cses_data_clean %>%
  distinct(isoname, election_year,election_date,type, survey, source, source_recode)

# Construction de la base
Base_cses_gender <- cses_data_clean_sexe %>%
  group_by(isoname, election_year,election_date) %>%
  group_split() %>%
  map_dfr(~ build_votes_by_gender(.x)) %>%
  left_join(meta_info_cses, by = c("isoname", "election_year","election_date")) %>%
  mutate(
    bias = "androcracy"
  ) %>%
  rename(category = gender)




##CSES educ 50----
cses_data_clean_educ_50 <- cses_data_clean %>%
  filter(educ < 6)

build_votes_by_educ_fractional_50 <- function(df){
  
  # 1. Distribution pondérée de l'éducation
  dist_educ <- df %>%
    group_by(educ) %>%
    summarise(
      n = sum(weight, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(educ) %>%
    mutate(
      pct = n / sum(n) * 100,
      cum_pct = cumsum(pct),
      cum_prev = lag(cum_pct, default = 0)
    )
  
  # Deux groupes : bas / haut niveau d'éducation
  educ_groups <- data.frame(
    educ_group = c(1, 2),
    lower = c(0, 50),
    upper = c(50, 100)
  )
  
  # 2. Calcul des poids de répartition
  weights_matrix <- dist_educ %>%
    crossing(educ_groups) %>%
    mutate(
      overlap = pmax(0, pmin(cum_pct, upper) - pmax(cum_prev, lower)),
      weight_group = overlap / (cum_pct - cum_prev)
    ) %>%
    filter(weight_group > 0)
  
  # 3. Attribution des groupes
  df_groups <- df %>%
    left_join(
      weights_matrix %>%
        select(educ, educ_group, weight_group),
      by = "educ",
      relationship = "many-to-many"
    ) %>%
    filter(!is.na(weight_group)) %>%
    mutate(
      weight_final = weight * weight_group,
      vote = case_when(
        is.na(dataset_party_id) ~ NA_character_,
        str_detect(dataset_party_id, "Abstention$") ~ "Abstention",
        TRUE ~ dataset_party_id
      )
    )
  
  # 4. Votes pondérés
  votes <- df_groups %>%
    group_by(isoname, election_year,election_date, educ_group, vote, partyfacts_id) %>%
    summarise(
      votes = sum(weight_final, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    group_by(isoname, election_year, election_date,educ_group) %>%
    mutate(
      pct_votes = votes / sum(votes) * 100
    ) %>%
    ungroup()
  
  # 5. Participation pondérée
  participation <- df_groups %>%
    group_by(isoname,election_year, election_date,educ_group) %>%
    summarise(
      nbr_obs = sum(weight_final, na.rm = TRUE),
      votes_valides = sum(
        weight_final * (!str_detect(dataset_party_id, "Abstention$")),
        na.rm = TRUE
      ),
      taux_participation = votes_valides / nbr_obs * 100,
      .groups = "drop"
    )
  
  # 6. Output
  votes %>%
    left_join(
      participation,
      by = c("isoname",
             "election_year",
             "election_date",
             "educ_group")
    )
}

# Construction de la base finale

cses_dataset_educ_50 <- cses_data_clean_educ_50 %>%
  group_by(isoname, election_year,election_date) %>%
  group_split() %>%
  map_dfr(~ build_votes_by_educ_fractional_50(.x)) %>%
  left_join(meta_info_cses, by = c("isoname", "election_year","election_date")) %>%
  mutate(
    bias = "epistocracy",
    category = case_when(
      educ_group == 1 ~ "bot-educ-50",
      educ_group == 2 ~ "top-educ-50"
    )
  ) %>%
  select(-educ_group)


##CSES educ 10 ----
cses_data_clean_educ_10 <- cses_data_clean %>%
  filter(educ < 6)

build_votes_by_educ_fractional_10 <- function(df){
  
  # 1. Distribution pondérée de l'éducation
  dist_educ <- df %>%
    group_by(educ) %>%
    summarise(
      n = sum(weight, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(educ) %>%
    mutate(
      pct = n / sum(n) * 100,
      cum_pct = cumsum(pct),
      cum_prev = lag(cum_pct, default = 0)
    )
  
  # Deux groupes : bas / haut niveau d'éducation
  educ_groups <- data.frame(
    educ_group = 1:10,
    lower = seq(0, 90, by = 10),
    upper = seq(10, 100, by = 10)
  )
  
  # 2. Calcul des poids de répartition
  weights_matrix <- dist_educ %>%
    crossing(educ_groups) %>%
    mutate(
      overlap = pmax(0, pmin(cum_pct, upper) - pmax(cum_prev, lower)),
      weight_group = overlap / (cum_pct - cum_prev)
    ) %>%
    filter(weight_group > 0)
  
  # 3. Attribution des groupes
  df_groups <- df %>%
    left_join(
      weights_matrix %>%
        select(educ, educ_group, weight_group),
      by = "educ",
      relationship = "many-to-many"
    ) %>%
    filter(!is.na(weight_group)) %>%
    mutate(
      weight_final = weight * weight_group,
      vote = case_when(
        is.na(dataset_party_id) ~ NA_character_,
        str_detect(dataset_party_id, "Abstention$") ~ "Abstention",
        TRUE ~ dataset_party_id
      )
    )
  
  # 4. Votes pondérés
  votes <- df_groups %>%
    group_by(isoname, election_year,election_date, educ_group, vote, partyfacts_id) %>%
    summarise(
      votes = sum(weight_final, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    group_by(isoname, election_year, election_date,educ_group) %>%
    mutate(
      pct_votes = votes / sum(votes) * 100
    ) %>%
    ungroup()
  
  # 5. Participation pondérée
  participation <- df_groups %>%
    group_by(isoname,election_year, election_date,educ_group) %>%
    summarise(
      nbr_obs = sum(weight_final, na.rm = TRUE),
      votes_valides = sum(
        weight_final * (!str_detect(dataset_party_id, "Abstention$")),
        na.rm = TRUE
      ),
      taux_participation = votes_valides / nbr_obs * 100,
      .groups = "drop"
    )
  
  # 6. Output
  votes %>%
    left_join(
      participation,
      by = c("isoname",
             "election_year",
             "election_date",
             "educ_group")
    )
}

# Construction de la base finale

cses_dataset_educ_10 <- cses_data_clean_educ_10 %>%
  group_by(isoname, election_year,election_date) %>%
  group_split() %>%
  map_dfr(~ build_votes_by_educ_fractional_10(.x)) %>%
  left_join(meta_info_cses, by = c("isoname", "election_year","election_date")) %>%
  mutate(
    bias = "epistocracy",
    category = case_when(
      educ_group == 1 ~ "bot-educ-10",
      educ_group == 10 ~ "top-educ-10"
    )
  ) %>%
  select(-educ_group)

Base_cses_educ <- bind_rows(cses_dataset_educ_10, cses_dataset_educ_50)
Base_cses_educ <- Base_cses_educ %>% filter(!is.na(category))
Base_cses_educ <- Base_cses_educ %>% arrange(isoname,election_year,source)
unique(Base_cses_educ$category)

##CSES age 50 ----
cses_data_clean_age_50 <- cses_data_clean %>%
  filter(age < 9997)

build_votes_by_age_fractional_50 <- function(df){
  
  # 1. Distribution pondérée de l'âge
  dist_age <- df %>%
    group_by(age) %>%
    summarise(
      n = sum(weight, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(age) %>%
    mutate(
      pct = n / sum(n) * 100,
      cum_pct = cumsum(pct),
      cum_prev = lag(cum_pct, default = 0)
    )
  
  # Deux groupes : jeunes / âgés
  age_groups <- data.frame(
    age_group = c(1, 2),
    lower = c(0, 50),
    upper = c(50, 100)
  )
  
  # 2. Poids de répartition
  weights_matrix <- dist_age %>%
    crossing(age_groups) %>%
    mutate(
      overlap = pmax(0, pmin(cum_pct, upper) - pmax(cum_prev, lower)),
      weight_group = overlap / (cum_pct - cum_prev)
    ) %>%
    filter(weight_group > 0)
  
  # 3. Attribution des groupes
  df_groups <- df %>%
    left_join(
      weights_matrix %>%
        select(age, age_group, weight_group),
      by = "age",
      relationship = "many-to-many"
    ) %>%
    filter(!is.na(weight_group)) %>%
    mutate(
      weight_final = weight * weight_group,
      vote = case_when(
        is.na(dataset_party_id) ~ NA_character_,
        str_detect(dataset_party_id, "Abstention$") ~ "Abstention",
        TRUE ~ dataset_party_id
      )
    )
  
  # 4. Votes pondérés
  votes <- df_groups %>%
    group_by(isoname, election_year,election_date, age_group, vote, partyfacts_id) %>%
    summarise(
      votes = sum(weight_final, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    group_by(isoname, election_year,election_date,age_group) %>%
    mutate(
      pct_votes = votes / sum(votes) * 100
    ) %>%
    ungroup()
  
  # 5. Participation pondérée
  participation <- df_groups %>%
    group_by(isoname, election_year, election_date,age_group) %>%
    summarise(
      nbr_obs = sum(weight_final, na.rm = TRUE),
      votes_valides = sum(
        weight_final * (!str_detect(dataset_party_id, "Abstention$")),
        na.rm = TRUE
      ),
      taux_participation = votes_valides / nbr_obs * 100,
      .groups = "drop"
    )
  
  # 6. Output
  votes %>%
    left_join(
      participation,
      by = c( "isoname",
              "election_year",
              "election_date",
              "age_group")
    )
}

# Construction de la base finale

cses_dataset_age_50 <- cses_data_clean_age_50 %>%
  group_by(isoname, election_year,election_date) %>%
  group_split() %>%
  map_dfr(~ build_votes_by_age_fractional_50(.x)) %>%
  left_join(meta_info_cses, by = c("isoname", "election_year","election_date")) %>%
  mutate(
    bias = "gerontocracy",
    category = case_when(
      age_group == 1 ~ "bot-age-50",
      age_group == 2 ~ "top-age-50"
    )
  ) %>%
  select(-age_group)


##CSES age 10 ----
cses_data_clean_age_10 <- cses_data_clean %>%
  filter(age < 9997)

build_votes_by_age_fractional_10 <- function(df){
  
  # 1. Distribution pondérée de l'âge
  dist_age <- df %>%
    group_by(age) %>%
    summarise(
      n = sum(weight, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(age) %>%
    mutate(
      pct = n / sum(n) * 100,
      cum_pct = cumsum(pct),
      cum_prev = lag(cum_pct, default = 0)
    )
  
  # Deux groupes : jeunes / âgés
  age_groups <- data.frame(
    age_group = 1:10,
    lower = seq(0, 90, by = 10),
    upper = seq(10, 100, by = 10)
  )
  
  
  # 2. Poids de répartition
  weights_matrix <- dist_age %>%
    crossing(age_groups) %>%
    mutate(
      overlap = pmax(0, pmin(cum_pct, upper) - pmax(cum_prev, lower)),
      weight_group = overlap / (cum_pct - cum_prev)
    ) %>%
    filter(weight_group > 0)
  
  # 3. Attribution des groupes
  df_groups <- df %>%
    left_join(
      weights_matrix %>%
        select(age, age_group, weight_group),
      by = "age",
      relationship = "many-to-many"
    ) %>%
    filter(!is.na(weight_group)) %>%
    mutate(
      weight_final = weight * weight_group,
      vote = case_when(
        is.na(dataset_party_id) ~ NA_character_,
        str_detect(dataset_party_id, "Abstention$") ~ "Abstention",
        TRUE ~ dataset_party_id
      )
    )
  
  # 4. Votes pondérés
  votes <- df_groups %>%
    group_by(isoname, election_year,election_date, age_group, vote, partyfacts_id) %>%
    summarise(
      votes = sum(weight_final, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    group_by(isoname, election_year,election_date,age_group) %>%
    mutate(
      pct_votes = votes / sum(votes) * 100
    ) %>%
    ungroup()
  
  # 5. Participation pondérée
  participation <- df_groups %>%
    group_by(isoname, election_year, election_date,age_group) %>%
    summarise(
      nbr_obs = sum(weight_final, na.rm = TRUE),
      votes_valides = sum(
        weight_final * (!str_detect(dataset_party_id, "Abstention$")),
        na.rm = TRUE
      ),
      taux_participation = votes_valides / nbr_obs * 100,
      .groups = "drop"
    )
  
  # 6. Output
  votes %>%
    left_join(
      participation,
      by = c( "isoname",
              "election_year",
              "election_date",
              "age_group")
    )
}

# Construction de la base finale

cses_dataset_age_10 <- cses_data_clean_age_10 %>%
  group_by(isoname, election_year,election_date) %>%
  group_split() %>%
  map_dfr(~ build_votes_by_age_fractional_10(.x)) %>%
  left_join(meta_info_cses, by = c("isoname", "election_year","election_date")) %>%
  mutate(
    bias = "gerontocracy",
    category = case_when(
      age_group == 1 ~ "bot-age-10",
      age_group == 10 ~ "top-age-10"
    )
  ) %>%
  select(-age_group)

Base_cses_age <- bind_rows(cses_dataset_age_10,cses_dataset_age_50)
Base_cses_age <- Base_cses_age %>% filter(!is.na(category))
Base_cses_age <- Base_cses_age %>% arrange(isoname,election_year,source)
unique(Base_cses_age$category)


#WVS ----
wvs_data_clean <- Base_all_sources %>%
  filter(Base_all_sources$source_recode == "WVS")

wvs_data_clean <- wvs_data_clean %>%
  filter(wvs_data_clean$partyfacts_id != "Other")

#WVS income 10 ----  
wvs_data_clean_income_10 <- wvs_data_clean %>%
  filter(inc >= 1)
build_wvs_base_long_10 <- function(df, annee, country){
  
  # sécurité
  election_year <- unique(df$election_year)[1]
  election_date <- unique(df$election_date)[1]
  survey <- unique(df$survey)[1]
  type <- unique(df$type)[1]
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
    filter(!is.na(weight_decile)) %>%
    mutate(weight_final = weight * weight_decile)
  
  
  # 3. participation
  nbr_obs <- df_deciles %>%
    group_by(isoname, election_year, election_date,decile) %>%
    summarise(
      nbr_obs = sum(weight_final, na.rm = TRUE),
      .groups = "drop"
    )
  
  votes_valides <- df_deciles %>%
    filter(!str_detect(dataset_party_id, "Abstention$")) %>%
    group_by(isoname,election_year, election_date,decile) %>%
    summarise(
      votes_valides = sum(weight_final, na.rm = TRUE),
      .groups = "drop"
    )
  
  participation <- nbr_obs %>%
    left_join(votes_valides, by = c("isoname","election_year","election_date","decile")) %>%
    mutate(
      taux_participation = votes_valides / nbr_obs * 100
    )
  
  
  # 4. votes
  
  votes <- df_deciles %>%
    mutate(
      vote = case_when(
        is.na(dataset_party_id) ~ NA_character_,
        str_detect(dataset_party_id, "Abstention$") ~ "Abstention",
        TRUE ~ dataset_party_id
      )
    ) %>%
    group_by(isoname, election_year, election_date, decile, vote, partyfacts_id) %>%
    summarise(votes = sum(weight_final,na.rm = TRUE), .groups = "drop") %>%
    group_by(isoname, election_year, election_date, decile) %>%
    mutate(pct_votes = votes / sum(votes) * 100)
  
  
  # 5. output
  
  votes %>%
    left_join(participation, by = c("isoname", "election_year", "election_date", "decile")) %>%
    mutate(
      annee = annee,
      election_year = election_year,
      election_date = election_date,
      type = type,
      isoname = country,
      survey = survey,
      source = source,
      source_recode = source_recode
    ) %>%
    relocate(isoname, annee, election_year, election_date,vote, decile, survey,type)
}


# PIPELINE MULTI-PAYS


wvs_dataset_income_10 <- wvs_data_clean_income_10 %>%
  group_by(isoname, election_year,election_date) %>%
  group_split() %>%
  map_dfr(~ build_wvs_base_long_10(.x, unique(.x$election_year), unique(.x$isoname)))
wvs_dataset_income_10 <- wvs_dataset_income_10 %>%
  mutate(bias = "plutocracy")
wvs_dataset_income_10 <- wvs_dataset_income_10 %>%
  rename(category = decile)
wvs_dataset_income_10 <- wvs_dataset_income_10 %>%
  mutate(
    category = case_when(
      category == 1 ~ "bot-income-10",
      category == 10 ~ "top-income-10",
      TRUE ~ NA_character_))

unique(wvs_dataset_income_10$category)

##WVS income 50 ----
wvs_data_clean_income_50 <- wvs_data_clean %>%
  filter(inc >= 1)
build_wvs_base_long_50 <- function(df, annee, country){
  
  # sécurité
  election_year <- unique(df$election_year)[1]
  election_date <- unique(df$election_date)[1]
  survey <- unique(df$survey)[1]
  type <- unique(df$type)[1]
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
    decile = c(1, 2),
    lower = c(0, 50),
    upper = c(50, 100)
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
    filter(!is.na(weight_decile)) %>%
    mutate(weight_final = weight * weight_decile)
  
  
  # 3. participation
  nbr_obs <- df_deciles %>%
    group_by(isoname, election_year, election_date,decile) %>%
    summarise(
      nbr_obs = sum(weight_final, na.rm = TRUE),
      .groups = "drop"
    )
  
  votes_valides <- df_deciles %>%
    filter(!str_detect(dataset_party_id, "Abstention$")) %>%
    group_by(isoname,election_year, election_date,decile) %>%
    summarise(
      votes_valides = sum(weight_final, na.rm = TRUE),
      .groups = "drop"
    )
  
  participation <- nbr_obs %>%
    left_join(votes_valides, by = c("isoname","election_year","election_date","decile")) %>%
    mutate(
      taux_participation = votes_valides / nbr_obs * 100
    )
  
  
  # 4. votes
  
  votes <- df_deciles %>%
    mutate(
      vote = case_when(
        is.na(dataset_party_id) ~ NA_character_,
        str_detect(dataset_party_id, "Abstention$") ~ "Abstention",
        TRUE ~ dataset_party_id
      )
    ) %>%
    group_by(isoname, election_year, election_date, decile, vote, partyfacts_id) %>%
    summarise(votes = sum(weight_final,na.rm = TRUE), .groups = "drop") %>%
    group_by(isoname, election_year, election_date, decile) %>%
    mutate(pct_votes = votes / sum(votes) * 100)
  
  
  # 5. output
  
  votes %>%
    left_join(participation, by = c("isoname", "election_year", "election_date", "decile")) %>%
    mutate(
      annee = annee,
      election_year = election_year,
      election_date = election_date,
      type = type,
      isoname = country,
      survey = survey,
      source = source,
      source_recode = source_recode
    ) %>%
    relocate(isoname, annee, election_year, election_date,vote, decile, survey,type)
}


# PIPELINE MULTI-PAYS


wvs_dataset_income_50 <- wvs_data_clean_income_50 %>%
  group_by(isoname, election_year,election_date) %>%
  group_split() %>%
  map_dfr(~ build_wvs_base_long_50(.x, unique(.x$election_year), unique(.x$isoname)))
wvs_dataset_income_50 <- wvs_dataset_income_50 %>%
  mutate(bias = "plutocracy")
wvs_dataset_income_50 <- wvs_dataset_income_50 %>%
  rename(category = decile)
wvs_dataset_income_50 <- wvs_dataset_income_50 %>%
  mutate(
    category = case_when(
      category == 1 ~ "bot-income-50",
      category == 2 ~ "top-income-50",
      TRUE ~ NA_character_))

unique(wvs_dataset_income_50$category)

Base_wvs_income <- bind_rows(wvs_dataset_income_10, wvs_dataset_income_50)
Base_wvs_income <- Base_wvs_income %>% filter(!is.na(category))
Base_wvs_income <- Base_wvs_income %>% arrange(isoname,election_year,source)
Base_wvs_income <- Base_wvs_income %>% select(-annee)
unique(Base_wvs_income$category)

##WVS gender ---
wvs_data_clean_sexe <- wvs_data_clean %>%
  filter(gender %in% c("men", "women"))

###WVS gender ----

build_votes_by_gender <- function(df){
  
  df <- df %>%
    mutate(
      vote = case_when(
        is.na(dataset_party_id) ~ NA_character_,
        str_detect(dataset_party_id, "Abstention$") ~ "Abstention",
        TRUE ~ as.character(dataset_party_id)
      )
    )
  
  # Votes pondérés par sexe × parti
  votes <- df %>%
    group_by(isoname, election_year,election_date, gender, vote, partyfacts_id) %>%
    summarise(
      votes = sum(weight, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    group_by(isoname, election_year,election_date, gender) %>%
    mutate(
      pct_votes = votes / sum(votes) * 100
    ) %>%
    ungroup()
  
  # Participation pondérée
  participation <- df %>%
    group_by(isoname, election_year,election_date, gender) %>%
    summarise(
      nbr_obs = sum(weight, na.rm = TRUE),
      votes_valides = sum(
        weight * (!str_detect(dataset_party_id, "Abstention$")),
        na.rm = TRUE
      ),
      taux_participation = votes_valides / nbr_obs * 100,
      .groups = "drop"
    )
  
  votes %>%
    left_join(
      participation,
      by = c("isoname", "election_year","election_date", "gender")
    )
}

# Informations de contexte
meta_info_wvs <- wvs_data_clean %>%
  distinct(isoname, election_year,election_date, survey, source, source_recode,type)

# Construction de la base
Base_wvs_gender <- wvs_data_clean_sexe %>%
  group_by(isoname, election_year,election_date) %>%
  group_split() %>%
  map_dfr(~ build_votes_by_gender(.x)) %>%
  left_join(meta_info_wvs, by =  c("isoname", "election_year","election_date")) %>%
  mutate(
    bias = "androcracy"
  ) %>%
  rename(category = gender)





##WVS educ 50 ----
wvs_data_clean_educ_50 <- wvs_data_clean %>%
  filter(educ >= 1)

###WVS calcul fonction educ ----
build_votes_by_educ_fractional_50 <- function(df){
  
  # 1. Distribution pondérée de l'éducation
  dist_educ <- df %>%
    group_by(educ) %>%
    summarise(
      n = sum(weight, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(educ) %>%
    mutate(
      pct = n / sum(n) * 100,
      cum_pct = cumsum(pct),
      cum_prev = lag(cum_pct, default = 0)
    )
  
  # Deux groupes : bas / haut niveau d'éducation
  educ_groups <- data.frame(
    educ_group = c(1, 2),
    lower = c(0, 50),
    upper = c(50, 100)
  )
  
  # 2. Calcul des poids de répartition
  weights_matrix <- dist_educ %>%
    crossing(educ_groups) %>%
    mutate(
      overlap = pmax(0, pmin(cum_pct, upper) - pmax(cum_prev, lower)),
      weight_group = overlap / (cum_pct - cum_prev)
    ) %>%
    filter(weight_group > 0)
  
  # 3. Attribution des groupes
  df_groups <- df %>%
    left_join(
      weights_matrix %>%
        select(educ, educ_group, weight_group),
      by = "educ",
      relationship = "many-to-many"
    ) %>%
    filter(!is.na(weight_group)) %>%
    mutate(
      weight_final = weight * weight_group,
      vote = case_when(
        is.na(dataset_party_id) ~ NA_character_,
        str_detect(dataset_party_id, "Abstention$") ~ "Abstention",
        TRUE ~ dataset_party_id
      )
    )
  
  # 4. Votes pondérés
  votes <- df_groups %>%
    group_by(isoname, election_year,election_date, educ_group, vote, partyfacts_id) %>%
    summarise(
      votes = sum(weight_final, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    group_by(isoname,election_year,election_date,educ_group) %>%
    mutate(
      pct_votes = votes / sum(votes) * 100
    ) %>%
    ungroup()
  
  # 5. Participation pondérée
  participation <- df_groups %>%
    group_by(isoname,election_year,election_date,educ_group) %>%
    summarise(
      nbr_obs = sum(weight_final, na.rm = TRUE),
      votes_valides = sum(
        weight_final * (!str_detect(dataset_party_id, "Abstention$")),
        na.rm = TRUE
      ),
      taux_participation = votes_valides / nbr_obs * 100,
      .groups = "drop"
    )
  
  # 6. Output
  votes %>%
    left_join(
      participation,
      by = c(
        "isoname",
        "election_year",
        "election_date",
        "educ_group"))
    
}

# Construction de la base finale

wvs_dataset_educ_50 <- wvs_data_clean_educ_50 %>%
  group_by(isoname, election_year,election_date) %>%
  group_split() %>%
  map_dfr(~ build_votes_by_educ_fractional_50(.x)) %>%
  left_join(meta_info_wvs, by = c("isoname", "election_year","election_date")) %>%
  mutate(
    bias = "epistocracy",
    category = case_when(
      educ_group == 1 ~ "bot-educ-50",
      educ_group == 2 ~ "top-educ-50"
    )
  ) %>%
  select(-educ_group)

##WVS educ 10 ----
wvs_data_clean_educ_10 <- wvs_data_clean %>%
  filter(educ >= 1)

###WVS calcul fonction educ ----
build_votes_by_educ_fractional_10 <- function(df){
  
  # 1. Distribution pondérée de l'éducation
  dist_educ <- df %>%
    group_by(educ) %>%
    summarise(
      n = sum(weight, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(educ) %>%
    mutate(
      pct = n / sum(n) * 100,
      cum_pct = cumsum(pct),
      cum_prev = lag(cum_pct, default = 0)
    )
  
  # Deux groupes : bas / haut niveau d'éducation
  educ_groups <- data.frame(
    educ_group = 1:10,
    lower = seq(0, 90, by = 10),
    upper = seq(10, 100, by = 10)
  )
  
  # 2. Calcul des poids de répartition
  weights_matrix <- dist_educ %>%
    crossing(educ_groups) %>%
    mutate(
      overlap = pmax(0, pmin(cum_pct, upper) - pmax(cum_prev, lower)),
      weight_group = overlap / (cum_pct - cum_prev)
    ) %>%
    filter(weight_group > 0)
  
  # 3. Attribution des groupes
  df_groups <- df %>%
    left_join(
      weights_matrix %>%
        select(educ, educ_group, weight_group),
      by = "educ",
      relationship = "many-to-many"
    ) %>%
    filter(!is.na(weight_group)) %>%
    mutate(
      weight_final = weight * weight_group,
      vote = case_when(
        is.na(dataset_party_id) ~ NA_character_,
        str_detect(dataset_party_id, "Abstention$") ~ "Abstention",
        TRUE ~ dataset_party_id
      )
    )
  
  # 4. Votes pondérés
  votes <- df_groups %>%
    group_by(isoname, election_year,election_date, educ_group, vote, partyfacts_id) %>%
    summarise(
      votes = sum(weight_final, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    group_by(isoname,election_year,election_date,educ_group) %>%
    mutate(
      pct_votes = votes / sum(votes) * 100
    ) %>%
    ungroup()
  
  # 5. Participation pondérée
  participation <- df_groups %>%
    group_by(isoname,election_year,election_date,educ_group) %>%
    summarise(
      nbr_obs = sum(weight_final, na.rm = TRUE),
      votes_valides = sum(
        weight_final * (!str_detect(dataset_party_id, "Abstention$")),
        na.rm = TRUE
      ),
      taux_participation = votes_valides / nbr_obs * 100,
      .groups = "drop"
    )
  
  # 6. Output
  votes %>%
    left_join(
      participation,
      by = c(
        "isoname",
        "election_year",
        "election_date",
        "educ_group"))
  
}

# Construction de la base finale

wvs_dataset_educ_10 <- wvs_data_clean_educ_10 %>%
  group_by(isoname, election_year,election_date) %>%
  group_split() %>%
  map_dfr(~ build_votes_by_educ_fractional_10(.x)) %>%
  left_join(meta_info_wvs, by = c("isoname", "election_year","election_date")) %>%
  mutate(
    bias = "epistocracy",
    category = case_when(
      educ_group == 1 ~ "bot-educ-10",
      educ_group == 10 ~ "top-educ-10"
    )
  ) %>%
  select(-educ_group)


Base_wvs_educ <- bind_rows(wvs_dataset_educ_10, wvs_dataset_educ_50)
Base_wvs_educ <- Base_wvs_educ %>% filter(!is.na(category))
Base_wvs_educ <- Base_wvs_educ %>% arrange(isoname,election_year,source)
unique(Base_wvs_educ$category)


##WVS age 50 ----
wvs_data_clean_age_50 <- wvs_data_clean %>%
  filter(age >= 1)

build_votes_by_age_fractional_50 <- function(df){
  
  # 1. Distribution pondérée de l'âge
  dist_age <- df %>%
    group_by(age) %>%
    summarise(
      n = sum(weight, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(age) %>%
    mutate(
      pct = n / sum(n) * 100,
      cum_pct = cumsum(pct),
      cum_prev = lag(cum_pct, default = 0)
    )
  
  # Deux groupes : jeunes / âgés
  age_groups <- data.frame(
    age_group = c(1, 2),
    lower = c(0, 50),
    upper = c(50, 100)
  )
  
  # 2. Poids de répartition
  weights_matrix <- dist_age %>%
    crossing(age_groups) %>%
    mutate(
      overlap = pmax(0, pmin(cum_pct, upper) - pmax(cum_prev, lower)),
      weight_group = overlap / (cum_pct - cum_prev)
    ) %>%
    filter(weight_group > 0)
  
  # 3. Attribution des groupes
  df_groups <- df %>%
    left_join(
      weights_matrix %>%
        select(age, age_group, weight_group),
      by = "age",
      relationship = "many-to-many"
    ) %>%
    filter(!is.na(weight_group)) %>%
    mutate(
      weight_final = weight * weight_group,
      vote = case_when(
        is.na(dataset_party_id) ~ NA_character_,
        str_detect(dataset_party_id, "Abstention$") ~ "Abstention",
        TRUE ~ dataset_party_id
      )
    )
  
  # 4. Votes pondérés
  votes <- df_groups %>%
    group_by(isoname, election_year,election_date, age_group, vote, partyfacts_id) %>%
    summarise(
      votes = sum(weight_final, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    group_by(isoname, election_year, election_date,age_group) %>%
    mutate(
      pct_votes = votes / sum(votes) * 100
    ) %>%
    ungroup()
  
  # 5. Participation pondérée
  participation <- df_groups %>%
    group_by(isoname,election_year,election_date,age_group) %>%
    summarise(
      nbr_obs = sum(weight_final, na.rm = TRUE),
      votes_valides = sum(
        weight_final * (!str_detect(dataset_party_id, "Abstention$")),
        na.rm = TRUE
      ),
      taux_participation = votes_valides / nbr_obs * 100,
      .groups = "drop"
    )
  
  # 6. Output
  votes %>%
    left_join(
      participation,
      by = c("isoname", "election_year","election_date", "age_group")
    )
}

# Construction de la base finale

wvs_dataset_age_50 <- wvs_data_clean_age_50 %>%
  group_by(isoname, election_year,election_date) %>%
  group_split() %>%
  map_dfr(~ build_votes_by_age_fractional_50(.x)) %>%
  left_join(meta_info_wvs, by = c("isoname", "election_year","election_date")) %>%
  mutate(
    bias = "gerontocracy",
    category = case_when(
      age_group == 1 ~ "bot-age-50",
      age_group == 2 ~ "top-age-50"
    )
  ) %>%
  select(-age_group)


##WVS age 10 ----
wvs_data_clean_age_10 <- wvs_data_clean %>%
  filter(age >= 1)

build_votes_by_age_fractional_10 <- function(df){
  
  # 1. Distribution pondérée de l'âge
  dist_age <- df %>%
    group_by(age) %>%
    summarise(
      n = sum(weight, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(age) %>%
    mutate(
      pct = n / sum(n) * 100,
      cum_pct = cumsum(pct),
      cum_prev = lag(cum_pct, default = 0)
    )
  
  # Deux groupes : jeunes / âgés
  age_groups <- data.frame(
    age_group = 1:10,
    lower = seq(0, 90, by = 10),
    upper = seq(10, 100, by = 10)
  )
  
  # 2. Poids de répartition
  weights_matrix <- dist_age %>%
    crossing(age_groups) %>%
    mutate(
      overlap = pmax(0, pmin(cum_pct, upper) - pmax(cum_prev, lower)),
      weight_group = overlap / (cum_pct - cum_prev)
    ) %>%
    filter(weight_group > 0)
  
  # 3. Attribution des groupes
  df_groups <- df %>%
    left_join(
      weights_matrix %>%
        select(age, age_group, weight_group),
      by = "age",
      relationship = "many-to-many"
    ) %>%
    filter(!is.na(weight_group)) %>%
    mutate(
      weight_final = weight * weight_group,
      vote = case_when(
        is.na(dataset_party_id) ~ NA_character_,
        str_detect(dataset_party_id, "Abstention$") ~ "Abstention",
        TRUE ~ dataset_party_id
      )
    )
  
  # 4. Votes pondérés
  votes <- df_groups %>%
    group_by(isoname, election_year,election_date, age_group, vote, partyfacts_id) %>%
    summarise(
      votes = sum(weight_final, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    group_by(isoname, election_year, election_date,age_group) %>%
    mutate(
      pct_votes = votes / sum(votes) * 100
    ) %>%
    ungroup()
  
  # 5. Participation pondérée
  participation <- df_groups %>%
    group_by(isoname,election_year,election_date,age_group) %>%
    summarise(
      nbr_obs = sum(weight_final, na.rm = TRUE),
      votes_valides = sum(
        weight_final * (!str_detect(dataset_party_id, "Abstention$")),
        na.rm = TRUE
      ),
      taux_participation = votes_valides / nbr_obs * 100,
      .groups = "drop"
    )
  
  # 6. Output
  votes %>%
    left_join(
      participation,
      by = c("isoname", "election_year","election_date", "age_group")
    )
}

# Construction de la base finale

wvs_dataset_age_10 <- wvs_data_clean_age_10 %>%
  group_by(isoname, election_year,election_date) %>%
  group_split() %>%
  map_dfr(~ build_votes_by_age_fractional_10(.x)) %>%
  left_join(meta_info_wvs, by = c("isoname", "election_year","election_date")) %>%
  mutate(
    bias = "gerontocracy",
    category = case_when(
      age_group == 1 ~ "bot-age-10",
      age_group == 10 ~ "top-age-10"
    )
  ) %>%
  select(-age_group)

Base_wvs_age <- bind_rows(wvs_dataset_age_10, wvs_dataset_age_50)
Base_wvs_age <- Base_wvs_age %>% filter(!is.na(category))
Base_wvs_age <- Base_wvs_age %>% arrange(isoname,election_year,source)
unique(Base_wvs_age$category)



#ESS ----
ess_data_clean <- Base_all_sources %>%
  filter(Base_all_sources$source_recode == "ESS")

ess_data_clean <- ess_data_clean %>%
  filter(ess_data_clean$partyfacts_id != "Other")

ess_data_clean <- ess_data_clean %>%
  mutate(
    partyfacts_id = case_when(
      dataset_party_id == "Abstention" ~ "Abstention",
      TRUE ~ as.character(partyfacts_id)))
      
      
sum(ess_data_clean$partyfacts_id == "Abstention", na.rm = TRUE)


#ESS income 10 ----
ess_data_clean_income_10 <- ess_data_clean %>%
  filter(inc < 77)

build_ess_base_long_10 <- function(df, annee, country){
  
  # sécurité
  election_year <- unique(df$election_year)[1]
  election_date <- unique(df$election_date)[1]
  type <- unique(df$type)[1]
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
    filter(!is.na(weight_decile)) %>%
    mutate(weight_final = weight * weight_decile)
  
  
  # 3. participation
  nbr_obs <- df_deciles %>%
    group_by(isoname, election_year, election_date,decile) %>%
    summarise(
      nbr_obs = sum(weight_final, na.rm = TRUE),
      .groups = "drop"
    )
  
  votes_valides <- df_deciles %>%
    filter(!str_detect(dataset_party_id, "Abstention$")) %>%
    group_by(isoname,election_year, election_date,decile) %>%
    summarise(
      votes_valides = sum(weight_final, na.rm = TRUE),
      .groups = "drop"
    )
  
  participation <- nbr_obs %>%
    left_join(votes_valides, by = c("isoname","election_year","election_date","decile")) %>%
    mutate(
      taux_participation = votes_valides / nbr_obs * 100
    )
  
  
  # 4. votes
  
  votes <- df_deciles %>%
    mutate(
      vote = case_when(
        is.na(dataset_party_id) ~ NA_character_,
        str_detect(dataset_party_id, "Abstention$") ~ "Abstention",
        TRUE ~ dataset_party_id
      )
    ) %>%
    group_by(isoname, election_year, election_date, decile, vote, partyfacts_id) %>%
    summarise(votes = sum(weight_final,na.rm = TRUE), .groups = "drop") %>%
    group_by(isoname, election_year, election_date, decile) %>%
    mutate(pct_votes = votes / sum(votes) * 100)
  
  
  # 5. output
  
  votes %>%
    left_join(participation, by = c("isoname", "election_year", "election_date", "decile")) %>%
    mutate(
      annee = annee,
      election_year = election_year,
      election_date = election_date,
      type = type,
      isoname = country,
      survey = survey,
      source = source,
      source_recode = source_recode
    ) %>%
    relocate(isoname, annee, election_year,election_date, vote, decile, survey,type)
}


# PIPELINE MULTI-PAYS


ess_dataset_income_10 <- ess_data_clean_income_10 %>%
  group_by(isoname, election_year,election_date) %>%
  group_split() %>%
  map_dfr(~ build_ess_base_long_10(.x, unique(.x$election_year), unique(.x$isoname)))
ess_dataset_income_10 <- ess_dataset_income_10 %>%
  mutate(bias = "plutocracy")
ess_dataset_income_10 <- ess_dataset_income_10 %>%
  rename(category = decile)

ess_dataset_income_10 <- ess_dataset_income_10 %>%
  mutate(
    category = case_when(
      category == 1 ~ "bot-income-10",
      category == 10 ~ "top-income-10",
      TRUE ~ NA_character_))

unique(ess_dataset_income_10$category)

##ESS income 50 ----
ess_data_clean_income_50 <- ess_data_clean %>%
  filter(inc < 77)

build_ess_base_long_50 <- function(df, annee, country){
  
  # sécurité
  election_year <- unique(df$election_year)[1]
  election_date <- unique(df$election_date)[1]
  type <- unique(df$type)[1]
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
    decile = c(1, 2),
    lower = c(0, 50),
    upper = c(50, 100)
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
    filter(!is.na(weight_decile)) %>%
    mutate(weight_final = weight * weight_decile)
  
  
  # 3. participation
  nbr_obs <- df_deciles %>%
    group_by(isoname, election_year, election_date,decile) %>%
    summarise(
      nbr_obs = sum(weight_final, na.rm = TRUE),
      .groups = "drop"
    )
  
  votes_valides <- df_deciles %>%
    filter(!str_detect(dataset_party_id, "Abstention$")) %>%
    group_by(isoname,election_year, election_date,decile) %>%
    summarise(
      votes_valides = sum(weight_final, na.rm = TRUE),
      .groups = "drop"
    )
  
  participation <- nbr_obs %>%
    left_join(votes_valides, by = c("isoname","election_year","election_date","decile")) %>%
    mutate(
      taux_participation = votes_valides / nbr_obs * 100
    )
  
  
  # 4. votes
  
  votes <- df_deciles %>%
    mutate(
      vote = case_when(
        is.na(dataset_party_id) ~ NA_character_,
        str_detect(dataset_party_id, "Abstention$") ~ "Abstention",
        TRUE ~ dataset_party_id
      )
    ) %>%
    group_by(isoname, election_year, election_date, decile, vote, partyfacts_id) %>%
    summarise(votes = sum(weight_final,na.rm = TRUE), .groups = "drop") %>%
    group_by(isoname, election_year, election_date, decile) %>%
    mutate(pct_votes = votes / sum(votes) * 100)
  
  
  # 5. output
  
  votes %>%
    left_join(participation, by = c("isoname", "election_year", "election_date", "decile")) %>%
    mutate(
      annee = annee,
      election_year = election_year,
      election_date = election_date,
      type = type,
      isoname = country,
      survey = survey,
      source = source,
      source_recode = source_recode
    ) %>%
    relocate(isoname, annee, election_year,election_date, vote, decile, survey,type)
}


# PIPELINE MULTI-PAYS


ess_dataset_income_50 <- ess_data_clean_income_50 %>%
  group_by(isoname, election_year,election_date) %>%
  group_split() %>%
  map_dfr(~ build_ess_base_long_50(.x, unique(.x$election_year), unique(.x$isoname)))
ess_dataset_income_50 <- ess_dataset_income_50 %>%
  mutate(bias = "plutocracy")
ess_dataset_income_50 <- ess_dataset_income_50 %>%
  rename(category = decile)

ess_dataset_income_50 <- ess_dataset_income_50 %>%
  mutate(
    category = case_when(
      category == 1 ~ "bot-income-50",
      category == 2 ~ "top-income-50",
      TRUE ~ NA_character_))

unique(ess_dataset_income_50$category)

Base_ess_income <- bind_rows(ess_dataset_income_10, ess_dataset_income_50)
Base_ess_income <- Base_ess_income %>% filter(!is.na(category))
Base_ess_income <- Base_ess_income %>% arrange(isoname,election_year,source)
Base_ess_income <- Base_ess_income %>% select(-annee)
unique(Base_ess_income$category)


##ESS gender ----
ess_data_clean_sexe <- ess_data_clean %>%
  filter(gender %in% c("men", "women"))

###Calcul ESS gender ----

build_votes_by_gender <- function(df){
  
  df <- df %>%
    mutate(
      vote = case_when(
        is.na(dataset_party_id) ~ NA_character_,
        str_detect(dataset_party_id, "Abstention$") ~ "Abstention",
        TRUE ~ as.character(dataset_party_id)
      )
    )
  
  # Votes pondérés par sexe × parti
  votes <- df %>%
    group_by(isoname, source,election_year,election_date, gender, vote, partyfacts_id) %>%
    summarise(
      votes = sum(weight, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    group_by(isoname,source, election_year,election_date, gender) %>%
    mutate(
      pct_votes = votes / sum(votes) * 100
    ) %>%
    ungroup()
  
  # Participation pondérée
  participation <- df %>%
    group_by(isoname, source,election_year,election_date, gender) %>%
    summarise(
      nbr_obs = sum(weight, na.rm = TRUE),
      votes_valides = sum(
        weight * (!str_detect(dataset_party_id, "Abstention$")),
        na.rm = TRUE
      ),
      taux_participation = votes_valides / nbr_obs * 100,
      .groups = "drop"
    )
  
  votes %>%
    left_join(
      participation,
      by = c("isoname","source", "election_year","election_date", "gender")
    )
}

# Informations de contexte
meta_info_ess <- ess_data_clean %>%
  distinct(isoname, election_year,election_date, survey, source, source_recode,type)

# Construction de la base
Base_ess_gender <- ess_data_clean_sexe %>%
  group_by(isoname, election_year,election_date,source) %>%
  group_split() %>%
  map_dfr(~ build_votes_by_gender(.x)) %>%
  left_join(meta_info_ess, by = c("isoname", "election_year","election_date","source")) %>%
  mutate(
    bias = "androcracy"
  ) %>%
  rename(category = gender)




##ESS educ 50 ----
ess_data_clean_educ_50 <- ess_data_clean %>% filter(!is.na(educ))#vérifier à quoi correspond educ = 55

build_votes_by_educ_fractional_50 <- function(df){
  
  # 1. Distribution pondérée de l'éducation
  dist_educ <- df %>%
    group_by(educ) %>%
    summarise(
      n = sum(weight, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(educ) %>%
    mutate(
      pct = n / sum(n) * 100,
      cum_pct = cumsum(pct),
      cum_prev = lag(cum_pct, default = 0)
    )
  
  # Deux groupes : bas / haut niveau d'éducation
  educ_groups <- data.frame(
    educ_group = c(1, 2),
    lower = c(0, 50),
    upper = c(50, 100)
  )
  
  # 2. Calcul des poids de répartition
  weights_matrix <- dist_educ %>%
    crossing(educ_groups) %>%
    mutate(
      overlap = pmax(0, pmin(cum_pct, upper) - pmax(cum_prev, lower)),
      weight_group = overlap / (cum_pct - cum_prev)
    ) %>%
    filter(weight_group > 0)
  
  # 3. Attribution des groupes
  df_groups <- df %>%
    left_join(
      weights_matrix %>%
        select(educ, educ_group, weight_group),
      by = "educ",
      relationship = "many-to-many"
    ) %>%
    filter(!is.na(weight_group)) %>%
    mutate(
      weight_final = weight * weight_group,
      vote = case_when(
        is.na(dataset_party_id) ~ NA_character_,
        str_detect(dataset_party_id, "Abstention$") ~ "Abstention",
        TRUE ~ dataset_party_id
      )
    )
  
  # 4. Votes pondérés
  votes <- df_groups %>%
    group_by(isoname,source, election_year,election_date, educ_group, vote, partyfacts_id) %>%
    summarise(
      votes = sum(weight_final, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    group_by(isoname,source, election_year, election_date,educ_group) %>%
    mutate(
      pct_votes = votes / sum(votes) * 100
    ) %>%
    ungroup()
  
  # 5. Participation pondérée
  participation <- df_groups %>%
    group_by(isoname, source,election_year, election_date,educ_group) %>%
    summarise(
      nbr_obs = sum(weight_final, na.rm = TRUE),
      votes_valides = sum(
        weight_final * (!str_detect(dataset_party_id, "Abstention$")),
        na.rm = TRUE
      ),
      taux_participation = votes_valides / nbr_obs * 100,
      .groups = "drop"
    )
  
  # 6. Output
  votes %>%
    left_join(
      participation,
      by = c(
        "isoname",
        "source",
        "election_year",
        "election_date",
        "educ_group"
      )
    )
}

# Construction de la base finale

ess_dataset_educ_50 <- ess_data_clean_educ_50 %>%
  group_by(isoname, election_year,election_date,source) %>%
  group_split() %>%
  map_dfr(~ build_votes_by_educ_fractional_50(.x)) %>%
  left_join(meta_info_ess, by = c("isoname", "election_year","election_date","source")) %>%
  mutate(
    bias = "epistocracy",
    category = case_when(
      educ_group == 1 ~ "bot-educ-50",
      educ_group == 2 ~ "top-educ-50"
    )
  ) %>%
  select(-educ_group)


##ESS educ 10 ----
ess_data_clean_educ_10 <- ess_data_clean %>% filter(!is.na(educ))#vérifier à quoi correspond educ = 55

build_votes_by_educ_fractional_10 <- function(df){
  
  # 1. Distribution pondérée de l'éducation
  dist_educ <- df %>%
    group_by(educ) %>%
    summarise(
      n = sum(weight, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(educ) %>%
    mutate(
      pct = n / sum(n) * 100,
      cum_pct = cumsum(pct),
      cum_prev = lag(cum_pct, default = 0)
    )
  
  # Deux groupes : bas / haut niveau d'éducation
  educ_groups <- data.frame(
    educ_group = 1:10,
    lower = seq(0, 90, by = 10),
    upper = seq(10, 100, by = 10)
  )
  
  # 2. Calcul des poids de répartition
  weights_matrix <- dist_educ %>%
    crossing(educ_groups) %>%
    mutate(
      overlap = pmax(0, pmin(cum_pct, upper) - pmax(cum_prev, lower)),
      weight_group = overlap / (cum_pct - cum_prev)
    ) %>%
    filter(weight_group > 0)
  
  # 3. Attribution des groupes
  df_groups <- df %>%
    left_join(
      weights_matrix %>%
        select(educ, educ_group, weight_group),
      by = "educ",
      relationship = "many-to-many"
    ) %>%
    filter(!is.na(weight_group)) %>%
    mutate(
      weight_final = weight * weight_group,
      vote = case_when(
        is.na(dataset_party_id) ~ NA_character_,
        str_detect(dataset_party_id, "Abstention$") ~ "Abstention",
        TRUE ~ dataset_party_id
      )
    )
  
  # 4. Votes pondérés
  votes <- df_groups %>%
    group_by(isoname,source, election_year,election_date, educ_group, vote, partyfacts_id) %>%
    summarise(
      votes = sum(weight_final, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    group_by(isoname,source, election_year, election_date,educ_group) %>%
    mutate(
      pct_votes = votes / sum(votes) * 100
    ) %>%
    ungroup()
  
  # 5. Participation pondérée
  participation <- df_groups %>%
    group_by(isoname, source,election_year, election_date,educ_group) %>%
    summarise(
      nbr_obs = sum(weight_final, na.rm = TRUE),
      votes_valides = sum(
        weight_final * (!str_detect(dataset_party_id, "Abstention$")),
        na.rm = TRUE
      ),
      taux_participation = votes_valides / nbr_obs * 100,
      .groups = "drop"
    )
  
  # 6. Output
  votes %>%
    left_join(
      participation,
      by = c(
        "isoname",
        "source",
        "election_year",
        "election_date",
        "educ_group"
      )
    )
}

# Construction de la base finale

ess_dataset_educ_10 <- ess_data_clean_educ_10 %>%
  group_by(isoname, election_year,election_date,source) %>%
  group_split() %>%
  map_dfr(~ build_votes_by_educ_fractional_10(.x)) %>%
  left_join(meta_info_ess, by = c("isoname", "election_year","election_date","source")) %>%
  mutate(
    bias = "epistocracy",
    category = case_when(
      educ_group == 1 ~ "bot-educ-10",
      educ_group == 10 ~ "top-educ-10"
    )
  ) %>%
  select(-educ_group)

Base_ess_educ <- bind_rows(ess_dataset_educ_10, ess_dataset_educ_50)
Base_ess_educ <- Base_ess_educ %>% filter(!is.na(category))
Base_ess_educ <- Base_ess_educ %>% arrange(isoname,election_year,source)
unique(Base_ess_educ$category)


##ESS age 50 ----
ess_data_clean_age_50 <- ess_data_clean %>%
  filter(age < 99)

build_votes_by_age_fractional_50 <- function(df){
  
  # 1. Distribution pondérée de l'âge
  dist_age <- df %>%
    group_by(age) %>%
    summarise(
      n = sum(weight, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(age) %>%
    mutate(
      pct = n / sum(n) * 100,
      cum_pct = cumsum(pct),
      cum_prev = lag(cum_pct, default = 0)
    )
  
  # Deux groupes : jeunes / âgés
  age_groups <- data.frame(
    age_group = c(1, 2),
    lower = c(0, 50),
    upper = c(50, 100)
  )
  
  # 2. Poids de répartition
  weights_matrix <- dist_age %>%
    crossing(age_groups) %>%
    mutate(
      overlap = pmax(0, pmin(cum_pct, upper) - pmax(cum_prev, lower)),
      weight_group = overlap / (cum_pct - cum_prev)
    ) %>%
    filter(weight_group > 0)
  
  # 3. Attribution des groupes
  df_groups <- df %>%
    left_join(
      weights_matrix %>%
        select(age, age_group, weight_group),
      by = "age",
      relationship = "many-to-many"
    ) %>%
    filter(!is.na(weight_group)) %>%
    mutate(
      weight_final = weight * weight_group,
      vote = case_when(
        is.na(dataset_party_id) ~ NA_character_,
        str_detect(dataset_party_id, "Abstention$") ~ "Abstention",
        TRUE ~ dataset_party_id
      )
    )
  
  # 4. Votes pondérés
  votes <- df_groups %>%
    group_by(isoname,source, election_year,election_date, age_group, vote, partyfacts_id) %>%
    summarise(
      votes = sum(weight_final, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    group_by(isoname, election_year, election_date,age_group) %>%
    mutate(
      pct_votes = votes / sum(votes) * 100
    ) %>%
    ungroup()
  
  # 5. Participation pondérée
  participation <- df_groups %>%
    group_by(isoname,source, election_year, election_date,age_group) %>%
    summarise(
      nbr_obs = sum(weight_final, na.rm = TRUE),
      votes_valides = sum(
        weight_final * (!str_detect(dataset_party_id, "Abstention$")),
        na.rm = TRUE
      ),
      taux_participation = votes_valides / nbr_obs * 100,
      .groups = "drop"
    )
  
  # 6. Output
  votes %>%
    left_join(
      participation,
      by = c(
        "isoname",
        "source",
        "election_year",
        "election_date",
        "age_group"
      )
    )
}

# Construction de la base finale

ess_dataset_age_50 <- ess_data_clean_age_50 %>%
  group_by(isoname, election_year,election_date,source) %>%
  group_split() %>%
  map_dfr(~ build_votes_by_age_fractional_50(.x)) %>%
  left_join(meta_info_ess,  by = c("isoname", "election_year","election_date","source")) %>%
  mutate(
    bias = "gerontocracy",
    category = case_when(
      age_group == 1 ~ "bot-age-50",
      age_group == 2 ~ "top-age-50"
    )
  ) %>%
  select(-age_group)

##ESS age 10 ----
ess_data_clean_age_10 <- ess_data_clean %>%
  filter(age < 99)

build_votes_by_age_fractional_10 <- function(df){
  
  # 1. Distribution pondérée de l'âge
  dist_age <- df %>%
    group_by(age) %>%
    summarise(
      n = sum(weight, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(age) %>%
    mutate(
      pct = n / sum(n) * 100,
      cum_pct = cumsum(pct),
      cum_prev = lag(cum_pct, default = 0)
    )
  
  # Deux groupes : jeunes / âgés
  age_groups <- data.frame(
    age_group = 1:10,
    lower = seq(0, 90, by = 10),
    upper = seq(10, 100, by = 10)
  )
  
  # 2. Poids de répartition
  weights_matrix <- dist_age %>%
    crossing(age_groups) %>%
    mutate(
      overlap = pmax(0, pmin(cum_pct, upper) - pmax(cum_prev, lower)),
      weight_group = overlap / (cum_pct - cum_prev)
    ) %>%
    filter(weight_group > 0)
  
  # 3. Attribution des groupes
  df_groups <- df %>%
    left_join(
      weights_matrix %>%
        select(age, age_group, weight_group),
      by = "age",
      relationship = "many-to-many"
    ) %>%
    filter(!is.na(weight_group)) %>%
    mutate(
      weight_final = weight * weight_group,
      vote = case_when(
        is.na(dataset_party_id) ~ NA_character_,
        str_detect(dataset_party_id, "Abstention$") ~ "Abstention",
        TRUE ~ dataset_party_id
      )
    )
  
  # 4. Votes pondérés
  votes <- df_groups %>%
    group_by(isoname,source, election_year,election_date, age_group, vote, partyfacts_id) %>%
    summarise(
      votes = sum(weight_final, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    group_by(isoname, election_year, election_date,age_group) %>%
    mutate(
      pct_votes = votes / sum(votes) * 100
    ) %>%
    ungroup()
  
  # 5. Participation pondérée
  participation <- df_groups %>%
    group_by(isoname,source, election_year, election_date,age_group) %>%
    summarise(
      nbr_obs = sum(weight_final, na.rm = TRUE),
      votes_valides = sum(
        weight_final * (!str_detect(dataset_party_id, "Abstention$")),
        na.rm = TRUE
      ),
      taux_participation = votes_valides / nbr_obs * 100,
      .groups = "drop"
    )
  
  # 6. Output
  votes %>%
    left_join(
      participation,
      by = c(
        "isoname",
        "source",
        "election_year",
        "election_date",
        "age_group"
      )
    )
}

# Construction de la base finale

ess_dataset_age_10 <- ess_data_clean_age_10 %>%
  group_by(isoname, election_year,election_date,source) %>%
  group_split() %>%
  map_dfr(~ build_votes_by_age_fractional_10(.x)) %>%
  left_join(meta_info_ess,  by = c("isoname", "election_year","election_date","source")) %>%
  mutate(
    bias = "gerontocracy",
    category = case_when(
      age_group == 1 ~ "bot-age-10",
      age_group == 10 ~ "top-age-10"
    )
  ) %>%
  select(-age_group)

Base_ess_age <- bind_rows(ess_dataset_age_10, ess_dataset_age_50)
Base_ess_age <- Base_ess_age %>% filter(!is.na(category))
Base_ess_age <- Base_ess_age %>% arrange(isoname,election_year,source)
unique(Base_ess_age$category)



#Espace pour empiler les bases votes entre elles ----
bases <- list(
  Base_wpid_income,
  Base_wpid_educ,
  Base_wpid_age,
  Base_wpid_gender,
  Base_cses_income,
  Base_cses_educ,
  Base_cses_age,
  Base_cses_gender,
  Base_wvs_income,
  Base_wvs_educ,
  Base_wvs_age,
  Base_wvs_gender,
  Base_ess_income,
  Base_ess_educ,
  Base_ess_age,
  Base_ess_gender)

bases <- lapply(
  bases,
  \(x) x %>% mutate(category = as.character(category),
                    partyfacts_id = as.character(partyfacts_id)))

Base_all_clivages <- bind_rows(bases)

#vérif qu'on a bien tout 
unique(Base_all_clivages$bias)
unique(Base_all_clivages$bias[Base_all_clivages$source_recode == "WPID"])
unique(Base_all_clivages$category)
unique(Base_all_clivages$source_recode)

Base_all_clivages <- Base_all_clivages %>%
  select(source,source_recode,survey,bias,isoname,election_year,election_date,type, 
         category,vote,partyfacts_id,votes, pct_votes,nbr_obs,votes_valides,taux_participation)


#Liste de toutes nos élections par années et datasets
elections_valides <- Base_all_clivages %>%
  group_by(isoname,election_year,source, source_recode) %>%
  summarise(.groups = "drop")

write.csv(
  elections_valides,
  "data/intermediary/elections/valid elections.csv",
  row.names = FALSE
)


View(Base_all_clivages %>%
       filter(!is.na(Base_all_clivages$partyfacts_id))%>%
       count(
         source,
         source_recode,
         isoname,
         election_year,
         election_date,
         type,
         bias,
         category,
         partyfacts_id,
         name = "n"
       ) %>%
       filter(n > 1) %>%
       arrange(desc(n)))
#Traitement sur Base_all_clivages pour traiter tous les joins manquants entre bases 

verification_pct_votes <- Base_all_clivages %>%
  group_by(isoname,source,source_recode, election_year,election_date,type,bias, category) %>%
  summarise(
    somme_pct_votes = sum(pct_votes, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(abs(somme_pct_votes - 100) > 0.01)


write.csv(
  Base_all_clivages,
  "data/intermediary/elections/dataset with all clivages and elections.csv",
  row.names = FALSE)


unique(Base_all_clivages$bias[Base_all_clivages$source_recode == "WPID"])

