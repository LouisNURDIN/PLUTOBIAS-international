library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(scales)
library(fixest)
library(lubridate)
library(purrr)



#REGIMES LEGISLATIFS ----
Base_complete_legislative_index <-  read.csv("data/final/legislative dataset complete with index.csv", sep = ",")
unique(Base_complete_legislative_index$isoname[Base_complete_legislative_index$source_recode == "CSES"])
all_elections <- read.csv ("data/intermediary/elections/all elections update.csv", sep = ";")

annees_elections <- all_elections %>%
  distinct(isoname,year) %>%  mutate(wpid_election = 1L)

Base_complete_legislative_index <- Base_complete_legislative_index %>%
  left_join(annees_elections,
    by = c("isoname", "year")) %>%
  mutate(
    year_election = case_when(
      !is.na(election_date) ~ as.integer(year == lubridate::year(election_date)),
      source_recode == "WPID" & !is.na(wpid_election) ~ 1L,
      TRUE ~ 0L)) %>%
  select(-wpid_election)

#Hiérarchiser les sources ---- 
Base_complete_legislative_index <- Base_complete_legislative_index  %>%
  mutate(
    survey_post = case_when(
      survey == "Post-electoral" ~ "1",
      survey == "Pre/Post-electoral" ~ "1",
      survey == "Pre-electoral" ~ "0",
      TRUE ~ survey))



Base_complete_legislative_index <- Base_complete_legislative_index  %>%
  mutate(
    survey_specific = case_when(
      source_recode == "CSES" ~ "1",
      source_recode == "WPID" ~ "1",
      source_recode == "ESS" ~ "0",
      source_recode == "WVS" ~ "0",
      TRUE ~ source_recode))


Base_complete_legislative_index <- Base_complete_legislative_index  %>%
  mutate(
    score_source = case_when(
      survey_post == "1" & survey_specific == "1" ~ "1",  #Post-electoral + specific = meilleure source,
      survey_post == "1" & survey_specific == "0" ~ "2", #Post-electoral et général,
      survey_post == "0" & survey_specific == "1" ~ "3", #Pre-electoral et specific,
      survey_post == "0" & survey_specific == "0" ~ "4", #Pre-electoral et général,
      TRUE ~ survey))


#Filtre pour garder pour chaque combinaison la meilleure source au sein de chaque source_recode
Base_legislative_grosses_sources <- Base_complete_legislative_index %>%
  group_by(isoname, year, bias,source_recode) %>%
  filter(
    !is.na(score_source),
    score_source == min(score_source, na.rm = TRUE)
  ) %>%
  mutate(
    nbr_sources = n_distinct(source)
  ) %>%
  ungroup()

#check si on a bien une observation par pays/année/bias
Base_legislative_grosses_sources <- Base_legislative_grosses_sources %>%
  filter(election_couverture_seats >= 75 & election_couverture_ministers >= 0.75)

Base_legislative_grosses_sources %>% count(isoname, year, bias,source_recode) %>% filter(n > 1)


#Calcul moyenne indices quand on a plusieurs sources ----
ratio_cols <- names(Base_legislative_grosses_sources)[grepl("^ratio", names(Base_legislative_grosses_sources))]

Base_legislative_global_sources <- Base_legislative_grosses_sources %>%
  group_by(isoname, year, bias,source_recode) %>%
  summarise(
    
    # nombre de sources conservées
    nbr_sources = n_distinct(source),
    
    # moyenne géométrique des ratios
    across(
      all_of(ratio_cols),
      ~ {
        x <- .x
        x <- x[!is.na(x) & x > 0]
        
        if (length(x) == 0) {
          NA_real_
        } else {
          exp(mean(log(x)))
        }
      }
    ),
    
    # autres variables inchangées
    across(
      -c(all_of(ratio_cols), source),
      first
    ),
    
    .groups = "drop"
  )
Base_legislative_global_sources %>%
  count(isoname, year, bias, source_recode) %>%
  filter(n > 1)


#Filtre pour garder pour chaque combinaison pays/année/biais la meilleure source
Base_complete_legislative_best_sources <- Base_complete_legislative_index %>%
  filter(
    !is.na(ratio_gouvernement_top_bot2),
    !is.na(score_source)
  ) %>%
  group_by(isoname, year, bias) %>%
  filter(
    score_source == min(score_source)
  ) %>%
  mutate(
    nbr_sources = n_distinct(source)
  ) %>%
  ungroup()

unique(Base_complete_legislative_best_sources$year[Base_complete_legislative_best_sources$isoname == "France" & Base_complete_legislative_best_sources$bias == "plutocracy"])
unique(Base_complete_legislative_index$year[Base_complete_legislative_index$isoname == "France"& Base_complete_legislative_index$bias == "plutocracy"])
unique(Base_complete_legislative_best_sources$year[Base_complete_legislative_best_sources$isoname == "France"])

#check si on a bien une observation par pays/année/bias
Base_complete_legislative_best_sources <- Base_complete_legislative_best_sources %>%
  filter(election_couverture_seats >= 75 & election_couverture_ministers >= 0.75)

Base_complete_legislative_best_sources %>% count(isoname, year, bias) %>% filter(n > 1)


#Calcul moyenne indices quand on a plusieurs sources ----
ratio_cols <- names(Base_complete_legislative_best_sources)[grepl("^ratio", names(Base_complete_legislative_best_sources))]

Base_legislative_finale <- Base_complete_legislative_best_sources %>%
  group_by(isoname, year, bias) %>%
  summarise(
    
    # nombre de sources conservées
    nbr_best_sources = n_distinct(source),
    
    # moyenne géométrique des ratios
    across(
      all_of(ratio_cols),
      ~ {
        x <- .x
        x <- x[!is.na(x) & x > 0]
        
        if (length(x) == 0) {
          NA_real_
        } else {
          exp(mean(log(x)))
        }
      }
    ),
    
    # autres variables inchangées
    across(
      -c(all_of(ratio_cols), source),
      first
    ),
    
    .groups = "drop"
  )

unique(Base_complete_legislative_best_sources$year[Base_complete_legislative_best_sources$isoname == "France"])
#Création des datasets par biais

Base_regimes_presidentiels_index <-  read.csv("data/final/dataset complete regimes presidentiels.csv", sep = ",")

Base_legislative_index_income <-Base_legislative_finale %>%filter(Base_legislative_finale$bias == "plutocracy")

Base_legislative_index_gender<- Base_legislative_finale %>%filter(Base_legislative_finale$bias == "androcracy")

Base_legislative_index_educ <- Base_legislative_finale %>%filter(Base_legislative_finale$bias == "epistocracy")

Base_legislative_index_age <- Base_legislative_finale %>%filter(Base_legislative_finale$bias == "gerontocracy")

#Filtre pour travailler sur des bases propres
Base_legislative_index_income <- Base_legislative_index_income %>%
  filter(election_couverture_seats >= 75 & election_couverture_ministers >= 0.75)

Base_legislative_index_gender <- Base_legislative_index_gender %>%
  filter(election_couverture_seats >= 75 & election_couverture_ministers >= 0.75)

Base_legislative_index_educ <- Base_legislative_index_educ %>%
  filter(election_couverture_seats >= 75 & election_couverture_ministers >= 0.75)

Base_legislative_index_age <- Base_legislative_index_age %>%
  filter(election_couverture_seats >= 75 & election_couverture_ministers >= 0.75)


#Code pour chaque biais et par source ----
##Box plot plutocracy----
# 2. Long format
Base_income_legislative_long <- Base_legislative_global_sources %>%
  filter(Base_legislative_global_sources$bias == "plutocracy") %>%
  pivot_longer(
    cols = starts_with("ratio_"),
    names_to = "Indice",
    values_to = "Value"
  ) 

# 3. Séparation 10/10 et 50/50
data_50_50_legislative <- Base_income_legislative_long %>% filter(str_detect(Indice, "top_bot2"))
data_10_10_legislative <- Base_income_legislative_long %>% filter(str_detect(Indice, "top_bot"))

# 4. Recodage commun
recodage <- function(df) {
  df %>%
    mutate(
      Indice = recode(
        Indice,
        "ratio_participation_top_bot2" = "Participation",
        "ratio_votes_valides_en_sieges_top_bot2" = "Votes → sièges",
        "ratio_sieges_ministres_top_bot2" = "Sièges → ministres",
        "ratio_gouvernement_top_bot2" = "Gouvernement",
        "ratio_participation_top_bot" = "Participation",
        "ratio_votes_valides_en_sieges_top_bot" = "Votes → sièges",
        "ratio_sieges_ministres_top_bot" = "Sièges → ministres",
        "ratio_gouvernement_top_bot" = "Gouvernement"
      ),
      Indice = factor(Indice, levels = c(
        "Participation",
        "Votes → sièges",
        "Sièges → ministres",
        "Gouvernement")))
}

data_50_50_legislative <- recodage(data_50_50_legislative)
data_10_10_legislative <- recodage(data_10_10_legislative)

filtre_annees_electorales <- function(df) {
  df %>%
    filter(
      !(Indice %in% c("Participation", "Votes → sièges") &
          year_election == 0))}

data_50_50_legislative <- data_50_50_legislative %>%
  filtre_annees_electorales()

data_10_10_legislative <- data_10_10_legislative %>%
  filtre_annees_electorales()

### PLOT 50/50 ----
p_50_50_legislatives <- ggplot(data_50_50_legislative, aes(x = Indice, y = Value, fill = Indice)) +
  geom_boxplot(
    position = position_nudge(x = -0.35),
    width = 0.2,
    alpha = 1,
    color = "black",
    size = 0.2,
    outlier.size = 0) +
  
  geom_jitter(
    aes(color = Indice),
    position = position_jitter(width = 0.08, height = 0),
    alpha = 0.1,
    size = 2
  ) +
  
  stat_summary(
    position = position_nudge(x = 0),
    geom = "pointrange",
    fun.data = "mean_cl_boot",
    size = 0.3,
    color = "black"
  ) +
  
  geom_hline(yintercept = 1, linetype = "dashed") +
  facet_wrap(~ source_recode) +
  scale_y_continuous(
    trans = log_trans(),
    breaks = c(0.3, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,2.5,3,4)) +
  
  coord_cartesian(ylim = c(0.3, 4)) +
  
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),
    text = element_text(size = 14) ) +
  
  labs(
    title = "Distribution des indices de ploutocratie entre les 50% plus riches / 50% plus pauvres",
    x = "",
    y = "Poids électoral 50% riches / 50% pauvres"
  )


### PLOT 10/10 ----

p_10_10_legislatives <- ggplot(data_10_10_legislative, aes(x = Indice, y = Value, fill = Indice)) +
  geom_boxplot(
    position = position_nudge(x = -0.35),
    width = 0.2,
    alpha = 1,
    color = "black",
    size = 0.2,
    outlier.size = 0
  ) + 
  geom_jitter(
    aes(color = Indice),
    position = position_jitter(width = 0.08, height = 0),
    alpha = 0.1,
    size = 1.5
  ) +
  stat_summary(
    fun = mean,
    geom = "point",
    color = "black",
    size = 2.5
  ) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  
  facet_wrap(~ source_recode) +
  
  scale_y_continuous(trans = log_trans()) +
  
  coord_cartesian(ylim = c(0.3,4 )) +
  
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    text = element_text(size = 14)
  ) +
  
  labs(
    title = "Distribution des indices de ploutocratie entre les 10% plus riches / 10% plus pauvres",
    x = "",
    y = "Poids électoral D10 / D1"
  )


#### Graphiques Box plot ----

grid::grid.newpage()
p_50_50_legislatives
ggsave(
  filename = "results/figures/Boxplot plutocracy 50 50.jpg",
  plot = p_50_50_legislatives,width = 10,height = 6,dpi = 300)

grid::grid.newpage()
p_10_10_legislatives
ggsave(
  filename = "results/figures/Boxplot plutocracy 10 10.jpg",
  plot = p_10_10_legislatives,width = 10,height = 6,dpi = 300)


##Androcracy 50 50 ----
Base_gender_legislative_long_50 <- Base_legislative_global_sources %>%
  filter(Base_legislative_global_sources$bias == "androcracy") %>%
  pivot_longer(
    cols = starts_with("ratio_") & !ends_with("top_bot")& !ends_with("ratio_sieges_top_bot2"),
    names_to = "Indice",
    values_to = "Value"
  ) 

# 3. Séparation 10/10 et 50/50
data_top_bot_gender_legislative_50 <- Base_gender_legislative_long_50 %>% filter(str_detect(Indice, "top_bot"))

# 4. Recodage commun
recodage <- function(df) {
  df %>%
    mutate(
      Indice = recode(
        Indice,
        "ratio_participation_top_bot2" = "Participation",
        "ratio_votes_valides_en_sieges_top_bot2" = "Votes → sièges",
        "ratio_sieges_ministres_top_bot2" = "Sièges → ministres",
        "ratio_gouvernement_top_bot2" = "Gouvernement"
      ),
      Indice = factor(Indice, levels = c(
        "Participation",
        "Votes → sièges",
        "Sièges → ministres",
        "Gouvernement")))
}



data_top_bot_gender_legislative_50 <- recodage(data_top_bot_gender_legislative_50)

data_top_bot_gender_legislative_50 <- data_top_bot_gender_legislative_50 %>%
  filtre_annees_electorales()


plot_top_bot_gender_legislative_50 <- ggplot(data_top_bot_gender_legislative_50, aes(x = Indice, y = Value, fill = Indice)) +
  geom_boxplot(
    position = position_nudge(x = -0.35),
    width = 0.2,
    alpha = 1,
    color = "black",
    size = 0.2,
    outlier.size = 0) +
  
  geom_jitter(
    aes(color = Indice),
    position = position_jitter(width = 0.08, height = 0),
    alpha = 0.1,
    size = 2
  ) +
  
  stat_summary(
    position = position_nudge(x = 0),
    geom = "pointrange",
    fun.data = "mean_cl_boot",
    size = 0.3,
    color = "black"
  ) +
  
  geom_hline(yintercept = 1, linetype = "dashed") +
  facet_wrap(~ source_recode) +
  scale_y_continuous(
    trans = log_trans(),
    breaks = c(0.5, 0.75, 1, 1.25, 1.5, 1.75, 2)) +
  
  coord_cartesian(ylim = c(0.5, 2)) +
  
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),
    text = element_text(size = 14) ) +
  
  labs(
    title = "Distribution des indices d'androcratie",
    x = "",
    y = "Poids électoral des hommes comparé à celui des femmes"
  )

grid::grid.newpage()
plot_top_bot_gender_legislative_50
ggsave(
  filename = "results/figures/Boxplot androcracy 50 50.jpg",
  plot = plot_top_bot_gender_legislative_50,width = 10,height = 6,dpi = 300)


##Epistocracy 50 50  ----
Base_educ_legislative_long <- Base_legislative_global_sources %>%
  filter(Base_legislative_global_sources$bias == "epistocracy") %>%
  pivot_longer(
    cols = starts_with("ratio_") & !ends_with("top_bot") & !ends_with("ratio_sieges_top_bot2"),
    names_to = "Indice",
    values_to = "Value"
  ) 

# 3. Séparation 10/10 et 50/50
data_top_bot_educ_legislatives <- Base_educ_legislative_long %>% filter(str_detect(Indice, "top_bot"))

# 4. Recodage commun
recodage <- function(df) {
  df %>%
    mutate(
      Indice = recode(
        Indice,
        "ratio_participation_top_bot2" = "Participation",
        "ratio_votes_valides_en_sieges_top_bot2" = "Votes → sièges",
        "ratio_sieges_ministres_top_bot2" = "Sièges → ministres",
        "ratio_gouvernement_top_bot2" = "Gouvernement"
      ),
      Indice = factor(Indice, levels = c(
        "Participation",
        "Votes → sièges",
        "Sièges → ministres",
        "Gouvernement")))
}

data_top_bot_educ_legislatives <- recodage(data_top_bot_educ_legislatives)

data_top_bot_educ_legislatives <- data_top_bot_educ_legislatives %>%
  filtre_annees_electorales()

plot_top_bot_educ_legislatives <- ggplot(data_top_bot_educ_legislatives, aes(x = Indice, y = Value, fill = Indice)) +
  geom_boxplot(
    position = position_nudge(x = -0.35),
    width = 0.2,
    alpha = 1,
    color = "black",
    size = 0.2,
    outlier.size = 0) +
  
  geom_jitter(
    aes(color = Indice),
    position = position_jitter(width = 0.08, height = 0),
    alpha = 0.1,
    size = 2
  ) +
  
  stat_summary(
    position = position_nudge(x = 0),
    geom = "pointrange",
    fun.data = "mean_cl_boot",
    size = 0.3,
    color = "black"
  ) +
  
  geom_hline(yintercept = 1, linetype = "dashed") +
  facet_wrap(~ source_recode) +
  scale_y_continuous(
    trans = log_trans(),
    breaks = c(0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,3)) +
  
  coord_cartesian(ylim = c(0.5, 3)) +
  
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),
    text = element_text(size = 14) ) +
  
  labs(
    title = "Distribution des indices d'épistocratie (top 50 / bottom 50)",
    x = "",
    y = "Poids électoral 50% plus diplômés / 50% moins diplômés"
  )

grid::grid.newpage()
plot_top_bot_educ_legislatives
ggsave(
  filename = "results/figures/Boxplot epistocracy 50 50.jpg",
  plot = plot_top_bot_educ_legislatives,width = 10,height = 6,dpi = 300)




##Gerontocracy ----
Base_age_legislatives_long <- Base_legislative_global_sources %>%
  filter(Base_legislative_global_sources$bias == "gerontocracy") %>%
  pivot_longer(
    cols = starts_with("ratio_") & !ends_with("top_bot"),
    names_to = "Indice",
    values_to = "Value"
  ) 

# 3. Séparation 10/10 et 50/50
data_top_bot_age_legislatives <- Base_age_legislatives_long %>% filter(str_detect(Indice, "top_bot"))

# 4. Recodage commun
recodage <- function(df) {
  df %>%
    mutate(
      Indice = recode(
        Indice,
        "ratio_participation_top_bot2" = "Participation",
        "ratio_votes_valides_en_sieges_top_bot2" = "Votes → sièges",
        "ratio_sieges_ministres_top_bot2" = "Sièges → ministres",
        "ratio_gouvernement_top_bot2" = "Gouvernement"
      ),
      Indice = factor(Indice, levels = c(
        "Participation",
        "Votes → sièges",
        "Sièges → ministres",
        "Gouvernement")))
}

data_top_bot_age_legislatives <- recodage(data_top_bot_age_legislatives)
data_top_bot_age_legislatives <- data_top_bot_age_legislatives %>%
  filtre_annees_electorales()

plot_top_bot_age_legislatives <- ggplot(data_top_bot_age_legislatives, aes(x = Indice, y = Value, fill = Indice)) +
  geom_boxplot(
    position = position_nudge(x = -0.35),
    width = 0.2,
    alpha = 1,
    color = "black",
    size = 0.2,
    outlier.size = 0) +
  
  geom_jitter(
    aes(color = Indice),
    position = position_jitter(width = 0.08, height = 0),
    alpha = 0.1,
    size = 2
  ) +
  
  stat_summary(
    position = position_nudge(x = 0),
    geom = "pointrange",
    fun.data = "mean_cl_boot",
    size = 0.3,
    color = "black"
  ) +
  
  geom_hline(yintercept = 1, linetype = "dashed") +
  facet_wrap(~ source_recode) +
  scale_y_continuous(
    trans = log_trans(),
    breaks = c(0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,3)) +
  
  coord_cartesian(ylim = c(0.5, 3)) +
  
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),
    text = element_text(size = 14) ) +
  
  labs(
    title = "Distribution des indices de gérontocratie (top 50 / bottom 50)",
    x = "",
    y = "Poids électoral 50% plus âgés / 50% moins âgés"
  )

grid::grid.newpage()
plot_top_bot_age_legislatives
ggsave(
  filename = "results/figures/Boxplot gerontocracy 50 50.jpg",
  plot = plot_top_bot_age_legislatives,width = 10,height = 6,dpi = 300)



#HEATMAP corrélations biais/indices ----
##Garder la meilleure source au sein de chaque source_recode


##Prépa base heatmap ----
Base_legislative_global_sources_long <- Base_legislative_global_sources %>%
  pivot_longer(
    cols = starts_with("ratio_") & !ends_with("top_bot"),
    names_to = "Indice",
    values_to = "Value")

table(Base_legislative_global_sources_long$Indice)

Base_legislative_global_sources_long <- recodage(Base_legislative_global_sources_long)

Base_legislative_global_sources_long <- Base_legislative_global_sources_long %>%
  filtre_annees_electorales()


make_heatmap <- function(df, bias_i, indice_i) {
  
  df_sub <- df %>%
    filter(bias == bias_i, Indice == indice_i)
  
  sources <- unique(df_sub$source_recode)
  
  expand_grid(src1 = sources, src2 = sources) %>%
    rowwise() %>%
    mutate(
      tmp = list({
        
        d1 <- df_sub %>%
          filter(source_recode == src1) %>%
          select(isoname, year, v1 = Value)
        
        d2 <- df_sub %>%
          filter(source_recode == src2) %>%
          select(isoname, year, v2 = Value)
        
        inner <- inner_join(d1, d2, by = c("isoname", "year"))
        
        list(
          cor = if (nrow(inner) < 5) NA_real_ else cor(inner$v1, inner$v2, use = "complete.obs"),
          n = nrow(inner)
        )
      }),
      
      cor = tmp$cor,
      n = tmp$n
    ) %>%
    select(-tmp)
}

biases <- c("androcracy", "plutocracy", "gerontocracy", "epistocracy")
indices <- c(
  "Participation","Votes → sièges","Sièges → ministres","Gouvernement")


heatmaps <- crossing(bias = biases, indice = indices) %>%
  mutate(data = map2(bias, indice, ~ make_heatmap(Base_legislative_global_sources_long, .x, .y)))

plot_heatmap <- function(df, title) {
  
  df <- df %>%
    mutate(
      upper_triangle = src1 <= src2
    )
  
  ggplot(df, aes(src1, src2)) +
    
    # triangle inférieur grisé
    geom_tile(
      data = ~subset(.x, !upper_triangle),
      fill = "grey90"
    ) +
    
    # triangle supérieur coloré
    geom_tile(
      data = ~subset(.x, upper_triangle),
      aes(fill = cor)
    ) +
    
    # labels (corrélation + N)
    geom_text(
      data = ~subset(.x, upper_triangle),
      aes(label = paste0(round(cor, 2), "\nN=", n)),
      size = 3
    ) +
    
    scale_fill_gradient2(
      low = "blue",
      mid = "white",
      high = "red",
      midpoint = 0,
      limits = c(-1, 1)
    ) +
    
    theme_minimal() +
    labs(title = title, x = "", y = "")
}

plots <- heatmaps %>%
  mutate(plot = map2(data, paste(bias, indice, sep = " — "), plot_heatmap))


walk2(
  plots$plot,
  paste0("results/figures/heatmap_legislatives", plots$bias, "_", plots$indice, ".png"),
  ~ ggsave(
    filename = .y,
    plot = .x,
    width = 10,
    height = 7,
    dpi = 300
  )
)

#Visualiser les heatmap
plots$plot[[1]]  #Androcracy - Gouvernement
plots$plot[[5]]  #Epistocracy - Gouvernement
plots$plot[[9]]  #Gerontocracy - Gouvernement
plots$plot[[13]] #Plutocracy - Gouvernement

plots$plot[[2]] #Androcracy - Participation
plots$plot[[6]] #Epistocracy - Participation
plots$plot[[10]] #Gerontocracy - Participation
plots$plot[[14]] #Plutocracy - Participation

plots$plot[[4]] #Androcracy - Votes → Sièges
plots$plot[[8]] #Epistocracy - Votes → Sièges
plots$plot[[12]] #Gerontocracy - Votes → Sièges
plots$plot[[16]] #Plutocracy - Votes → Sièges

plots$plot[[3]] #Androcracy - Sièges → Ministres
plots$plot[[7]] #Epistocracy - Sièges → Ministres
plots$plot[[11]] #Gerontocracy - Sièges → Ministres
plots$plot[[15]] #Plutocracy - Sièges → Ministres




#REGIMES PRESIDENTIELS ----
Base_complete_presidentielles_index <- read.csv("data/final/dataset complete regimes presidentiels.csv", sep = ",")

Base_complete_presidentielles_index <- Base_complete_presidentielles_index %>%
  left_join(annees_elections,
            by = c("isoname", "year")) %>%
  mutate(
    year_election = case_when(
      !is.na(election_date) ~ as.integer(year == lubridate::year(election_date)),
      source_recode == "WPID" & !is.na(wpid_election) ~ 1L,
      TRUE ~ 0L)) %>%
  select(-wpid_election)

#Hiérarchiser les sources ---- 
Base_complete_presidentielles_index <- Base_complete_presidentielles_index  %>%
  mutate(
    survey_post = case_when(
      survey == "Post-electoral" ~ "1",
      survey == "Pre/Post-electoral" ~ "1",
      survey == "Pre-electoral" ~ "0",
      TRUE ~ survey))

table(Base_complete_presidentielles_index$survey_post)

Base_complete_presidentielles_index <- Base_complete_presidentielles_index  %>%
  mutate(
    survey_specific = case_when(
      source_recode == "CSES" ~ "1",
      source_recode == "WPID" ~ "1",
      source_recode == "ESS" ~ "0",
      source_recode == "WVS" ~ "0",
      TRUE ~ source_recode))
table(Base_complete_presidentielles_index$survey_specific)

Base_complete_presidentielles_index <- Base_complete_presidentielles_index  %>%
  mutate(
    score_source = case_when(
      survey_post == "1" & survey_specific == "1" ~ "1",  #Post-electoral + specific = meilleure source,
      survey_post == "1" & survey_specific == "0" ~ "2", #Post-electoral et général,
      survey_post == "0" & survey_specific == "1" ~ "3", #Pre-electoral et specific,
      survey_post == "0" & survey_specific == "0" ~ "4", #Pre-electoral et général,
      TRUE ~ survey))
table(Base_complete_presidentielles_index$score_source)

#Filtre pour garder pour chaque combinaison pays/année/biais la meilleure source
##Garder la meilleure source au sein de chaque source_recode
Base_presidentielles_grosses_sources <- Base_complete_presidentielles_index %>%
  group_by(isoname, year, bias,source_recode) %>%
  filter(
    !is.na(score_source),
    score_source == min(score_source, na.rm = TRUE)
  ) %>%
  mutate(
    nbr_sources = n_distinct(source)
  ) %>%
  ungroup()

#check si on a bien une observation par pays/année/bias
Base_presidentielles_grosses_sources <- Base_presidentielles_grosses_sources %>%
  filter(election_couverture_ministers >= 0.75)

Base_presidentielles_grosses_sources %>% count(isoname, year, bias,source_recode) %>% filter(n > 1)


#Calcul moyenne indices quand on a plusieurs sources ----
ratio_cols <- names(Base_presidentielles_grosses_sources)[grepl("^ratio", names(Base_presidentielles_grosses_sources))]

Base_presidentielles_global_sources <- Base_presidentielles_grosses_sources %>%
  group_by(isoname, year, bias,source_recode) %>%
  summarise(
    
    # nombre de sources conservées
    nbr_sources = n_distinct(source),
    
    # moyenne géométrique des ratios
    across(
      all_of(ratio_cols),
      ~ {
        x <- .x
        x <- x[!is.na(x) & x > 0]
        
        if (length(x) == 0) {
          NA_real_
        } else {
          exp(mean(log(x)))
        }
      }
    ),
    
    # autres variables inchangées
    across(
      -c(all_of(ratio_cols), source),
      first
    ),
    
    .groups = "drop"
  )
Base_presidentielles_global_sources %>%
  count(isoname, year, bias, source_recode) %>%
  filter(n > 1)

#Calculer la meilleure source pour chaque pays/année/biais ----
Base_complete_best_sources_presidentielles <- Base_complete_presidentielles_index %>%
  group_by(isoname, year, bias) %>%
  filter(
    !is.na(score_source),
    score_source == min(score_source, na.rm = TRUE)
  ) %>%
  mutate(
    nbr_sources = n_distinct(source)
  ) %>%
  ungroup()

#check si on a bien une observation par pays/année/bias
Base_complete_best_sources_presidentielles <- Base_complete_best_sources_presidentielles %>%
  filter(election_couverture_ministers >= 0.75)

Base_complete_best_sources_presidentielles %>% count(isoname, year, bias) %>% filter(n > 1)


#Calcul moyenne indices quand on a plusieurs sources ----
ratio_cols <- names(Base_complete_best_sources_presidentielles)[grepl("^ratio", names(Base_complete_best_sources_presidentielles))]

Base_finale_presidentielles <- Base_complete_best_sources_presidentielles %>%
  group_by(isoname, year, bias) %>%
  summarise(
    
    # nombre de sources conservées
    nbr_best_sources = n_distinct(source),
    
    # moyenne géométrique des ratios
    across(
      all_of(ratio_cols),
      ~ {
        x <- .x
        x <- x[!is.na(x) & x > 0]
        
        if (length(x) == 0) {
          NA_real_
        } else {
          exp(mean(log(x)))
        }
      }
    ),
    
    # autres variables inchangées
    across(
      -c(all_of(ratio_cols), source),
      first
    ),
    
    .groups = "drop")

#Création des datasets par biais
Base_finale_presidentielles_income <- Base_finale_presidentielles %>%filter(Base_finale_presidentielles$bias == "plutocracy")

Base_finale_presidentielles_gender<- Base_finale_presidentielles %>%filter(Base_finale_presidentielles$bias == "androcracy")

Base_finale_presidentielles_educ <- Base_finale_presidentielles %>%filter(Base_finale_presidentielles$bias == "epistocracy")

Base_finale_presidentielles_age <- Base_finale_presidentielles %>%filter(Base_finale_presidentielles$bias == "gerontocracy")

#Filtre pour travailler sur des bases propres
Base_finale_presidentielles_income <- Base_finale_presidentielles_income %>%
  filter(election_couverture_ministers >= 0.75)

Base_finale_presidentielles_gender <- Base_finale_presidentielles_gender %>%
  filter(election_couverture_ministers >= 0.75)

Base_finale_presidentielles_educ <- Base_finale_presidentielles_educ %>%
  filter( election_couverture_ministers >= 0.75)

Base_finale_presidentielles_age <- Base_finale_presidentielles_age %>%
  filter(election_couverture_ministers >= 0.75)


#Code pour chaque biais et par source ----
##Box plot plutocracy----
# 2. Long format
Base_income_long_presidentielles <- Base_presidentielles_global_sources %>%
  filter(Base_presidentielles_global_sources$bias == "plutocracy")%>%
  pivot_longer(
    cols = starts_with("ratio_"),
    names_to = "Indice",
    values_to = "Value"
  ) 

# 3. Séparation 10/10 et 50/50
data_50_50_presidentielles <- Base_income_long_presidentielles %>% filter(str_detect(Indice, "top_bot2"))
data_10_10_presidentielles <- Base_income_long_presidentielles %>% filter(str_detect(Indice, "top_bot"))

# 4. Recodage commun
recodage <- function(df) {
  df %>%
    mutate(
      Indice = recode(
        Indice,
        "ratio_participation_top_bot2" = "Participation",
        "ratio_votes_valides_en_ministres_top_bot2" = "Votes → Ministres",
        "ratio_gouvernement_top_bot2" = "Gouvernement",
        "ratio_participation_top_bot" = "Participation",
        "ratio_votes_valides_en_ministres_top_bot" = "Votes → Ministres",
        "ratio_gouvernement_top_bot" = "Gouvernement"
      ),
      Indice = factor(Indice, levels = c(
        "Participation",
        "Votes → Ministres",
        "Gouvernement")))
}

data_50_50_presidentielles <- recodage(data_50_50_presidentielles)
data_10_10_presidentielles <- recodage(data_10_10_presidentielles)

filtre_annees_electorales <- function(df) {
  df %>%
    filter(
      !(Indice %in% c("Participation") &
          year_election == 0))}

data_50_50_presidentielles <- data_50_50_presidentielles %>%
  filtre_annees_electorales()

data_10_10_presidentielles <- data_10_10_presidentielles %>%
  filtre_annees_electorales()

### PLOT 50/50 ----
p_50_50_presidentielles <- ggplot(data_50_50_presidentielles, aes(x = Indice, y = Value, fill = Indice)) +
  geom_boxplot(
    position = position_nudge(x = -0.35),
    width = 0.2,
    alpha = 1,
    color = "black",
    size = 0.2,
    outlier.size = 0) +
  
  geom_jitter(
    aes(color = Indice),
    position = position_jitter(width = 0.08, height = 0),
    alpha = 0.1,
    size = 2
  ) +
  
  stat_summary(
    position = position_nudge(x = 0),
    geom = "pointrange",
    fun.data = "mean_cl_boot",
    size = 0.3,
    color = "black"
  ) +
  
  geom_hline(yintercept = 1, linetype = "dashed") +
  facet_wrap(~ source_recode) +
  scale_y_continuous(
    trans = log_trans(),
    breaks = c(0.3, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,2.5,3,4)) +
  
  coord_cartesian(ylim = c(0.3, 4)) +
  
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),
    text = element_text(size = 14) ) +
  
  labs(
    title = "Distribution des indices de ploutocratie dans les régimes présidentiels (top50 / bottom 50 income)",
    x = "",
    y = "Poids électoral 50% riches / 50% pauvres"
  )


### PLOT 10/10 ----

p_10_10_presidentielles <- ggplot(data_10_10_presidentielles, aes(x = Indice, y = Value, fill = Indice)) +
  geom_boxplot(
    position = position_nudge(x = -0.35),
    width = 0.2,
    alpha = 1,
    color = "black",
    size = 0.2,
    outlier.size = 0
  ) + 
  geom_jitter(
    aes(color = Indice),
    position = position_jitter(width = 0.08, height = 0),
    alpha = 0.1,
    size = 1.5
  ) +
  stat_summary(
    fun = mean,
    geom = "point",
    color = "black",
    size = 2.5
  ) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  
  facet_wrap(~ source_recode) +
  
  scale_y_continuous(trans = log_trans()) +
  
  coord_cartesian(ylim = c(0.3,4 )) +
  
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    text = element_text(size = 14)
  ) +
  
  labs(
    title = "Distribution des indices de ploutocratie dans les régimes présidentiels (top10 / bottom 10 income)",
    x = "",
    y = "Poids électoral D10 / D1"
  )


#### Graphiques Box plot ----

grid::grid.newpage()
p_50_50_presidentielles
ggsave(
  filename = "results/figures/Boxplot plutocracy presidentielles 50 50.jpg",
  plot = p_50_50_presidentielles,width = 10,height = 6,dpi = 300)

grid::grid.newpage()
p_10_10_presidentielles
ggsave(
  filename = "results/figures/Boxplot plutocracy presidentielles 10 10.jpg",
  plot = p_10_10_presidentielles,width = 10,height = 6,dpi = 300)



##Box plot androcracy ----
Base_gender_long_presidentielles <- Base_presidentielles_global_sources %>%
  filter(Base_presidentielles_global_sources$bias == "androcracy")%>%
  pivot_longer(
    cols = starts_with("ratio_") & !ends_with("top_bot"),
    names_to = "Indice",
    values_to = "Value"
  ) 

# 3. Séparation 10/10 et 50/50
data_top_bot_gender_presidentielles <- Base_gender_long_presidentielles %>% filter(str_detect(Indice, "top_bot"))

# 4. Recodage commun
recodage <- function(df) {
  df %>%
    mutate(
      Indice = recode(
        Indice,
        "ratio_participation_top_bot2" = "Participation",
        "ratio_votes_valides_en_ministres_top_bot2" = "Votes → Ministres",
        "ratio_gouvernement_top_bot2" = "Gouvernement"
      ),
      Indice = factor(Indice, levels = c(
        "Participation",
        "Votes → Ministres",
        "Gouvernement")))
}



data_top_bot_gender_presidentielles <- recodage(data_top_bot_gender_presidentielles)

data_top_bot_gender_presidentielles <- data_top_bot_gender_presidentielles %>%
  filtre_annees_electorales()
### PLOT 50/50 ----
plot_top_bot_gender_presidentielles <- ggplot(data_top_bot_gender_presidentielles, aes(x = Indice, y = Value, fill = Indice)) +
  geom_boxplot(
    position = position_nudge(x = -0.35),
    width = 0.2,
    alpha = 1,
    color = "black",
    size = 0.2,
    outlier.size = 0) +
  
  geom_jitter(
    aes(color = Indice),
    position = position_jitter(width = 0.08, height = 0),
    alpha = 0.1,
    size = 2
  ) +
  
  stat_summary(
    position = position_nudge(x = 0),
    geom = "pointrange",
    fun.data = "mean_cl_boot",
    size = 0.3,
    color = "black"
  ) +
  
  geom_hline(yintercept = 1, linetype = "dashed") +
  facet_wrap(~ source_recode) +
  scale_y_continuous(
    trans = log_trans(),
    breaks = c(0.5, 0.75, 1, 1.25, 1.5, 1.75, 2)) +
  
  coord_cartesian(ylim = c(0.5, 2)) +
  
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),
    text = element_text(size = 14) ) +
  
  labs(
    title = "Distribution des indices d'androcratie dans les régimes présidentiels",
    x = "",
    y = "Poids électoral des hommes comparé à celui des femmes"
  )

grid::grid.newpage()
plot_top_bot_gender_presidentielles
ggsave(
  filename = "results/figures/Boxplot androcracy presidentielles 50 50.jpg",
  plot = plot_top_bot_gender_presidentielles,width = 10,height = 6,dpi = 300)



##Box plot epistocracy ----
Base_educ_long_presidentielles <- Base_presidentielles_global_sources %>%
  filter(Base_presidentielles_global_sources$bias == "epistocracy")%>%
  pivot_longer(
    cols = starts_with("ratio_") & !ends_with("top_bot"),
    names_to = "Indice",
    values_to = "Value"
  ) 

# 3. Séparation 10/10 et 50/50
data_top_bot_educ_presidentielles <- Base_educ_long_presidentielles %>% filter(str_detect(Indice, "top_bot"))

# 4. Recodage commun
recodage <- function(df) {
  df %>%
    mutate(
      Indice = recode(
        Indice,
        "ratio_participation_top_bot2" = "Participation",
        "ratio_votes_valides_en_ministres_top_bot2" = "Votes → Ministres",
        "ratio_gouvernement_top_bot2" = "Gouvernement"
      ),
      Indice = factor(Indice, levels = c(
        "Participation",
        "Votes → Ministres",
        "Gouvernement")))
}

data_top_bot_educ_presidentielles <- recodage(data_top_bot_educ_presidentielles)

data_top_bot_educ_presidentielles <- data_top_bot_educ_presidentielles %>%
  filtre_annees_electorales()

plot_top_bot_educ_presidentielles <- ggplot(data_top_bot_educ_presidentielles, aes(x = Indice, y = Value, fill = Indice)) +
  geom_boxplot(
    position = position_nudge(x = -0.35),
    width = 0.2,
    alpha = 1,
    color = "black",
    size = 0.2,
    outlier.size = 0) +
  
  geom_jitter(
    aes(color = Indice),
    position = position_jitter(width = 0.08, height = 0),
    alpha = 0.1,
    size = 2
  ) +
  
  stat_summary(
    position = position_nudge(x = 0),
    geom = "pointrange",
    fun.data = "mean_cl_boot",
    size = 0.3,
    color = "black"
  ) +
  
  geom_hline(yintercept = 1, linetype = "dashed") +
  facet_wrap(~ source_recode) +
  scale_y_continuous(
    trans = log_trans(),
    breaks = c(0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,3)) +
  
  coord_cartesian(ylim = c(0.5, 3)) +
  
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),
    text = element_text(size = 14) ) +
  
  labs(
    title = "Distribution des indices d'épistocratie dans les régimes présidentiels (top 50 / bottom 50)",
    x = "",
    y = "Poids électoral 50% plus diplômés / 50% moins diplômés"
  )

grid::grid.newpage()
plot_top_bot_educ_presidentielles
ggsave(
  filename = "results/figures/Boxplot epistocracy presidentielles 50 50.jpg",
  plot = plot_top_bot_educ_presidentielles,width = 10,height = 6,dpi = 300)

##Box plot gerontocracy ----
Base_age_long_presidentielles <- Base_presidentielles_global_sources %>%
  filter(Base_presidentielles_global_sources$bias == "gerontocracy")%>%
  pivot_longer(
    cols = starts_with("ratio_") & !ends_with("top_bot"),
    names_to = "Indice",
    values_to = "Value"
  ) 

# 3. Séparation 10/10 et 50/50
data_top_bot_age_presidentielles <- Base_age_long_presidentielles %>% filter(str_detect(Indice, "top_bot"))

# 4. Recodage commun
recodage <- function(df) {
  df %>%
    mutate(
      Indice = recode(
        Indice,
        "ratio_participation_top_bot2" = "Participation",
        "ratio_votes_valides_en_ministres_top_bot2" = "Votes → Ministres",
        "ratio_gouvernement_top_bot2" = "Gouvernement"
      ),
      Indice = factor(Indice, levels = c(
        "Participation",
        "Votes → Ministres",
        "Gouvernement")))
}

data_top_bot_age_presidentielles <- recodage(data_top_bot_age_presidentielles)
data_top_bot_age_presidentielles <- data_top_bot_age_presidentielles %>%
  filtre_annees_electorales()

plot_top_bot_age_presidentielles <- ggplot(data_top_bot_age_presidentielles, aes(x = Indice, y = Value, fill = Indice)) +
  geom_boxplot(
    position = position_nudge(x = -0.35),
    width = 0.2,
    alpha = 1,
    color = "black",
    size = 0.2,
    outlier.size = 0) +
  
  geom_jitter(
    aes(color = Indice),
    position = position_jitter(width = 0.08, height = 0),
    alpha = 0.1,
    size = 2
  ) +
  
  stat_summary(
    position = position_nudge(x = 0),
    geom = "pointrange",
    fun.data = "mean_cl_boot",
    size = 0.3,
    color = "black"
  ) +
  
  geom_hline(yintercept = 1, linetype = "dashed") +
  facet_wrap(~ source_recode) +
  scale_y_continuous(
    trans = log_trans(),
    breaks = c(0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,3)) +
  
  coord_cartesian(ylim = c(0.5, 3)) +
  
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),
    text = element_text(size = 14) ) +
  
  labs(
    title = "Distribution des indices de gérontocratie dans les régimes présidentiels (top 50 / bottom 50)",
    x = "",
    y = "Poids électoral 50% plus âgés / 50% moins âgés"
  )

grid::grid.newpage()
plot_top_bot_age_presidentielles
ggsave(
  filename = "results/figures/Boxplot gerontocracy presidentielles 50 50.jpg",
  plot = plot_top_bot_age_presidentielles,width = 10,height = 6,dpi = 300)



#BOX PLOT TOUTES SOURCES MELANGEES ----
Base_best_sources_all_regimes <- bind_rows(Base_legislative_finale, Base_finale_presidentielles)


#Gouvernement 50 50 ----
plot_all_global_bias_50_50_all_regimes <- ggplot(Base_best_sources_all_regimes, aes(x = bias, y = ratio_gouvernement_top_bot2, fill = bias)) +
  geom_boxplot(
    position = position_nudge(x = -0.35),
    width = 0.2,
    alpha = 1,
    color = "black",
    size = 0.2,
    outlier.size = 0) +
  
  geom_jitter(
    aes(color = bias),
    position = position_jitter(width = 0.08, height = 0),
    alpha = 0.1,
    size = 2
  ) +
  
  stat_summary(
    position = position_nudge(x = 0),
    geom = "pointrange",
    fun.data = "mean_cl_boot",
    size = 0.3,
    color = "black"
  ) +
  
  geom_hline(yintercept = 1, linetype = "dashed") +
  scale_y_continuous(
    trans = log_trans(),
    breaks = c(0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,3,4)) +
  
  coord_cartesian(ylim = c(0.5, 4)) +
  
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),
    text = element_text(size = 14) ) +
  
  labs(
    title = "Distribution des indices globaux par biais et sources, top 50 / bottom 50 (> 75% des députés et ministres)",
    subtitle = "Indice global de représentation au gouvernement",
    x = "",
    y = "Poids électoral top 50% / bottom 50%"
  )

grid::grid.newpage()
plot_all_global_bias_50_50_all_regimes

ggsave(
  filename = "results/figures/Biais global de représentation au gouvernement 50 50.jpg",
  plot = plot_all_global_bias_50_50_all_regimes,width = 10,height = 6,dpi = 300)

##Gouvernement 10 10 ----
plot_all_global_bias_10_10_all_regimes <- ggplot(Base_best_sources_all_regimes, aes(x = bias, y = ratio_gouvernement_top_bot, fill = bias)) +
  geom_boxplot(
    position = position_nudge(x = -0.35),
    width = 0.2,
    alpha = 1,
    color = "black",
    size = 0.2,
    outlier.size = 0) +
  
  geom_jitter(
    aes(color = bias),
    position = position_jitter(width = 0.08, height = 0),
    alpha = 0.1,
    size = 2
  ) +
  
  stat_summary(
    position = position_nudge(x = 0),
    geom = "pointrange",
    fun.data = "mean_cl_boot",
    size = 0.3,
    color = "black"
  ) +
  
  geom_hline(yintercept = 1, linetype = "dashed") +
  scale_y_continuous(
    trans = log_trans(),
    breaks = c(0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,3,4)) +
  
  coord_cartesian(ylim = c(0.5, 4)) +
  
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),
    text = element_text(size = 14) ) +
  
  labs(
    title = "Distribution des indices globaux par biais et sources, top 10 / bottom 10 ( > 75% des députés et ministres)",
    subtitle = "Indice global de représentation au gouvernement",
    x = "",
    y = "Poids électoral top 10% / bottom 10%"
  )

grid::grid.newpage()
plot_all_global_bias_10_10_all_regimes

ggsave(
  filename = "results/figures/Biais global de représentation au gouvernement 10 10.jpg",
  plot = plot_all_global_bias_10_10_all_regimes,width = 10,height = 6,dpi = 300)


##Participation 50 50 ----
plot_all_participation_bias_50_50_all_regimes <- ggplot(Base_best_sources_all_regimes, aes(x = bias, y = ratio_participation_top_bot2, fill = bias)) +
  geom_boxplot(
    position = position_nudge(x = -0.35),
    width = 0.2,
    alpha = 1,
    color = "black",
    size = 0.2,
    outlier.size = 0) +
  
  geom_jitter(
    aes(color = bias),
    position = position_jitter(width = 0.08, height = 0),
    alpha = 0.1,
    size = 2
  ) +
  
  stat_summary(
    position = position_nudge(x = 0),
    geom = "pointrange",
    fun.data = "mean_cl_boot",
    size = 0.3,
    color = "black"
  ) +
  
  geom_hline(yintercept = 1, linetype = "dashed") +
  scale_y_continuous(
    trans = log_trans(),
    breaks = c(0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,3,4)) +
  
  coord_cartesian(ylim = c(0.5, 4)) +
  
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),
    text = element_text(size = 14) ) +
  
  labs(
    title = "Distribution des indices de participation par biais et sources, top 50 / bottom 50 (> 75% des députés et ministres)",
    subtitle = "Biais de participation",
    x = "",
    y = "Poids électoral top 50% / bottom 50%"
  )

grid::grid.newpage()
plot_all_participation_bias_50_50_all_regimes

ggsave(
  filename = "results/figures/Biais de participation tous regimes 50 50.jpg",
  plot = plot_all_participation_bias_50_50_all_regimes,width = 10,height = 6,dpi = 300)

##Participation 10 10 ----
plot_all_participation_bias_10_10_all_regimes <- ggplot(Base_best_sources_all_regimes, aes(x = bias, y = ratio_participation_top_bot, fill = bias)) +
  geom_boxplot(
    position = position_nudge(x = -0.35),
    width = 0.2,
    alpha = 1,
    color = "black",
    size = 0.2,
    outlier.size = 0) +
  
  geom_jitter(
    aes(color = bias),
    position = position_jitter(width = 0.08, height = 0),
    alpha = 0.1,
    size = 2
  ) +
  
  stat_summary(
    position = position_nudge(x = 0),
    geom = "pointrange",
    fun.data = "mean_cl_boot",
    size = 0.3,
    color = "black"
  ) +
  
  geom_hline(yintercept = 1, linetype = "dashed") +
  scale_y_continuous(
    trans = log_trans(),
    breaks = c(0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,3,4)) +
  
  coord_cartesian(ylim = c(0.5, 4)) +
  
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),
    text = element_text(size = 14) ) +
  
  labs(
    title = "Distribution des indices de participation par biais et sources, top 10 / bottom 10 (> 75% des députés et ministres)",
    subtitle = "Biais de participation",
    x = "",
    y = "Poids électoral top 10% / bottom 10%"
  )

grid::grid.newpage()
plot_all_participation_bias_10_10_all_regimes

ggsave(
  filename = "results/figures/Biais de participation tous regimes 10 10.jpg",
  plot = plot_all_participation_bias_10_10_all_regimes,width = 10,height = 6,dpi = 300)



# Votes → Sièges 50 50 ----
plot_vote_seat_bias_50_50_all_regimes <- ggplot(Base_best_sources_all_regimes, aes(x = bias, y = ratio_votes_valides_en_sieges_top_bot2, fill = bias)) +
  geom_boxplot(
    position = position_nudge(x = -0.35),
    width = 0.2,
    alpha = 1,
    color = "black",
    size = 0.2,
    outlier.size = 0) +
  
  geom_jitter(
    aes(color = bias),
    position = position_jitter(width = 0.08, height = 0),
    alpha = 0.1,
    size = 2
  ) +
  
  stat_summary(
    position = position_nudge(x = 0),
    geom = "pointrange",
    fun.data = "mean_cl_boot",
    size = 0.3,
    color = "black"
  ) +
  
  geom_hline(yintercept = 1, linetype = "dashed") +
  scale_y_continuous(
    trans = log_trans(),
    breaks = c(0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,3,4)) +
  
  coord_cartesian(ylim = c(0.5, 4)) +
  
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),
    text = element_text(size = 14) ) +
  
  labs(
    title = "Distribution des indices de conversion de votes à sièges par biais et sources, top 50 / bottom 50 ",
    subtitle = "Biais Votes → Sièges (> 75% des députés et ministres)",
    x = "",
    y = "Poids électoral top 50% / bottom 50%"
  )

grid::grid.newpage()
plot_vote_seat_bias_50_50_all_regimes

ggsave(
  filename = "results/figures/Biais de conversion de votes à sièges 50 50.jpg",
  plot = plot_vote_seat_bias_50_50_all_regimes,width = 10,height = 6,dpi = 300)


# Votes → Sièges 10 10 ----
plot_vote_seat_bias_10_10_all_regimes <- ggplot(Base_best_sources_all_regimes, aes(x = bias, y = ratio_votes_valides_en_sieges_top_bot, fill = bias)) +
  geom_boxplot(
    position = position_nudge(x = -0.35),
    width = 0.2,
    alpha = 1,
    color = "black",
    size = 0.2,
    outlier.size = 0) +
  
  geom_jitter(
    aes(color = bias),
    position = position_jitter(width = 0.08, height = 0),
    alpha = 0.1,
    size = 2
  ) +
  
  stat_summary(
    position = position_nudge(x = 0),
    geom = "pointrange",
    fun.data = "mean_cl_boot",
    size = 0.3,
    color = "black"
  ) +
  
  geom_hline(yintercept = 1, linetype = "dashed") +
  scale_y_continuous(
    trans = log_trans(),
    breaks = c(0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,3,4)) +
  
  coord_cartesian(ylim = c(0.5, 4)) +
  
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),
    text = element_text(size = 14) ) +
  
  labs(
    title = "Distribution des indices de conversion de votes à sièges par biais et sources, top 10 / bottom 10" ,
    subtitle = "Biais Votes → Sièges (> 75% des députés et ministres)",
    x = "",
    y = "Poids électoral top 10% / bottom 10%"
  )

grid::grid.newpage()
plot_vote_seat_bias_10_10_all_regimes

ggsave(
  filename = "results/figures/Biais de conversion de votes à sièges 10 10.jpg",
  plot = plot_vote_seat_bias_10_10_all_regimes,width = 10,height = 6,dpi = 300)


#Sièges → Ministres 50 50 ----
plot_seat_minister_bias_50_50 <- ggplot(Base_best_sources_all_regimes, aes(x = bias, y = ratio_sieges_ministres_top_bot2, fill = bias)) +
  geom_boxplot(
    position = position_nudge(x = -0.35),
    width = 0.2,
    alpha = 1,
    color = "black",
    size = 0.2,
    outlier.size = 0) +
  
  geom_jitter(
    aes(color = bias),
    position = position_jitter(width = 0.08, height = 0),
    alpha = 0.1,
    size = 2
  ) +
  
  stat_summary(
    position = position_nudge(x = 0),
    geom = "pointrange",
    fun.data = "mean_cl_boot",
    size = 0.3,
    color = "black"
  ) +
  
  geom_hline(yintercept = 1, linetype = "dashed") +
  scale_y_continuous(
    trans = log_trans(),
    breaks = c(0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,3,4)) +
  
  coord_cartesian(ylim = c(0.5, 4)) +
  
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),
    text = element_text(size = 14) ) +
  
  labs(
    title = "Distribution des indices de conversion de sièges à ministres par biais et sources, top 50 / bottom 50 ",
    subtitle = "Biais Sièges → Ministres ( > 75% des députés et ministres)",
    x = "",
    y = "Poids électoral top 50% / bottom 50%"
  )

grid::grid.newpage()
plot_seat_minister_bias_50_50

ggsave(
  filename = "results/figures/Biais de conversion de sièges à ministres 50 50.jpg",
  plot = plot_seat_minister_bias_50_50,width = 10,height = 6,dpi = 300)


#Sièges → Ministres 10 10 ----
plot_seat_minister_bias_10_10 <- ggplot(Base_best_sources_all_regimes, aes(x = bias, y = ratio_sieges_ministres_top_bot, fill = bias)) +
  geom_boxplot(
    position = position_nudge(x = -0.35),
    width = 0.2,
    alpha = 1,
    color = "black",
    size = 0.2,
    outlier.size = 0) +
  
  geom_jitter(
    aes(color = bias),
    position = position_jitter(width = 0.08, height = 0),
    alpha = 0.1,
    size = 2
  ) +
  
  stat_summary(
    position = position_nudge(x = 0),
    geom = "pointrange",
    fun.data = "mean_cl_boot",
    size = 0.3,
    color = "black"
  ) +
  
  geom_hline(yintercept = 1, linetype = "dashed") +
  scale_y_continuous(
    trans = log_trans(),
    breaks = c(0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,3,4)) +
  
  coord_cartesian(ylim = c(0.5, 4)) +
  
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),
    text = element_text(size = 14) ) +
  
  labs(
    title = "Distribution des indices de conversion de sièges à ministres par biais et sources, top 10 / bottom 10 ",
    subtitle = "Biais Sièges → Ministres ( > 75% des députés et ministres)",
    x = "",
    y = "Poids électoral top 10% / bottom 10%"
  )

grid::grid.newpage()
plot_seat_minister_bias_10_10

ggsave(
  filename = "results/figures/Biais de conversion de sièges à ministres 10 10.jpg",
  plot = plot_seat_minister_bias_10_10,width = 10,height = 6,dpi = 300)


#Votes → Ministres (régimes présidentiels) 50 50 ----
plot_vote_minister_bias_50_50 <- ggplot(Base_best_sources_all_regimes, aes(x = bias, y = ratio_votes_valides_en_ministres_top_bot2, fill = bias)) +
  geom_boxplot(
    position = position_nudge(x = -0.35),
    width = 0.2,
    alpha = 1,
    color = "black",
    size = 0.2,
    outlier.size = 0) +
  
  geom_jitter(
    aes(color = bias),
    position = position_jitter(width = 0.08, height = 0),
    alpha = 0.1,
    size = 2
  ) +
  
  stat_summary(
    position = position_nudge(x = 0),
    geom = "pointrange",
    fun.data = "mean_cl_boot",
    size = 0.3,
    color = "black"
  ) +
  
  geom_hline(yintercept = 1, linetype = "dashed") +
  scale_y_continuous(
    trans = log_trans(),
    breaks = c(0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,3,4)) +
  
  coord_cartesian(ylim = c(0.5, 4)) +
  
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),
    text = element_text(size = 14) ) +
  
  labs(
    title = "Distribution des indices de conversion de votes à ministres par biais et sources, top 50 - bottom 50 ",
    subtitle = "Biais Votes → Ministres (> 75% des députés et ministres)",
    x = "",
    y = "Poids électoral top 50% / bottom 50%"
  )

grid::grid.newpage()
plot_vote_minister_bias_50_50

ggsave(
  filename = "results/figures/Biais de conversion de votes à ministres (régimes présidentiels) 50 50.jpg",
  plot = plot_vote_minister_bias_50_50,width = 10,height = 6,dpi = 300)


#Votes → Ministres (régimes présidentiels) 10 10 ----
plot_vote_minister_bias_10_10 <- ggplot(Base_best_sources_all_regimes, aes(x = bias, y = ratio_votes_valides_en_ministres_top_bot, fill = bias)) +
  geom_boxplot(
    position = position_nudge(x = -0.35),
    width = 0.2,
    alpha = 1,
    color = "black",
    size = 0.2,
    outlier.size = 0) +
  
  geom_jitter(
    aes(color = bias),
    position = position_jitter(width = 0.08, height = 0),
    alpha = 0.1,
    size = 2
  ) +
  
  stat_summary(
    position = position_nudge(x = 0),
    geom = "pointrange",
    fun.data = "mean_cl_boot",
    size = 0.3,
    color = "black"
  ) +
  
  geom_hline(yintercept = 1, linetype = "dashed") +
  scale_y_continuous(
    trans = log_trans(),
    breaks = c(0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,3,4)) +
  
  coord_cartesian(ylim = c(0.5, 4)) +
  
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),
    text = element_text(size = 14) ) +
  
  labs(
    title = "Distribution des indices de conversion de votes à ministres par biais et sources, top 10 - bottom 10 ",
    subtitle = "Biais Votes → Ministres (> 75% des députés et ministres)",
    x = "",
    y = "Poids électoral top 10% / bottom 10%"
  )

grid::grid.newpage()
plot_vote_minister_bias_10_10

ggsave(
  filename = "results/figures/Biais de conversion de votes à ministres (régimes présidentiels) 10 10.jpg",
  plot = plot_vote_minister_bias_10_10,width = 10,height = 6,dpi = 300)




#HEATMAP corrélations biais/indices ----


##Prépa base heatmap ----
Base_presidentielles_global_sources_long <- Base_presidentielles_global_sources %>%
  pivot_longer(
    cols = starts_with("ratio_") & !ends_with("top_bot"),
    names_to = "Indice",
    values_to = "Value")

table(Base_presidentielles_global_sources_long$Indice)

Base_presidentielles_global_sources_long <- recodage(Base_presidentielles_global_sources_long)

Base_presidentielles_global_sources_long <- Base_presidentielles_global_sources_long %>%
  filtre_annees_electorales()


make_heatmap <- function(df, bias_i, indice_i) {
  
  df_sub <- df %>%
    filter(bias == bias_i, Indice == indice_i)
  
  sources <- unique(df_sub$source_recode)
  
  expand_grid(src1 = sources, src2 = sources) %>%
    rowwise() %>%
    mutate(
      tmp = list({
        
        d1 <- df_sub %>%
          filter(source_recode == src1) %>%
          select(isoname, year, v1 = Value)
        
        d2 <- df_sub %>%
          filter(source_recode == src2) %>%
          select(isoname, year, v2 = Value)
        
        inner <- inner_join(d1, d2, by = c("isoname", "year"))
        
        inner_complete <- inner %>%
          filter(!is.na(v1), !is.na(v2))
        
        list(
          cor = if (nrow(inner_complete) < 5) {
            NA_real_
          } else {
            cor(inner_complete$v1, inner_complete$v2)
          },
          n = nrow(inner_complete)
        )
      }),
      
      cor = tmp$cor,
      n = tmp$n
    ) %>%
    select(-tmp)
}

biases <- c("androcracy", "plutocracy", "gerontocracy", "epistocracy")
indices <- c(
  "Participation","Votes → Ministres","Gouvernement")


heatmaps <- crossing(bias = biases, indice = indices) %>%
  mutate(data = map2(bias, indice, ~ make_heatmap(Base_presidentielles_global_sources_long, .x, .y)))

plot_heatmap <- function(df, title) {
  
  df <- df %>%
    mutate(
      upper_triangle = src1 <= src2
    )
  
  ggplot(df, aes(src1, src2)) +
    
    # triangle inférieur grisé
    geom_tile(
      data = ~subset(.x, !upper_triangle),
      fill = "grey90"
    ) +
    
    # triangle supérieur coloré
    geom_tile(
      data = ~subset(.x, upper_triangle),
      aes(fill = cor)
    ) +
    
    # labels (corrélation + N)
    geom_text(
      data = ~subset(.x, upper_triangle),
      aes(label = paste0(round(cor, 2), "\nN=", n)),
      size = 3
    ) +
    
    scale_fill_gradient2(
      low = "blue",
      mid = "white",
      high = "red",
      midpoint = 0,
      limits = c(-1, 1)
    ) +
    
    theme_minimal() +
    labs(title = title, x = "", y = "")
}

plots <- heatmaps %>%
  mutate(plot = map2(data, paste(bias, indice,"régimes présidentiels", sep = " — "), plot_heatmap))


walk2(
  plots$plot,
  paste0("results/figures/heatmap_presidentiels", plots$bias, "_", plots$indice, ".png"),
  ~ ggsave(
    filename = .y,
    plot = .x,
    width = 10,
    height = 7,
    dpi = 300
  )
)

#Visualiser les heatmap
plots$plot[[1]]  #Androcracy - Gouvernement
plots$plot[[4]]  #Epistocracy - Gouvernement
plots$plot[[7]]  #Gerontocracy - Gouvernement
plots$plot[[10]] #Plutocracy - Gouvernement

plots$plot[[2]] #Androcracy - Participation
plots$plot[[5]] #Epistocracy - Participation
plots$plot[[8]] #Gerontocracy - Participation
plots$plot[[11]] #Plutocracy - Participation

plots$plot[[3]] #Androcracy - Votes → Ministres
plots$plot[[6]] #Epistocracy - Votes → Ministres
plots$plot[[9]] #Gerontocracy - Votes → Ministres
plots$plot[[12]] #Plutocracy - Votes → Ministres





#____________________________________________________________________________
#Graphiques articles ----
Base_legislative_finale <- Base_legislative_finale %>% mutate(regime = "legislatif")
Base_finale_presidentielles <- Base_finale_presidentielles %>% mutate(regime = "présidentiel")

Base_all_regimes <- Base_legislative_finale %>%
  bind_rows(Base_finale_presidentielles) 

unique(Base_all_regimes$isoname)

#Evolution 1980 50 50 ----
base_graph_bias_countries_1980_50 <- Base_all_regimes %>%
  filter(
    year >= 1980,
    !is.na(ratio_gouvernement_top_bot2),
    ratio_gouvernement_top_bot2 > 0
  ) %>%
  group_by(isoname) %>%
  filter(n_distinct(year) >= 10) %>%
  ungroup() %>%
  group_by(isoname, bias) %>%
  tidyr::complete(
    year = full_seq(year, 1)
  ) %>%
  fill(regime, .direction = "downup") %>%
  ungroup() %>%
  mutate(
    isoname_label = if_else(
      regime == "présidentiel",
      paste0(isoname, " *"),
      isoname
    )
  )


plot_evolution_bias_by_country_50 <- ggplot(
    base_graph_bias_countries_1980_50,
  aes(x = year,y = ratio_gouvernement_top_bot2,color = bias
  )
) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey50") +
  facet_wrap(~ isoname_label) +
  coord_cartesian(ylim = c(0,5)) +
  scale_color_manual(
    values = c(
      "androcracy" = "red",
      "epistocracy" = "green",
      "gerontocracy" = "blue",
      "plutocracy" = "violet")
    ) +
  labs(
    title = "Evolution dans le temps des indices par pays (top 50 / bot 50)",
    subtitle = "* Régime présidentiel",
    x = "Année",y = "ratio top/bot 50 50",color = "Indice"
  ) +
  
  theme_minimal()
print(plot_evolution_bias_by_country_50)
ggsave(
  filename = "results/figures/evolution bias by country top50 bot50.jpg",
  plot = plot_evolution_bias_by_country_50,width = 10,height = 6,dpi = 300)


#Evolution 1980 10 10 ----
base_graph_bias_countries_1980_10 <- Base_all_regimes %>%
  filter(
    year >= 1980,
    !is.na(ratio_gouvernement_top_bot),
    ratio_gouvernement_top_bot > 0
  ) %>%
  group_by(isoname) %>%
  filter(n_distinct(year) >= 10) %>%
  ungroup() %>%
  group_by(isoname, bias) %>%
  tidyr::complete(
    year = full_seq(year, 1)
  ) %>%
  fill(regime, .direction = "downup") %>%
  ungroup() %>%
  mutate(
    isoname_label = if_else(
      regime == "présidentiel",
      paste0(isoname, " *"),
      isoname
    )
  )


plot_evolution_bias_by_country_10 <- ggplot(
  base_graph_bias_countries_1980_10,
  aes(x = year,y = ratio_gouvernement_top_bot,color = bias
  )
) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey50") +
  facet_wrap(~ isoname_label) +
  coord_cartesian(ylim = c(0,5)) +
  scale_color_manual(
    values = c(
      "androcracy" = "red",
      "epistocracy" = "green",
      "gerontocracy" = "blue",
      "plutocracy" = "violet")
  ) +
  labs(
    title = "Evolution dans le temps des indices par pays (top 10 / bot 10)",
    subtitle = "* Régime présidentiel",
    x = "Année",y = "ratio top/bot 10 10",color = "Indice"
  ) +
  
  theme_minimal()
print(plot_evolution_bias_by_country_10)
ggsave(
  filename = "results/figures/evolution bias by country top10 bot10.jpg",
  plot = plot_evolution_bias_by_country_10,width = 10,height = 6,dpi = 300)



#Barres valeur moyenne des indices finaux ----
#50 50 ----
#Fonction 
# Colonnes de ratios à utiliser
getwd()
ratios <- names(Base_all_regimes) %>%
  str_subset("^ratio_gouvernement.*2$")  %>%
  setdiff("ratio_gouvernement_top_bot")

# Libellés des ratios (facultatif)
labels_ratios <- c(
  ratio_gouvernement_top_bot2 = "Gouvernement - Indice top 50 / bot 50"
)
names(Base_all_regimes)
plot_bias <- function(bias_value){
  
  base_bias <- Base_all_regimes %>%
    filter(
      bias == bias_value,
      year >= 2000
    ) %>%
    mutate(
      isoname_label = if_else(
        regime == "présidentiel",
        paste0(isoname, " *"),
        isoname
      )) %>%
    group_by(isoname,isoname_label) %>%
    filter(n_distinct(year) >= 10) %>%
    summarise(
      across(
        all_of(ratios),
        ~{
          x <- .x[!is.na(.x) & .x > 0]
          
          if(length(x) == 0){
            NA_real_
          } else{
            exp(mean(log(x)))
          }
        }
      ),
      .groups = "drop"
    ) %>%
    pivot_longer(
      cols = all_of(ratios),
      names_to = "ratio",
      values_to = "moyenne_geo"
    ) %>%
    mutate(
      ratio = recode(ratio, !!!labels_ratios),
      ratio = factor(
        ratio,
        levels = labels_ratios
      )
    )
  
  ggplot(
    base_bias,
    aes(
      x = reorder(isoname_label, moyenne_geo, mean, na.rm = TRUE),
      y = log(moyenne_geo),
      fill = ratio
    )
  ) +
    geom_col(position = position_dodge(width = .8)) +
    geom_hline(
      yintercept = 0,
      linetype = "dashed",
      colour = "grey50"
    ) +
    scale_y_continuous(
      breaks = log(c(0.25, 0.5, 1, 2, 4)),
      labels = c("0.25", "0.5", "1", "2", "4")
    ) +
    labs(
      title = paste("Bias :", bias_value, "- top 50 / bottom 50"),
      subtitle = "* Régime présidentiel",
      x = "Pays",
      y = "Moyenne géométrique depuis 2000",
      fill = "Ratio"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(
        angle = 90,
        hjust = 1,
        vjust = .5
      )
    )
  
}

biais <- c(
  "plutocracy",
  "androcracy",
  "gerontocracy",
  "epistocracy"
)

walk(
  biais,
  function(b){
    
    p <- plot_bias(b)
    
    ggsave(
      filename = paste0(
        "results/figures/Mean_",
        b,
        "top50_bot50_by_country.jpg"
      ),
      plot = p,
      width = 16,
      height = 8,
      dpi = 300
    )
    
  }
) #Les graphiques s'exportent directement dans "results/figures"

#10 10 ----
ratios <- names(Base_all_regimes) %>%
  str_subset("ratio_gouvernement") %>%
  setdiff("ratio_gouvernement_top_bot2")

# Libellés des ratios (facultatif)
labels_ratios <- c(
  ratio_gouvernement_top_bot = "Gouvernement - top10 / bot 10"
)
ratios
names(Base_all_regimes)
plot_bias <- function(bias_value){
  
  base_bias <- Base_all_regimes %>%
    filter(
      bias == bias_value,
      year >= 2000
    ) %>%
    mutate(
      isoname_label = if_else(
        regime == "présidentiel",
        paste0(isoname, " *"),
        isoname
      )) %>%
    group_by(isoname,isoname_label) %>%
    filter(n_distinct(year) >= 10) %>%
    summarise(
      across(
        all_of(ratios),
        ~{
          x <- .x[!is.na(.x) & .x > 0]
          
          if(length(x) == 0){
            NA_real_
          } else{
            exp(mean(log(x)))
          }
        }
      ),
      .groups = "drop"
    ) %>%
    pivot_longer(
      cols = all_of(ratios),
      names_to = "ratio",
      values_to = "moyenne_geo"
    ) %>%
    mutate(
      ratio = recode(ratio, !!!labels_ratios),
      ratio = factor(
        ratio,
        levels = labels_ratios
      )
    )
  
  ggplot(
    base_bias,
    aes(
      x = reorder(isoname_label, moyenne_geo, mean, na.rm = TRUE),
      y = log(moyenne_geo),
      fill = ratio
    )
  ) +
    geom_col(position = position_dodge(width = .8)) +
    geom_hline(
      yintercept = 0,
      linetype = "dashed",
      colour = "grey50"
    ) +
    scale_y_continuous(
      breaks = log(c(0.25, 0.5, 1, 2, 4)),
      labels = c("0.25", "0.5", "1", "2", "4")
    ) +
    labs(
      title = paste("Bias :", bias_value, "- top 10 / bottom 10"),
      subtitle = "* Régime présidentiel",
      x = "Pays",
      y = "Moyenne géométrique depuis 2000",
      fill = "Ratio"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(
        angle = 90,
        hjust = 1,
        vjust = .5
      )
    )
  
}

biais <- c(
  "plutocracy",
  "gerontocracy",
  "epistocracy"
)

walk(
  biais,
  function(b){
    
    p <- plot_bias(b)
    
    ggsave(
      filename = paste0(
        "results/figures/Mean_",
        b,
        "top10_bot10_by_country.jpg"
      ),
      plot = p,
      width = 16,
      height = 8,
      dpi = 300
    )
    
  }
)





#Les deux mélangés ----
ratios <- names(Base_all_regimes) %>%
  str_subset("ratio_gouvernement")

# Libellés des ratios (facultatif)
labels_ratios <- c(
  ratio_gouvernement_top_bot = "Gouvernement - Indice top 10 / bot 10",
  ratio_gouvernement_top_bot2 = "Gouvernement - Indice top 50 / bot 50"
)
names(Base_all_regimes)
plot_bias <- function(bias_value){
  
  base_bias <- Base_all_regimes %>%
    filter(
      bias == bias_value,
      year >= 2000
    ) %>%
    mutate(
      isoname_label = if_else(
        regime == "présidentiel",
        paste0(isoname, " *"),
        isoname
      )) %>%
    group_by(isoname,isoname_label) %>%
    filter(n_distinct(year) >= 10) %>%
    summarise(
      across(
        all_of(ratios),
        ~{
          x <- .x[!is.na(.x) & .x > 0]
          
          if(length(x) == 0){
            NA_real_
          } else{
            exp(mean(log(x)))
          }
        }
      ),
      .groups = "drop"
    ) %>%
    pivot_longer(
      cols = all_of(ratios),
      names_to = "ratio",
      values_to = "moyenne_geo"
    ) %>%
    mutate(
      ratio = recode(ratio, !!!labels_ratios),
      ratio = factor(
        ratio,
        levels = labels_ratios
      )
    )
  
  ggplot(
    base_bias,
    aes(
      x = reorder(isoname_label, moyenne_geo, mean, na.rm = TRUE),
      y = log(moyenne_geo),
      fill = ratio
    )
  ) +
    geom_col(position = position_dodge(width = .8)) +
    geom_hline(
      yintercept = 0,
      linetype = "dashed",
      colour = "grey50"
    ) +
    scale_y_continuous(
      breaks = log(c(0.25, 0.5, 1, 2, 4)),
      labels = c("0.25", "0.5", "1", "2", "4")
    ) +
    labs(
      title = paste("Bias :", bias_value),
      subtitle = "* Régime présidentiel",
      x = "Pays",
      y = "Moyenne géométrique depuis 2000",
      fill = "Ratio"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(
        angle = 90,
        hjust = 1,
        vjust = .5
      )
    )
  
}

biais <- c(
  "plutocracy",
  "gerontocracy",
  "epistocracy"
)

walk(
  biais,
  function(b){
    
    p <- plot_bias(b)
    
    ggsave(
      filename = paste0(
        "results/figures/Mean_",
        b,
        "10_and_50_by_country.jpg"
      ),
      plot = p,
      width = 16,
      height = 8,
      dpi = 300
    )
    
  }
)


names(Base_all_regimes)


#EXPLORATOIRE ----
#ANDROCRATIE ----
base_androcracy <- Base_all_regimes %>% filter(bias == "androcracy")

base_plutocracy <- Base_all_regimes %>% filter(bias == "plutocracy")

base_epistocracy <- Base_all_regimes %>% filter(bias == "epistocracy")

base_gerontocracy <- Base_all_regimes %>% filter(bias == "gerontocracy")

#Autres boxplot ----
base_androcracy_plot <- Base_all_regimes %>%
  filter(bias == "androcracy",
    year >= 1980,
    !is.na(ratio_gouvernement_top_bot2),
    ratio_gouvernement_top_bot2 > 0) %>%
  group_by(isoname) %>%
  filter(n_distinct(year) >= 10) %>%
  ungroup() %>%
  group_by(isoname, bias) %>%
  tidyr::complete(year = full_seq(year, 1)
  ) %>%
  fill(regime, .direction = "downup") %>%
  ungroup() %>%
  mutate(isoname_label = if_else(regime == "présidentiel",paste0(isoname, " *"),isoname))


plot_evolution_androcracy_by_country <- ggplot(
  base_androcracy_plot,
  aes(x = year,y = ratio_gouvernement_top_bot2,color = bias
  )
) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey50") +
  facet_wrap(~ isoname_label) +
  coord_cartesian(ylim = c(0,2)) +

  labs(
    title = "Evolution dans le temps des indices d'androcratie par pays",
    subtitle = "* Régime présidentiel",
    x = "Année",y = "ratio tob/bot 50 50",color = "Indice"
  ) +
  
  theme_minimal()
print(plot_evolution_androcracy_by_country)
ggsave(
  filename = "results/figures androcracy/evolution adrocracy par pays.jpg",
  plot = plot_evolution_androcracy_by_country,width = 10,height = 6,dpi = 300)






#Mêrme chose avec l'androcratie au parlement
base_androcracy_parlement_plot <- Base_all_regimes %>%
  filter(bias == "androcracy",
         year >= 1980,
         !is.na(ratio_sieges_top_bot2),
         ratio_sieges_top_bot2 > 0) %>%
  group_by(isoname) %>%
  filter(n_distinct(year) >= 10) %>%
  ungroup() %>%
  group_by(isoname, bias) %>%
  tidyr::complete(year = full_seq(year, 1)
  ) %>%
  fill(regime, .direction = "downup") %>%
  ungroup() %>%
  mutate(isoname_label = if_else(regime == "présidentiel",paste0(isoname, " *"),isoname))


plot_evolution_androcracy_parlement_by_country <- ggplot(
  base_androcracy_parlement_plot,
  aes(x = year,y = ratio_sieges_top_bot2,color = bias
  )) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey50") +
  facet_wrap(~ isoname_label) +
  coord_cartesian(ylim = c(0,2)) +
  
  labs(
    title = "Evolution dans le temps des indices d'androcratie au parlement par pays",
    subtitle = "* Régime présidentiel",
    x = "Année",y = "ratio tob/bot 50 50",color = "Indice"
  ) +
  
  theme_minimal()
print(plot_evolution_androcracy_parlement_by_country)
ggsave(
  filename = "results/figures androcracy/evolution adrocracy au parlement par pays.jpg",
  plot = plot_evolution_androcracy_parlement_by_country,width = 10,height = 6,dpi = 300)

#Test androcracy ----
cor(base_androcracy$women_share_government, base_androcracy$Percentage.of.women.diputees, 
    use = "complete.obs")



#Représentation des femmes ----
parline <- read.csv("data/raw/parline/share of women diputees accross all countries.csv", sep = ";")
whogov_clean <- read.csv("data/intermediary/government/whogov clean.csv", sep = ",")

#vérification rapide qu'il n'y a qu'une seule valeur pour le taux de femmes ministres par pays/années
whogov_clean %>%
  group_by(isoname, year) %>%
  summarise(n_valeurs = n_distinct(women_share_government),
    .groups = "drop") %>% filter(n_valeurs > 1)

parline <- parline %>%
  mutate(election_year = substr(date_from, nchar(date_from) - 3, nchar(date_from)))

parline <- parline %>%
  rename(isoname = Country)
parline <- parline %>%
  rename(Percentage.of.women.diputees = Percentage.of.women)
parline <- parline %>%
  mutate(election_year = as.integer(election_year))

parline <- parline %>%
  arrange(isoname, election_year) %>%   # si tu as une date
  group_by(isoname, election_year) %>%
  slice_tail(n = 1) %>%                                # garde la dernière élection de l'année
  ungroup()


parline <- parline %>%
  arrange(isoname, election_year) %>%
  group_by(isoname) %>%
  group_modify(~{
    
    dat <- arrange(.x, election_year)
    
    map_dfr(seq_len(nrow(dat)), function(i){
      
      ligne <- dat[i, ]
      
      debut <- ligne$election_year
      
      if(i < nrow(dat)){
        fin <- dat$election_year[i + 1] - 1
      } else {
        fin <- debut
      }
      
      ligne[rep(1, fin - debut + 1), ] %>%
        mutate(year = debut:fin)
      
    })
    
  }) %>%
  ungroup()

#vérif qu'on ait une observation par pays/année (ça doit renvoyer 0 observations)
parline %>%
  count(isoname, year) %>%
  filter(n > 1)


whogov_unique <- whogov_clean %>%
  distinct(isoname, year, .keep_all = TRUE)

parline <- parline %>%
  mutate(
    isoname = case_when(
      isoname == "Türkiye" ~ "Turkey",
      isoname == "Venezuela (Bolivarian Republic of)" ~ "Venezuela",
      isoname == "Republic of Korea" ~ "South Korea",
      TRUE ~ isoname))



women_representation <- parline %>%
  left_join(
    whogov_unique %>%
      select(isoname, year, women_share_government),
    by = c("isoname", "year"))

women_representation <- women_representation %>%
  filter(isoname %in% Base_all_regimes$isoname)

#Graphique représentation des femmes dans les institutions par pays  ----
women_representation <- women_representation %>%
  mutate(women_share_government = women_share_government * 100)

women_representation <- women_representation %>%
  mutate(Percentage.of.women.diputees = as.numeric(Percentage.of.women.diputees))



women_representation_long <- women_representation %>%
  pivot_longer(
    cols = c(Percentage.of.women.diputees, women_share_government),
    names_to = "Indice",
    values_to = "Value")



plot_women_representation <- ggplot(
  women_representation_long,
  aes(x = year,y = Value,color = Indice,group = Indice
  )
) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 50, linetype = "dashed", color = "grey50") +
  facet_wrap(~ isoname) +
  coord_cartesian(ylim = c(0, 100)) +
  scale_color_manual(
    values = c(
      "Percentage.of.women.diputees" = "blue",
      "women_share_government" = "orange"),
    labels = c(
      "Percentage.of.women.diputees" = "Proportion de femmes élues au Parlement (%)",
      "women_share_government" = "Proportion de femmes ministres (%)"
    )) +
  labs(
    x = "Année",y = "% de femmes dans les institutions",color = "Indice"
  ) +
  
  theme_minimal()
print(plot_women_representation)
ggsave(
  filename = "results/figures androcracy/taux de femmes deputees et ministres par pays.jpg",
  plot = plot_women_representation,width = 10,height = 6,dpi = 300)




women_representation_androcracy <- women_representation %>%
  left_join(base_androcracy %>% select(isoname, year, ratio_gouvernement_top_bot2,ratio_sieges_top_bot2),
            by = c("isoname", "year"))

women_representation_plutocracy <- women_representation %>%
  left_join(base_plutocracy %>% select(isoname, year, ratio_gouvernement_top_bot2,ratio_sieges_top_bot2),
            by = c("isoname", "year"))

women_representation_epistocracy <- women_representation %>%
  left_join(base_epistocracy %>% select(isoname, year, ratio_gouvernement_top_bot2,ratio_sieges_top_bot2),
            by = c("isoname", "year"))

women_representation_gerontocracy <- women_representation %>%
  left_join(base_gerontocracy %>% select(isoname, year, ratio_gouvernement_top_bot2,ratio_sieges_top_bot2),
            by = c("isoname", "year"))


#Régression entre les biais et la proportion de femmes ministres ----
library(modelsummary)
reg_androcracy_femmes_ministres <- lm(
  women_share_government ~ ratio_gouvernement_top_bot2 + year + factor(isoname),
  data = women_representation_androcracy)
summary(reg_androcracy_femmes_ministres)

##Test avec d'autres biais ----
### Education et femmes ministres ----
reg_epistocracy_femmes_ministres <- lm(
  women_share_government ~ ratio_gouvernement_top_bot2 + year + factor(isoname),
  data = women_representation_epistocracy)

### Revenu et femmes ministres ----
women_representation_plutocracy <- women_representation_plutocracy %>%
  filter(!is.infinite(ratio_gouvernement_top_bot2))

reg_plutocracy_femmes_ministres <- lm(
  women_share_government ~ ratio_gouvernement_top_bot2 + year + factor(isoname),
  data = women_representation_plutocracy)
summary(reg_plutocracy_femmes_ministres)


### Age et femmes ministres ----
reg_gerontocracy_femmes_ministres <- lm(
  women_share_government ~ ratio_gouvernement_top_bot2 + year + factor(isoname),
  data = women_representation_gerontocracy)
summary(reg_gerontocracy_femmes_ministres)


library(modelsummary)
modelsummary(
  list(
    "Androcratie et taux de femmes ministres" = reg_androcracy_femmes_ministres,
    "Epistocratie et taux de femmes ministres" = reg_epistocracy_femmes_ministres,
    "Ploutocratie et taux de femmes ministres" = reg_plutocracy_femmes_ministres,
    "Gérontocratie et taux de femmes ministres" = reg_gerontocracy_femmes_ministres
  ),
  coef_map = c(
    "(Intercept)" = "Intercept",
    "ratio_gouvernement_top_bot2" = "Indice de représentation au gouvernement (top50 / bot50)",
    "year" = "Année supplémentaire",
    "session.label_recodePublic Good" = "Public Good",
    "revele_cache_recodeRevele:session.label_recodeAltruistic" = "Révélé × Altruistic",
    "revele_cache_recodeRevele:session.label_recodePublic Good" = "Révélé × Public Good"
  ),
  stars = c('*' = .05, '**' = .01, '***' = .001),
  statistic = "std.error",
  fmt = 3,
  title = "Effet de nos indices de représentation au gouvernement sur le taux de femmes ministres, par clivages 
  (contrôle de l'année et du pays) ",
  output = "results/tables androcracy/indices par clivages et femmes ministres.tex"
)



#Même chose avec l'androcratie au Parlement et le taux de femmes députées
reg_androcracy_femmes_deputees <- lm(
  women_share_government ~ ratio_sieges_top_bot2 + year + factor(isoname),
  data = women_representation_androcracy)
summary(reg_androcracy_femmes_deputees)

##Test avec d'autres biais ----
### Education et femmes ministres ----
reg_epistocracy_femmes_deputees <- lm(
  women_share_government ~ ratio_sieges_top_bot2 + year + factor(isoname),
  data = women_representation_epistocracy)

### Revenu et femmes ministres ----
women_representation_plutocracy <- women_representation_plutocracy %>%
  filter(!is.infinite(ratio_gouvernement_top_bot2))

reg_plutocracy_femmes_deputees <- lm(
  women_share_government ~ ratio_sieges_top_bot2 + year + factor(isoname),
  data = women_representation_plutocracy)
summary(reg_plutocracy_femmes_deputees)


### Age et femmes ministres ----
reg_gerontocracy_femmes_deputees <- lm(
  women_share_government ~ ratio_sieges_top_bot2 + year + factor(isoname),
  data = women_representation_gerontocracy)
summary(reg_gerontocracy_femmes_deputees)


library(modelsummary)
modelsummary(
  list(
    "Androcratie et taux de femmes députées" = reg_androcracy_femmes_deputees,
    "Epistocratie et taux de femmes députées" = reg_epistocracy_femmes_deputees,
    "Ploutocratie et taux de femmes députées" = reg_plutocracy_femmes_deputees,
    "Gérontocratie et taux de femmes députées" = reg_gerontocracy_femmes_deputees
  ),
  coef_map = c(
    "(Intercept)" = "Intercept",
    "ratio_sieges_top_bot2" = "Indice de représentation au Parlement (top50 / bot50)",
    "year" = "Année supplémentaire"
  ),
  stars = c('*' = .05, '**' = .01, '***' = .001),
  statistic = "std.error",
  fmt = 3,
  title = "Effet de nos indices de représentation au Parlement sur le taux de femmes députées, par clivages 
  (contrôle de l'année et du pays) ",
  output = "results/tables androcracy/indices au parlement par clivages et femmes députées.tex"
)

























#Brouillon ----
#Bias et femmes ministres----
##Graphiques ----
ggplot(
  women_representation,
  aes(x = ratio_gouvernement_top_bot2,y = women_share_government)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "Ratio gouvernement top/bottom",y = "Part des femmes au gouvernement (%)") +
  theme_minimal()

ggplot(Base_complete_index_educ,
       aes(x = ratio_gouvernement_top_bot2, y = women_share_government)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "Ratio gouvernement éducation top/bottom",y = "Part des femmes au gouvernement (%)") +
  theme_minimal()

ggplot(Base_complete_index_income,
       aes(x = ratio_gouvernement_top_bot2,y = women_share_government)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "Ratio gouvernement revenu top/bottom",y = "Part des femmes au gouvernement (%)") +
  theme_minimal()

ggplot(Base_complete_index_age,
       aes(x = ratio_gouvernement_top_bot2,y = women_share_government)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "Ratio gouvernement âge top/bottom", y = "Part des femmes au gouvernement (%)") +
  theme_minimal()

