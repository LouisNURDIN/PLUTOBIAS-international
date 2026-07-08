library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(scales)
library(fixest)
library(lubridate)
library(purrr)

Base_complete_legislative_index <-  read.csv("data/final/legislative dataset complete with index.csv", sep = ",")
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


#Filtre pour garder pour chaque combinaison pays/année/biais la meilleure source
Base_complete_legislative_best_sources <- Base_complete_legislative_index %>%
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
Base_complete_legislative_best_sources <- Base_complete_legislative_best_sources %>%
  filter(election_couverture_seats >= 80 & election_couverture_ministers >= 0.80)

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
#Création des datasets par biais
Base_regimes_presidentiels_index <-  read.csv("data/final/dataset complete regimes presidentiels.csv", sep = ",")

Base_legislative_index_income <- Base_legislative_finale %>%filter(Base_legislative_finale$bias == "plutocracy")

Base_legislative_index_gender<- Base_legislative_finale %>%filter(Base_legislative_finale$bias == "androcracy")

Base_legislative_index_educ <- Base_legislative_finale %>%filter(Base_legislative_finale$bias == "epistocracy")

Base_legislative_index_age <- Base_legislative_finale %>%filter(Base_legislative_finale$bias == "gerontocracy")

#Filtre pour travailler sur des bases propres
Base_legislative_index_income <- Base_legislative_index_income %>%
  filter(election_couverture_seats >= 80 & election_couverture_ministers >= 0.80)

Base_legislative_index_gender <- Base_legislative_index_gender %>%
  filter(election_couverture_seats >= 80 & election_couverture_ministers >= 0.80)

Base_legislative_index_educ <- Base_legislative_index_educ %>%
  filter(election_couverture_seats >= 80 & election_couverture_ministers >= 0.80)

Base_legislative_index_age <- Base_legislative_index_age %>%
  filter(election_couverture_seats >= 80 & election_couverture_ministers >= 0.80)


#Code pour chaque biais et par source ----
##Box plot plutocracy----
# 2. Long format
Base_income_legislative_long <- Base_legislative_index_income %>%
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

##Box plot androcracy ----
Base_gender_legislative_long <- Base_legislative_index_gender %>%
  pivot_longer(
    cols = starts_with("ratio_") & !ends_with("top_bot2"),
    names_to = "Indice",
    values_to = "Value"
  ) 

# 3. Séparation 10/10 et 50/50
data_top_bot_gender_legislative <- Base_gender_legislative_long %>% filter(str_detect(Indice, "top_bot"))

# 4. Recodage commun
recodage <- function(df) {
  df %>%
    mutate(
      Indice = recode(
        Indice,
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



data_top_bot_gender_legislative <- recodage(data_top_bot_gender_legislative)

data_top_bot_gender_legislative <- data_top_bot_gender_legislative %>%
  filtre_annees_electorales()
### PLOT 50/50 ----
plot_top_bot_gender_legislative <- ggplot(data_top_bot_gender_legislative, aes(x = Indice, y = Value, fill = Indice)) +
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
plot_top_bot_gender_legislative
ggsave(
  filename = "results/figures/Boxplot androcracy 50 50.jpg",
  plot = plot_top_bot_gender_legislative,width = 10,height = 6,dpi = 300)

##Box plot epistocracy ----
Base_educ_legislative_long <- Base_legislative_index_educ %>%
  pivot_longer(
    cols = starts_with("ratio_") & !ends_with("top_bot2"),
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

##Box plot gerontocracy ----
Base_age_legislatives_long <- Base_legislative_index_age %>%
  pivot_longer(
    cols = starts_with("ratio_") & !ends_with("top_bot2"),
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


#Box plot indice avec tous les biais ----
plot_all_global_bias_50_50_legislatives <- ggplot(Base_legislative_finale, aes(x = bias, y = ratio_gouvernement_top_bot2, fill = bias)) +
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
  facet_wrap(~ source_recode) +
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
    title = "Distribution des indices globaux par biais et sources > 80% des députés et ministres)",
    x = "",
    y = "Poids électoral top 50% / bottom 50%"
  )

grid::grid.newpage()
plot_all_global_bias_50_50_legislatives

ggsave(
filename = "results/figures/Boxplot global bias 50 50.jpg",
plot = plot_all_global_bias_50_50_legislatives,width = 10,height = 6,dpi = 300)


#HEATMAP corrélations biais/indices ----
##Garder la meilleure source au sein de chaque source_recode
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
  filter(election_couverture_seats >= 80 & election_couverture_ministers >= 0.80)

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

##Prépa base heatmap ----
Base_legislative_global_sources_long <- Base_legislative_global_sources %>%
  pivot_longer(
    cols = starts_with("ratio_") & !ends_with("top_bot2"),
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

setwd("results/figures")

walk2(
  plots$plot,
  paste0("heatmap_", plots$bias, "_", plots$indice, ".png"),
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
  filter(election_couverture_ministers >= 0.80)

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
    
    .groups = "drop"
  )
#Création des datasets par biais
Base_finale_presidentielles_income <- Base_finale_presidentielles %>%filter(Base_finale_presidentielles$bias == "plutocracy")

Base_finale_presidentielles_gender<- Base_finale_presidentielles %>%filter(Base_finale_presidentielles$bias == "androcracy")

Base_finale_presidentielles_educ <- Base_finale_presidentielles %>%filter(Base_finale_presidentielles$bias == "epistocracy")

Base_finale_presidentielles_age <- Base_finale_presidentielles %>%filter(Base_finale_presidentielles$bias == "gerontocracy")

#Filtre pour travailler sur des bases propres
Base_finale_presidentielles_income <- Base_finale_presidentielles_income %>%
  filter(election_couverture_ministers >= 0.80)

Base_finale_presidentielles_gender <- Base_finale_presidentielles_gender %>%
  filter(election_couverture_ministers >= 0.80)

Base_finale_presidentielles_educ <- Base_finale_presidentielles_educ %>%
  filter( election_couverture_ministers >= 0.80)

Base_finale_presidentielles_age <- Base_finale_presidentielles_age %>%
  filter(election_couverture_ministers >= 0.80)


#Code pour chaque biais et par source ----
##Box plot plutocracy----
# 2. Long format
Base_income_long_presidentielles <- Base_finale_presidentielles_income %>%
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
Base_gender_long_presidentielles <- Base_finale_presidentielles_gender %>%
  pivot_longer(
    cols = starts_with("ratio_") & !ends_with("top_bot2"),
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
        "ratio_participation_top_bot" = "Participation",
        "ratio_votes_valides_en_ministres_top_bot" = "Votes → Ministres",
        "ratio_gouvernement_top_bot" = "Gouvernement"
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
    title = "Distribution des indices d'androcratie",
    x = "",
    y = "Poids électoral des hommes comparé à celui des femmes"
  )

grid::grid.newpage()
plot_top_bot_gender_presidentielles
ggsave(
  filename = "results/figures/Boxplot androcracy presidentielles 50 50.jpg",
  plot = plot_top_bot_gender_presidentielles,width = 10,height = 6,dpi = 300)



##Box plot epistocracy ----
Base_educ_long_presidentielles <- Base_finale_presidentielles_educ %>%
  pivot_longer(
    cols = starts_with("ratio_") & !ends_with("top_bot2"),
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
        "ratio_participation_top_bot" = "Participation",
        "ratio_votes_valides_en_ministres_top_bot" = "Votes → Ministres ",
        "ratio_gouvernement_top_bot" = "Gouvernement"
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
Base_age_long_presidentielles <- Base_finale_presidentielles_age %>%
  pivot_longer(
    cols = starts_with("ratio_") & !ends_with("top_bot2"),
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
        "ratio_participation_top_bot" = "Participation",
        "ratio_votes_valides_en_ministres_top_bot" = "Votes → Ministres",
        "ratio_gouvernement_top_bot" = "Gouvernement"
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
  filename = "results/figures/Boxplot gerontocracy 50 50.jpg",
  plot = plot_top_bot_age_presidentielles,width = 10,height = 6,dpi = 300)


#Box plot indice avec tous les biais ----
plot_all_global_bias_50_50_presidentielles <- ggplot(Base_finale_presidentielles, aes(x = bias, y = ratio_gouvernement_top_bot2, fill = bias)) +
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
  facet_wrap(~ source_recode) +
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
    title = "Distribution des indices globaux dans les régimes présidentiels par biais et sources > 80% des députés et ministres)",
    x = "",
    y = "Poids électoral top 50% / bottom 50%"
  )

grid::grid.newpage()
plot_all_global_bias_50_50_presidentielles

ggsave(
  filename = "results/figures/Boxplot global bias presidential elections 50 50.jpg",
  plot = plot_all_global_bias_50_50_presidentielles,width = 10,height = 6,dpi = 300)


#HEATMAP corrélations biais/indices ----
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
  filter(election_couverture_ministers >= 0.80)

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

##Prépa base heatmap ----
Base_presidentielles_global_sources_long <- Base_presidentielles_global_sources %>%
  pivot_longer(
    cols = starts_with("ratio_") & !ends_with("top_bot2"),
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
  "Participation","Votes → ministres","Gouvernement")


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
  mutate(plot = map2(data, paste(bias, indice, sep = " — "), plot_heatmap))

setwd("results/figures")

walk2(
  plots$plot,
  paste0("heatmap_", plots$bias, "_", plots$indice, ".png"),
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
#EXPLORATOIRE ----
#Autres boxplot ----
ggplot(
  data_top_bot_gender %>%
    filter(Indice == "Gouvernement"),
  aes(x = source_recode, y = Value, fill = source_recode)
) +
  geom_boxplot() +
  facet_wrap(~ isoname) +
  scale_y_continuous(
    trans = scales::log_trans(),
    breaks = c(0.5, 0.75, 1, 1.25, 1.5, 1.75, 2)
  ) +
  coord_cartesian(ylim = c(0.5, 2))


#Plot de mes indices par biais ----
plot_bias_by_country <- ggplot(
  Base_complete_index_filtre,
  aes(x = year,y = ratio_gouvernement_top_bot2,color = bias
  )
) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey50") +
  facet_wrap(~ isoname) +
  coord_cartesian(ylim = c(0,4)) +
  scale_color_manual(
    values = c(
      "androcracy" = "red",
      "epistocracy" = "green",
      "gerontocracy" = "blue",
      "plutocracy" = "violet")
    ) +
  labs(
    x = "Année",y = "ratio tob/bot 50 50",color = "Indice"
  ) +
  
  theme_minimal()
print(plot_bias_by_country)



#Test androcracy ----
cor(Base_complete_index_gender$women_share_government, Base_complete_index_gender$Percentage.of.women.diputees, 
    use = "complete.obs")


#Box-plot représentation des femmes dans les institutions par pays  ----
Base_complete_index_gender <- Base_complete_index_gender %>%
  mutate(women_share_government = women_share_government * 100)

women_representation <- Base_complete_index_gender %>%
  pivot_longer(
    cols = c(Percentage.of.women.diputees, women_share_government),
    names_to = "Indice",
    values_to = "Value"
  )



#Représentation des femmes ----
plot_women_representation <- ggplot(
  women_representation,
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



#Bias et femmes ministres----
##Graphiques ----
ggplot(
  Base_complete_index_gender,
  aes(x = ratio_gouvernement_top_bot,y = women_share_government)) +
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


#Régression entre les biais et la proportion de femmes ministres ----
library(modelsummary)
reg_androcracy_femmes_ministres <- lm(
  women_share_government ~ ratio_gouvernement_top_bot2 + year + factor(isoname),
  data = Base_complete_index_gender)
summary(reg_androcracy_femmes_ministres)

modelsummary(
  reg_androcracy_femmes_ministres,
  coef_map = c(
    "(Intercept)" = "Intercept",
    "ratio_gouvernement_top_bot2" = "Indice global d'androcratie",
    "year" = "Année"
  ),
  statistic = "std.error",
  stars = c('*' = .05, '**' = .01, '***' = .001),
  fmt = 3,
  title = "Effet de l'androcratie sur le taux de femmes ministres, en contrôlant par l'année")


reg_androcracy_femmes_ministres2 <- feols(
  women_share_government ~ ratio_gouvernement_top_bot2 | isoname + year,
  data = Base_complete_index_gender)
summary(reg_androcracy_femmes_ministres2)

##Test avec d'autres biais ----
### Education et femmes ministres ----
reg_epistocracy_femmes_ministres <- lm(
  women_share_government ~ ratio_gouvernement_top_bot2 + year + factor(isoname),
  data = Base_complete_index_educ)
modelsummary(
  reg_epistocracy_femmes_ministres,
  coef_map = c(
    "(Intercept)" = "Intercept",
    "ratio_gouvernement_top_bot2" = "Indice global d'épistocratie",
    "year" = "Année"),
  statistic = "std.error", stars = c('*' = .05, '**' = .01, '***' = .001),
  fmt = 3,title = "Effet de l'épistocratie sur le taux de femmes ministres, en contrôlant par l'année")

### Revenu et femmes ministres ----
reg_plutocracy_femmes_ministres <- lm(
  women_share_government ~ ratio_gouvernement_top_bot2 + year + factor(isoname),
  data = Base_complete_index_income)
summary(reg_plutocracy_femmes_ministres)
modelsummary(
  reg_plutocracy_femmes_ministres,
  coef_map = c(
    "(Intercept)" = "Intercept",
    "ratio_gouvernement_top_bot2" = "Indice global de ploutocratie",
    "year" = "Année"),
  statistic = "std.error", stars = c('*' = .05, '**' = .01, '***' = .001),
  fmt = 3,title = "Effet de la ploutocratie sur le taux de femmes ministres, en contrôlant par l'année")



### Age et femmes ministres ----
reg_gerontocracy_femmes_ministres <- lm(
  women_share_government ~ ratio_gouvernement_top_bot2 + year + factor(isoname),
  data = Base_complete_index_age)
summary(reg_gerontocracy_femmes_ministres)

modelsummary(reg_gerontocracy_femmes_ministres,
  coef_map = c(
    "(Intercept)" = "Intercept",
    "ratio_gouvernement_top_bot2" = "Indice global de gérontocratie",
    "year" = "Année"),
  statistic = "std.error", stars = c('*' = .05, '**' = .01, '***' = .001),
  fmt = 3,title = "Effet de la gérontocratie sur le taux de femmes ministres, en contrôlant par l'année")



