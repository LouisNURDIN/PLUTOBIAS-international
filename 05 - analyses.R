library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(scales)
Base_complete_index <-  read.csv("data/final/dataset complete with index.csv", sep = ",")
Base_regimes_presidentiels_index <-  read.csv("data/final/dataset complete regimes presidentiels.csv", sep = ",")

Base_complete_index_income <- Base_complete_index %>%filter(Base_complete_index$bias == "plutocracy")

Base_complete_index_gender<- Base_complete_index %>%filter(Base_complete_index$bias == "androcracy")

Base_complete_index_educ <- Base_complete_index%>%filter(Base_complete_index$bias == "epistocracy")

Base_complete_index_age <- Base_complete_index %>%filter(Base_complete_index$bias == "gerontocracy")

#Filtre pour travailler sur des bases propres
Base_complete_index_income <- Base_complete_index_income %>%
  filter(election_couverture_seats >= 80 & election_couverture_ministers >= 0.80)

Base_complete_index_gender <- Base_complete_index_gender %>%
  filter(election_couverture_seats >= 80 & election_couverture_ministers >= 0.80)

Base_complete_index_educ <- Base_complete_index_educ %>%
  filter(election_couverture_seats >= 80 & election_couverture_ministers >= 0.80)

Base_complete_index_age <- Base_complete_index_age %>%
  filter(election_couverture_seats >= 80 & election_couverture_ministers >= 0.80)


#Code pour chaque biais et par source ----
##Box plot plutocracy----
# 2. Long format
Base_income_long <- Base_complete_index_income %>%
  pivot_longer(
    cols = starts_with("ratio_"),
    names_to = "Indice",
    values_to = "Value"
  ) 

# 3. Séparation 10/10 et 50/50
data_50_50 <- Base_income_long %>% filter(str_detect(Indice, "top_bot2"))
data_10_10 <- Base_income_long %>% filter(str_detect(Indice, "top_bot"))

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

data_50_50 <- recodage(data_50_50)
data_10_10 <- recodage(data_10_10)

### PLOT 50/50 ----
p_50_50 <- ggplot(data_50_50, aes(x = Indice, y = Value, fill = Indice)) +
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
    breaks = c(0.3, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,2.5,3,3.5,4,4.5,5)) +
  
  coord_cartesian(ylim = c(0.3, 5)) +
  
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

p_10_10 <- ggplot(data_10_10, aes(x = Indice, y = Value, fill = Indice)) +
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
  
  coord_cartesian(ylim = c(0.3,5 )) +
  
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
p_50_50
ggsave(
  filename = "results/figures/Boxplot global bias 50 50.jpg",
  plot = p_50_50,width = 10,height = 6,dpi = 300)

grid::grid.newpage()
p_10_10
ggsave(
  filename = "results/figures/Boxplot global bias 50 50.jpg",
  plot = p_10_10,width = 10,height = 6,dpi = 300)

##Box plot androcracy ----
Base_gender_long <- Base_complete_index_gender %>%
  pivot_longer(
    cols = starts_with("ratio_") & !ends_with("top_bot2"),
    names_to = "Indice",
    values_to = "Value"
  ) 

# 3. Séparation 10/10 et 50/50
data_top_bot_gender <- Base_gender_long %>% filter(str_detect(Indice, "top_bot"))

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

data_top_bot_gender <- recodage(data_top_bot_gender)

### PLOT 50/50 ----
plot_top_bot_gender <- ggplot(data_top_bot_gender, aes(x = Indice, y = Value, fill = Indice)) +
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
plot_top_bot_gender
ggsave(
  filename = "results/figures/Boxplot global bias 50 50.jpg",
  plot = plot_top_bot_gender,width = 10,height = 6,dpi = 300)

##Box plot epistocracy ----
Base_educ_long <- Base_complete_index_educ %>%
  pivot_longer(
    cols = starts_with("ratio_") & !ends_with("top_bot2"),
    names_to = "Indice",
    values_to = "Value"
  ) 

# 3. Séparation 10/10 et 50/50
data_top_bot_educ <- Base_educ_long %>% filter(str_detect(Indice, "top_bot"))

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

data_top_bot_educ <- recodage(data_top_bot_educ)


plot_top_bot_educ <- ggplot(data_top_bot_educ, aes(x = Indice, y = Value, fill = Indice)) +
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
    title = "Distribution des indices d'épistocratie",
    x = "",
    y = "Poids électoral 50% plus diplômés / 50% moins diplômés"
  )

grid::grid.newpage()
plot_top_bot_educ
ggsave(
  filename = "results/figures/Boxplot global bias 50 50.jpg",
  plot = plot_top_bot_educ,width = 10,height = 6,dpi = 300)

##Box plot gerontocracy ----
Base_age_long <- Base_complete_index_age %>%
  pivot_longer(
    cols = starts_with("ratio_") & !ends_with("top_bot2"),
    names_to = "Indice",
    values_to = "Value"
  ) 

# 3. Séparation 10/10 et 50/50
data_top_bot_age <- Base_age_long %>% filter(str_detect(Indice, "top_bot"))

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

data_top_bot_age <- recodage(data_top_bot_age)


plot_top_bot_age <- ggplot(data_top_bot_age, aes(x = Indice, y = Value, fill = Indice)) +
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
    title = "Distribution des indices de gérontocratie",
    x = "",
    y = "Poids électoral 50% plus âgés / 50% moins âgés"
  )

grid::grid.newpage()
plot_top_bot_age
ggsave(
  filename = "results/figures/Boxplot global bias 50 50.jpg",
  plot = plot_top_bot_age,width = 10,height = 6,dpi = 300)

#Box plot indice avec tous les biais ----
plot_all_global_bias_50_50 <- ggplot(Base_complete_index, aes(x = bias, y = ratio_gouvernement_top_bot2, fill = bias)) +
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
    breaks = c(0.5, 0.75, 1, 1.25, 1.5, 1.75, 2)) +
  
  coord_cartesian(ylim = c(0.5, 2)) +
  
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),
    text = element_text(size = 14) ) +
  
  labs(
    title = "Distribution des indices globaux par biais et sources",
    x = "",
    y = "Poids électoral top 50% / bottom 50%"
  )

grid::grid.newpage()
plot_all_global_bias_50_50

ggsave(
filename = "results/figures/Boxplot global bias 50 50.jpg",
plot = plot_all_global_bias_50_50,width = 10,height = 6,dpi = 300)

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



#Régression entre androcracy et représentation des femmes dans les institutions
ggplot(
  Base_complete_index_gender,
  aes(
    x = ratio_gouvernement_top_bot,
    y = women_share_government
  )
) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    x = "Ratio gouvernement top/bottom",
    y = "Part des femmes au gouvernement (%)"
  ) +
  theme_minimal()


library(modelsummary)
reg_androcracy_femmes_ministres <- lm(
  women_share_government ~ ratio_gouvernement_top_bot2 + year,
  data = Base_complete_index_gender
)
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
  title = "Effet de l'androcratie sur le taux de femmes ministres, en contrôlant par l'année"
)


