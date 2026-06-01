#Base avec données gouvernements
base_vote_parlement_legislatives <- read.csv("data/intermediary/parliament/elections and parliament dataset.csv", sep = ",")
whogov <- read.csv("data/raw/whogov/WhoGov_within_V3.1.csv", sep = ";")

#Calcul nombre de ministres par année
whogov <- whogov %>%
  rename(isoname = country_name)

whogov_parties <- whogov %>%
  group_by(isoname, year, partyfacts_id) %>%
  summarise(
    ministers_party = n(),
    .groups = "drop_last"
  ) %>%
  mutate(
    total_ministers = sum(ministers_party),
    ministers_share = ministers_party / total_ministers
  ) %>%
  ungroup()

#Join whogov dans ma base vote-parlement
whogov_parties <- whogov_parties %>%
  mutate(year = as.integer(year))
whogov_parties <- whogov_parties %>%
  mutate(partyfacts_id = as.character(partyfacts_id))

base_vote_parlement_legislatives <- base_vote_parlement_legislatives %>%
  left_join(
    whogov_parties,
    by = c("isoname", "year", "partyfacts_id")
  )

#verif nombre de ministres couverts par élection
base_vote_parlement_legislatives <- base_vote_parlement_legislatives %>%
  mutate(
    ministers_party = coalesce(ministers_party, 0),
    total_ministers = coalesce(total_ministers, 0),
    ministers_share = coalesce(ministers_share, 0)
  ) %>%
  group_by(isoname, year, decile) %>%
  mutate(
    election_couverture_ministers = sum(ministers_share, na.rm = TRUE)
  ) %>%
  ungroup()

#Liste des pays/années avec données incohérentes ----
View(
  base_vote_parlement_legislatives %>%
    ungroup() %>%
    filter(election_couverture_ministers > 1) %>%
    distinct(year,isoname,election_couverture_ministers)
)

##Liste des pays/années où tous les ministres ne sont pas couverts ----
View(
  base_vote_parlement_legislatives %>%
    ungroup() %>%
    filter(election_couverture_ministers < 1) %>%
    distinct(year,isoname,election_couverture_ministers)
)

#Avec méthode dinc ----