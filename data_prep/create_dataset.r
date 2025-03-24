source(".Rprofile")
use_libraries("arrow", "duckdb", "tidyverse", "duckplyr", "ggeffects", "marginaleffects", "modelsummary")


# ----- UNCOMMENT TO CREATE DATABASE -- ONLY NECESSARY ONCE-------------

# ---- connect to database ----
# This will create the database file eu_tdb.duckdb
# con <- dbConnect(duckdb(), dbdir = "data/D2000_eu_tdb_duckdb/eu_tdb.duckdb")


# ---- create database(only do one time and then never again) ----
# This will fill the database with the data from the parquet files
# This will take very long and be demanding on your computer
# The final file eu_tdb.duckdb should be about 5GB
# The database is stored only on your pc
# dbExecute(con, "CREATE TABLE eu_tdb AS SELECT * FROM read_parquet('data/D1000_eu_tdb/*.parquet')")

# ----- UNCOMMENT TO CREATE DATABASE -- ONLY NECESSARY ONCE-------------



# ---- connect to database () ----
con <- dbConnect(duckdb(), dbdir = "data/D2000_eu_tdb_duckdb/eu_tdb.duckdb")



# ---- cast date from datetime to date----
eu_tdb_long <- tbl(con, "eu_tdb") %>%
  mutate(created_at = as.Date(created_at)) %>%
  filter(category != "STATEMENT_CATEGORY_ANIMAL_WELFARE")


# ---- create absolute counts per day per day (unfiltered) ----

eu_tdb_temp <- eu_tdb_long %>%
  group_by(created_at, platform_name, category) %>%
  summarise(
    sor = n(),
    automod = sum(case_match(automated_decision, "AUTOMATED_DECISION_NOT_AUTOMATED" ~ 0, .default = 1)),
  ) %>%
  arrange(created_at) %>%
  ungroup() %>%
  complete(platform_name, created_at, category, fill = list(sor = 0, automod = 0)) %>%
  mutate(
    weekend = case_when(
      wday(created_at, week_start = 1) == 6 ~ 1,
      wday(created_at, week_start = 1) == 7 ~ 1,
      .default = 0
    ),
    pre_election = case_when(created_at > as.Date("2024-06-09") ~ 0, .default = 1),
    election_distance = case_when(
      created_at < as.Date("2024-06-06") ~ abs(created_at - as.Date("2024-06-06")),
      created_at > as.Date("2024-06-09") ~ abs(created_at - as.Date("2024-06-09")),
      .default = 0
    )
  ) %>%
  arrange(created_at, platform_name)


eu_tdb_n_sor_day <- eu_tdb_long %>%
  group_by(created_at, platform_name) %>%
  summarise(
    n_sor_day = n(),
    automod_all = sum(case_match(automated_decision, "AUTOMATED_DECISION_NOT_AUTOMATED" ~ 0, .default = 1)),
  ) %>%
  arrange(created_at)

eu_tdb_absolute <- eu_tdb_temp %>%
  left_join(eu_tdb_n_sor_day, by = c("created_at", "platform_name")) %>%
  arrange(created_at, platform_name) %>%
  collect()


eu_tdb_absolute_analysis <- eu_tdb_temp %>%
  left_join(eu_tdb_n_sor_day, by = c("created_at", "platform_name")) %>%
  arrange(created_at, platform_name) %>%
  mutate(
    category = str_replace_all(category, "(STATEMENT_CATEGORY_)*", "") %>%
      str_replace_all("_+", " ") %>%
      tolower()
  ) %>%
  collect()


# ---- share necde ----

eu_tdb_share <- eu_tdb_absolute %>%
  filter(category == "STATEMENT_CATEGORY_NEGATIVE_EFFECTS_ON_CIVIC_DISCOURSE_OR_ELECTIONS") %>%
  group_by(created_at, platform_name) %>%
  summarise(
    necde_share = sor / n_sor_day
  ) %>%
  # mutate(necde_share = sprintf("%.10f", necde_share)) %>%
  ungroup()





# --- automod dataset ----

# we can replace the NaN with 0 because wherever the denominator is 0, the enurmerator must also be zero, because the denominator is all SORs of the day, and the enumerator is a subset of that (sor per day per category)


automod_all <- eu_tdb_absolute %>%
  filter(category != "STATEMENT_CATEGORY_NEGATIVE_EFFECTS_ON_CIVIC_DISCOURSE_OR_ELECTIONS") %>%
  group_by(created_at, platform_name) %>%
  summarise(
    automod_share_all = sum(automod / n_sor_day)
  ) %>%
  arrange(created_at, platform_name) %>%
  collect()


# look into if it makes a difference to use n_sor_day or sum(sor)


automod_necde <- eu_tdb_absolute %>%
  filter(category == "STATEMENT_CATEGORY_NEGATIVE_EFFECTS_ON_CIVIC_DISCOURSE_OR_ELECTIONS") %>%
  group_by(created_at, platform_name) %>%
  summarise(
    automod_share_necde = automod / n_sor_day
  ) %>%
  arrange(created_at, platform_name) %>%
  collect()


# ---- share & absolute other categories ----
illegal_speech <- eu_tdb_absolute %>%
  filter(category == "STATEMENT_CATEGORY_ILLEGAL_OR_HARMFUL_SPEECH") %>%
  group_by(created_at, platform_name) %>%
  summarise(
    n_illegal_speech = sor,
    share_illegal_speech = case_when(sor != 0 & !is.na(sor) ~ sum(automod) / n_sor_day, .default = 0)
  ) %>%
  arrange(created_at, platform_name) %>%
  collect()


illegal_product <- eu_tdb_absolute %>%
  filter(category == "STATEMENT_CATEGORY_UNSAFE_AND_ILLEGAL_PRODUCTS") %>%
  group_by(created_at, platform_name) %>%
  summarise(
    n_illegal_product = sor,
    share_illegal_product = case_when(sor != 0 & !is.na(sor) ~ sum(automod) / n_sor_day, .default = 0)
  ) %>%
  arrange(created_at, platform_name) %>%
  collect()

porn <- eu_tdb_absolute %>%
  filter(category == "STATEMENT_CATEGORY_PORNOGRAPHY_OR_SEXUALIZED_CONTENT") %>%
  group_by(created_at, platform_name) %>%
  summarise(
    n_porn = sor,
    share_porn = case_when(sor != 0 & !is.na(sor) ~ sum(automod) / n_sor_day, .default = 0)
  ) %>%
  arrange(created_at, platform_name) %>%
  collect()

self_harm <- eu_tdb_absolute %>%
  filter(category == "STATEMENT_CATEGORY_SELF_HARM") %>%
  group_by(created_at, platform_name) %>%
  summarise(
    n_self_harm = sor,
    share_self_harm = case_when(sor != 0 & !is.na(sor) ~ sum(automod) / n_sor_day, .default = 0)
  ) %>%
  arrange(created_at, platform_name) %>%
  collect()

scope <- eu_tdb_absolute %>%
  filter(category == "STATEMENT_CATEGORY_SCOPE_OF_PLATFORM_SERVICE") %>%
  group_by(created_at, platform_name) %>%
  summarise(
    n_scope = sor,
    share_scope = case_when(sor != 0 & !is.na(sor) ~ sum(automod) / n_sor_day, .default = 0)
  ) %>%
  arrange(created_at, platform_name) %>%
  collect()

security <- eu_tdb_absolute %>%
  filter(category == "STATEMENT_CATEGORY_RISK_FOR_PUBLIC_SECURITY") %>%
  group_by(created_at, platform_name) %>%
  summarise(
    n_security = sor,
    share_security = case_when(sor != 0 & !is.na(sor) ~ sum(automod) / n_sor_day, .default = 0)
  ) %>%
  arrange(created_at, platform_name) %>%
  collect()


# ---- join ----

eu_tdb_analysis <- eu_tdb_share %>%
  mutate(
    weekend = case_when(
      wday(created_at, week_start = 1) == 6 ~ 1,
      wday(created_at, week_start = 1) == 7 ~ 1,
      .default = 0
    ),
    pre_election = case_when(created_at > as.Date("2024-06-09") ~ 0, .default = 1),
    post_election = case_when(created_at > as.Date("2024-06-09") ~ 1, .default = 0),
    election_proximity = case_when(
      created_at < as.Date("2024-06-10") ~ as.numeric(ymd(created_at) - ymd("2024-04-19")),
      created_at > as.Date("2024-06-09") ~ as.numeric(ymd("2024-07-30") - ymd(created_at)),
      .default = 0
    ),
    election_distance = case_when(
      created_at < as.Date("2024-06-10") ~ as.numeric(ymd("2024-06-09") - ymd(created_at)),
      created_at > as.Date("2024-06-09") ~ as.numeric(ymd(created_at) - ymd("2024-06-9")),
      .default = 0
    ),
    election_counter = case_when(
      created_at < as.Date("2024-06-10") ~ as.numeric(ymd(created_at) - ymd("2024-04-19")),
      created_at > as.Date("2024-06-09") ~ as.numeric(ymd(created_at) - ymd("2024-06-9")),
      .default = 0
    ),
    election_neg = case_when(
      created_at < as.Date("2024-06-10") ~ as.numeric(ymd(created_at) - ymd("2024-06-09")),
      created_at > as.Date("2024-06-09") ~ as.numeric(ymd(created_at) - ymd("2024-06-9")),
      .default = 0
    ),
  ) %>%
  inner_join(
    automod_necde,
    by = c("created_at", "platform_name")
  ) %>%
  inner_join(
    automod_all,
    by = c("created_at", "platform_name")
  ) %>%
  inner_join(
    illegal_product,
    by = c("created_at", "platform_name")
  ) %>%
  inner_join(
    illegal_speech,
    by = c("created_at", "platform_name")
  ) %>%
  inner_join(
    porn,
    by = c("created_at", "platform_name")
  ) %>%
  inner_join(
    scope,
    by = c("created_at", "platform_name")
  ) %>%
  inner_join(
    security,
    by = c("created_at", "platform_name")
  ) %>%
  inner_join(
    self_harm,
    by = c("created_at", "platform_name")
  ) %>%
  replace(is.na(.), 0) %>%
  collect()




# ---- [ONLY DO THIS ONCE] create analysis table ----
dbWriteTable(con, "eu_tdb_analysis", eu_tdb_analysis, overwrite = TRUE)

dbWriteTable(con, "eu_tdb_absolute_analysis", eu_tdb_absolute_analysis, overwrite = TRUE)


# ---- DISCONNECT FROM DATABASE TO MAKE CHANGES -----
dbDisconnect(con)
