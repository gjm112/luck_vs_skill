library(tidyverse)
library(duckdb)
library(cmdstanr)

con <- dbConnect(duckdb(), dbdir = "./data/game_data.duckdb", read_only = TRUE)
mlb_score_per_inning <- dbGetQuery(
  con,
  "SELECT *
  FROM mlb_score_per_inning
  WHERE inning <= 9
  AND date_part('year', game_date) = 2026
  ORDER BY random();"
) |>
  as_tibble() |>
  mutate(
    home_team = as_factor(home_team)
  )

home_team <- mlb_score_per_inning$home_team
# team_indictator <- model.matrix(~home_team)

model <- cmdstan_model("./modeling/model.stan")

data_list <- list(
  N = nrow(mlb_score_per_inning),
  num_teams = length(unique(home_team)),
  home_team = as.integer(home_team),
  home_scored_in_inning = mlb_score_per_inning$home_scored_in_inning
)


fit <- model$sample(
  data = data_list,
  seed = 123,
  chains = 4,
  parallel_chains = 4,
  refresh = 500 # print update every 500 iters
)

fit$save_object("./modeling/home_scored_in_inning_model.rds")
