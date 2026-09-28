library(tidyverse)
library(duckdb)
library(cmdstanr)
library(posterior)
library(bayesplot)
color_scheme_set("brightblue")

con <- dbConnect(duckdb(), dbdir = "./data/game_data.duckdb", read_only = TRUE)
mlb_score_per_inning <- dbGetQuery(con, "SELECT * FROM mlb_score_per_inning WHERE inning <= 9;") |>
  as_tibble() |>
  filter(home_team == "SD", year(game_date) == 2026) |>
  select(home_scored_in_inning)

model <- cmdstan_model("./modeling/model.stan")
data_list <- list(
  N = length(mlb_score_per_inning$home_scored_in_inning),
  home_scored_in_inning = mlb_score_per_inning$home_scored_in_inning
)
test <- rnbinom(10000, 10, 0.3)
data_list <- list(
  N = length(test),
  home_scored_in_inning = test
)

fit <- model$sample(
  data = data_list,
  seed = 123,
  chains = 4,
  parallel_chains = 4,
  refresh = 500 # print update every 500 iters
)

print(fit$summary())
