library(tidyverse)
library(duckdb)

con <- dbConnect(duckdb(), dbdir = "./data/game_data.duckdb", read_only = TRUE)
mlb_score_per_inning <- dbGetQuery(con, "SELECT * FROM mlb_score_per_inning WHERE inning <= 9;") |>
  as_tibble()

print(unique(mlb_score_per_inning$home_team))

sd_home <- mlb_score_per_inning |>
  filter(year(game_date) == 2026, home_team == "SD")

print(mean(sd_home$home_scored_in_inning))
print(var(sd_home$home_scored_in_inning))

sd_home_plot <- sd_home |>
  group_by(inning) |>
  summarise(
    home_mean_inning_score = mean(home_scored_in_inning)
  ) |>
  ggplot() +
  geom_line(aes(inning, home_mean_inning_score)) +
  scale_y_continuous(limits = c(0, 1))

ggsave("home_mean_scores.png", sd_home_plot)

az_away <- mlb_score_per_inning |>
  filter(year(game_date) == 2026, away_team == "AZ")

print(mean(az_away$away_scored_in_inning))
print(var(az_away$away_scored_in_inning))

az_away_plot <- az_away |>
  group_by(inning) |>
  summarise(
    away_mean_inning_score = mean(away_scored_in_inning)
  ) |>
  ggplot() +
  geom_line(aes(inning, away_mean_inning_score)) +
  scale_y_continuous(limits = c(0, 1))

ggsave("away_mean_scores.png", az_away_plot)
