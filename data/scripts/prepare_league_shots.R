library(tidyverse)

cache_path <- "./soccer_league_shot_data"

bundesliga <- read_csv2(paste0(cache_path, "/", "Bundesliga_shot_data.csv")) |>
  select(!c(X, Y, xG))
bundesliga$league <- "Bundesliga"

elp <- read_csv2(paste0(cache_path, "/", "EPL_shot_data.csv")) |>
  select(!c(X, Y, xG))
elp$league <- "ELP"

la_liga <- read_csv2(paste0(cache_path, "/", "La_Liga_shot_data.csv")) |>
  select(!c(X, Y, xG))
la_liga$league <- "La Liga"

ligue_1 <- read_csv(paste0(cache_path, "/", "Ligue_1_shot_data.csv")) |>
  select(!c(X, Y, xG))
ligue_1$league <- "Ligue 1"

repl <- read_csv(paste0(cache_path, "/", "RFPL_shot_data.csv")) |>
  select(!c(X, Y, xG))
repl$league <- "RFPL"

serie_a <- read_csv(paste0(cache_path, "/", "Serie_A_shot_data.csv")) |>
  select(!c(X, Y, xG))
serie_a$league <- "Serie A"

league <- bind_rows(bundesliga, elp, la_liga, repl, serie_a) |>
  mutate(game_date = as_date(date))

determine_winner_scalar <- function(home_score, away_score) {
  if (home_score > away_score) {
    y <- 1
  } else if (home_score < away_score) {
    y <- -1
  } else {
    y <- 0
  }
  y
}

determine_winner <- Vectorize(determine_winner_scalar)

minutes <- tibble(
  minute = seq(1, 300, by = 1),
  minute_bucket = ceiling(seq(1, 300, by = 1) / 15),
  one = 1
)

matches <- league |>
  group_by(match_id) |>
  summarize(minute_end = max(minute)) |>
  ungroup() |>
  mutate(one = 1)

match_minutes <- inner_join(
  minutes, matches,
  by = "one",
  relationship = "many-to-many"
) |>
  glimpse() |>
  filter(minute <= minute_end) |>
  glimpse()


home_away <- left_join(match_minutes, league, by = c("minute", "match_id")) |>
  select(
    match_id, league, season,
    game_date, minute, minute_bucket,
    h_team, a_team, h_a, result,
  ) |>
  arrange(match_id, minute) |>
  fill(
    match_id, game_date,
    league, season,
    h_team, a_team,
    .direction = "downup",
    .by = "match_id"
  ) |>
  mutate(
    h_a = if_else(is.na(h_a), "Other", h_a),
    result = if_else(is.na(result), "Other", result),
  ) |>
  group_by(match_id) |>
  mutate(
    home_scored = cumsum(if_else(
      (h_a == "h" & result == "Goal") | (h_a == "a" & result == "OwnGoal"), 1, 0
    )),
    away_scored = cumsum(if_else(
      (h_a == "a" & result == "Goal") | (h_a == "h" & result == "OwnGoal"), 1, 0
    )),
  ) |>
  ungroup() |>
  group_by(
    match_id, game_date, minute_bucket, league, season,
    h_team, a_team,
  ) |>
  summarize(away_score = max(away_scored), home_score = max(home_scored)) |>
  mutate(yij = determine_winner(home_score, away_score)) |>
  rename(home_team = h_team, away_team = a_team) |>
  arrange(match_id, minute_bucket)

home_away |>
  arrange(match_id, minute_bucket) |>
  write_rds("soccer_league_scores_by_15_mins.rds")
