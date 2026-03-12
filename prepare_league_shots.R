library(tidyverse)

cache_path <- "./soccer_league_shot_data"

bundesliga <- read_csv2(paste0(cache_path, "/", "Bundesliga_shot_data.csv")) |> select(!c(X, Y, xG))
bundesliga$league <- "Bundesliga"
elp <- read_csv2(paste0(cache_path, "/", "EPL_shot_data.csv")) |> select(!c(X, Y, xG))
elp$league <- "ELP"
la_liga <- read_csv2(paste0(cache_path, "/", "La_Liga_shot_data.csv")) |> select(!c(X, Y, xG))
la_liga$league <- "La Liga"
ligue_1 <- read_csv(paste0(cache_path, "/", "Ligue_1_shot_data.csv")) |> select(!c(X, Y, xG))
ligue_1$league <- "Ligue 1"
repl <- read_csv(paste0(cache_path, "/", "RFPL_shot_data.csv")) |> select(!c(X, Y, xG))
repl$league <- "RFPL"
serie_a <- read_csv(paste0(cache_path, "/", "Serie_A_shot_data.csv")) |> select(!c(X, Y, xG))
serie_a$league <- "Serie A"
league <- bind_rows(bundesliga, elp, la_liga, repl, serie_a) |> filter(result == "Goal")

league |>
  select(league, season, match_id, minute, result, h_team, a_team, h_goals, a_goals)
