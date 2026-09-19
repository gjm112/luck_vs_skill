library(tidyverse)

mlb_cache_path <- "./mlb_statcast/"

download_mlb_statcast <- function() {
  search_dates <- seq(
    ymd("2015-03-01"),
    ymd("2026-01-01"),
    by = "day"
  ) |>
    format("%Y-%m-%d")
  for (date in search_dates) {
    file_path <- paste0(mlb_cache_path, "gameday_", date, ".rds")
    print(file_path)
    if (!file.exists(file_path)) {
      gameday <- baseballr::statcast_search(date, date) |> as_tibble()
      if (nrow(gameday) > 0) {
        saveRDS(gameday, file_path)
      }
    }
  }
}

download_and_concat_mlb_statcast <- function() {
  mlb_rds_file <- "mlb_statscast.rds"
  if (file.exists(mlb_rds_file)) {
    games <- readRDS(mlb_rds_file)
  } else {
    files <- list.files(mlb_cache_path)
    games_list <- list()
    for (file in files) {
      if (file.exists(file)) {
        gameday <- readRDS(file)
        games_list <- append(games_list, gameday)
      }
    }
    games <- bind_rows(games_list)
    saveRDS(games |> as_tibble(), mlb_rds_file)
  }
  games
}

determine_winner_scalar <- function(home_score, away_score) {
  if (home_score >
      away_score) {
    y <- 1
  } else if (home_score < away_score) {
    y <- -1
  } else {
    y <- 0
  }
  y
}

load_mlb_inning_scores <- function() {
  mlb_games <- load_mlb_statcast()()
  cache_path <- "./mlb_scores_by_inning.rds"
  if (file.exists(cache_path)) {
    scores_per_inning <- readRDS(cache_path)
  } else {
    scores_per_inning <- mlb_games |>
      filter(game_type == "R") |>
      group_by(game_pk, game_date, inning, home_team, away_team) |>
      summarize(
        home_score = max(post_home_score),
        away_score = max(post_away_score)
      ) |>
      ungroup() |>
      mutate(yij = determine_winner(home_score, away_score)) |>
      arrange(game_pk, inning)

    saveRDS(scores_per_inning, cache_path)
  }
  scores_per_inning
}

