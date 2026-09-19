library(tibble)
library(dplyr)
library(lubridate)

mlb_cache_path <- "./mlb_statcast/"
mlb_rds_file <- "mlb_statscast.rds"
scores_per_inning <- "scores_per_inning.rds"

download_mlb_statcast <- function() {
  if (!file.exists(mlb_cache_path)) {
    stop("mlb_cache_path does not exist")
  }

  search_dates <- seq(
    ymd("2015-03-01"),
    ymd("2026-09-01"),
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

concat_mlb_statcast <- function() {
  if (!file.exists(mlb_cache_path)) {
    stop("mlb_cache_path does not exist")
  }
  files <- list.files(mlb_cache_path)
  games_list <- list()
  for (file in files) {
    if (file.exists(file)) {
      gameday <- readRDS(file)
      games_list <- append(games_list, gameday)
    }
  }
  games <- bind_rows(games_list) |> as_tibble()
  saveRDS(games, mlb_rds_file)
}

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

transform_mlb_inning_scores <- function() {
  if (!file.exists(mlb_rds_file)) {
    stop("mlb_statscast.rds does not exist")
  }
  mlb_statcast <- readRDS(mlb_rds_file)
  tranformed_scores_per_inning <- mlb_statcast |>
    filter(game_type == "R") |>
    group_by(game_pk, game_date, inning, home_team, away_team) |>
    summarize(
      home_score = max(post_home_score),
      away_score = max(post_away_score)
    ) |>
    ungroup() |>
    mutate(yij = determine_winner(home_score, away_score)) |>
    arrange(game_pk, inning)

  saveRDS(tranformed_scores_per_inning, scores_per_inning)
}

download_mlb_statcast()
concat_mlb_statcast()
transform_mlb_inning_scores()
