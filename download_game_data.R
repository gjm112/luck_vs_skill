library(tidyverse)

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
nfl_cache_path <- "./nfl_play_by_play/"

download_nfl_play_by_play <- function() {
  first_recorded_season <- 1999
  current_season <- 2025
  for (season_year in first_recorded_season:current_season) {
    file_path <- paste0(
      nfl_cache_path, "play_by_play_", season_year, ".parquet"
    )
    if (!file.exists(file_path)) {
      nflreadr::load_pbp(season_year) |>
        as_tibble() |>
        arrow::write_parquet(file_path)
    }
  }
}

load_nfl_play_by_play <- function() {
  nfl_rds_file <- "nfl_play_by_play.rds"
  if (file.exists(nfl_rds_file)) {
    pbps <- readRDS(nfl_rds_file)
  } else {
    files <- list.files(nfl_cache_path)
    pbp_list <- list()
    for (file in files) {
      if (file.exists(file)) {
        pbp_season <- arrow::read_parquet(file)
        pbp_list <- append(pbp_list, pbp_season)
      }
    }
    pbp <- bind_rows(pbp_list)
    saveRDS(pbp |> as_tibble(), nfl_rds_file)
  }
  pbps
}

load_nfl_quarterly_scores <- function() {
  cache_path <- "./nfl_scores_by_quarter.rds"
  nfl_pbp <- load_nfl_play_by_play()
  if (file.exists(cache_path)) {
    scores_per_quarter <- readRDS(cache_path)
  } else {
    scores_per_quarter <- nfl_pbp |>
      filter(season_type == "REG") |>
      group_by(game_id, season, game_date, qtr, home_team, away_team) |>
      summarize(
        home_score = max(total_home_score),
        away_score = max(total_away_score)
      ) |>
      ungroup() |>
      mutate(yij = determine_winner(home_score, away_score))

    saveRDS(scores_per_quarter, cache_path)
  }
  scores_per_quarter
}

mlb_cache_path <- "./mlb_statcast/"
download_mlb_statcast <- function() {
  search_dates <- seq(
    ymd("2015-01-01"),
    ymd("2026-01-01"),
    by = "day"
  ) |>
    format("%Y-%m-%d")
  for (date in search_dates) {
    file_path <- paste0(mlb_cache_path, "gameday_", date, ".parquet")
    if (!file.exists(file_path)) {
      gameday <- baseballr::statcast_search(date, date) |> as_tibble()
      if (nrow(gameday) > 0) {
        arrow::write_parquet(gameday, file_path)
      }
    }
  }
}

load_mlb_statcast <- function() {
  mlb_rds_file <- "mlb_statscast.rds"
  if (file.exists(mlb_rds_file)) {
    games <- readRDS(mlb_rds_file)
  } else {
    files <- list.files(mlb_cache_path)
    games_list <- list()
    for (file in files) {
      if (file.exists(file)) {
        gameday <- arrow::read_parquet(file)
        games_list <- append(games_list, gameday)
      }
    }
    games <- bind_rows(games_list)
    saveRDS(games |> as_tibble(), mlb_rds_file)
  }
  games
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

download_mlb_statcast()
