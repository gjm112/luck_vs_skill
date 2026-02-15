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

load_nfl_play_by_play <- function() {
  first_recorded_season <- 1999
  current_season <- 2025
  cache_path <- "./nfl_play_by_play.rds"
  if (file.exists(cache_path)) {
    nfl_pbp <- readRDS(cache_path)
  } else {
    season_pbps <- list()
    for (season_year in first_recorded_season:current_season) {
      season_pbps[[season_year]] <- nflreadr::load_pbp(season_year)
    }
    nfl_pbp <- dplyr::bind_rows(season_pbps)
    saveRDS(nfl_pbp, cache_path)
  }
  nfl_pbp
}


load_nfl_quarterly_scores <- function() {
  nfl_pbp <- load_nfl_play_by_play()
  cache_path <- "./nfl_scores_by_quarter.rds"
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

load_mlb_play_by_play <- function() {
  cache_path <- "./mlb_play_by_play.rds"
  if (file.exists(cache_path)) {
    mlb_pbp <- readRDS(cache_path)
  } else {
    search_dates <- seq(
      ymd("2015-01-01"),
      ymd("2026-01-01"),
      by = "day"
    ) |>
      format("%Y-%m-%d")
    seasons_pbps <- list()
    for (date in search_dates) {
      pbp_day <- baseballr::statcast_search(date, date)
      seasons_pbps <- append(seasons_pbps, pbp_day)
      print(pbp_day)
    }
    mlb_pbp <- bind_rows(seasons_pbps)
    saveRDS(mlb_pbp, cache_path)
  }
  mlb_pbp
}

load_mlb_inning_scores <- function() {
  mlb_pbp <- load_mlb_play_by_play()
  cache_path <- "./mlb_scores_by_inning.rds"
  if (file.exists(cache_path)) {
    scores_per_inning <- readRDS(cache_path)
  } else {
    scores_per_inning <- mlb_pbp |>
      filter(game_type == "R") |>
      group_by(game_pk, game_year, game_date, inning, home_team, away_team) |>
      summarize(
        home_score = max(home_score),
        away_score = max(away_score),
        post_home_score = max(post_home_score),
        post_away_score = max(post_away_score)
      ) |>
      ungroup() |>
      mutate(yij = determine_winner(home_score, away_score)) |>
      arrange(game_pk, inning)

    saveRDS(scores_per_inning, cache_path)
  }
  scores_per_inning
}

load_mlb_play_by_play()
