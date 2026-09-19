library(tibble)
library(dplyr)

nfl_cache_path <- "./nfl_play_by_play/"
nfl_rds_file <- "nfl_pbp.rds"

download_nfl_play_by_play <- function() {
  if (!file.exists(nfl_cache_path)) {
    stop("nfl_cache_path does not exist")
  }
  first_recorded_season <- 1999
  current_season <- 2025
  for (season_year in first_recorded_season:current_season) {
    file_path <- paste0(
      nfl_cache_path, "play_by_play_", season_year, ".rds"
    )
    if (!file.exists(file_path)) {
      season_data <- nflreadr::load_pbp(season_year) |> as_tibble()
      saveRDS(season_data, file_path)
    }
  }
}

concat_nfl_play_by_play <- function() {
  if (!file.exists(nfl_cache_path)) {
    stop("nfl_cache_path does not exist")
  }
  files <- list.files(nfl_cache_path)
  pbp_list <- list()
  for (file in files) {
    if (file.exists(file)) {
      pbp_season <- readRDS(file)
      pbp_list <- append(pbp_list, pbp_season)
    }
  }
  pbp <- bind_rows(pbp_list) |> as_tibble()
  saveRDS(pbp, nfl_rds_file)
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

determine_winner <- Vectorize(determine_winner_scalar)

load_nfl_quarterly_scores <- function() {
  cache_path <- "./nfl_scores_by_quarter.rds"
  nfl_pbp <- readRDS(nfl_rds_file)
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
