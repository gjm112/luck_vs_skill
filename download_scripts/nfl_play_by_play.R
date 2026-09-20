library(tibble)
library(dplyr)
library(readr)
library(purrr)
library(here)

source(here("utility.R"))

# NOTE: This script needs to be run from the top-level directory.
# That is, Rscript download_scripts/nfl_play_by_play.R

nfl_cache_path <- "./nfl_play_by_play/"
nfl_rds_file <- "./nfl_pbp.rds"
scores_per_quarter <- "./nfl_scores_by_quarter.rds"

download_nfl_play_by_play <- function() {
  if (!file.exists(nfl_cache_path)) {
    dir.create(nfl_cache_path)
  }
  first_recorded_season <- 1999
  current_season <- 2025
  for (season_year in first_recorded_season:current_season) {
    file_path <- paste0(
      nfl_cache_path, "play_by_play_", season_year, ".rds"
    )
    if (!file.exists(file_path)) {
      season_data <- nflreadr::load_pbp(season_year) |> as_tibble()
      if (nrow(season_data) > 0) {
        print(file_path)
        write_rds(season_data, file_path)
      }
      Sys.sleep(3)
    }
  }
}

concat_nfl_play_by_play <- function() {
  if (!file.exists(nfl_cache_path)) {
    stop("nfl_cache_path does not exist")
  }
  files <- list.files(nfl_cache_path, pattern = "\\.rds$", full.names = TRUE)
  map(files, read_rds) |>
    list_rbind() |>
    as_tibble() |>
    write_rds(file = nfl_rds_file)
}

transform_nfl_quarter_scores <- function() {
  nfl_pbp <- read_rds(nfl_rds_file)
  transformed_scores_per_quarter <- nfl_pbp |>
    # We deliberately only want regular season games.
    filter(season_type == "REG") |>
    group_by(game_id, season, game_date, qtr, home_team, away_team) |>
    summarize(
      home_score = max(total_home_score),
      away_score = max(total_away_score)
    ) |>
    ungroup() |>
    mutate(yij = determine_winner(home_score, away_score))

  write_rds(transformed_scores_per_quarter, scores_per_quarter)
}

download_nfl_play_by_play()
concat_nfl_play_by_play()
transform_nfl_quarter_scores()
