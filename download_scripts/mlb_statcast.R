library(tibble)
library(dplyr)
library(lubridate)
library(readr)
library(purrr)

source(here("utility.R"))

# NOTE: This script needs to be run from the top-level directory.
# That is, Rscript download_scripts/mlb_statcast.R

mlb_cache_path <- "./mlb_statcast/"
mlb_rds_file <- "./mlb_statcast.rds"
scores_per_inning <- "./scores_per_inning.rds"

download_mlb_statcast <- function() {
  if (!file.exists(mlb_cache_path)) {
    dir.create(mlb_cache_path)
  }
  search_dates <- seq(
    ymd("2017-07-24"),
    ymd("2026-09-01"),
    by = "day"
  ) |>
    format("%Y-%m-%d")
  for (date in search_dates) {
    file_path <- paste0(mlb_cache_path, "gameday_", date, ".rds")
    if (!file.exists(file_path)) {
      gameday <- baseballr::statcast_search(date, date) |> as_tibble()
      if (nrow(gameday) > 0) {
        print(file_path)
        write_rds(gameday, file_path)
      }
      Sys.sleep(3)
    }
  }
}

concat_mlb_statcast <- function() {
  if (!file.exists(mlb_cache_path)) {
    stop("mlb_cache_path does not exist")
  }
  files <- list.files(mlb_cache_path, pattern = "\\.rds$", full.names = TRUE)
  map(files, read_rds) |>
    list_rbind() |>
    as_tibble() |>
    write_rds(file = mlb_rds_file)
}

transform_mlb_inning_scores <- function() {
  if (!file.exists(mlb_rds_file)) {
    stop("mlb_statcast.rds does not exist")
  }
  mlb_statcast <- read_rds(mlb_rds_file)
  tranformed_scores_per_inning <- mlb_statcast |>
    # We deliberately only want regular season games.
    filter(game_type == "R") |>
    group_by(game_pk, game_date, inning, home_team, away_team) |>
    summarize(
      home_score = max(post_home_score),
      away_score = max(post_away_score)
    ) |>
    ungroup() |>
    mutate(yij = determine_winner(home_score, away_score)) |>
    arrange(game_pk, inning)

  write_rds(tranformed_scores_per_inning, scores_per_inning)
}

download_mlb_statcast()
concat_mlb_statcast()
transform_mlb_inning_scores()
