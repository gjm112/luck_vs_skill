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

load_parquets_to_rds <- function(parquets_cache_path, rds_file) {
  if (file.exists(rds_file)) {
    pbp_rds_tibble <- readRDS(rds_file) |> as_tibble()
    if (nrow(pbp_rds_tibble) == 0) {
      stop("The reloaded RDS file has zero rows.")
    }
  } else {
    parquet_files <- list.files(parquets_cache_path)
    game_data_list <- map(parquet_files, function(file) {
      parquet_file_path <- paste0(parquets_cache_path, "/", file)
      arrow::read_parquet(parquet_file_path) |> as_tibble()
    })

    pbp_rds_tibble <- bind_rows(game_data_list)
    if (nrow(pbp_rds_tibble) == 0) {
      stop("The game PBP tibble created from parquets has zero rows.")
    } else {
      saveRDS(pbp_rds_tibble, rds_file)
    }
  }
  pbp_rds_tibble
}

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

save_nfl_quarterly_scores <- function(nfl_pbp) {
  cache_path <- "./nfl_scores_by_quarter.rds"
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

mlb_cache_path <- "./mlb_statcast/"
download_mlb_statcast <- function() {
  search_dates <- seq(
    ymd("2015-03-01"),
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

save_mlb_inning_scores <- function(mlb_statcast) {
  file_to_compute <- "./mlb_scores_by_inning.rds"
  scores_per_inning <- mlb_statcast |>
    filter(game_type == "R") |>
    group_by(game_pk, game_date, inning, home_team, away_team) |>
    summarize(
      home_score = max(post_home_score),
      away_score = max(post_away_score)
    ) |>
    ungroup() |>
    mutate(yij = determine_winner(home_score, away_score)) |>
    arrange(game_pk, inning)

  saveRDS(scores_per_inning, file_to_compute)
}

mlb_statcast <- load_parquets_to_rds(mlb_cache_path, "mlb_statcast.rds")
save_mlb_inning_scores(mlb_statcast)

nfl_pbp <- load_parquets_to_rds(nfl_cache_path, "nfl_play_by_play.rds")
save_nfl_quarterly_scores(nfl_pbp)
