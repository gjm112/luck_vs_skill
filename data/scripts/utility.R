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

# TODO: Finish this conversion function
convert_rds_to_parquet <- function(rds_file_name) {
  if (!file.exists(rds_file_name)) {
    stop("rds file does not exist")
  }
}
