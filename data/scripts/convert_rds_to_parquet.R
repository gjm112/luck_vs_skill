# This script assumes that you have mirrror the RDS directory structure.
# So, if there's the directories data -> rds -> mlb_statcast,
# then you need to manually create data -> parquet -> mlb_statcast
# before running the script.

convert_rds_to_parquet <- function(rds_file_name) {
  parquet_file_name <- stringr::str_replace_all(rds_file_name, "rds", "parquet")
  readr::read_rds(rds_file_name) |> arrow::write_parquet(parquet_file_name)
  print(stringr::str_c("File written ", parquet_file_name))
}

rds_folder <- "./data/rds/nfl_play_by_play"
files <- list.files(rds_folder, full.names = TRUE)
purrr::walk(files, convert_rds_to_parquet)
