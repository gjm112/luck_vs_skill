simulate_team_scoring <- function(game_i, team_i) {
  mu_i <- home_skill_mean_draws[game_i, team_i]
  phi_i <- home_skill_dispersion_draws[game_i, team_i]
  team_i_score <- cumsum(MASS::rnegbin(innings, mu = mu_i, theta = phi_i))

  repeat_team_i <- rep_len(team_i, innings)
  repeat_mu_i <- rep_len(mu_i, innings)
  repeat_phi_i <- rep_len(phi_i, innings)

  simulated_match <- data.frame(
    inning = 1:innings,
    team_i = repeat_team_i,
    team_i_score = team_i_score,
    mu_i = repeat_mu_i,
    phi_i = repeat_phi_i
  )
  simulated_match
}

fit <- readRDS("./modeling/home_scored_in_inning_model.rds")

home_skill_mean_draws <- fit$draws("home_skill_mean") |>
  posterior::as_draws_matrix()
home_skill_dispersion_draws <- fit$draws("home_skill_dispersion") |>
  posterior::as_draws_matrix()

games_played <- 250
teams <- 1:30
innings <- 1000000
rows <- vector("list", games_played * games_played)

for (game_i in 1:games_played) {
  for (team_i in teams) {
    rows[[game_i]] <- simulate_team_scoring(game_i, team_i)
  }
}

do.call(rbind, rows) |>
  arrow::write_parquet("./modeling/simulated_games.parquet")
