library(tidyverse)
mlb <- readRDS("./mlb_scores_by_inning.rds")

mlb <- mlb %>%
  group_by(game_pk) %>%
  mutate(
    home_inn_score = home_score - replace_na(lag(home_score), 0),
    away_inn_score = away_score - replace_na(lag(away_score), 0)
  ) %>% view()

library(tidyverse)

expit <- function(x) 1 / (1 + exp(-x))

# ── Tilted resampling weights ─────────────────────────────────────────────────
# For each value in the distribution, weight = exp(lambda * score)
# lambda > 0  →  Team A more likely to draw high-scoring innings
# lambda = 0  →  uniform (equal teams)

tilted_sample <- function(dist, lambda, n) {
  weights <- exp(lambda * dist)           # higher scores get more weight
  weights <- weights / sum(weights)       # normalize to probabilities
  sample(dist, size = n, replace = TRUE, prob = weights)
}


calibrate_lambda <- function(dist, n_units, theta = 0.1, n_sim = 50000) {
  target_p <- expit(theta)
  
  uniroot(
    f = function(lambda) {
      # Team A: tilted draws (better team)
      score_a <- matrix(
        tilted_sample(dist, lambda, n_units * n_sim),
        nrow = n_sim
      )
      # Team B: uniform draws (reference team)
      score_b <- matrix(
        sample(dist, n_units * n_sim, replace = TRUE),
        nrow = n_sim
      )
      
      p_win <- mean(rowSums(score_a) > rowSums(score_b)) +
        0.5 * mean(rowSums(score_a) == rowSums(score_b))
      p_win - target_p
    },
    interval = c(0, 3),
    tol      = 1e-3
  )$root
}

lambda <- calibrate_lambda(mlb$home_inn_score, n_units = 9, theta = 100)
lambda


n <- 1
tilted_sample(mlb$home_inn_score, lambda, n)
sample(dist, size = n, replace = TRUE)                         


