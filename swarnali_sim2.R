#exponetial tilting: exp.tilt() in boot
#https://www.rdocumentation.org/packages/boot/versions/1.3-32/topics/exp.tilt
#Davison, A.C. and Hinkley, D.V. (1997). Bootstrap Methods and Their Application. Cambridge University Press. Chapter 4.

library(tidyverse)
mlb <- readRDS("./mlb_scores_by_inning.rds")
nfl <- readRDS("./nfl_scores_by_quarter.rds")

mlb <- mlb %>%
  group_by(game_pk) %>%
  mutate(
    home_inn_score = home_score - replace_na(lag(home_score), 0),
    away_inn_score = away_score - replace_na(lag(away_score), 0)
  ) 

nfl <- nfl %>%
  group_by(game_id) %>%
  mutate(
    home_unit_score = home_score - replace_na(lag(home_score), 0),
    away_unit_score = away_score - replace_na(lag(away_score), 0)
  ) 
# ── Core function: separability curve for one sport ──────────────────────────
# Returns P(better team wins) as a function of lambda
# Lambda = 0 → equal teams (50%)
# Lambda → ∞ → maximum separability (the ceiling we want)

separability_curve <- function(dist, n_units, n_sim = 100000,
                               lambdas = seq(0, 10, by = 0.25)) {
  
  # Precompute unique values and base counts for efficiency
  score_vals <- sort(unique(dist))
  base_probs <- tabulate(match(dist, score_vals)) / length(dist)
  
  map_dfr(lambdas, function(lambda) {
    
    # Tilt distribution toward higher scores
    tilted_probs <- exp(lambda * score_vals) * base_probs
    tilted_probs <- tilted_probs / sum(tilted_probs)
    
    # Simulate games
    score_a <- matrix(
      sample(score_vals, n_units * n_sim, replace = TRUE, prob = tilted_probs),
      nrow = n_sim
    )
    score_b <- matrix(
      sample(score_vals, n_units * n_sim, replace = TRUE, prob = base_probs),
      nrow = n_sim
    )
    
    total_a <- rowSums(score_a)
    total_b <- rowSums(score_b)
    
    tibble(
      lambda     = lambda,
      p_win      = mean(total_a > total_b) + 0.5 * mean(total_a == total_b),
      mean_a     = mean(total_a),
      mean_b     = mean(total_b),
      sd_a       = sd(total_a),
      sd_b       = sd(total_b)
    )
  })
}

# ── Run for each sport ────────────────────────────────────────────────────────
sports <- tribble(
  ~sport,     ~dist,          ~natural_units,
  "Baseball", baseball_dist,  9,
  "NHL",      nhl_dist,       3,
  "NFL",      nfl_dist,       25,
  "NBA",      nba_dist,       200
)

separability <- sports %>%
  mutate(curve = map2(dist, natural_units, separability_curve)) %>%
  select(sport, natural_units, curve) %>%
  unnest(curve)

mlb_sep <- separability_curve(mlb$home_inn_score, 9, 100000,lambdas = seq(0, 1, by = 0.01))
mlb_sep <- separability_curve(mlb$home_inn_score, 25, 100000,lambdas = seq(0, 1, by = 0.01))
nfl_sep <- separability_curve(nfl$home_unit_score, 1, 100000,lambdas = seq(0, 1, by = 0.01))
mlb_sep %>% view()
nfl_sep %>% view()

stack <- rbind(mlb_sep %>% mutate(sport = "mlb"),nfl_sep %>% mutate(sport = "nfl"))
stack %>% ggplot(aes(x = lambda, y = p_win, color = sport)) + geom_point()

# ── Ceiling = asymptote as lambda → large ────────────────────────────────────
ceilings <- separability %>%
  group_by(sport) %>%
  summarise(
    ceiling       = max(p_win),
    min_upset     = 1 - max(p_win),
    # Lambda needed to reach 90% of the ceiling
    lambda_90pct  = min(lambda[p_win >= 0.9 * max(p_win)], na.rm = TRUE)
  ) %>%
  arrange(desc(ceiling))

ceilings


# ── Separability curves ───────────────────────────────────────────────────────
separability %>%
  ggplot(aes(x = lambda, y = p_win, color = sport)) +
  geom_line(linewidth = 1.2) +
  # Mark ceiling for each sport
  geom_hline(
    data = ceilings,
    aes(yintercept = ceiling, color = sport),
    linetype = "dashed", linewidth = 0.5
  ) +
  # Mark lambda needed to reach 90% of ceiling
  geom_point(
    data = separability %>%
      group_by(sport) %>%
      slice_min(abs(p_win - 0.9 * max(p_win))) %>%
      slice(1),
    size = 3, shape = 21, fill = "white", stroke = 2
  ) +
  scale_y_continuous(labels = scales::percent, limits = c(0.5, 1)) +
  scale_color_brewer(palette = "Set1") +
  labs(
    title    = "Maximum Separability by Sport",
    subtitle = "Dashed lines = structural ceiling on win probability | Circles = 90% of ceiling",
    x        = "Skill advantage (λ)",
    y        = "P(better team wins)",
    color    = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top")


ceilings %>%
  mutate(
    ceiling_pct   = scales::percent(ceiling, .1),
    min_upset_pct = scales::percent(min_upset, .1),
    statement     = glue::glue(
      "{sport}: no matter how dominant, best possible win rate = {ceiling_pct}",
      " (irreducible upset rate = {min_upset_pct})"
    )
  ) %>%
  pull(statement)


#Exampel numbers
# "NBA: no matter how dominant, best possible win rate = 97.3% (irreducible upset rate = 2.7%)"
# "NFL: no matter how dominant, best possible win rate = 89.1% (irreducible upset rate = 10.9%)"
# "Baseball: no matter how dominant, best possible win rate = 78.4% (irreducible upset rate = 21.6%)"
# "NHL: no matter how dominant, best possible win rate = 74.2% (irreducible upset rate = 25.8%)"


# Diagnose: what drives the ceiling?
sports %>%
  mutate(
    mean_score  = map_dbl(dist, mean),
    sd_score    = map_dbl(dist, sd),
    cv          = sd_score / mean_score,   # coefficient of variation
    pct_zero    = map_dbl(dist, ~ mean(.x == 0))
  ) %>%
  select(sport, natural_units, mean_score, sd_score, cv, pct_zero) %>%
  arrange(desc(cv))

#"This is the clean structural story: 
#baseball and hockey have low-scoring, zero-inflated units
#played few times — so luck is baked in. 
#NBA has hundreds of possessions with low variance per 
#possession — skill wins out." - Claude