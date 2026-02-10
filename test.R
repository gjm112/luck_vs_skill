library(dplyr)
library(rstan)

set.seed(123)

# Parameters
N_team <- 6         # number of teams
n_games <- 20       # total matches
sigma_true <- 0.5   # luck magnitude
beta_true <- 1.2    # skill scaling

# True team abilities
theta_true <- rnorm(N_team, 0, 1)

# Simulate pairwise matches
matches <- data.frame(
  team_i = sample(1:N_team, n_games, replace = TRUE),
  team_j = sample(1:N_team, n_games, replace = TRUE)
) %>% filter(team_i != team_j) # no self matches

matches$n <- runif(nrow(matches), 1, 5) # game length varies between 1 and 5

# Simulate outcomes
matches$y <- mapply(function(i, j, n_len){
  eps <- rnorm(1, 0, sigma_true / sqrt(n_len))
  p <- plogis(beta_true * n_len * (theta_true[i] - theta_true[j]) + eps)
  rbinom(1, 1, p)
}, matches$team_i, matches$team_j, matches$n)

head(matches)

stan_data <- list(
  M = nrow(matches),
  N_team = N_team,
  y = matches$y,
  team_i = matches$team_i,
  team_j = matches$team_j,
  n = matches$n
)

stan_model_code <- "
data {
  int<lower=1> M;
  int<lower=1> N_team;
  int<lower=0,upper=1> y[M];
  int<lower=1,upper=N_team> team_i[M];
  int<lower=1,upper=N_team> team_j[M];
  real<lower=0> n[M];
}

parameters {
  vector[N_team] theta;
  real<lower=0> beta;
  real<lower=0> sigma;
  vector[M] eps_raw;
}

transformed parameters {
  vector[M] eps;
  vector[M] logit_p;

  for (m in 1:M) {
    eps[m] = eps_raw[m] * (sigma / sqrt(n[m]));
    logit_p[m] = beta * n[m] * (theta[team_i[m]] - theta[team_j[m]]) + eps[m];
  }
}


model {
  theta ~ normal(0, 1);
  beta ~ normal(0, 1);
  sigma ~ normal(0, 1);
  eps_raw ~ normal(0, 1);
  y ~ bernoulli_logit(logit_p);
}

generated quantities {
  vector[M] p_hat;
  for (m in 1:M)
    p_hat[m] = inv_logit(beta * n[m] * (theta[team_i[m]] - theta[team_j[m]]) +  eps[m]);
}
"


# Compile Stan model
fit <- stan(model_code = stan_model_code, data = stan_data,
            iter = 2000, chains = 4, seed = 123)

# Print results
print(fit, pars = c("theta", "beta", "sigma"))


