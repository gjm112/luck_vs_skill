library(tidyverse)
library(rstan)
determine_winner_scalar <- function(home_score, away_score) {
  if (home_score >
      away_score) {
    y <- 1
  } else if (home_score < away_score) {
    y <- 0
  } else {
    y <- rbinom(1,1,0.5)
  }
  y
}
determine_winner <- Vectorize(determine_winner_scalar)
dat <- readRDS("./nfl_scores_by_quarter.rds")

dat <- dat %>% mutate(yij = determine_winner(home_score, away_score))

team <- sort(unique(dat$home_team))
indexes <- data.frame(team = team, index = 1:length(team))

dat <- dat %>% left_join(indexes, by = c("home_team" = "team")) %>% rename(i = index) %>% left_join(indexes, by = c("away_team" = "team")) %>% rename(j = index)
dat <- dat %>% filter(year(game_date) == 2025)

set.seed(1984)
sub <- dat %>% group_by(game_id) %>% slice_sample(n = 1)

stan_data <- list(
  M = nrow(sub),
  N_team = length(team),
  y = sub$yij,
  #y = rbinom(nrow(sub),1,0.5),
  team_i = sub$i,
  team_j = sub$j,
  n = sub$qtr/4
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
  vector[N_team - 1] theta_free;
  real<lower=0> sigma;
  real<lower=0> alpha;
  vector[M] eps_raw;
}

transformed parameters {
  vector[M] eps;
  vector[M] logit_p;
  vector[N_team] theta;
  
  theta[N_team] = 0;
  for (t in 1:(N_team-1)){
    theta[t] = theta_free[t];
  }
  
  for (m in 1:M) {
  
    eps[m] = eps_raw[m] * (sigma / sqrt(n[m]));
    logit_p[m] = n[m]^alpha * (theta[team_i[m]] - theta[team_j[m]]) + eps[m];
  }
}


model {
  theta ~ normal(0, 1);
  sigma ~ normal(0, 1);
  eps_raw ~ normal(0, 1);
  alpha ~ normal(1, 0.5);
  
  
  y ~ bernoulli_logit(logit_p);
}

generated quantities {
  vector[M] p_hat;
  vector[M] p_hat_fix;
  for (m in 1:M){
    p_hat[m] = inv_logit(n[m]^alpha * (theta[team_i[m]] - theta[team_j[m]]) +  eps[m]);
    p_hat_fix[m] = inv_logit(n[m]^alpha * (theta[team_i[m]] - theta[team_j[m]]));
  }
}
"

#fit_random <- stan(model_code = stan_model_code, data = stan_data,
#                  iter = 2000, chains = 4, seed = 123)

fitnfl <- stan(model_code = stan_model_code, data = stan_data,
            iter = 2000, chains = 4, seed = 123)


print(fitnfl, pars = c("theta",  "sigma","alpha"))


