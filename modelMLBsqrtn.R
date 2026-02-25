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

dat <- readRDS("./mlb_scores_by_inning.rds")
dat <- dat %>% mutate(yij = determine_winner(home_score, away_score))

team <- sort(unique(dat$home_team))
indexes <- data.frame(team = team, index = 1:length(team))

dat <- dat %>% left_join(indexes, by = c("home_team" = "team")) %>% rename(i = index) %>% left_join(indexes, by = c("away_team" = "team")) %>% rename(j = index)
dat <- dat %>% filter(year(game_date) == 2025)

set.seed(1984)
sub <- dat %>% group_by(game_pk) %>% slice_sample(n = 1)


stan_data <- list(
  M = nrow(sub),
  N_team = length(team),
  y = sub$yij,
  #y = rbinom(nrow(sub),1,0.5),
  team_i = sub$i,
  team_j = sub$j,
  n = sub$inning
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
    logit_p[m] = sqrt(n[m]) * (theta[team_i[m]] - theta[team_j[m]]) + eps[m];
  }
}


model {
  theta ~ normal(0, 1);
  sigma ~ normal(0, 1);
  eps_raw ~ normal(0, 1);
  
  y ~ bernoulli_logit(logit_p);
}

generated quantities {
  vector[M] p_hat;
  vector[M] p_hat_fix;
  for (m in 1:M){
    p_hat[m] = inv_logit(sqrt(n[m]) * (theta[team_i[m]] - theta[team_j[m]]) +  eps[m]);
    p_hat_fix[m] = inv_logit(sqrt(n[m]) * (theta[team_i[m]] - theta[team_j[m]]));
  }
}
"

#fit_random <- stan(model_code = stan_model_code, data = stan_data,
#                  iter = 2000, chains = 4, seed = 123)

fit <- stan(model_code = stan_model_code, data = stan_data,
                   iter = 2000, chains = 4, seed = 123)


print(fit, pars = c("theta",  "sigma"))
print(fit_random, pars = c("theta", "sigma"))

s <- 0.1
plogis(1:9*s)
plogis(sqrt(1:9)*s)

str(fit)

traceplot(fit, pars = c("sigma"))
library(rstan)       # Interface to Stan
library(tidyverse)   # Data manipulation and plotting
library(bayesplot)   # Posterior visualization and diagnostics
library(loo) 
ppc_draws_theta <- extract(fit, "theta")$theta
apply(ppc_draws_theta,2,mean)

ppc_draws <- extract(fit, "p_hat")$p_hat   # dims: iterations x M
sub$phatmean <- apply(ppc_draws,2,median)

ppc_draws_p_hat_fix <- extract(fit, "p_hat_fix")$p_hat_fix   # dims: iterations x M
sub$p_hat_fixmean <- apply(ppc_draws_p_hat_fix ,2,mean)

ppc_draws_eps <- extract(fit, "eps")$eps   # dims: iterations x M
sub$eps <- apply(ppc_draws_eps,2,median)

sub %>% ggplot(aes(x = inning, y = phatmean)) + geom_point()
sub %>% ggplot(aes(x = inning, y = p_hat_fixmean)) + geom_point()
sub %>% ggplot(aes(x = inning, y = eps)) + geom_point()

#Chat gpt seems dead set on doing this.  
library(ggplot2)
library(dplyr)
library(rstan)

# 1️⃣ Extract posterior samples
theta_post <- as.matrix(rstan::extract(fit, "theta")$theta)  # iterations x N_team
sigma_post <- as.vector(rstan::extract(fit, "sigma")$sigma)  # iterations

# 2️⃣ Compute θ differences for all unique matchups
team_pairs <- expand.grid(i = 1:stan_data$N_team, j = 1:stan_data$N_team) %>%
  filter(i < j)  # only unique pairs

# Function to compute variance of θ differences per posterior draw
var_theta_diff <- apply(theta_post, 1, function(theta_draw) {
  diffs <- sapply(1:nrow(team_pairs), function(k) {
    i <- team_pairs$i[k]
    j <- team_pairs$j[k]
    theta_draw[i] - theta_draw[j]
  })
  var(diffs)
})

# 3️⃣ Define game lengths of interest
n_vals <- seq(1, 18, by = 1)  # e.g., 1 to 18 innings

# 4️⃣ Compute skill fraction for each game length
skill_frac_df <- data.frame()
for (n in n_vals) {
  skill_frac_post <- n * var_theta_diff / (n * var_theta_diff + sigma_post^2)
  
  skill_frac_df <- rbind(skill_frac_df,
                         data.frame(
                           n = n,
                           median = median(skill_frac_post),
                           lower = quantile(skill_frac_post, 0.025),
                           upper = quantile(skill_frac_post, 0.975)
                         ))
}

# 5️⃣ Plot skill fraction vs game length
ggplot(skill_frac_df, aes(x = n, y = median)) +
  geom_line(color = "blue", size = 1.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "blue", alpha = 0.2) +
  scale_y_continuous(limits = c(0,1)) +
  labs(
    x = "Game length (n)",
    y = "Skill fraction",
    title = "Skill vs Luck as a Function of Game Length",
    subtitle = "Posterior median and 95% credible interval"
  ) +
  theme_minimal()

# Example 1: overlay posterior predictive distribution vs observed
y_obs <- stan_data$y
ppc_dens_overlay(y = y_obs, yrep = ppc_draws[401:600, ])

# Example 2: binned predicted vs observed
ppc_stat(y = y_obs, yrep = ppc_draws[1000:1200, ], stat = "mean")

# Example 3: scatter plot for predicted vs observed
ppc_scatter_avg(y = y_obs, yrep = ppc_draws[1:200, ])

