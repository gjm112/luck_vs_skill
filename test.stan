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
  eps = eps_raw .* (sigma ./ sqrt(n));
  for (m in 1:M)
    logit_p[m] = beta * n[m] * (theta[team_i[m]] - theta[team_j[m]]) + eps[m];
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
    p_hat[m] = inv_logit(beta * n[m] * (theta[team_i[m]] - theta[team_j[m]]));
}
"
