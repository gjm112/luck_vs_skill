data {
  int<lower=0> N;
  array[N] int<lower=0> home_scored_in_inning;
}

parameters {
  real<lower=0> alpha;
  real<lower=0> beta;
}

model {
  home_scored_in_inning ~ neg_binomial(alpha, beta);
}
