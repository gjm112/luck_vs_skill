data {
  int<lower=1> P;
  int<lower=1> N;
  vector<lower=0>[N] n;
  int<lower=0,upper=1> y[N];
  matrix[N,P] x;
}

parameters {
  vector[P] theta_raw;
  real<lower=0.1> sigma;          // randomness per unit of play
}

transformed parameters {
  vector[P] theta;
  theta = theta_raw - mean(theta_raw);
}

model {
  theta_raw ~ normal(0, 1);
  //sigma ~ exponential(1);
  sigma ~ normal(1, 10) T[0.1,];

  for (i in 1:N) {
    real z;
    //z = dot_product(theta, x[i,1:P])*sqrt(n[i])/sigma;
    //y[i] ~ bernoulli(Phi(z));
    //z = dot_product(theta, x[i,1:P])*sqrt(n[i])/sigma;
    y[i] ~ bernoulli_logit(dot_product(theta, x[i,1:P])*sqrt(n[i])/sigma);
  }
}


generated quantities {
  //int y_pred_unit[6];          // posterior predictive draws for n=1
  vector[6] p_unit1;
  vector[6] p_unit20;// probabilities for n=1

  for (i in 1:6) {
    p_unit1[i] = inv_logit(dot_product(theta, x[i,1:P]) / sigma);
    p_unit20[i] = inv_logit(dot_product(theta, x[i,1:P]) * 20 / sigma);
  }
}
