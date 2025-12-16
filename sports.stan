//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//

// The input data is a vector 'y' of length 'N'.
data {
  int<lower=1> P;
  int<lower=1> N;
  vector<lower=0>[N] n;
  int<lower=0,upper=1> y[N];
  matrix[N,P] x;
}


// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  vector[P-1] theta;//vector[P] theta;
  vector[N] eps;
  real<lower=0.1> sigma;
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  for (i in 1:N) {
    eps[i] ~ normal(0, sigma / n[i]);
    y[i] ~ bernoulli_logit(dot_product(theta, x[i,2:P]) + eps[i]);
  }
}

