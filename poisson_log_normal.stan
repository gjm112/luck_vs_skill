parameters{

  vector[T-1] off_raw;
  vector[T-1] def_raw;

  real alpha;

  real<lower=0> sigma;

  vector[N] eps_raw;
}

transformed parameters{

  vector[T] off;
  vector[T] def;

  off[1:(T-1)] = off_raw;
  off[T] = -sum(off_raw);

  def[1:(T-1)] = def_raw;
  def[T] = -sum(def_raw);

  vector[N] log_lambda;

  for(i in 1:N)
    log_lambda[i] =
      alpha +
      off[offense[i]] -
      def[defense[i]] +
      sigma * eps_raw[i];
}