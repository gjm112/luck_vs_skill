nfl2025 <- read.csv("./nfl2025.csv")
nfl2024 <- read.csv("./nfl2024.csv")
names(nfl2024)[8] <- "Date.1"
names(nfl2025)[8] <- "Date.1"
nfl <- rbind(nfl2024,nfl2025)
teams <- sort(unique(nfl$Winner.tie))
ids <- data.frame(teams = teams, id = 1:length(teams))

nfl <- nfl %>% mutate(Home = ifelse(X == "@",Loser.tie,Winner.tie),
                      Away = ifelse(X == "@",Winner.tie,Loser.tie),
                      PtsHome = ifelse(X == "@", PtsL,PtsW),
                      PtsAway = ifelse(X == "@", PtsW,PtsL)) %>% 
  left_join(ids, by = c("Home" = "teams")) %>% rename(Homeid = id) %>% 
  left_join(ids, by = c("Away" = "teams")) %>% rename(Awayid = id) %>% 
  mutate(y = (PtsHome > PtsAway) +0) %>% select(Homeid,Awayid,y)



stan_data <- list(
  M = nrow(nfl),
  N_team = length(teams),
  y = nfl$y,#sample(nfl$y,length(nfl$y)),
  team_i = nfl$Homeid,
  team_j = nfl$Awayid,
  n = rep(1,nrow(nfl))
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
  real<lower=0> beta;
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
            iter = 10000, chains = 4, seed = 123)

# Print results
print(fit, pars = c("theta", "beta", "sigma"))



  
