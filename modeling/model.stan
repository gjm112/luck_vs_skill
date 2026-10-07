data {
  int<lower=0> N;
  int<lower=0> num_teams;
  array[N] int<lower=0, upper=num_teams> home_team;
  array[N] int<lower=0> home_scored_in_inning;
}

parameters {
  vector<lower=0>[num_teams] home_skill_mean;
  vector<lower=0>[num_teams] home_skill_dispersion;
}

model {
  home_scored_in_inning ~ neg_binomial_2(
    home_skill_mean[home_team],
    home_skill_dispersion[home_team]
  );
}
