fit <- readRDS("./modeling/home_scored_in_inning_model.rds")
shinystan::launch_shinystan(fit)
