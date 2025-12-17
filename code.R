theta <- c(4:1)
sigma <- 1
x <- data.frame()

for (q in 1:20){
  for (j in c(1:10)){
    
xtemp <- as.data.frame(rbind(c(1, -1, 0, 0),
           c(1, 0, -1, 0),
           c(1, 0, 0, -1),
           c(0, 1, -1, 0),
           c(0, 1, 0, -1),
           c(0,0, 1, -1)))
xtemp$n <- j
x <- rbind(x,xtemp)
}
}
x$y <- NA




for (i in 1:nrow(x)){
        temp <- x[i,]
        xb <- (theta[which(temp[1:4] == 1)] - theta[which(temp[1:4] == -1)]) * sqrt(temp$n)/sigma
        x$pstar[i]  <- pstar <- exp(xb)/(1+exp(xb))
        x$y[i] <- rbinom(1,1,pstar)
}

x








library(rstan)

#Create a list of the data 
stan_data <- list(
  P = length(theta),
  N = nrow(x),
  n = x$n,
  y = x$y,
  x = x[,1:4]
)

fit_rstan <- stan(
  file = "./sports2.stan",
  data = stan_data
)
summary(fit_rstan)$summary
plot(fit_rstan)




# 
# 
# 
# 
# theta <- c(1,0)
# p <- exp(theta[1]-theta[2])/(1+exp(theta[1] - theta[2]))
# sigma <- 10
# pstar <-c()
# n <- 100
# for (q in 1:100000){
# eps <- c()
# eps[1] <- rnorm(n,0,sigma/n)
# eps[2] <- rnorm(n,0,sigma/n)
# xbeps <- theta[1] + eps[1] - theta[2] - eps[2]
# pstar[q]  <- exp(xbeps)/(1+exp(xbeps))
# }
# hist(pstar)
# 
# mean(rbinom(length(pstar),1,pstar))
# 
# rbinom(length(pstar),1,pstar)
# p
# 
