mu <- c(0,.5,1)
sigma <- 2

nsim <- 100000
n <- 1
eps <- rnorm(nsim,0,sigma)
lambda1 <- exp(mu[1] + eps)
x1 <- unlist(lapply(1:nsim,function(x){sum(rpois(n,lambda1[x]))}))

eps <- rnorm(nsim,0,sigma)
lambda2 <- exp(mu[2] + eps)
x2 <- unlist(lapply(1:nsim,function(x){sum(rpois(n,lambda2[x]))}))

eps <- rnorm(nsim,0,sigma)
lambda3 <- exp(mu[3] + eps)
x3 <- unlist(lapply(1:nsim,function(x){sum(rpois(n,lambda3[x]))}))

mean(x3 > x2)
mean(x3 > x1)
mean(x2 > x1)




