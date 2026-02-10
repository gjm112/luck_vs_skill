#sigma 10, n 1, beta 3 is beautiful 
#alpha <- 0
beta <- 0
theta <- c(.1,0)
n <- 1
s <- seq(-3,3,0.1)
sigma <- 1
#y <- 1/2 + (1-alpha)*(1-exp(-n))*(plogis(sqrt(n)*beta*(s)) - 1/2)
eps <- rnorm(1,0,sigma/sqrt(n))
y <- plogis(n*beta*(s) + eps)
plot(s, y, type = "l", ylim = c(0,1), col = rgb(0,0,0,0.01))
vec <- c()
for (i in 1:10000){
eps <- rnorm(1,0,sigma/sqrt(n))
y <- plogis(n*beta*(s) + eps)
vec[i] <- rbinom(1,1,y)
points(s, y, type = "l", ylim = c(0,1), col = rgb(0,0,0,0.01))
}
mean(vec)





alpha <- 0.25
beta <- 1
theta <- c(1,0)
n <- 10
s <- seq(-3,3,0.1)
y <- 1/2 + (1-alpha/sqrt(n))*(plogis(sqrt(n)*beta*(s)) - 1/2)
points(s, y, type = "l",col = "red")

alpha <- 0.25
beta <- 1
theta <- c(1,0)
n <- 1000
s <- seq(-3,3,0.1)
y <- 1/2 + (1-alpha/sqrt(n))*(plogis(sqrt(n)*beta*(s)) - 1/2)
points(s, y, type = "l",col = "blue")

alpha <- .25
beta <- 1
theta <- c(0.1,0)
n <- 1000
s <- seq(-3,3,0.1)
y <- 1/2*alpha + (1-alpha)*plogis(sqrt(n)*beta*(s))
points(s, y, type = "l",col = "red")

alpha <- .25
beta <- 1
theta <- c(0.1,0)
n <- 1000
s <- seq(-3,3,0.1)
y <- 1/2*alpha/sqrt(n) + (1-alpha/sqrt(n))*plogis(sqrt(n)*beta*(s))
points(s, y, type = "l",col = "blue")

alpha <- .25
beta <- 1
theta <- c(0.1,0)
n <- 1000
s <- seq(-3,3,0.1)
y <- 1/2*alpha/sqrt(n) + (1-alpha/sqrt(n))*plogis(beta*(s))
points(s, y, type = "l",col = "gold")


################################################################
alpha <- 0.99
beta <- 1
theta <- c(1,0)
n <- 1
s <- seq(-3,3,0.1)
y <- 1/2*alpha + (1-alpha)*plogis(sqrt(n)*beta*(s))
plot(s, y, type = "l", ylim = c(0,1))

alpha <- 0.99
beta <- 1
theta <- c(0.1,0)
n <- 1000
s <- seq(-3,3,0.1)
y <- 1/2*alpha + (1-alpha)*plogis(sqrt(n)*beta*(s))
points(s, y, type = "l",col = "red")

alpha <- 0.99
beta <- 1
theta <- c(0.1,0)
n <- 1000
s <- seq(-3,3,0.1)
y <- 1/2*alpha/sqrt(n) + (1-alpha/sqrt(n))*plogis(sqrt(n)*beta*(s))
points(s, y, type = "l",col = "blue")

alpha <- 0.99
beta <- 1
theta <- c(0.1,0)
n <- 1000
s <- seq(-3,3,0.1)
y <- 1/2*alpha/sqrt(n) + (1-alpha/sqrt(n))*plogis(beta*(s))
points(s, y, type = "l",col = "gold")