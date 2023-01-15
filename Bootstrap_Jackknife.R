## ==============
## Empirical CDF
## ==============
ecdf <- function(x) {
  n <- length(x)
  x <- sort(x)
  Fn <- numeric(n)
  for (i in 1:n) {
    Fn[i] <- sum(x <= x[i])/n
  }
  return(Fn)
}
  
n <- 100
x <- rnorm(n)
Fn <- ecdf(x)
plot(sort(x), Fn, type = 'p')
lines(sort(x), pnorm(sort(x)))
lines(sort(x), Fn, col="red")

  
# CI
ecdfCI <- function(x, alpha = 0.05){
  n <- length(x)
  Fn <- ecdf(x)
  eps <- sqrt(0.5*log(2/alpha)/n)
  
  U <- Fn + eps
  upperI <- U > rep(1,n)
  U[upperI] <- 1
  
  L <- Fn - eps
  lowerI <- Fn-eps < rep(0,n)
  L[lowerI] <- 0
  
  list(lower=L, upper=U)
}
    
out <- ecdfCI(x)
Fnt <- pnorm(sort(x))

plot(sort(x), Fn, type='b', col='blue')
lines(sort(x), out$lower, col='red')
lines(sort(x), out$upper, col='red')
lines(sort(x), Fnt, col='black', lwd = 2)


nsim <- 1000
c <- 0
for (isim in 1:nsim) {
  x <- rnorm(100)
  x <- sort(x)
  Fnt <- pnorm(x)
  outi <- ecdfCI(x)
  if (all(outi$lower <= Fnt) & all(outi$upper >= Fnt)) {
    c <- c + 1
  }
}
c/nsim



## ===========
## Bootstrap
## ===========
library(bootstrap)
data("patch", package = "bootstrap");patch
patch <- read.csv("patch.csv"); patch
y <- patch$y
z <- patch$z 
n <- length(y)

nb <- 1000
theta_hat <- mean(y)/mean(z)
theta_b <- numeric(nb)
for (ib in 1:nb) {
  I <- sample(n,n,replace=TRUE)
  theta_b[ib] <- mean(y[I])/mean(z[I])
}

bias <- mean(theta_b) - theta_hat; bias
se <- sd(theta_b); se

#normal interval
alpha <- 0.05
c(theta_hat-qnorm(1-alpha/2)*se,theta_hat+qnorm(1-alpha/2)*se)

#pivotal interval
q <- (1-alpha/2)
c(2*theta_hat - quantile(theta_b, q), 2*theta_hat - quantile(theta_b, 1-q))

#percentile interval
c(quantile(theta_b, 1-q), quantile(theta_b, q))


## ==========
## Jackknife
## ==========
# bias
theta.hat <- mean(y)/mean(z)
theta.jack <- numeric(n)
for (i in 1:n)
  theta.jack[i] <- mean(y[-i]) / mean(z[-i])
bias <- (n - 1) * (mean(theta.jack) - theta.hat)
bias  #jackknife estimate of bias


# standard error
se <- sqrt((n-1) * mean((theta.jack - mean(theta.jack))^2))
se


## ===========================
## Jackknife-after-Bootstrap
## ===========================
seboot <- function(y, z, nboot = 1000){
  n <- length(y)
  theta_b <- numeric(nb)
  for (ib in 1:nb) {
    I <- sample(n,n,replace=TRUE)
    theta_b[ib] <- mean(y[I])/mean(z[I])
  }
  sd(theta_b)
}
  
n <- length(y)
se_J <- numeric(n)
for (i in 1:n){
  se_J[i] <- seboot(y[-i],z[-i])
}

se_bar <- mean(se_J)
se_jack <- sqrt((n-1) * mean((se_J - se_bar)**2))    
se_jack
seboot(y,z)

