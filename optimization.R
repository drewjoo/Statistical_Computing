
# Newton's method
lp1 <- function(x, theta) {
	sum(2*(x - theta)/(1 + (x - theta)^2))
}


lp2 <- function(x, theta) {
	-2*sum((1 + 3*(x-theta)^2)/(1 + (x - theta)^2)^2)
}

set.seed(1)
theta <- 3
n <- 100
x <- rcauchy(n) + theta

d <- 100
eps <- 0.0001
theta_old <- 0
while(d > eps) {
	theta_new <- theta_old - lp1(x, theta_old)/lp2(x, theta_old)
	d <- abs(theta_new - theta_old)
	if (d > eps) {
		theta_old <- theta_new
	}
}
theta_new

# EM algorithm
set.seed(1)

theta <- 0.6
pr <- c(1/2, theta/4, (1-theta)/4, (1-theta)/4, theta/4)
X <- rowSums(rmultinom(200, 1, pr)); X
Y <- c(X[1]+X[2], X[3:5]); Y

mle <- (X[2]+X[5]) / sum(X[-1]); mle

N <- sum(Y)

maxIter <- 1000
theta <- 0.1
for (i in 1:maxIter) {
	X2 <- Y[1]*theta/(2 + theta)
	theta <- (X2 + Y[4]) / (X2 + N - Y[1])
}

theta

