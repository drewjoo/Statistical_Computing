
# histogram
set.seed(11)

n <- 100
x <- rlnorm(n)
R <- diff(range(x))
k <- ceiling(log2(n))
M <- k + 1
h <- R/M
a <- min(x)
b <- max(x)
B <- seq(a, b, by = h)
f <- numeric(M-1)
for(i in 1:(M-1)) {
	f[i] <- mean(x >= B[i] & x < B[i+1])/h
}

hist(x, prob = T, breaks = 'Sturges')$density


# kernel method
set.seed(11)

n <- 100
x <- c(rnorm(n/2), rnorm(n/2, 3))
iqr <- IQR(x)
s <- sd(x)
h <- min(s, iqr/1.34)*1.06/(n^(1/5))

fd <- density(x, bw = h, n = n)
fd$x

f <- numeric(n)
for (i in 1:n) {
	f[i] <- mean(dnorm((x - fd$x[i])/h)) / h
}
round(cbind(f, fd$y), 3)
