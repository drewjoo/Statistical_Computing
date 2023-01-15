# 1. The standard Laplace distribution has density 
# f(x)=exp(-|x|)/2, x: 실수 전체. 
# Use the inverse transform method to generate a random sample of size 1000 from this distribution.
# 생성한 것(x)과 아래 샘플(y)과 비교해서 비슷하게 나오면 됩니다.

# Inf>=x1>= 0
u1<-runif(5000,1/2,1)
x1=-log(2-2*u1)
# -Inf<x2<0
u2<-runif(5000,0,1/2)
x2<-log(u2)
# -Inf<x<Inf
x<-x1+x2

install.packages('ExtDist')
library(ExtDist)
y <- rLaplace(1000)
plot(density(x),col="blue",main = "x와 y 비교")
lines(density(y),col='red')
legend("topright", c("x","y" ),col=c("blue","red"),pch = 15)

# 또 다른 방법
y<-rLaplace(1000)
plot(density(y))

n<-1000
U<-runif(n)

x<-numeric(length(U))
for(i in 1:length(U)){
  if(U[i]>=0.5){
    x[i]<- -log(-2*U[i]+2)
  }else{
    x[i]<-log(2*U[i])
  }
}
plot(density(x))
lines(density(y), col="red")


# 2. Generate a random sample(x) of size 1000 from a normal location mixture.
# The components of the mixture have N(0,1) and N(3,1) distributions with mixing probabilities p1 and p2=1-p1.
# p1=0.75
# 약간의 쌍봉형 density 를 갖는 것을 확인하기 위해 아래 그림을 그리면 됩니다.
# plot(density(x))
n=1000
p1=0.75;p2=1-p1
s<-sample(0:1,size =n, replace = T, prob = c(p1,p2));table(s)
x1<-rnorm(1000,mean = 0,sd=1)
x2<-rnorm(1000,mean=3,sd=1)
x<-(1-s)*x1+s*x2
plot(density(x))

# 다른 방법 
n<-1000
p<-c(0.75,0.25)

mu<-sample(c(0,3), size=n, replace=T, prob=p)
mu
x<-rnorm(n, mean = mu, sd=1)
plot(density(x))
