## Week 4-2

#Ex 3.3
n<-1000
u<-runif(n)
lambda<-3
x<-log(1-u)/(-lambda)
qqplot(x, rexp(n, rate=lambda))
abline(a=0,b=1, col="red")

#Ex 3.5
n<-1000
u<-runif(n)
p<-0.3
x<-floor(log(u)/log(1-p))+1;x
ceiling(log(u)/log(1-p))
y<-rgeom(n,p)
x11();par(mfrow=c(1,2));hist(x);hist(y)

#Ex 3.7
n<-1000
j<-0
k<-0
C<-6 
# C가 6인경우, 그리고 3/2 인 경우 두 케이스 모두 살펴보기 
# 마지막으로 k/j로 확률 비교
y<-numeric(n)

while(k<n){
  j<-j+1
  x<-runif(1)
  u<-runif(1)
  if(6*x*(1-x)/C>u){
    k<-k+1
    y[k]<-x
  }
}
j
k/j

qqplot(y,rbeta(n,2,2))
abline(a=0,b=1, col="red")

## Transformation method
#1.
n<-1000
z<-rnorm(n)
v<-z^2
qqplot(v,rchisq(n,df=1))

#2.
n<-1000
m1<-3
m2<-5

u<-rnorm(n)^2
for(i in 2:m1){u<-u+rnorm(n)^2}
v<-rnorm(n)^2
for(i in 2:m2){v<-v+rnorm(n)^2}


f<-(u/m1)/(v/m2)

qqplot(f, rf(n,m1,m2))

#3.
n<-1000
m<-30
v<-rnorm(n)^2
for(i in 2:m){v<-v+rnorm(n)^2}
t<-rnorm(n)/sqrt(v/m)
qqplot(t, rt(n,m))

rbinom(1000,1,0.5)
x<-sample(0:1,100, replace = T,prob=c(0.4,0.6))
table(x)


## Mixture
#1. normal mixture
n<-1000
x1<-rnorm(n, mean=3, sd=1)
x2<-rnorm(n, mean=1, sd=1)
par(mfrow=c(1,2))
hist(x1,probability = T)
hist(x2,probability = T)
u<-runif(1000)
s<-as.integer(u>0.5)
x<-s*x1+(1-s)*x2
par(mfrow=c(1,1))
hist(x, probability = T, breaks=20)
mean(x)

#2. negative binomial
n<-10000
p=0.3
a<-5 # shape
b<-(1-p)/p #b is scale # p = rate/(1+rate)
theta<-rgamma(n, scale=a, shape=b)
x11();par(mfrow=c(1,2))
x<-rpois(n, theta)
barplot(table(x))
y<-rnbinom(n,5,prob=p)
barplot(table(y))

#3. t-distribution
n<-1000
r<-10 # df
a<-r/2
b<-2/r
theta<-rgamma(n,shape=a,scale=b)
x<-rnorm(n,mean=0,sd=sqrt(1/theta))
y<-rt(n,r)
qqplot(x,y)
