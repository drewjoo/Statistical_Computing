#1.
n<-1000
x<-1
X<-rnorm(n)
mean(X<=x)
pnorm(x)


#2.
mc_normcdf=function(x,n,antithetic=F){
  u<-runif(n)
  if(antithetic){
    u<-c(u,1-u)
  }else{
    v<-runif(n)
    u<-c(u,v)
  }
  mean(x*exp(-(x*u)^2/2))/sqrt(2*pi)+0.5
}

mc_normcdf(x,n)
mc_normcdf(x,n, T)

m<-100
mc<-numeric(m)
mc.a<-numeric(m)

for(i in 1:m){
  mc[i]<-mc_normcdf(x,n)
  mc.a[i]<-mc_normcdf(x,n,T)
}
sd(mc)
sd(mc.a) # antithetic을 이용하면 분산 값이 작아진다. 


#3.
g<-function(x){
  exp(-x)/(1+x^2)
}
h<-function(x){
  exp(-0.5)/(1+x^2)
}

MC_I<-function(n,cv=FALSE){
  if(cv){
    u<-runif(n)
    hbar<-mean(h(u))
    gbar<-mean(g(u))
    mu<-exp(-0.5)*(pi/4)
    
    c=-sum((g(u)-gbar)*(h(u)-hbar))/sum((h(u)-hbar)^2)
    
    uu<-runif(n)
    return(mean(g(uu))+c*(h(uu)-mu))
  }else{
    uu<-runif(n)
    return(mean(exp(-uu)/(1+uu^2)))
  }
}
MC_I(n)
MC_I(n, cv=TRUE)

m<-100
mc<-numeric(m)
mc.a<-numeric(m)
for(i in 1:m){
  mc[i]<-MC_I(n)
  mc.a[i]<-MC_I(n, TRUE)
}
sd(mc)
sd(mc.a)
#4.
n=1000
y<-rexp(n)
gy<-g(y)
gy[y>=1]<-0
mean(gy/dexp(y))
z<-y[y<=1];z
sum(g(z)/dexp(z))/sum(1/dexp(z))
#5.
m<-100
Ij<-numeric(m)
for(j in 1:m){
  u<-(1/m)*runif(floor(n/m))+j/m
  Ij[j]<-mean(exp(-u)/(1+u^2))
}
mean(Ij)

#6.
g<-function(x){
  exp(-x^2)
}
x<-rt(n,df=3)
mean(g(x))
inv_y<-rgamma(n,3/2,3/2)
y<-1/inv_y
mean(1/sqrt(2*y+1))
