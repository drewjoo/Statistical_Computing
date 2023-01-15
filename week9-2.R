#전산통계 week9-2
#ex11-10 이변량 정규분포

rho=0.7
nsamp=10000
y<-z<-rep(NA, nsamp)
y[1]<-z[1]<-0
sdy<-sdz<-sqrt(1-rho^2)

for(isamp in 2:nsamp){
  y[isamp]<-rnorm(n=1, mean=rho*z[isamp-1],sd=sdy)
  z[isamp]<-rnorm(n=1, mean=rho*y[isamp],sd=sdz)
}
x11()
par(mfrow=c(3,2))
x=seq(-6,6,length=100)
hist(y, freq=F)
curve(dnorm(x,0,1),add=T)
hist(z, freq=F)
curve(dnorm(x,0,1),add=T)

plot(y,z,xlim = c(-6,6), ylim = c(-6,6))

x11()
contour(x1,x2,mv.mat, nlevels=15, lwd=2)
points(y,z,pch='.')

x1<-seq(-6,6,length=25)
x2<-seq(-6,6,length=25)
mv.mat<-matrix(NA, length(x1), length(x2))
sigma<-matrix(c(1,rho,rho,1),2,2)
for(i in 1:length(x2)){
  for(j in 1:length(x2)){
  mv.mat[i,j]<-mvtnorm::dmvnorm(c(x1[i],x2[j]), sigma=sigma)
  }
}

mean(y);mean(z)
sd(z);sd(z)
cor(y,z)
sort(y)[floor(nsamp*0.05)]
sort(y)[floor(nsamp*0.95)]
sort(y)[floor(nsamp*0.025)]
sort(y)[floor(nsamp*0.975)]
sort(z)[floor(nsamp*0.025)]
sort(z)[floor(nsamp*0.975)]

