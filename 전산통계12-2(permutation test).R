# 자료를 생성해서 평균비교

#1.
x<-rnorm(20)
y<-rnorm(25, mean=2, sd=2)

Tstat<-function(x,y){
  abs(mean(x)-mean(y))
}
tobs=Tstat(x,y)

B<-1000
tperm<-numeric(B)
z<-c(x,y);N<-length(z);m<-length(x)

for(i in 1:B){
  idx<-sample(N,m)
  tperm[i]<-Tstat(z[idx],z[-idx])
}
mean(tperm>tobs)
hist(tperm);points(tobs,0, col="red",pc=19)

#2.
# briefly checking the data
data("chickwts")
library(dplyr)
chickwts%>%group_by(feed)%>%
  summarise(n(), mean=mean(weight))

attach(chickwts)
x<-weight[feed=="sunflower"]
y<-weight[feed=="linseed"]

tobs<-t.test(x,y)$statistic
B<-1000
tperm<-numeric(B)
z<-c(x,y);N<-length(z);m<-length(x)

for(i in 1:B){
  idx<-sample(N,m)
  tperm[i]<-t.test(z[idx],z[-idx])$statistic
}
mean(tperm>tobs)
hist(tperm);points(tobs,0, col="red",pch=19)
summary(cbind(x,y))
boxplot(cbind(x,y))
detach(chickwts)
