# 1. 표준정규분포를 이용하여 theta = E[g(X)] 의 monte carlo 추정치와 표준오차를 구하시오.
# rnorm(n=30,0,1) 이용.
# g: 범위(range)
m<-1000
g<-numeric(m)
for(i in 1:m){
  x<-rnorm(30,0,1)
  g[i]<-range(x)[2]-range(x)[1]
}

mean(g) # 추정치
se=sd(g)/sqrt(m);se # 표준오차

# 2. Suppose X_1,...,X_n are a random sample from a normal distribution.
# Construct a 95% confidence interval for the parameter mu(평균). 
# Use a Monte Carlo method to obtain an empirical estimate of the confidence level 
# when data is generated from standard normal. 
# rnorm(n=30,0,1) 이용.

UCL<-replicate(1000, expr = {
  x<-rnorm(30,0,1)
  (30-1)*var(x)/qchisq(0.05, df=30-1)
})

sum(UCL>1)
mean(UCL>1)



