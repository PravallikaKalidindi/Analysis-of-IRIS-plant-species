library(Ecdat)

data<-iris
hist(iris$Sepal.Width,main="Distribution of Sepal Width")

#ecdf
x<-ecdf(iris$Sepal.Width)
plot(x,main="ECDF of sepal width")
mean(iris$Sepal.Width)
Alpha=0.05
n=length(iris$Sepal.Width)
Eps=sqrt(log(2/Alpha)/(2*n))
grid<-seq(0,150, length.out = 1000)
lines(grid, pmin(x(grid)+Eps,1))
lines(grid, pmax(x(grid)-Eps,0))

#MLE Estimate
#Tau = mu1-mu2 (1-setosa) , (2-versicolor)
mle_est = mean(iris[iris$Species == 'setosa',]$Sepal.Width) - mean(iris[iris$Species == 'versicolor',]$Sepal.Width)

#parametric bootstrap
sample1<- iris[iris$Species == 'setosa',]
sample2<-iris[iris$Species == 'versicolor',]
mu1.hat = mean(sample1$Sepal.Width)
mu2.hat = mean(sample2$Sepal.Width)
n1=length(sample1$Sepal.Width)
n2=length(sample2$Sepal.Width)
sd1.hat = sd(sample1$Sepal.Width)
sd2.hat = sd(sample2$Sepal.Width)
 
omega_hat<-c()
for (i in 1:1000){
  x1<-rnorm(n1,mu1.hat,sd1.hat)
  x2<-rnorm(n2,mu2.hat,sd2.hat)
  omega_hat[i]=mean(x1)-mean(x2)
}
parametric_se <- sd(omega_hat)
parametric_CI <- c(mean(omega_hat)-2*sd(omega_hat),mean(omega_hat)+2*sd(omega_hat))
mean(omega_hat)
hist(omega_hat,main = "Distribution using Parametric Bootstrap")

#Non parametric bootstrap
omega_hat2<-c()
for (i in 1:1000){
  x1<-sample(sample1$Sepal.Width,n1,replace=T)
  x2<-sample(sample2$Sepal.Width,n2,replace=T)
  omega_hat2[i]=mean(x1)-mean(x2)
}

non_parametric_se <- sd(omega_hat2)
non_parametric_CI <- c(mean(omega_hat2)-2*sd(omega_hat2),mean(omega_hat2)+2*sd(omega_hat2))
mean(omega_hat2)
hist(omega_hat2,main = "Distribution using Non Parametric Bootstrap")

#Hypothesis Testing
# Null hypothesis = mu1=mu2 , Alternate hypothesis = mu1 not equal to mu2
## WALD TEST
sigma.hat<-sqrt((sd(sample1$Sepal.Width)^2/n1)+(sd(sample2$Sepal.Width)^2/n2))
z.stat<-(mle_est-0)/(sigma.hat)
p.value=2*(1-pnorm(abs(z.stat)))
#p value is less than 0 and hence fail to reject null hypothosis

#Bayesian analysis
 #prior of mu1 ~ N(0,1), prior of mu2 ~ N(0,1)
#posterior of mu1 ~ N(n1*xbar/1+n1,1/1+n1), posterior of mu2 ~ N(n2*xbar/1+n2,1/1+n2)

#posterior of mu1
Ib.1=1
Ix.1 = n1/var(sample1$Sepal.Width)
pos1.mean=((mean(sample1$Sepal.Width))*Ix.1)/(Ib.1+Ix.1)
pos1.var = 1/(Ib.1+Ix.1)
posterior.mu1 = rnorm(100,pos1.mean,sqrt(pos1.var))

#posterior of mu2
Ib.2=1
Ix.2 = n2/var(sample2$Sepal.Width)
pos2.mean=((mean(sample2$Sepal.Width))*Ix.2)/(Ib.2+Ix.2)
pos2.var = 1/(Ib.2+Ix.2)
posterior.mu2 = rnorm(100,pos2.mean,sqrt(pos2.var))

#posterior of mu1-mu2
pos3.mean = pos1.mean-pos2.mean
pos3.var = posterior.mu1+pos2.var
posterior.mu1.mu2 = rnorm(100,pos3.mean,sqrt(pos3.var))
hist(posterior.mu1.mu2,main="Posterior distribution of mu1-mu2")
mean(posterior.mu1.mu2)

