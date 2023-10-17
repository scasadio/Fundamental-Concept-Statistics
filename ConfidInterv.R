# sample
set.seed(21)
x=c(94,197,16,38,99,141,23)
n=7

mean(x)

library(boot)

## nonparametric bootstrap 
B=10000
Xb=matrix(0,B,n)
sXb=matrix(0,B)
sYb=matrix(0,B)

for (i in 1:B) {
	Xb[i,]=sample(x,n,replace=TRUE)
	sXb[i]=mean(Xb[i,])
	sYb[i]=log(sXb[i])
}


hist(sXb)
hist(sYb)

### normal intervals normali for Xb and Yb

## standard error estimate 
std.X=sd(c(sXb))
## interval
mean(x)-1.96*std.X
mean(x)+1.96*std.X


## standard error estimate 
std.Y=sd(c(sYb))
## interval
log(mean(x))-1.96*std.Y
log(mean(x))+1.96*std.Y


### percentile intervals
quantile(sXb,c(0.025,0.975))
quantile(sYb,c(0.025,0.975))


## check of the TRANSFORMING RESPECTING property
exp(quantile(sYb,c(0.025,0.975)))



## check with the other methods


stima.1=function(data)
{
	out=mean(data)
	return(out)
}


stima.2=function(data)
{
	out=log(mean(data))
	return(out)
}



theta1<-function(data,i){
    stima.1(data[i])
}

theta2<-function(data,i){
    stima.2(data[i])
}


### non parametric BOOTSTRAP 

set.seed(1)
stima1.boot<-boot(x,theta1,R=10000)
set.seed(1)
stima2.boot<-boot(x,theta2,R=10000)

plot(stima1.boot)
plot(stima2.boot)

### confidence intervals

int.conf.stima1<-boot.ci(stima1.boot,conf=0.95,type=c("norm","perc","basic","bca"))
int.conf.stima2<-boot.ci(stima2.boot,conf=0.95,type=c("norm","perc","basic","bca"))

exp(int.conf.stima2$normal[-1])
exp(int.conf.stima2$basic[-c(1,2,3)])
exp(int.conf.stima2$percent[-c(1,2,3)])
exp(int.conf.stima2$bca[-c(1,2,3)])



library(bootstrap)

# function for intervals t bootstrap

?boott


# intervals t bootstrap

int.conf.stima1.tboot<-boott(x,stima.1,nbootsd=200, nboott=1000)
int.conf.stima2.tboot<-boott(x,stima.2,nbootsd=200, nboott=1000)

