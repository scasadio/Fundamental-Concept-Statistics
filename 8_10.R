#ex 1
seq_mean<-NULL
x<-rexp(10000,rate=2)
m<-1/2
for(i in 1:10000){
  seq_mean[i]<-sum(x[1:i])/i
}
plot(seq_mean,type = 'l',xlim = c(0,10000))
abline(h=m, col='red')


##
p<-0.05

#ex2
install.packages("ggplot2")
library(ggplot2)
install.packages("knitr")
library(knitr)
install.packages("scales")
library(scales)

tossCoin=function(n,p){
  outcomes=c(0,1)
  probabilities=c(1-p,p)
  #create a random sample of n flips
  flips=sample(outcomes,n, replace = TRUE, prob = probabilities)
  cum_sum=cumsum(flips)
  index=c(1:n)
  cum_mean=cum_sum/index
  return(data.frame(index,cum_mean))
}
tossCoin(100,0.5)

ggplotCoinTosses=function(n,p){
  trial1=tossCoin(n,p)
  last_mean=trial1$cum_mean[n]
  
  plot1=ggplot(trial1,aes(x=index, y=cum_mean))+
    geom_abline(colour="blue")+
    geom_abline(intercept = p,slope = 0, col="red", size=0.50)+
    labs(x="n (number of tosses)",y= "cumulative average")+
    scale_y_continuous(limits=c(0,1))+
    annotate("text",label=paste("cumulative mean",last_mean),y=0.8,x=n/2, colour="darkgreen")+
  return(plot1)
}
ggplotCoinTosses(100000,0.5)

#ex 3
CLT.ber<-function(n,p){
  mn<-numeric(500)
  z<-numeric1(500)
  for(i in 1:500){
    x<-rbinom(n,1,p)
    mn[i]<-mean(x)
    z[i]<-(mn[i]-p)/sqrt(p*(1-p)/n)
  }
  return(z)
}
n<-1000
z<-CLT.ber(n,0.3)
hist(z,freq = FALSE,ylim = c(0,pnorm(0)),xlim = c(-4,4),col = "red")
curve(dnorm(x),-4,4,add=TRUE)

#ex 4
n<-4
s<-2000
m<-c(20,100,500,1000)
EX<-n*p
VX<-n*p*(1-p)
Z_score<-matrix(NA,nrow = s,ncol = lenght(m))
for (i in 1:s) {
  for (j in 1:length(m)) {
    samp<-rbinom(n=m[j],size=n,prob = 0.05)
    sample_mean<-mean(samp)
    #calculate z score for mean of each sample size
    z_score[i,j]<-(sample_mean-EX)/sqrt(VX/m[j])
  }
}
par(mfrow=c(4,1))
for (j in 1:4) {
  hist(z_score[,j],xlim=c(-5,5),freq = FALSE,ylim = c(0,0.5),ylab="probbility")
  #density curve
  x<-seq(-4,4,0.01)
  y<-dnorm(x)
  lines(x,y,col="blue")
}

data<-read.csv(choose.files())
str(data)
mean(data$Wall.Thickness)
hist(data$Wall.Thickness,col = "pink",main = "histogram for wall thickness")

s10<-c()
n=9000
for (i in 1:n){
  s10[i]=mean(sample(data$Wall.Thickness,10, replace = TRUE))
}
hist(s10, col="lightgreen", main= "sample size 10", xlab = "wall thickness")

s500<-c()
n=9000
for (i in 1:n){
  s500[i]=mean(sample(data$Wall.Thickness,500,replace = TRUE))
}
hist(s500, col="pink",main="sample size 500", xlab = "wall thickness")
