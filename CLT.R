
##LAW OF LARGE NUMBERS (ex1)

seq_mean<-NULL
x<-rexp(10000,rate=2)
m<-1/2

for(i in 1:10000){
  
  seq_mean[i]<-sum(x[1:i])/i
  
}
plot(seq_mean,type='l',xlim=c(0,10000))
abline(h=m,col='red',pch=2)


##ex2 
#install.packages("")
dev.off()
library(knitr)
library(ggplot2)
library(scales)

#generate data
#create a FUNCTION to generate data simulating tosses of a coin

tossCoin=function(n,p){
  outcomes=c(0,1)
  probabilities=c(1-p,p)
  
  #create a random sample of n flips 
  flips=sample(outcomes,n,replace=TRUE,prob=probabilities)
  cum_sum=cumsum(flips)
  index=c(1:n)
  cum_mean=cum_sum/index
  
  return(data.frame(index,cum_mean))
}

tossCoin(n=50,p=0.5)

ggplotCoinTosses = function(n, p) {
  # visualize how cumulative average converges on p  
  # roll the dice n times and calculate means
  trial1 = tossCoin(n,p)
  
  # calculate last mean
  last_mean = round(trial1$cum_mean[n],9)
  
  # plot the results together
  plot1 = ggplot(trial1, aes(x=index,y=cum_mean)) +
    geom_line(colour = "blue") +
    geom_abline(intercept=p,slope=0, color = 'red', size=.5) +
    labs(x = "n (number of tosses)", 
         y = "Cumulative Average") +
    scale_y_continuous(limits = c(0, 1))  +
    annotate("text",
             label=paste("Cumulative mean =", last_mean), 
             y=(0.8), 
             x=n/2, colour="darkgreen") 
  
  return(plot1)
}  
ggplotCoinTosses(100000,0.5)





##CENTRAL LIMIT THEOREM
#ex.3
CLT.ber<-function(n,p){
  mn<-numeric(500) 
  z<-numeric(500) 
  for(i in 1:500){
    x<-rbinom(n,1,p)
    mn[i]<-mean(x)
    z[i]<-(mn[i]-p)/sqrt(p*(1-p)/n)
  }
  return(z)
}
n<-1000
z<-CLT.ber(n,0.3)
hist(z,freq=FALSE,ylim=c(0,pnorm(0)),
     xlim=c(-4,4),col="red",main=paste("n=",n))
curve(dnorm(x),-4,4,add=TRUE)

##CENTRAL LIMIT THEOREM 
#ex 4
n <- 4     # Number of trials (population size)
s <- 2000  # Number of simulations
m <- c(20, 100, 500, 1000)
p<-0.05
EX <- n*p
VarX <- n*p*(1-p)

Z_score <- matrix(NA, nrow = s, ncol = length(m))
for (i in 1:s){
  for (j in 1:length(m)){ # loop over sample size
    samp <- rbinom(n = m[j], size = n, prob = 0.05)
    sample_mean <- mean(samp) # sample mean
    # Calculate Z score
    Z_score[i,j] <- (sample_mean-EX)/sqrt(VarX/m[j]) 
  }
}
#Now let's plot a series of four stacked histograms of the Z-score - one for each sample size - and add the density curve from the normal distribution to each histogram.

# Display distribution of means
# Display distribution of means
windows()
par(mfrow=c(4,1)) 
for (j in 1:4){
  hist(Z_score[,j], xlim=c(-5,5), 
       freq=FALSE, ylim=c(0, 0.5),
       ylab="Probability", xlab="", 
       main=paste("Sample Size =", m[j]))
  # Density curve
  x <- seq(-4, 4, by=0.01)
  y <- dnorm(x)
  lines(x, y, col="blue") 
}
##CENTRAL LIMIT THEOREM 

#ex5

##central limit theorem on a real dataset
data<-read.csv(file.choose())

#Count of Rows and columns
dim(data)

#View top 10 rows of the dataset
head(data,10)
#Calculate the population mean
mean(data$Wall.Thickness)

#Plot all the observations in the data
dev.off()
hist(data$Wall.Thickness,col = "pink",main = "Histogram for Wall Thickness",xlab = "wall thickness")
abline(v=12.8,col="red",lty=1)
#We will take sample size=10, samples=9000
#Calculate the arithmetic mean and plot the mean of sample 9000 times

s10<-c()
n=9000
for (i in 1:n) {
  s10[i] = mean(sample(data$Wall.Thickness,10, replace = TRUE))}
hist(s10, col ="lightgreen", main="Sample size =10",xlab = "wall thickness")
abline(v = mean(s10), col = "Red")
abline(v = 12.8, col = "blue")

#We will take sample size=30, 50 & 500 samples=9000
#Calculate the arithmetic mean and plot the mean of sample 9000 times

s500<-c()
n=9000
for (i in 1:n){
  s500[i]=mean(sample(data$Wall.Thickness,500,replace=TRUE))
  
}

hist(s500,col="pink",main="sample size 500",xlab="wall thickness")

abline(v = mean(s500), col = "Red")
abline(v = 12.8, col = "blue")
