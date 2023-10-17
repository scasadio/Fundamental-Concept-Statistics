
##IF ELSE STATEMENT
#if
x <- 7
y <- 5
if(x > y) print("x is greater")

#if else
x <- 5
y <- 7
if(x > y) {
  print("x is greater")
} else {
  print("y is greater")
}


#nested if else statement
#else if
x <- 5
y <- 5
if(x > y) {
  print("x is greater")
} else if(x < y) {
  print("y is greater")
} else {
  print("x and y are equal")
}


#R LOOPS

#for
#loops are execute for a prescribed number of times,
#as controlled by a counter or an index, incremented at each iteration cycle. 

a<-0
for (i in 1:10)a<-a+i
a




#loops based on the onset and verification of a logical condition. 
#The condition is tested at the start or the end of the loop construct. 

#repeat
#break statement is the only way to come out of the repeat loop
sum <- 1
repeat
{
  sum <- sum + 2
  print(sum)
  if (sum > 11)
    break
}

#while
#while loop consists of a constraint condition to start with, given after the keyword "while".
x<-0
while (x < 10)
{
  x<- x+4
  print (x)
}

x<-0

while (x < 10)
{x <- x + 4
print (x)
if ( x == 8)
  break
}



##BUILDING FUNCTION IN R
#function (arglist)  {body}
sum.1<-function (a,b) a+b
sum.1(2,3)
sum.1(10 ,12)

sum.2<-function(a,b){
  if((a>0)& (b>0))
    a+b
  else
    -1
}
sum.2(2,3)
sum.2(2,-4)

sum.3<-function(a,b){
  if((a>0)| (b>0))
    a+b
  else
    -1
}
sum.3(2,2)
sum.3(2,-4)
sum.3(-2,-4)

#return argument
check <- function(x) {
  if (x > 0) {
    result <- "Positive"
  }
  else if (x < 0) {
    result <- "Negative"
  }
  else {
    result <- "Zero"
  }
  return(result)
}

check(0)
check(-2)
#some examples
new.function <- function(a) {
  for(i in 1:a) {
    b <- i^2
    print(b)
  }
}
new.function(4)
new.function <- function(a) {
  b<-c(0,length(a))
  for(i in 1:a) {
    b[i] <- i^2
    
  }
  return(b)
}
new.function(4)



## PROBABILITY DISTRIBUTIONS IN R
#DISCRETE RANDOM VARIABLES
#BINOMIAL DISTRIBUTION
?dbinom
dbinom(x=3,size=10,prob=0.3)
pbinom(q=3,size=10,prob=0.3)
qbinom(p=0.64,size=10,prob=0.3)
x <- rbinom(n=1000,size=100,p=0.2)
hist(x, 
     xlim = c(min(x), max(x)), 
     probability = TRUE, 
     nclass = max(x) - min(x) + 1, 
     col = 'lightblue',
     main = 'Binomial distribution, n=100, p=.2')
#generate 10 values from a Bernoulli distribution
rbinom(10, 1, 0.4)

#GEOMETRIC DISTRIBUTION
dgeom(x=10, prob=0.12)
pgeom(q=10, prob=0.12)
qgeom(p=0.75, prob=0.12)
N <- 10000
x <- rgeom(N, .12)
hist(x, 
     xlim=c(min(x),max(x)), probability=T, nclass=20,
     col='lightblue',
     main='Geometric distribution, p=.01')
#POISSON DISTRIBUTION
dpois(x=2,lambda=5)
ppois(q=2,lambda=5)
qpois(p=0.12,lambda=5)
N <- 10000
x <- rpois(N, 5)
hist(x, 
     xlim=c(min(x),max(x)), probability=T, nclass=20,
     col='lightblue',
     main='Poisson distribution, lambda=5')



#CONTINOUS RANDOM VARIABLES
#UNIFORM DISTRIBUTION
?runif
dunif(x=0.3, min = -4, max = 1)
dunif(x=0.2, min = -1, max = 1)
punif(q=0.5, min = -1, max = 1)
qunif(p=0.75, min = -1, max = 1)
x<-runif(n=10000, min = -1, max = 1)
hist(x, 
     xlim=c(min(x),max(x)), probability=T, nclass=20,
     col='lightblue',
     main='Uniform (0,1) distribution')


#NORMAL DISTRIBUTION
x <- seq(-4,4,0.1)
d<-dnorm(x)#by deafult mean 0 and SD 1
dnorm(0,mean=4,sd=10)
plot(x,d,type="l")
curve(dnorm(x), from=-4, to=4,col='red',add=T)

pnorm(0,mean=2,sd=3)
pnorm(0,mean=2,sd=3,lower.tail=FALSE)
y <- pnorm(x)
plot(x,y, type='l')

qnorm(0.5) # gives the quantile function:#if you give the function a probability it returns the associated Z-score
qnorm(0.2524925,mean=2,sd=3)
rnorm(10)#generates random deviates
rnorm(10,mean=7,sd=5)

#EXPONENTIAL DISTRIBUTION
n<-1000
dexp(4, rate = 1, log = FALSE)
pexp(4, rate = 1, log = FALSE)
qexp(0.98, rate = 1, log = FALSE)
x <- rexp(n)
hist(x, probability=T,
     col='light blue', main='Exponential Distribution')
curve(dexp(x), xlim=c(0,10), col='red', lwd=3,
      main='Exponential Distribution',add=T)


#CHI-SQUARED DISTRIBUTION
dchisq(x=2, df=3, ncp = 0, log = FALSE)
pchisq(q=2, df=3, ncp = 0)
qchisq(p=0.42, df=3)
curve(dchisq(x,1), xlim=c(0,10), ylim=c(0,.6), col='red', lwd=3)
curve(dchisq(x,2), add=T, col='green', lwd=3)
curve(dchisq(x,3), add=T, col='blue', lwd=3)
curve(dchisq(x,5), add=T, col='orange', lwd=3)
abline(h=0,lty=3)
abline(v=0,lty=3)
legend(par('usr')[2], par('usr')[4], xjust=1,
       c('df=1', 'df=2', 'df=3', 'df=5'),
       lwd=3,
       lty=1,
       col=c('red', 'green', 'blue', 'orange')
)
title(main='Chi^2 Distributions')
