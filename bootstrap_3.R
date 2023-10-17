# sample

x=c(94,197,16,38,99,141,23)
plot(ecdf(x),lwd=3)
curve(pnorm(x,mean=mean(x),sd=sd(x)),add=TRUE,lwd=3,lty=2,col=2)


### Non parametric analysis
library(boot)

media.f.np<-function(data,i){
  mean(data[i])
}

bootstrap.np<-boot(x,media.f.np,R=5000)
plot(bootstrap.np)

# Comparison between nonparametric bootstrap and CLT
media=mean(x)
varianza=var(x)
plot(density(bootstrap.np$t),main="non parametric analysis",lwd=2,xlab=" ",ylim=c(0,0.02))
curve(dnorm(x,mean=media,sd=sqrt(varianza/7)),add=TRUE,col=2,lwd=2,lty=2)
legend(110,0.020,legend=c("bootstrap,B=5000","CLT"),
       lwd=2,lty=c(1:2),col=c(1:2))


###### parametric solutions

plot(ecdf(x),lwd=3)
curve(pgamma(x,shape=1,scale=media),add=TRUE,lwd=3,lty=2,col=3)
curve(pnorm(x,mean=mean(x),sd=sd(x)),add=TRUE,lwd=3,lty=2,col=2)



## Parametric Analysis
media.f.p<-function(data){
  mean(data)
}

rg <- function(data, mle){
  out <- rgamma(length(data), shape=1,scale=mle)
  out
}

bootstrap.p <- boot(x, media.f.p, R=5000, sim="parametric",
                    ran.gen=rg, mle=mean(x))

## Comparison among parametric bootstrap (esponential), analytical solution and CLT

plot(density(bootstrap.p$t),main="parametric analysis",lwd=2,xlab=" ",ylim=c(0,0.016))
curve(dnorm(x,mean=media,sd=sqrt(media^2/7)),add=TRUE, col=2,lwd=2,lty=2)
curve(dgamma(x,shape=7,scale=media/7),add=TRUE, col=4,lwd=2,lty=3)
legend(150,0.016,legend=c("bootstrap,B=5000","exact","CLT"),
       lwd=2,lty=c(1,3,2),col=c(1,4,2))


## Comparison between parametric bootstrap (esponential) and nonparametric bootstrap 


plot(density(bootstrap.p$t),main="comparison bootstrap,B=5000",lwd=2,
     xlab=" ",ylim=c(0,0.017))
lines(density(bootstrap.np$t),col=2,lwd=2,lty=2)

legend(130,0.016,legend=c("parametric - esponential","nonparametric"),
       lwd=2,lty=c(1,2),col=c(1,2))


########### parametric bootstrap normal


media.f.p<-function(data){
  mean(data)
}

rg <- function(data, theta){
  media=theta[1]
  varianza=theta[2]
  out <- rnorm(length(data), mean=media,sd=sqrt(varianza))
  out
}

theta=c(mean(x),var(x))
bootstrap.p.norm <- boot(x, media.f.p, R=50000, sim="parametric",
                         ran.gen=rg, mle=theta)


## comparison


plot(density(bootstrap.np$t),main="comparison,B=5000",lwd=2,
     xlab=" ",ylim=c(0,0.017))
lines(density(bootstrap.p.norm$t),col=2,lwd=2,lty=2)
lines(density(bootstrap.p$t),col=3,lwd=2,lty=3)

legend(130,0.016,legend=c("boot np","boot p norm","boot p exp"),
       lwd=2,lty=c(1,2,3),col=c(1,2,3))



