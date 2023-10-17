library(boot)
library(bootstrap)

#mouse.c
mouse.c
mouse.t

#create dataset
survival<-c(mouse.c,mouse.t)
group<-factor(c(rep("c",9),rep("T",7)))
dati.mouse<-data.frame(survival,group)

#bootstrap hypothesis testing
#case A:unique sample

statistica.test<-function(data){
  media.T<-mean(data$survival[-(1:9)])
  media.C<-mean(data$survival[1:9])
  var.T<-var(data$survival[-(1:9)])
  var.C<-var(data$survival[1:9])
  
  (media.C-media.T)/sqrt(var.T/7+var.C/9)
}


theta1<-function(data,i){
  d<-data[i,]
  statistica.test(d)
}

diff.mean.mouse.bootstrap.A<-boot(dati.mouse,theta1, R=10000)

rep.boot.A<-diff.mean.mouse.bootstrap.A$t #value of the test statistic
plot(density(rep.boot.A))

#we can compute the value of the statistic in our observed sample
t.oss<-statistica.test(dati.mouse)
abline(v=t.oss,col=2)
p.value.boot.A<-sum(abs(rep.boot.A)>=abs(t.oss))/n

#case B:ricentering and stratification
# it is necessary to define again the funtion of the
# statistics of interest (by using the groups)
# case different variances

statistica.test<-function(data){
  n.T<-sum(data$group=="T")
  n.C<-sum(data$group=="C")
  media.T<-mean(data$survival[data$group=="T"])
  media.C<-mean(data$survival[data$group=="C"])
  var.T<-var(data$survival[data$group=="T"])
  var.C<-var(data$survival[data$group=="C"])
  (media.C-media.T)/sqrt(var.T/n.T+var.c/n.C)
}
theta1<-function(data,i)
{
  d<-data[i,]
  statistica.test(d)
}  
#recenter data
survival<-c(mouse.c-mean(mouse.c)+mean(dati.mouse$survival),mouse.t-mean(mouse.t)+mean(dati.mouse$survival))
