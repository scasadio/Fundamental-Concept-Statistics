####################################################################
#
# INSTALL THE PACKAGES BOOT AND BOOTSTRAP
#
####################################################################
 
library(boot)
library(bootstrap)


##############################################
#
# part 1: comparison between the mean of 2 groups
#
##############################################
 
# data of the example

?mouse.c
?mouse.t


# create the dataframe
survival<-c(mouse.c,mouse.t)
group<-factor(c(rep("C",9),rep("T",7)))

dati.mouse<-data.frame(survival,group)

# CASE A: unique sample
# creation of the function for the nonparametric bootstrap
#bootstrap hypothesis testing
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

set.seed(100)
diff.mean.mouse.bootstrap.A<-boot(dati.mouse,theta1,R=10000,sim="ordinary")

#we can compute the value of the statistic in our observed sample
t.oss<-statistica.test(dati.mouse)

rep.boot.A<-diff.mean.mouse.bootstrap.A$t  #value of test statistic

### grafico
plot(density(rep.boot.A))
abline(v=t.oss,col=2,lty=2)
n=length(rep.boot.A)
p.value.boot.A<-(sum(abs(rep.boot.A)>=abs(t.oss)))/n


# CASE B: ricentering and stratification
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

	(media.C-media.T)/sqrt(var.T/n.T+var.C/n.C)

	# or equivalently:
	# t.test(survival~group,data=dati.mouse)$statistic
	
}

theta1<-function(data,i){
	d<-data[i,]
	statistica.test(d)

}

# testing hypothesis
survival<-c(mouse.c-mean(mouse.c)+mean(dati.mouse$survival),mouse.t-mean(mouse.t)+mean(dati.mouse$survival))

mean(survival)
mean(dati.mouse$survival)
#new vector and old vector survival have the same mean
#the different is that 
mean(survival[1:9])
mean(survival[10:16])
#data now are centered, when we will do the non parametric bootstrap
#we will simulated our data from the null hypothesis

#create a new dataset that we will use in non parametric boot
dati.mouse.H0<-data.frame(survival,group)

#compute balanced bootstrap
set.seed(100)
diff.mean.mouse.bootstrap.B<-boot(dati.mouse.H0,theta1,R=10000,sim="ordinary",strata=dati.mouse.H0$group)

# distribution of the test statistics under H0

plot(diff.mean.mouse.bootstrap.B)
rep.boot.B<-diff.mean.mouse.bootstrap.B$t

### grafical representation
plot(density(rep.boot.B))
abline(v=t.oss,col=2,lty=2)
n=length(rep.boot.B)
p.value.boot.B<-(sum(abs(rep.boot.B)>=abs(t.oss)))/n


# comparison with a t of Student with n1+n2-2 d.o.f.

qqplot(rt(length(rep.boot.B),df=14),rep.boot.B)
abline(a=0,b=1,lty=2)
2*(1-pt(abs(t.oss),df=14)) #two tailed test



# comparison:

p.value.boot.A
p.value.boot.B

