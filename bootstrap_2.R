# sample

x=c(94,197,16,38,99,141,23)

# empirical distribution function

plot(ecdf(x))

# we create the samples in the bootstrap world by the e.d.f.


### 1 approach - all the possible

# vector with the positions

unit<-1:length(x)
campioni<-expand.grid(unit,unit,unit,unit,unit,unit,unit)
campioni<-as.matrix(campioni)
M=dim(campioni)[1]

# vector in which we put the computed bootstrap values

media.M<-rep(0,M)
for (i in 1:M) media.M[i]<-mean(x[campioni[i,]])

# sample estimate
stima.media<-mean(x)  ### this is the estimator in the real world

# expected value in the bootstrap world
EB.media<-mean(media.M)


TF.media<-mean(x)  ## parameter in the boostrap world

# estimate of distorsion, variance and mean square error in the boostrap world

bias.media<-EB.media-TF.media
varianza.media<-1/M*sum((media.M-EB.media)^2)
MSE.media<-1/M*sum((media.M-TF.media)^2)



### 2 approach - MONTECARLO

B=10000
media.B<-rep(0,B)

for (i in 1:B) media.B[i]<-mean(x[sample(1:7,7,replace=TRUE)])


mean(media.B)
mean(media.M)
mean(x)


# sample estimate
stima.media<-mean(x)

# parameter to estimate
TF.media<-mean(x)

# expected value in the bootstrap world
EB.media<-mean(media.B)


# estimate of distorsion, variance and mean square error in the boostrap world

bias.media2<-EB.media-TF.media
varianza.media2<-1/B*sum((media.B-EB.media)^2)
MSE.media2<-1/B*sum((media.B-TF.media)^2)



############################# MEDIAN ######################
## here we want to investivate the median estimator (s(x)) for the median of the population (TF)


### 1 approach - all the possible
# vector with the positions

unit<-1:length(x)
campioni<-expand.grid(unit,unit,unit,unit,unit,unit,unit)
campioni<-as.matrix(campioni)
M=dim(campioni)[1]


mediana.M<-rep(0,M)
for (i in 1:M) mediana.M[i]<-median(x[campioni[i,]])

# stima campionaria
stima.mediana<-median(x)

# TF
TF.mediana<-median(x)

# expected value in the bootstrap world

EB.mediana<-mean(mediana.M)


# estimate of distorsion, variance and mean square error in the boostrap world

bias.mediana<-EB.mediana-TF.mediana
varianza.mediana<-1/M*sum((mediana.M-EB.mediana)^2)
MSE.mediana<-1/M*sum((mediana.M-TF.mediana)^2)


bias.mediana
varianza.mediana

