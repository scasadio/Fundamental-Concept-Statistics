

#EXERCISE ON THE DATASET BABIES.DATA

#import data
dati<-read.table("babies.data",header=TRUE)#we specify the argument header =TRUE because the dataset includes columns names
str(dati)#str gives the structure of the data


#1
print(sum(dati$smoke==9))#print the number of null values for the variable smoke
print(sum(dati$gestation==999))
print(sum(dati$age==99))
print(sum(dati$height==99))
print(sum(dati$weight==999))
print(sum(dati$bwt==999))
print(sum(dati$parity==9))


dati=dati[dati$smoke!=9,]#remove the null values for the variable smoke
dati=dati[dati$gestation!=999,]
dati=dati[dati$age!=99,]
dati=dati[dati$height!=99,]
dati=dati[dati$weight!=999,]
dati=dati[dati$bwt!=999,]
dati=dati[dati$parity!=9,]


dim(dati)#dimension of the data (number of rows and columns)


#2

class(dati$smoke)#class gives the type (numeric, factor, etc.) of the variable smoke
dati$smoke<-as.factor(dati$smoke)#as.factor transforms the variable smoke into a factor variable
class(dati$smoke)

#summary
summary(dati)#summary gives the descriptive statistics for the variables in the dataset
?by
by(dati,dati$smoke,summary)#by computes the  descriptive statistics separated for the two groups of the factor variable smoke


#3

?t.test
t.test(bwt~smoke,dati)#t test 
#the null hypothesis is that the mean of the weights of the babies
#is the same in the groups of smokers and non-smokers mothers

#we refuse the null hypothesis because we get a pvalue <0.05

#4
?quantile
breaks<-quantile(dati$gestation,probs=seq(0,1,0.1))#produces sample quantiles for the variable gestation
?cut
classi<-cut(dati$gestation,breaks)#assign each value of gestation to the class identified by the values of the quantiles
tabella<-table(classi,dati$smoke)#frequency tables of the classes of gestation into the two groups of smokers and non-smokers
totali<-t(matrix(colSums(tabella),2,10))
tabella<-tabella/totali#relative frequency table (we divide each value in tabella by the total of smokers and non-smokers)
colnames(tabella)<-c("nonsmokers","smoker")#colnames gives a name to the columns of tabella

#plot the results in tabella
barplot(t(tabella),beside=TRUE,legend=colnames(tabella),col=c(5,8),ylim=c(0,0.2))

#5
#we create a new variable in the dataset that includes the classes of gestation
dati$classi.gest<-classi
?tapply
#in tabella 2 we compute the mean of the weights of the babies in the groups identified by the factor variables smoke and classi.gest
tabella2<-tapply(dati$bwt,list(dati$classi.gest,dati$smoke),mean)
barplot(t(tabella2),beside=TRUE,legend=colnames(tabella2),col=c(5,8),ylim=c(0,160))


#6
#create a dataset that includes only the group of smokers
dati.smoke<-dati[dati$smoke==1,]
dim(dati.smoke)

#create a dataset that includes only the group of nonsmokers
dati.nonsmoke<-dati[dati$smoke==0,]
dim(dati.nonsmoke)

?lm#compute a linear model
summary(lm(bwt~gestation,data=dati.smoke))#linear model where weights of the babies is the dependent variable and gestation is the regressor, only in the group of smokers
summary(lm(bwt~gestation,data=dati.nonsmoke))
#from the results the relation among the weights of the babies and the variable gestation
#is linear both in the group of smokers and non smokers


#7
dati$parity=as.factor(dati$parity)
t.test(dati$bwt~dati$parity)#t test

#8
t.test(dati$gestation~dati$smoke)

#9
t.test(dati$gestation~dati$parity)
#10

plot(dati$gestation,dati$bwt)
summary(lm(bwt~gestation,data=dati))
#11

plot(dati$age,dati$bwt)#the plot helps to see if the relation among bwt and age is linear (in this case is not linear)
summary(lm(bwt~age,data=dati))
plot(dati$weight,dati$bwt)
summary(lm(bwt~weight,data=dati))

#12
#we compute a linear model with more than 1 regressor
summary(lm(dati$bwt~dati$weight+dati$height+dati$age+dati$smoke+dati$gestation+dati$parity))

#since the variables are not on the same scale we standardize the numeric variables in the dataset
#the function scale standardize the numeric variables

dati$bwt<-scale(dati$bwt)
dati$weight<-scale(dati$weight)
dati$age<-scale(dati$age)
dati$height<-scale(dati$height)
dati$gestation<-scale(dati$gestation)

#we compute the linear model with the standardized variables
summary(lm(dati$bwt~dati$weight+dati$height+dati$age+dati$smoke+dati$gestation+dati$parity))
#the variable smoke has the largest effect on the weights of babies
