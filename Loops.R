#import data
setwd("C:/Users/Utente/OneDrive/Desktop/LM")
data<-read.table("babies.data",header =T) 
str(data)

#1
print(sum(data$smoke==9))
print(sum(data$gestation==999))
print(sum(data$age==99))
print(sum(data$height==99))
print(sum(data$weight==999))
print(sum(data$bwt==999))
print(sum(data$parity==9))

data=data[data$smoke!=9,]
data=data[data$gestation!=999,]
data=data[data$age!=99,]
data=data[data$height!=99,]
data=data[data$weight!=999,]
data=data[data$bwt!=999,]
data=data[data$parity!=9,]

dim(data)

#2
class(data$smoke)
data$smoke<-as.factor((data$smoke))
class(data$smoke)

#summary
summary(data)
by(data,data$smoke, summary)
#0=nonsmokers, 1=smokers

#3
t.test(bwt~smoke,data)

#4
breaks<-quantile(data$gestation, probs = seq(0,1,0.1))
classi<-cut(data$gestation, breaks)
tabella<-table(classi, data$smoke)
totali<-t(matrix(colSums(tabella),2,10))
tabella<-tabella/totali
colnames(tabella)<-c("nonsmokers","smoker")
barplot(t(tabella),besides=TRUE, legend=colnames(tabella),col=c(5,8),ylim = c(0,0.2))

#5
data$classi.gest<-classi
tabella2<-tapply(data$bwt,list(data$classi.gest,data$smoke), mean)
barplot(t(tabella2),beside=TRUE, legend=colnames(tabella2),col=c(5,8),ylim=c(0,160))

#6
data.smoke<-data[data$smoke==1,]
dim(data.smoke)
data.nonsmoke<-data[data$smoke==0,]
dim(data.nonsmoke)
summary(lm(bwt~gestation, data=data.smoke))
summary(lm(bwt~gestation, data=data.nonsmoke))

#7
data$parity=as.factor(data$parity)
t.test(data$bwt~data$parity)

#8
t.test(data$gestation~data$smoke)

#9
t.test(data$gestation~data$parity)

#10
plot(data$gestation,data$bwt)
summary(lm(bwt~gestation,data=data))



#if else statement
x<-5
y<-7
if(x>y){
  print("x is greater")
}else{
  print("y is greater")
}

#nested if else statement
x<-5
y<-5
if(x>y){
  print("x is greater")
}else if(x<y){
  print("y is greater")
}else{
  print("x and y are equal")
}

#R loop
#for
a<-0
for (i in 1:10) a<-a+i

#repeat
sum<-1
repeat{
  sum<-sum+2
  print(sum)
  if(sum>11)
    break
}

#while
x<-0
while(x<10)
{
  x<-x+4
  print(x)
}  

#build function in R
#function(rglist){}
sum.1<-fuction(a,b) a+b
sum.1(2,3)
sum.1(22,-3)
sum.2<-function(a,b){
  if((a>0)&(b>0))
    a+b
  else
    -1
}
sum.2(-2,3)
