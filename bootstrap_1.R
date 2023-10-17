## Usando il bootstrap calcola il bias 
## di 4 diversi stimatori per la varianza di una popolazione:
## 1) varianza del campione 
## 2) varianza campionaria rispetto alla mediana
##3) il quadrato della media delle differenze assolute dalla media
## 4) il quadrato della media delle differenze assolute dalla mediana 
## in un campione di 40 unità statistiche 

set.seme(1)
x=normale(40,5,1)
n=40

## I 4 stimatori

var1=funzione(x) {return(media((x-media(x))^2))}
var2=funzione(x) {return(media((x-mediana(x))^2))}
var3=funzione(x) {return((media(abs(x-media(x))))^2)}
var4=funzione(x) {return((media(abs(x-mediana(x))))^2)}

var1(x)
var2(x)
var3(x)
var4(x)


### STIMA DEL BIAS CON BOOTSTRAP NON PARAMETRICO

## usiamo i 4 stimatori per stimare la varianza della popolazione
M=40^40

B=1000000
var.boot.1=var.boot.2=var.boot.3=var.boot.4=numerico(B) #inizializzazione

for (i in 1:B) {indice=campione(1:n,n,sostituisci=VERO)
var.boot.1[i]<-var1(x[indice])
var.boot.2[i]<-var2(x[indice])
var.boot.3[i]<-var3(x[indice])
var.boot.4[i]<-var4(x[indice])
}

hist(var.boot.1,50)
par(mfrow=c(2,2))
plot(density(var.boot.1))
plot(densità(var.boot.2))
plot(density(var.boot.3))
plot(densità(var.boot.4))

## prima stima della varianza secondo i quattro stimatori (il vero è 1)
mean(var.boot.1)
mean(var.boot.2)
mean(var.boot.3)
mean(var.boot.4)

TF.theta=mean((x-mean(x))^2) ## formula per la varianza
Bias.boot.1=mean(var.boot.1)-TF.theta
## bias è noto ed è -1/n*var(x) 
-1/n*1

Bias.boot.2=mean(var.boot.2)-TF.theta
Bias.boot.3=mean(var.boot.3)-TF.theta
Bias.boot.4=mean(var.boot.4)-TF.theta

Bias.boot.1
Bias.boot.2
Bias.boot.3
Bias.boot.4



### migliora la stima bootstrap correggendo il bias
var1(x)-Bias.boot.1
var2(x)-Bias.boot.2
var3(x)-Bias.boot.3
var4(x)-Bias.boot.4