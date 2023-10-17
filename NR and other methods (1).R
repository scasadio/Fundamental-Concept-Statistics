
dati=read.table("oilspills.dat",header=TRUE)
y=dati[,2]
x=dati[,3]

### maximum likelihood estimation

theta1=sum(y)/sum(x)


## newton-raphson

log.L=function(theta1,y,x)
{a=sum(y*log(theta1*x))
 b=theta1*sum(x)
 c=sum(log(factorial(y)))
 return(a-b+c)
}

theta=seq(0,15,length.out=1000)
lik=0
for (i in 1:1000) lik[i]=log.L(theta[i],y,x)
plot(theta,lik,type="l",xlim=c(0,15))


S=function(theta1,y,x) {return(sum(y)/theta1-sum(x))}
H=function(theta1,y) {return(-sum(y)/theta1^2)}


newton.raphson=function(theta.0=4,y,x)
{
  delta=100
  epsilon=0.0001 ## tolerance level
  it=0
  
  theta.all=theta.0 
  theta=theta.0
  
  while (delta>epsilon) {
    
    it=it+1
    theta=theta-S(theta,y,x)/H(theta,y)
    theta.all=c(theta.all,theta)
    delta=abs(S(theta,y,x))
    print(paste("it=",it," theta=",theta))
  }
  
  return(invisible(list(theta=theta.all,it=it)))
}   


## multivariate case

y=dati[,2]
x=dati[,3]
z=dati[,4]

logL=function(y,x,z,theta)
{theta1=theta[1]
theta2=theta[2]
lambda=theta1*x+theta2*z
lik=sum(y*log(lambda)-lambda+log(factorial(y)))
return(lik)
}

S.theta=function(y,x,z,theta)
{
  theta1=theta[1]
  theta2=theta[2]
  lambda=theta1*x+theta2*z
  out=c(0,0)
  out[1]=sum(y*x/lambda)-sum(x)
  out[2]=sum(y*z/lambda)-sum(z)
  return(out)  
}


H.theta=function(y,x,z,theta)
{ 
  theta1=theta[1]
  theta2=theta[2]
  lambda=theta1*x+theta2*z
  out=matrix(0,2,2)
  out[1,1]=-sum(y*x^2/lambda^2)
  out[1,2]=out[2,1]=-sum(y*z*x/lambda^2)
  out[2,2]=-sum(y*z^2/lambda^2)
  return(out)  
}




#### Gradient descent algorithm

theta.init=c(1,1)
tol=10^-5
alpha=0.1


gd.alg=function(theta.init,y,x,z,alpha,tol=10^-5)
{
  it=1
  p=length(theta.init)
  theta.it=matrix(theta.init,1,p)
  l.it=logL(y,x,z,theta.init)
  print(c(it,l.it[it],theta))
  delta=100
  
while ( delta>tol )  {
  
  S.it=S.theta(y,x,z,theta.it[it,])
  theta = theta.it[it,]+alpha*S.it
  theta.it=rbind(theta.it,theta)
  l.it=c(l.it,logL(y,x,z,theta))
  delta=abs((l.it[it+1]-l.it[it])/l.it[it])
  it=it+1
  print(c(it,l.it[it],theta))
}
out=list(likelihood=l.it,theta=theta.it)
return(invisible(out))
}




nr.alg=function(theta.init,y,x,z,tol=10^-5)
{
  it=1
  p=length(theta.init)
  theta.it=matrix(theta.init,1,p)
  l.it=logL(y,x,z,theta.init)
  print(c(it,l.it[it],theta))
  delta=100

while (delta>tol)  {
  
  S.it=S.theta(y,x,z,theta.it[it,])
  H.it=H.theta(y,x,z,theta.it[it,])
  
  theta = c(theta.it[it,] - solve(H.it)%*%S.it)
  theta.it=rbind(theta.it,theta)
  l.it=c(l.it,logL(y,x,z,theta))
  delta=abs((l.it[it+1]-l.it[it])/l.it[it])
  it=it+1
  print(c(it,l.it[it],theta))
}
out=list(likelihood=l.it,theta=theta.it)
return(invisible(out))
}

I.theta=function(y,x,z,theta)
{ 
  theta1=theta[1]
  theta2=theta[2]
  lambda=theta1*x+theta2*z
  out=matrix(0,2,2)
  out[1,1]=sum(x^2/lambda)
  out[1,2]=out[2,1]=sum(z*x/lambda)
  out[2,2]=sum(z^2/lambda)
  return(out)  
}


alpha=1
theta.init=c(1,1)

fs.alg=function(theta.init,y,x,z,alpha,tol=10^-5)
{
  it=1
  p=length(theta.init)
  theta.it=matrix(theta.init,1,p)
  l.it=logL(y,x,z,theta.init)
  print(c(it,l.it[it],theta))
  delta=100

while (delta>tol)  {
  
  S.it=S.theta(y,x,z,theta.it[it,])
  I.it=I.theta(y,x,z,theta.it[it,])
  
  theta = c(theta.it[it,] + alpha*solve(I.it)%*%S.it)
  theta.it=rbind(theta.it,theta)
  l.it=c(l.it,logL(y,x,z,theta))
  delta=abs((l.it[it+1]-l.it[it])/l.it[it])
  it=it+1
  print(c(it,l.it[it],theta))
}
  
out=list(likelihood=l.it,theta=theta.it)
return(invisible(out))
}

output=fs.alg(theta.init,y,x,z,alpha)

## standard errors
final.theta=output$theta[nrow(output$theta),]
I.theta(y,x,z,final.theta)
diag(solve(I.theta(y,x,z,final.theta)))^0.5


optim(c(1,1), fn=logL,method="BFGS",control=list(fnscale=-1) ,y=y,x=x,z=z)






















