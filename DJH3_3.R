path<-"C:/Users/MONFORH/Onedrive - National Bank of Belgium/Documents/Training/Books/Actuarial/De Jong, Heller - Generalized linear models for insurance data/car.csv"

mydata<-read.csv(path)

attach(mydata)

loglik.gamma<-function(v,y){
n<-length(y)
mu<-mean(y)
-(-n*lgamma(v)+v*sum(log(y))+n*v*(log(v/mu)-1))
}

v<-optim(1,loglik.gamma,lower=0.01,y=claimcst0[claimcst0>0])$par 
#the $par at the end of the instruction means that v will only store
#the value of the parameter and not other values (such as the value
#of the function etc.)
v
mean(claimcst0[claimcst0>0])

gamplot<-function(y,max=15000,inc=200)

{
  
carhist<-hist(y,breaks = seq(0,max(y+inc),inc),include.lowest = T, freq=F, plot = F)

plot(1,0,xlim=c(0,max),ylim=c(0,max(carhist$density)), type="n", ylab="f(y)",xlab="Claim size")

for(i in 1:(length(carhist$breaks)-1))
  if (carhist$breaks[i+1]<max)
    rect(carhist$breaks[i],0,carhist$breaks[i+1],
         carhist$density[i], border=F,col="gray")

v<-optim(1,loglik.gamma,lower=0.01,y=y)$par
mu<-mean(y)

x<-seq(0.01,max,inc/10)
f<-(1/(x*gamma(v)))*((x*v/mu)^v)*exp(-x*v/mu)
lines(x,f)

}

gamplot(y=claimcst0[claimcst0>0],max=15000,inc=200)

detach(mydata)
