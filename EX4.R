library(quantmod)
SPX <- getSymbols("^GSPC",auto.assign = FALSE, from = "2019-01-02", to="2020-12-31")

head(SPX[,4])
# we work with the closed price
SPXprice<-na.omit(SPX[,4])
# we want to work on yearly basis

daysInOneYear<-365 #actual convention

obstime<-as.numeric(index(SPXprice))# we can convert it to the numeric object and determine delta.
n <- length(obstime) - 1

delta_i<-diff(obstime)/(daysInOneYear)

logreturn<- diff(log(as.numeric(SPXprice)))



minusLogLik<-function(par,logreturn,delta_i){
  mu<-par[1]
  sig<-par[2]
  vecMean = (mu -0.5*sig^2)*delta_i
  vecsd = sig*sqrt(delta_i)
  -sum(dnorm(logreturn,mean=vecMean,sd=vecsd,log=TRUE))
}

minusLogLik(par=c(0.5,0.2),logreturn=logreturn,delta_i=delta_i) # minus loglikelihood at mu= 0.5 sig=0.2

res<-optim(par=c(0.5,0.2),fn=minusLogLik,lower=c(-Inf,0),method="L-BFGS-B", logreturn=logreturn, delta_i=delta_i)
res$par 
#we want to see the value of the likelihood we are able to reach so;

-1*res$value #multiply it by -1 because it is a minimization : minusloglikelihood

res$convergence # its 0, successful.

volatility<-res$par[2] # volatiltiy on yearly basis estimated using the historical log return.
volatility


#Put option of Black and scholes through put-call parity

PutoptBs<-function(S,K,TimeToMat, sigma, Rfree){
  d1<-(log(S/K)+(Rfree+0.5*sigma^2)*(TimeToMat))/(sigma*sqrt(TimeToMat))
  d2<-d1-sigma*sqrt(TimeToMat)
  Pt0= K*exp(-Rfree*TimeToMat)*pnorm(-d2)-S*pnorm(-d1)
       
  return(Pt0)
  
}
sigma= volatility # on yearly basis estimated through MLE
#At the money option, starting on 30th of December 2020 and reaches the maturity on 27th of February 2021.

SPXprice[n+1,] # t0 = 2020-12-30
S = as.numeric(SPXprice[n+1,]) 
K=S # at the money
K
S
Rfree=0.015
NdaystoMat <- data.frame(date=c("2020/12/30"),tx_start=c("2021/02/27"))
NdaystoMat$date_diff<-as.Date(as.character(NdaystoMat$tx_start), format="%Y/%m/%d")-
  as.Date(as.character(NdaystoMat$date), format="%Y/%m/%d")

Maturity<-as.numeric(NdaystoMat$date_diff)
Maturity # daily basis
#covert Maturity on yearly basis
daysInOneYear<-365 #actual convention

Maturity<-Maturity/daysInOneYear
Put<-PutoptBs(S=S,K=S, TimeToMat = Maturity,sigma = sigma,Rfree = Rfree)
Put

