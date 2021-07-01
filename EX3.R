library(quantmod)

getSymbols('^VIX', from="2019-01-02", to="2020-12-30")
tail(VIX)
price_xts <- na.omit(VIX[, "VIX.Close"]) # Prices
P <- as.numeric(price_xts)
S <- log(P) # Log prices
X <- diff(S) # Log returns: log((P_t-P_{t-1})/P_{t-1})

days_year <- 365
t <- as.numeric(index(price_xts))
Deltat <- diff(t)/days_year


### MLE ESTIMATION ###

## Likelihood based on log-returns ##
# minus log-likelihood function based on transition density of log-returns
mLL_ret<- function(par, S, logret, Deltat){
  n <- length(S)-1
  lagS <- S[1:n]
  alpha <- par[1]
  mu <- par[2]
  sig <- par[3]
  
  vecMean <- (mu-lagS)*(1-exp(-alpha*Deltat))
  vecVar <- sig^2/(2*alpha)*(1-exp(-2*alpha*Deltat))
  
  return(-sum(dnorm(logret,mean=vecMean,sd=sqrt(vecVar),log=TRUE)))
}

mLL_ret(par=c(1,1,1),logret=X,S=S, Deltat=Deltat) # -472.6043

# Nelder-Mead optimization
mle_ret.fit.NM <- optim(par=c(1,1,1),fn = mLL_ret, method ="Nelder-Mead", 
                 logret=X,
                 S=S,
                 Deltat=Deltat,
                 control=list('maxit'=1000))

mle_ret.fit.NM

mle_ret.fit.BFGS <- optim(par=c(1,1,1),fn = mLL_ret, method ="L-BFGS-B", 
                      logret=X,
                      S=S,
                      Deltat=Deltat,
                      lower=c(0,-Inf,0),
                      control=list('maxit'=1000),
                      hessian=T)

mle_ret.fit.BFGS


## Likelihood based on log-prices ##
# minus log-likelihood function based on transition density of log-prices
mLL_pr<- function(par, S, Deltat){
  n <- length(S)-1
  lagS <- S[1:n]
  newS <- S[2:n+1]
  alpha <- par[1]
  mu <- par[2]
  sig <- par[3]
  
  vecMean <- (lagS-mu)*exp(-alpha*Deltat)+mu
  vecVar <- sig^2/(2*alpha)*(1-exp(-2*alpha*Deltat))
  
  return(-sum(dnorm(newS,mean=vecMean,sd=sqrt(vecVar),log=TRUE)))
}

mLL_pr(par=c(1,1,1),S=S, Deltat=Deltat) # -69.991

# Nelder-Mead optimization
mle_pr.fit.NM <- optim(par=c(1,1,1),fn = mLL_pr, method ="Nelder-Mead", 
                    S=S,
                    Deltat=Deltat,
                    control=list('maxit'=1000))

mle_pr.fit.NM

mle_pr.fit.BFGS <- optim(par=c(1,1,1),fn = mLL_pr, method ="L-BFGS-B",
                      S=S,
                      Deltat=Deltat,
                      lower=c(0,-Inf,0),
                      control=list('maxit'=1000),
                      hessian=T)

mle_pr.fit.BFGS



### QUASI MAXIMUM LIKELIHOOD ESTIMATION ###

# minus quasi-log-likelihood function
mQLL<- function(par, S, logret, Deltat){
  n <- length(S)-1
  lagS <- S[1:n]
  alpha <- par[1]
  mu <- par[2]
  sig <- par[3]
  
  vecMean <- alpha*(mu-lagS)*Deltat
  vecVar <- sig^2*Deltat
  
  return(-sum(dnorm(logret,mean=vecMean,sd=sqrt(vecVar),log=TRUE)))
}


qmle.fit <- optim(par=c(1,1,1),fn = mQLL, method ="L-BFGS-B", 
                 logret=X,
                 S=S,
                 Deltat=Deltat,
                 lower=c(0,-Inf,0),
                 control=list('maxit'=1000))

qmle.fit

