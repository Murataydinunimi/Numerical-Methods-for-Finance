errorBS <- function(sigma, S, K, maturity, r, t=0, obsPrice, call=TRUE, type="sq"){ 
  d1 <- (log(S/K)+(r+0.5*sigma^2)*(maturity-t))/(sigma*sqrt(maturity-t)) 
  d2 <- d1-sigma*sqrt(maturity-t) 
  theoPrice <- ifelse(call,
                      S*pnorm(d1)-K*exp(-r*(maturity-t))*pnorm(d2),
                      K*exp(-r*(maturity-t))*pnorm(-d2)-S*pnorm(-d1)
  )
  difference <- theoPrice - obsPrice 
  if(type=="sq"){
    return(difference^2)
  } else if(type=="abs"){
    return(abs(difference))
  } else if(type=="rsq"){
    return((difference/obsPrice)^2)
  }
}
