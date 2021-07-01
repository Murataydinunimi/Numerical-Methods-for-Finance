merton_constraints <- function(price, St, K, t, maturity, r, call=TRUE, ccr=TRUE){
  source("discount_factor.R")
  if(call){
    return(price <= St && price >= pmax(St-K*D(r,t,maturity,ccr), 0))
  }
  return(price <= K*D(r,t,maturity,ccr) && price >= pmax(K*D(r,t,maturity,ccr)-St, 0))
}
