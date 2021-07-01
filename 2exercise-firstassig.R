pricing_unibinom_put <- function(S0,K,r,u=1.25,d=0.8,t0=0,maturity=1){
  disc_factor <- ((1+r)^(maturity-t0))^(-1)
  
  qu <- (disc_factor^(-1)-d)/(u-d)
  qd <- 1 - qu
  
  if(disc_factor^(-1) >= u || disc_factor^(-1) <= d){
    print("Warning: there is an arbitrage opportunity!")
  }
  
  phi0 <- disc_factor*(qu*max(K-S0*u,0)+qd*max(K-S0*d,0))
  
  y <- (1/S0)*(max(K-S0*u,0) - max(K-S0*d,0))/(u-d)
  
  return(list("Price at t0"=phi0, "Q=(qu,qd)"=c(qu,qd), "y*"=y))
}



putcall<-function(S0,K,r,t0=0,maturity=1){
  timetomat=maturity-t0
  disc_factor <- ((1+r)^(timetomat))^(-1)
  x = pricing_unibinom_put(S0,K,r,t0=0,maturity=1)
  pt = as.numeric(x[1])
  ct = S0+pt-K*disc_factor
  
  
  
  if (ct >= max(c(S-K*disc_factor,0)) & ct<=S0){
    print("Merton constraint is satisfied")
  }else{
    print("Arbitrage opputurnity!")
  }
    
  return(list("call-price"=ct,"put-price"=pt))
}


