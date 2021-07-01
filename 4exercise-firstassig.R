price_asian_binomial <- function(S0, u, d, r, t0, maturity){
  disc_factor <- ((1+r)^(maturity-t0))^(-1)
  qu <- (disc_factor^(-1)-d)/(u-d)
  qd <- 1 - qu
  
  Auu <- 1/3*S0*max((2*u+1)*(u-1), 0)
  Aud <- 1/3*S0*max(2*u*d-u-1, 0)
  Adu <- 1/3*S0*max(2*u*d-d-1, 0)
  Add <- 1/3*S0*max((2*d+1)*(d-1), 0)
  
  A0 <- disc_factor*(qu^2*Auu + qu*qd*Aud + qu*qd*Adu + qd^2*Add)
  
  return(A0)
}

price_asian_binomial(S0=1,u=2,d=0.5,r=0,t0=0,maturity=1)
