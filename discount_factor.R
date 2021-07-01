D <- function(r,t,tf,ccr=TRUE){
  if(ccr){
    return(exp(-r*(tf-t)))
  }
  return((1+r)^(-(tf-t)))
}
