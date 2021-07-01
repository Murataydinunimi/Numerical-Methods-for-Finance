source("errorBS.R")
calibration_distance <- function(sigma, S, K, maturity, r, t=0, obsPrice, call=TRUE, meth="MSE"){
  if(meth=="MSE"){
    return(mean(errorBS(sigma, S, K, maturity, r, t, obsPrice, call, type="sq")))
  } else if(meth=="AME"){
    return(mean(errorBS(sigma, S, K, maturity, r, t, obsPrice, call, type="abs")))
  } else if(meth=="RMSE"){
    return(mean(errorBS(sigma, S, K, maturity, r, t, obsPrice, call, type="rsq")))
  }
}