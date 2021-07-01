disc <- function(r,t,tf){
  return(exp(-r*(tf-t)))
}


multi_binom <- function(N,K,S0,r,t0=0,maturity,u,d,European=TRUE){
  
  ## Triangular matrices whose rows j are scenarios and
  ## columns t are time points.
  
  # j is the column indexer. Columns range from 1 to N+1. 
  # 
  # Please, note that the time corresponding to index j is t0+(j-1)*Delta
  #
  # i is the row indexer. 
  # An element in row i and column j is associated with a scenario of
  # (i-1) down-movements and (i-j) up-movements.
  
  UndAssetValue <- matrix(nrow=N+1, ncol=N+1)
  UndAssetValue[1,1] <- S0
  for(t in c(2:(N+1))){
    NumberofSteps <- t-1
    for(j in c(1:t)){
      NumberofUp<- t-j
      UndAssetValue[j,t]<-S0*u^NumberofUp*d^(NumberofSteps-NumberofUp)
    }
  }
   
  Delta=(maturity-t0)/N
  qu <- ((1+r)^Delta-d)/(u-d)
  qd <- 1-qu

  
  FinalPayoff <- pmax(K-UndAssetValue[,N+1],0) #İf European True/False conditions to be added.
  
  PricePut <- matrix(nrow=N+1, ncol=N+1)
  PricePut[,N+1] <- FinalPayoff
  
  PayoffPut <- matrix(nrow=N+1, ncol=N+1)
  PayoffPut[,N+1] <- FinalPayoff
  
  

  
  # Here, the last
  # column is already inserted in the steps above, so j starts goes N to 1.
  for(j in c(N:1)){
    for(i in c(1:j)){
      if(European){
        PricePut[i,j] <- 1/(1+r)^(Delta)*(PricePut[i,j+1]*qu+PricePut[i+1,j+1]*qd)
        PayoffPut[i,j] <- 0
      } else{
        PricePut[i,j] <- max(1/(1+r)^(Delta)*(PricePut[i,j+1]*qu+PricePut[i+1,j+1]*qd),
                             max(K-UndAssetValue[i,j],0))
        PayoffPut[i,j] <- max(K*disc(r,t0+(j-1)*Delta,maturity)-UndAssetValue[i,j], 0)
      }
      
    }
  }
  
  out1 <- data.frame(PricePut)
  colnames(out1) <- c(paste0("t", c(0:(N-1))), "T")
  rownames(out1) <- NULL
  
  out2 <- data.frame(UndAssetValue)
  colnames(out2) <- c(paste0("t", c(0:(N-1))), "T")
  rownames(out2) <- NULL
  
  out3 <- data.frame(PayoffPut)
  colnames(out3) <- c(paste0("t", c(0:(N-1))), "T")
  rownames(out3) <- NULL

  cat("VALUE OF THE UNDERLYING ASSET\n(u: right) (d: down-right)\n\n")
  print(out2, row.names=FALSE)
  
  cat("\n\n")
  
  cat("PRICING TREE\n(u: right) (d: down-right)\n\n")
  print(out1, row.names=FALSE)
  
  cat("\n\n")
  
  cat("PAYOFF TREE\n(u: right) (d: down-right)\n\n")
  print(out3, row.names=FALSE)
 }

multi_binom(N=3,K=11,S0=10,r=0.04,maturity=1,u=1.25,d=0.8,European=TRUE)

multi_binom(N=3,K=11,S0=10,r=0.04,maturity=1,u=1.25,d=0.8,European=FALSE)


