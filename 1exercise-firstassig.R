butterfly <- function(j,h,S,K1,K2){
  w=j/(j+h)
  K_=w*K1+(1-w)*K2
  x=sort(c(K1,K2))
  Smax <- j*(K_-K1)
  
  if(Smax == h*(K2-K_)){
    print("Ok")
  }

  
  if(S<=x[1] || S >= x[2]){
    payoff<-0

   }else if(x[1] < S & S < K_){
      payoff<-j*(S-K1)

   }else if(K_ <= S & S < x[2]){
     payoff<-h*(K2-S)
   }

  plot(x=S,y=payoff, pch=20, col="red",
       xlim=c(0,x[2]*1.2), ylim=c(0,Smax*1.2),
       xlab="Value of underlying asset at time T",
       ylab="Final payoff of Butterfly Strategy")
  lines(c(0,x[1]),c(0,0), type='l', col='blue')
  lines(c(x[1],K_),c(0,Smax), type='l', col='blue')
  lines(c(K_,x[2]),c(Smax,0), type='l', col='blue')
  lines(c(x[2],x[2]*1.2),c(0,0), type='l', col='blue')
  text(S, payoff, labels=paste0("(",S,",",payoff,")"), cex=0.9, adj = -0.2, font=2)
  
  return(c("S_T"=S,"Payoff"=payoff))
}


