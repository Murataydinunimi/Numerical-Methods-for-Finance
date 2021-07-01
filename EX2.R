#Exercise 2, point A

Exactprob<-function(X0,mu,alpha,sigma,FinalT,t0,a,b){
  stdXT <- sigma/sqrt(2*alpha)*sqrt(1-exp(-2*alpha*(FinalT-t0)))
  meanXT <- X0*exp(-alpha*(FinalT-t0))+mu*(1-exp(-alpha*(FinalT-t0)))
  event1prob = pnorm(a, mean=meanXT, sd=stdXT)
  event2prob = 1-pnorm(b, mean=meanXT, sd=stdXT)
  Exactprob = event1prob+event2prob
  return(c("Event_a prob" = event1prob,"Event_b prob"= event2prob,"Exact prob"= Exactprob))
}


#inputs
X0 = 0
alpha=0.1
mu=0.04
sigma=0.15
t0=0
FinalT=1
a=0.1
b=0.5
Exactprob(X0=X0,mu=mu,alpha=alpha,sigma=sigma,FinalT=FinalT,t0=t0,a=a,b=b)

#Point B

#exact simulation scheme.

X0 = 0
alpha=0.1
mu=0.04
sigma=0.15
N=1 # only 1 subinterval because we use the exact simulation scheme, DElta t can be very large at most T-t0. So if we are 
#interested only in the final value of the process and we do not care about the value between T-t0, we can simulate the process
#using exact simulation scheme starting from t0 to T with only one time step. 
M=1000#trajectories
t0=0

FinalT=1



Mcexact<-function(X0,a, b, alpha,mu,sigma,N,M,t0,FinalT,conf_level=.95){
  
#Exact simulation scheme
set.seed(1)
PathExact<-matrix(0,nrow=M,ncol=N+1) # it has only two colums, today and final time.
gridtime <- seq(t0,FinalT, length.out = N+1)
Delta<-(FinalT-t0)/(N)
PathExact[,1]<-X0 #first column has the X0
for(pos in c(2: (N+1))){
  PathExact[,pos]<-PathExact[,pos-1]*exp(-alpha*(Delta))+mu*(1-exp(-alpha*(Delta))) +sigma/sqrt(2*alpha)*sqrt(1-exp(-2*alpha*Delta))*rnorm(M)}
# here we do not have any discretization error since we use the exact simulation.
colnames(PathExact)<-paste0("t = ",gridtime)

#Monte-Carlo Probability

ExactXT<-PathExact[,dim(PathExact)[2]]
McprobExact_a<- mean(ExactXT<a) 
McprobExact_b<-mean(ExactXT>b)
McprobExact__<-McprobExact_a+McprobExact_b

#Monte-Carlo Confidence intervals
std_a = sd(ExactXT<a)
std_b = sd(ExactXT>b)

UBexact_a <- McprobExact_a + qnorm(1-(1-conf_level)/2)*std_a/sqrt(M)
UBexact_b <- McprobExact_b + qnorm(1-(1-conf_level)/2)*std_b/sqrt(M)
UB_exact <- UBexact_a + UBexact_b


LBexact_a <- McprobExact_a + qnorm((1-conf_level)/2)*std_a/sqrt(M)
LBexact_b <- McprobExact_b + qnorm((1-conf_level)/2)*std_b/sqrt(M)
LB_exact <- LBexact_a + LBexact_b


return(c("LowerBound Exact prob" = LB_exact, "McProb Exact"=McprobExact__,"        Upper Bound Exact prob"=UB_exact))

}

Mcexact(X0=X0, a=a, b=b,alpha=alpha,mu=mu,sigma=sigma, N=N, M=M,t0=t0, FinalT=1)


#Point C
MCEuler<- function(X0,a,b,alpha,mu,sigma,N,M,t0,FinalT, conf_level=.95){
  set.seed(1)
  samplePath<-matrix(0,nrow=M,ncol=N+1)
  gridtime<-seq(t0,FinalT, length.out=N+1)
  Delta<- (FinalT-t0)/(N)
  samplePath[,1]<-X0
  for(pos in c(2: (N+1))){
    samplePath[,pos] <-samplePath[,pos-1]+alpha*(mu-samplePath[,pos-1])*Delta+sigma*sqrt(Delta)*rnorm(M) 
  }
  colnames(samplePath)<-paste0("t = ",gridtime)
  # M sample paths of Vasicek model obtained using the Euler simulation scheme
  # To reduce the error term due to the euler discretization we fix Delta =0.01 (N=100)
  
  
 

  # MC with Euler simulation Scheme
  EulerXT<-samplePath[,dim(samplePath)[2]] # Simulated X_T given X_0 using Euler Simulation Scheme
  MCprobEuler_a <- mean(EulerXT<a)
  MCprobEuler_b <- mean(EulerXT>b)
  MCProbEuler <-  MCprobEuler_a +MCprobEuler_b
  
  
  #confidence interval 
  
  std_a = sd(EulerXT<a)
  std_b = sd(EulerXT>b)
  
  
  Ubound_a <- MCprobEuler_a + qnorm(1-(1-conf_level)/2)*std_a/sqrt(M)
  Ubound_b <- MCprobEuler_b + qnorm(1-(1-conf_level)/2)*std_b/sqrt(M)
  Ubound <- Ubound_a+ Ubound_b
  
  lowerb_a <- MCprobEuler_a + qnorm((1-conf_level)/2)*std_a/sqrt(M)
  lowerb_b <- MCprobEuler_b + qnorm((1-conf_level)/2)*std_b/sqrt(M)
  lowerb <- lowerb_a +lowerb_b
  
  
  
  
  return(c("LowerBound Euler prob" = lowerb, "McProb Euler"=MCProbEuler,"        Upper Bound Euler prob"=Ubound))
  
}


#inputs
a=0.1
b=0.5
X0 = 0
alpha=0.1
mu=0.04
sigma=0.15
N=1000
M=1000#trajectories
t0=0
FinalT=1


N=1000
MCEuler(X0=X0, a=a, b=b,alpha=alpha,mu=mu,sigma=sigma, N=N, M=M,t0=t0, FinalT=1)
N=1
Mcexact(X0=X0, a=a, b=b,alpha=alpha,mu=mu,sigma=sigma, N=N, M=M,t0=t0, FinalT=1)
Exactprob(X0=X0,mu=mu,alpha=alpha,sigma=sigma,FinalT=FinalT,t0=t0,a=a,b=b)



