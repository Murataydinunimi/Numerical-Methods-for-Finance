# Please change the wd to where the source R scripts are stored
directory <- "C:/Users/fabio/OneDrive - Università degli Studi di Milano/Documenti/Istruzione/Università degli Studi di Milano/DATA SCIENCE AND ECONOMICS (LM-91)/RMNMF/Numerical Methods for Finance/Second Assignment"

setwd(directory)
library(readxl)
source("merton_constraints.R")
source("discount_factor.R")
source("errorBS.R")
source("calibration_distance.R")
days_year <- 252

raw_data <- read_excel("Dataset.xls")

my_data <- data.frame("S0"=raw_data$Underlyng,
                      "t0"=0/days_year, # conversion on yearly basis
                      "maturity"=lapply(raw_data[,'Maturity Opt']-raw_data[,'t0'], as.numeric)[[1]]/days_year, # conversion on yearly basis
                      "K"=raw_data$Strike,
                      "call"=ifelse(raw_data[,'Type Option'] == 'call', T, F),
                      "obsPrice"=raw_data[,'Price Option'],
                      "r"=raw_data$RiskFreeRate_ccr
                      )
colnames(my_data) <- c("S0", "t0", "maturity", "K", "call", "obsPrice", "r")

# Note: only K and obsPrice vary.
# S0 is fixed to 52.67
S0 <- my_data$S0[1]
# r is fixed to 0,00131207421568667 yearly
r <- my_data$r[1]
# t0 is fixed to 25/01/2021 ---> 0
t0 <- my_data$t0[1]
# maturity is fixed to 17/03/2021 ---> 0.202381
maturity <- my_data$maturity[1]



### Application of Merton's constraints ###
valid_merton <- logical()
for(i in 1:nrow(my_data)){
  valid_merton[i] <- merton_constraints(price=my_data$obsPrice[i],
                                 St=S0, 
                                 K=my_data$K[i], 
                                 t=t0, 
                                 maturity=maturity, 
                                 r=r, 
                                 call=my_data$call[i], 
                                 ccr=T)
}
valid_merton
# All data is valid........?

my_data <- my_data[valid_merton,]


### Subsetting out-of-the-money Options ###
ootm <- ifelse(my_data$call, (my_data$S0 < my_data$K), (my_data$S0 > my_data$K))
ootm
my_data <- my_data[ootm,]


### VOLATILITY SURFACE ###
vol.fit <- list()
for(i in 1:nrow(my_data)){
  vol.fit[[i]] <- optim(par = 0.1, fn = errorBS, 
                     S = S0, 
                     K = my_data$K[i], 
                     maturity = maturity, 
                     r = r,
                     obsPrice = my_data$obsPrice[i], 
                     call = my_data$call[i],
                     type = "sq",
                     method = c("L-BFGS-B"), lower = 0, upper = 100
  )
}

ImpVol <- sapply(vol.fit, '[[', 'par')
# check
assertthat::are_equal(sapply(vol.fit, '[[', 'value'),
         errorBS(sigma=ImpVol, S=S0, K=my_data$K, maturity=maturity, r=r, t=t0, obsPrice=my_data$obsPrice, call=my_data$call, type="sq"))

plot(my_data[my_data$call,'K'], ImpVol[my_data$call], type='l', col="blue",
     xlim=c(min(my_data$K)-2, max(my_data$K)+2), ylim=c(min(ImpVol)-.01, max(ImpVol)+.01),
     xlab = "Strike Price", ylab="Implied Volatility",
     main = "Volatility Surface")
lines(my_data[!my_data$call,'K'], lapply(vol.fit, '[[', 'par')[!my_data$call], col="orange")
legend("bottomleft",legend=c("Call", "Put"), lty=1, col=c("blue", "orange"), title="Option type")


### CALIBRATION ###
CalVol <- optim(par = 0.1, fn = calibration_distance, 
                S = S0, 
                K = my_data$K, 
                maturity = maturity, 
                r = r, 
                obsPrice = my_data$obsPrice, 
                call=my_data$call, # switches call/put automatically
                meth="MSE",
                method = c("L-BFGS-B"), lower = 0, upper = 100)

c("Calibrated Volatility" = CalVol$par)

