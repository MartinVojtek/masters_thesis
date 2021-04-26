## =================================================================================
## Preliminaries - libraries
## =================================================================================

{
  rm(list = ls())
  
  library(data.table)
  library(zoo)
  library(plyr)
  library(dplyr)  # union_all function
  library(ggplot2)
  library(MASS)
  library(fanplot)
  library(RcppBDT)
  library(matrixStats)
  library(tseries)
  library(trend)
  library(expm)   # sqrtm - matrix root
  library(readxl)
  library(astsa)  # SARIMA
  library(plotly) # 3d yield curve
  library(ggpubr) # density and qq plot
}

## =================================================================================
## Setting scenario
## =================================================================================

# set scenario:
# 1 - if you want to run the base case simulation (pre-pandemic)
# 2 - if you want to run the covid - non-adjusted data simulation
# 3 - if you want to run the covid - adjusted data simulation

scenario <- 1 

# set maturity of the bond you want to explore

bond_maturity           <- 80 # in quarters

## ================================================================================
## Reading real data - historical data for GDP, TIPS yields
## ================================================================================

# reading TIPS yields data as a base for cyclical part of real rate

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # sets working directory wherever you place the folder with code
setwd("../Data") # moves to directory with data
ct_sheet           <- c("CTcal","CTcal_covid","CTcal_covid_adj")
ct_end             <- c('2019-10-01','2020-10-01','2020-10-01')
TIPS               <- read_excel("cyclical_part_of_real_rate.xlsx", sheet = ct_sheet[scenario]) 
TIPS               <- as.data.frame(TIPS)
c_t_his            <- as.ts(TIPS$`c_t`, start = '1999-04-01', end = ct_end[scenario])

# reading GDP growth data for constant gain learning
QoQ_sheet          <- c("QoQ","QoQ_covid","QoQ_covid_adj")
historical_gdp.QoQ <- read_excel("US_GDP.xlsx", sheet = QoQ_sheet[scenario])
historical_gdp.QoQ <- data.table(historical_gdp.QoQ)
YoY_sheet          <- c("YoY","YoY_covid","YoY_covid_adj")
historical_gdp.YoY <- read_excel("US_GDP.xlsx", sheet = YoY_sheet[scenario])
historical_gdp.YoY <- data.table(historical_gdp.YoY)



## ================================================================================
## Functions - constant gain learning, term premium, DAR, annualization
## ================================================================================

# constant gain learning
{
  
  dmean  <- function(data,window,forget_f) {
    
    # pre-allocate
    N             = nrow(data)
    disc_mean     = data.table(data_mean = rep(0,N - window + 1))
    disc_mean_pad = data.table(data_mean = rep(0,N))
    scaling       = (1 - forget_f^window)/(1 - forget_f)
    
    # loop over dates
    for (i in window:N) {
      tmp = 0
      # compute discounted moving average for each date
      for (j in 0:(window - 1)) {
        tmp = tmp + forget_f^j*data[i - j,1]
      }
      
      disc_mean[i - window + 1,1] = tmp/scaling
    }
    
    disc_mean_pad[1:(window - 1),]     = NA
    disc_mean_pad[window:N,]           = disc_mean
    
    # add dates
    disc_mean_pad                  = cbind(disc_mean_pad,data[,1])
    #disc_mean_pad                  = cbind(disc_mean_pad,data[,4])
    return(disc_mean_pad)
  }
  
  
}

# term premium

risk_premium  <- function(N,h,sigma,beta) {
  # pre-allocate  
  b <- matrix(NA,N,1) 
  # first part of equation  
  for (n in 2:N) {
    temp <- 0 
    for (k in 2:n) {
      temp1 <- 0
      for (j in 2:(k-1)){
        temp1 <- temp1 + beta*h^(k-j-1)
      }
      temp <- temp + beta * h^(k-2) + temp1
    }
    
    first_eq <- temp
    
    # second part of equation    
    temp2 <- 0
    for(k in 2:n){
      for(j in 2:(k-1)){
        for(i in 1:(j-1)){
          temp2 <- temp2 + h^(j-i-1)*h^(k-i-1)
        }
      }
    }
    
    second_eq <- temp2
    # put parts together    
    b[1] <- -1 * sigma^2
    b[n] <- -1/n * sigma^2 * (first_eq + second_eq)
    
  }
  # transform output
  b <- b*12 # *12 because of annual transformation
  b
  return(b)
}


# Double Auto regressive process

dar.process.simul    <- function(mu.dar, phi.dar, Sigma0, Sigma1, nvar, sample.size, var.labels){
  # generate shocks
  eps.dar            <-   data.table(mvrnorm(n <- (sim.param$sample.size), matrix(0, nvar, 1), diag(nvar), tol = 1e-6))
  # drift
  c.dar              <- as.vector((diag(nvar) - phi.dar) %*% mu.dar)
  # simulate DAR
  X                <- matrix(0,nvar,(sim.param$sample.size))
  Omega            <- array(0, dim = c(nvar,nvar,(sim.param$sample.size)))
  X[,1]            <- r_star_start 
  Omega[,,1]       <- Sigma0 %*% t(Sigma0)
  
  for (i in 2:(sim.param$sample.size)) {
    
    Omega[,,i]       <-  Sigma0 %*% t(Sigma0) + Sigma1 %*% X[,i-1] %*% t(X[,i-1]) %*% t(Sigma1)
    X[,i]            <-  c.dar + phi.dar %*% X[,i-1] + sqrtm(Omega[,,i-1]) %*% t(eps.dar[i,])
  }
  X   <- t(X)
  dimnames(X) = list(NULL,var.labels)                        
  return(X)
}

# conversion of data from QoQ to YoY -- annualization

annualization <- function(data)
{
  data <- 1 + data/4
  annualized_data <- data.table(g.t = rep(0,nrow(data)))
  for (i in 4:nrow(data)){
    annualized_data[i] <- (data[i]*data[i-1]*data[i-2]*data[i-3]) -1 
  }
  return(annualized_data)
}

## ================================================================================
## Cyclical part of real rate from data
## ================================================================================

c_t.model     <- sarima(c_t_his,1,0,0,details = FALSE)
ar_coef       <- c_t.model$fit$coef[1]
mean_coef     <- c_t.model$fit$coef[2]
const_coef    <- (1-ar_coef) * mean_coef
wn_volatility <- c_t.model$fit$sigma2



## ================================================================================
## Simulation parameters 
## ================================================================================



# starting parameters - real data, GDP observed, liq and saf calculated as r-star-gdp
last_r_star             <- c(0.00414233452857331, 0.000292543168433665, -0.0037) # HLW for 1st and 2nd scenario, SPF calculation for 3rd
last_r_star             <- last_r_star[scenario]
gdp_perm_start          <- dmean(historical_gdp.YoY, 40, 0.97) # https://fred.stlouisfed.org/series/GDPC1
gdp_perm_start          <- (gdp_perm_start[!is.na(data_mean),.(data_mean)])
gdp_perm_start          <- as.numeric(gdp_perm_start) #conversion to numeric
liq_and_saf             <- last_r_star-gdp_perm_start #assigning the rest to liquidity and safety
gdp_cyc_start           <- 0
liq_start               <- -liq_and_saf * 46/73  # Krishnamurthy and Jorgensen ratio
saf_start               <- -liq_and_saf * 27/73  # Krishnamurthy and Jorgensen ratio
ct_start                <- c_t_his[length(c_t_his)]/100 # last historically calculated c_t


# parameters -- simulation setup
# adjust parameters to account for scenarios if needed, we believe the fundamentals remain unchanged
sim.param              <- list()
sim.param$sample.size  <- 80 # in quarters 
sim.param$nsim         <- 1000 # number of simulations
# parameters -- DAR
dar.param              <- list()
#mu
dar.param$mu.m         <- c(0.023/4,0.023/4,0.023/4)                    # GDP data
dar.param$mu.a         <- c(0, 0, 0)                                    # Muller, Stock and Watson
dar.param$mu.liq       <- c(0.0046/4,0.0046/4,0.0046/4)                 # Krishnamurthy and Jorgensen
dar.param$mu.saf       <- c(0.0027/4,0.0027/4,0.0027/4)                 # Krishnamurthy and Jorgensen
dar.param$mu.ct        <- c(const_coef/100,const_coef/100, const_coef/100)
#phi
dar.param$phi.m        <- c(1,1,1)                                      # super-persistent component of GDP
dar.param$phi.a        <- c(0,0,0)                                      # transitory component of GDP -> no persistence
dar.param$phi.liq      <- c(0.995,0.995,0.995)                          # current difference is not expected to jump to mean at once
dar.param$phi.saf      <- c(0.995,0.995,0.995)                          # current difference is not expected to jump to mean at once
dar.param$phi.ct       <- c(ar_coef,ar_coef,ar_coef) 
#Sigma0
dar.param$Sigma0m      <- c(0.007635365/4,0.007635365/4,0.007635365/4)  # Based on s.d of GDP of last 20y, m/a distribution based on MSW
dar.param$Sigma0a      <- c(0.015270731/4,0.015270731/4,0.015270731/4)  # Based on s.d of GDP of last 20y, m/a distribution based on MSW
dar.param$Sigma0.liq   <- c(1/1600,1/1600,1/1600)                       # Del Negro et. al.
dar.param$Sigma0.saf   <- c(1/1600,1/1600,1/1600)                       # Del Negro et. al.
dar.param$Sigma0.ct    <- c(sqrt(wn_volatility/(100*100)),sqrt(wn_volatility/(100*100)),sqrt(wn_volatility/(100*100)))
#Sigma1
dar.param$Sigma1m      <- c(0.04,0.04,0.04)                             # set to induce stationarity
dar.param$Sigma1a      <- c(0.03,0.03,0.03)                             # set to ensure pull towards 0
dar.param$Sigma1.liq   <- c(0.01,0.01,0.01)                             # empirically chosen 
dar.param$Sigma1.saf   <- c(0.01,0.01,0.01)                             # empirically chosen 
dar.param$Sigma1.ct    <- c(0,0,0)                                      # ct is an AR(1) process

# parameters  --  learning 
learn.param             <- list()
learn.param$v           <- 0.97                                         # based on constant gain learning literature 
learn.param$lookback    <- 40                                           # based on constant gain learning literature 

# parameters  --  term premium
premium.param          <- list()
premium.param$sigma    <- c(-3.17964e-05,-3.17964e-05,-3.17964e-05)     # calculated using Nelder-Mead algorithm and RMSE
premium.param$beta     <- c(-3.27618e+04,-3.27618e+04,-3.27618e+04)     # calculated using Nelder-Mead algorithm and RMSE


## ====================================================
## Parameters Consolidation
## ====================================================
nvar <- 5

r_star_start <- c(gdp_perm_start,gdp_cyc_start,liq_start,saf_start,ct_start)

mu.dar <- c(dar.param$mu.m[scenario],dar.param$mu.a[scenario],dar.param$mu.liq[scenario],dar.param$mu.saf[scenario],dar.param$mu.ct[scenario]) 

phi.dar              <- matrix(0, nvar, nvar)
phi.dar[1,1]         <- dar.param$phi.m[scenario]
phi.dar[2,2]         <- dar.param$phi.a[scenario]                       
phi.dar[3,3]         <- dar.param$phi.liq[scenario]
phi.dar[4,4]         <- dar.param$phi.saf[scenario]
phi.dar[5,5]         <- dar.param$phi.ct[scenario]



Sigma0 <- c(dar.param$Sigma0m[scenario],dar.param$Sigma0a[scenario],dar.param$Sigma0.liq[scenario],dar.param$Sigma0.saf[scenario],dar.param$Sigma0.ct[scenario])
Sigma0 <- diag(Sigma0)

Sigma1 <- c(dar.param$Sigma1m[scenario],dar.param$Sigma1a[scenario],dar.param$Sigma1.liq[scenario],dar.param$Sigma1.saf[scenario],dar.param$Sigma1.ct[scenario])
Sigma1 <- diag(Sigma1)


var.labels <- c('tau.g','cyc.g','liq','saf','ct')

## ====================================================
## Simulate processes  
## ====================================================

# run term premium
b_n_val <- risk_premium(sim.param$sample.size*3, dar.param$phi.ct[scenario]^(1/3), premium.param$sigma[scenario], premium.param$beta[scenario])

# calculate c_t weight based on bonds maturity 
# with maturity n
c_t.weigth <- matrix(0, bond_maturity, 1)
for (j in 1:sim.param$sample.size){
  c_t.weigth_temp <- 0
  for (i in 0:(j-1)){
    c_t.weigth_temp <- c_t.weigth_temp + 1/j * dar.param$phi.ct[scenario]^i
  }
  c_t.weigth[j] <-  c_t.weigth_temp
}


# run r-star

# pre-allocate 
r_star.sim                 <- matrix(NA, sim.param$nsim, (sim.param$sample.size))
bond_yield.sim_now         <- matrix(NA, sim.param$nsim, (sim.param$sample.size))
bond_yield.sim_now.c       <- matrix(NA, sim.param$nsim, (sim.param$sample.size))
bond_return.sim_now        <- matrix(NA, sim.param$nsim, (sim.param$sample.size))
bond_return.sim_now.c      <- matrix(NA, sim.param$nsim, (sim.param$sample.size))
pre                        <- rep(NA, sim.param$nsim*sim.param$sample.size*sim.param$sample.size)
yields                     <- array(pre, c(sim.param$sample.size, sim.param$sample.size, sim.param$nsim))
# code adjustment to account for bond yields where investment horizon / bond maturity is not an integer
end                        <- floor(sim.param$sample.size/bond_maturity)*bond_maturity

# calculate
for (j in 1:sim.param$nsim) {
  all_yields <- matrix(NA, sim.param$sample.size, sim.param$sample.size)
  # function simulating DAR process
  
  X <- dar.process.simul(mu.dar, phi.dar, Sigma0, Sigma1,nvar, sim.param$sample.size,var.labels)
  
  
  # put GDP components together
  g.t               <- data.table(g.t = X[,1] + X[,2])
  # merge historical GDP data with simulations
  g.t               <- rbind(historical_gdp.QoQ,g.t) 
  g.t               <- annualization(g.t)[-1:-3]
  # apply learning
  g.bar             <- dmean(g.t, learn.param$lookback, learn.param$v)
  g.bar             <- t(as.matrix(g.bar[!is.na(data_mean),.(data_mean)]))[2:(sim.param$sample.size+1)]
  # equal size of GDP component and remaining components
  liquidity         <- X[,3]
  safety            <- X[,4]
  c_t               <- X[,5]
  b_n_now           <- rep(b_n_val[3*bond_maturity], sim.param$sample.size)
  # put all components together
  r.star            <- g.bar - liquidity - safety
  cyclical_now      <- c_t.weigth[bond_maturity] * c_t 
  bond_yield_now    <- r.star + cyclical_now + b_n_now
  
  # preparation for returns
  c_t.weigth_i        <- rep(c_t.weigth[1:bond_maturity], sim.param$sample.size/bond_maturity)
  if (sim.param$sample.size %% bond_maturity !=0){ #adjustment see condition
    c_t.weigth_add    <- (c_t.weigth[1:bond_maturity])[1:(sim.param$sample.size-end)]
    c_t.weigth_i      <- union_all(c_t.weigth_i,c_t.weigth_add)
  }
  c_t.weigth_it     <- union_all(rev(c_t.weigth_i)[-1],c_t.weigth_i[bond_maturity])
  ct_timevar        <- c_t.weigth_it * c_t
  b_n_val1          <- b_n_val[seq(0, length(b_n_val), 3)]
  b_n_val2          <- rep(b_n_val1[1:bond_maturity], sim.param$sample.size/bond_maturity)
  if (sim.param$sample.size %% bond_maturity !=0){ #adjustment see condition
    b_n_val2_add      <- (b_n_val1[1:bond_maturity])[1:(sim.param$sample.size-end)]
    b_n_val2          <- union_all(b_n_val2,b_n_val2_add)
  }
  b_n_val3          <- rev(b_n_val2)
  b_n_val4          <- union_all(b_n_val3[-1],b_n_val3[1])
  return_prep       <- r.star + ct_timevar + b_n_val4
  
  # constant r-star
  r.star.c          <- matrix(sum(r.star[1]),sim.param$sample.size,1)
  bond_yield_now.c  <- r.star.c + cyclical_now + b_n_now
  return_prep.c     <- r.star.c + ct_timevar + b_n_val4
  
  # convert to percentage
  r_star                      <- (r.star) * 100
  bond_yield_now              <- (bond_yield_now) * 100
  bond_yield_now.c            <- (bond_yield_now.c) *  100
  return_prep                 <- (return_prep) * 100
  return_prep.c               <- (return_prep.c) * 100
  
  # all yields for 3D yield curve
  for (i in 1:sim.param$sample.size){
    all_yields[,i] <- r.star + c_t.weigth[i]*c_t + rep(b_n_val[i],sim.param$sample.size)
  }
  
  # load simulation
  r_star.sim[j,]              <- r_star[1:(sim.param$sample.size)]
  bond_yield.sim_now[j,]      <- bond_yield_now[1:(sim.param$sample.size)]
  bond_yield.sim_now.c[j,]    <- bond_yield_now.c[1:(sim.param$sample.size)]
  bond_return.sim_now[j,]     <- return_prep[1:(sim.param$sample.size)]
  bond_return.sim_now.c[j,]   <- return_prep.c[1:(sim.param$sample.size)]
  yields[,,j]                 <- all_yields[,1:(sim.param$sample.size)]
}


## ====================================================
## RETURNS FROM DAR R-STAR
## ====================================================

#yields from holding to maturity
cumul_return <- matrix(NA, sim.param$nsim, (sim.param$sample.size))
cumul_return[,bond_maturity] <- (1+(exp(bond_yield.sim_now[,1]/100 * bond_maturity/4)-1))
for (i in seq(bond_maturity*2, (sim.param$sample.size), by = bond_maturity)){
  cumul_return[,i] <- cumul_return[,i-bond_maturity]*(1+exp(bond_yield.sim_now[,i-bond_maturity]/100*(bond_maturity/4))-1)
}
cumul_return <- (cumul_return-1)*100


#returns until first maturity 
if (bond_maturity > 1){
  first_returns <- matrix(NA, sim.param$nsim, bond_maturity-1)
  for (i in 1:(bond_maturity-1)) {
    first_returns[,i] <- exp(-bond_maturity * (bond_return.sim_now[,i]/400-bond_yield.sim_now[,1]/400) + i* bond_return.sim_now[,i]/400)-1
  }
  cumul_return[,1:(bond_maturity-1)] <- first_returns * 100 
}
#returns intra maturities
if (bond_maturity > 1 && bond_maturity  < sim.param$sample.size){
  for (j in 1:(sim.param$sample.size/bond_maturity-1))
  {
    intra_returns <- matrix(NA, sim.param$nsim, (bond_maturity-1))
    for (i in 1:(bond_maturity-1)) {
      intra_returns[,i] <- (1+(exp(-bond_maturity * (bond_return.sim_now[,bond_maturity*j+i]/400-bond_yield.sim_now[,j*bond_maturity]/400)  + i * bond_return.sim_now[,bond_maturity*j+i]/400)-1)) * (1+cumul_return[,j*bond_maturity]/100)
    }
    cumul_return[,(j*bond_maturity+1):((j*bond_maturity)+(bond_maturity-1))] <- (intra_returns-1)*100
  }
}

#returns if investment horizon modulo bond_maturity is not zero
if (end != sim.param$sample.size){
  after_returns <- matrix(NA, sim.param$nsim, sim.param$sample.size-end)
  for (i in 1:sim.param$sample.size-end) {
    after_returns[,i] <- (1+(exp(-bond_maturity * (bond_return.sim_now[,end+i]/400-bond_yield.sim_now[,end]/400)  + i * bond_return.sim_now[,end+i]/400)-1)) * (1+cumul_return[,end]/100)
  }
  cumul_return[,(end+1):(sim.param$sample.size)] <- (after_returns-1)*100
}
## ====================================================
## RETURNS FROM CONSTANT R-STAR
## ====================================================
#yields from holding to maturity
cumul_return.c <- matrix(NA, sim.param$nsim, (sim.param$sample.size))
cumul_return.c[,bond_maturity] <- (1+(exp(bond_yield.sim_now.c[,1]/100 * bond_maturity/4)-1))
for (i in seq(bond_maturity*2, (sim.param$sample.size), by = bond_maturity)){
  cumul_return.c[,i] <- cumul_return.c[,i-bond_maturity]*(1+exp(bond_yield.sim_now.c[,i-bond_maturity]/100*(bond_maturity/4))-1)
}
cumul_return.c <- (cumul_return.c-1)*100


#returns until first maturity 
if (bond_maturity > 1){
  first_returns.c <- matrix(NA, sim.param$nsim, bond_maturity-1)
  for (i in 1:(bond_maturity-1)) {
    first_returns.c[,i] <- exp(-bond_maturity * (bond_return.sim_now.c[,i]/400-bond_yield.sim_now.c[,1]/400) + i* bond_return.sim_now.c[,i]/400)-1
  }
  cumul_return.c[,1:(bond_maturity-1)] <- first_returns.c * 100 
}
#returns intra maturities
if (bond_maturity > 1 && bond_maturity  < sim.param$sample.size){
  for (j in 1:(sim.param$sample.size/bond_maturity-1))
  {
    intra_returns.c <- matrix(NA, sim.param$nsim, (bond_maturity-1))
    for (i in 1:(bond_maturity-1)) {
      intra_returns.c[,i] <- (1+(exp(-bond_maturity * (bond_return.sim_now.c[,bond_maturity*j+i]/400-bond_yield.sim_now.c[,j*bond_maturity]/400)  + i * bond_return.sim_now.c[,bond_maturity*j+i]/400)-1)) * (1+cumul_return.c[,j*bond_maturity]/100)
    }
    cumul_return.c[,(j*bond_maturity+1):((j*bond_maturity)+(bond_maturity-1))] <- (intra_returns.c-1)*100
  }
}
#returns if investment horizon modulo bond_maturity is not zero
if (end != sim.param$sample.size){
  after_returns.c <- matrix(NA, sim.param$nsim, sim.param$sample.size-end)
  for (i in 1:sim.param$sample.size-end) {
    after_returns.c[,i] <- (1+(exp(-bond_maturity * (bond_return.sim_now.c[,end+i]/400-bond_yield.sim_now.c[,end]/400)  + i * bond_return.sim_now.c[,end+i]/400)-1)) * (1+cumul_return.c[,end]/100)
  }
  cumul_return.c[,(end+1):(sim.param$sample.size)] <- (after_returns.c-1)*100  
}
## ====================================================
## 3D YIELD CURVE
## ====================================================
# median call
yields_data <- yields * 100
yields_data_2d_median <- matrix(NA, sim.param$sample.size,sim.param$sample.size)
for (i in 1:sim.param$sample.size){
  for (j in 1:sim.param$sample.size){
    yields_data_2d_median[i,j] <- median(yields_data[i,j,])
  }}

# random call
yields_data_2d_random <- matrix(NA, sim.param$sample.size,sim.param$sample.size)
yields_data_2d_random <- (yields_data[,,sample(1:sim.param$nsim, 1)])

## ====================================================
## CHARTS
## ====================================================

#### R-STAR ####
plot(NULL, main="", xlim = c(0, (sim.param$sample.size)), ylim = c(-5,5),xlab = 'quarters', ylab = "% EOP",cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
# add fan
fan(data = r_star.sim, type = "interval",  probs = c(0.5, 0.68, 0.9,0.95), start = 1, frequency = 1, fan.col = colorRampPalette(c("#2C7399", "#B0DEF1")), ln = NULL, rlab = NULL, med.ln = TRUE,na.rm = TRUE)

#### BOND YIELD ####
# DAR
plot(NULL, main="", xlim = c(0, (sim.param$sample.size)), ylim = c(-5,5),xlab = 'quarters', ylab = "% EOP",cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
# add fan
fan(data = bond_yield.sim_now, type = "interval",  probs = c(0.5, 0.68, 0.9,0.95), start = 1, frequency = 1, fan.col = colorRampPalette(c("#2C7399", "#B0DEF1")), ln = NULL, rlab = NULL, med.ln = TRUE,na.rm = TRUE)

# CONSTANT
plot(NULL, main="", xlim = c(0, (sim.param$sample.size)), ylim = c(-5,5),xlab = 'quarters', ylab = "% EOP",cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
# add fan
fan(data = bond_yield.sim_now.c, type = "interval",  probs = c(0.5, 0.68, 0.9,0.95), start = 1, frequency = 1, fan.col = colorRampPalette(c("#2C7399", "#B0DEF1")), ln = NULL, rlab = NULL, med.ln = TRUE,na.rm = TRUE)


#### BOND RETURNS ####
# DAR
plot(NULL, main="", xlim = c(0, (sim.param$sample.size)), ylim = c(-20,80),xlab = 'quarters', ylab = "% EOP",cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
# add fan
fan(data = cumul_return, type = "interval",  probs = c(0.5, 0.68, 0.9,0.95), start = 1, frequency = 1, fan.col = colorRampPalette(c("#2C7399", "#B0DEF1")), ln = NULL, rlab = NULL, med.ln = TRUE,na.rm = TRUE)
# CONSTANT
plot(NULL, main="", xlim = c(0, (sim.param$sample.size)), ylim = c(-20,80),xlab = 'quarters', ylab = "% EOP",cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
# add fan
fan(data = cumul_return.c, type = "interval",  probs = c(0.5, 0.68, 0.9,0.95), start = 1, frequency = 1, fan.col = colorRampPalette(c("#2C7399", "#B0DEF1")), ln = NULL, rlab = NULL, med.ln = TRUE,na.rm = TRUE)

#### 2D term premium ####
plot(b_n_val * 100, type = "l", xlab = 'months' , ylab = "% p.a.")

#### 3D YIELD CURVE ####
## median ##
# create our 3d surface yield curve
yields_data_2d_median %>%
  # convert to numeric matrix
  data.matrix() %>% 
  # draw 3d surface
  plot_ly(
    x=c(1:sim.param$sample.size),
    y=c(1:sim.param$sample.size),
    z=.,
    type="surface"
  ) %>%
  plotly::layout(
    scene=list(
      xaxis=list(title="term"),
      yaxis=list(title="date"),
      zaxis=list(title="yield")
    )
  )
## random ##
# create our 3d surface yield curve
yields_data_2d_random %>%
  # convert to numeric matrix
  data.matrix() %>% 
  # draw 3d surface
  plot_ly(
    x=c(1:sim.param$sample.size),
    y=c(1:sim.param$sample.size),
    z=.,
    type="surface"
  ) %>%
  plotly::layout(
    scene=list(
      xaxis=list(title="term"),
      yaxis=list(title="date"),
      zaxis=list(title="yield")
    )
  )

#### NORMALITY OF RETURNS ####
# density plot
p <- ggdensity(cumul_return[,sim.param$sample.size], 
               main = "",
               xlab = "End of period returns",
               color = "red"
)

p +
  font("xlab", size = 20, color = "black")+
  font("ylab", size = 20, color = "black")+
  font("xy.text", size = 20, color = "black")

# Q-Q plot
q <- ggqqplot(cumul_return[,sim.param$sample.size],color = "red")
q +
  font("xlab", size = 20, color = "black")+
  font("ylab", size = 20, color = "black")+
  font("xy.text", size = 20, color = "black")

# Normality test
shapiro.test(cumul_return[,sim.param$sample.size])
# Log-normality test
shapiro.test(log(cumul_return[,sim.param$sample.size]))

################################################################################################################

## ====================================================
## SIMULATION ANALYSIS
## ====================================================
# quantiles of returns comparing DAR and constant
quantile(cumul_return[,sim.param$sample.size],  probs = c(0.5, 1, 5, 10, 25, 50, 75, 90, 95, 99, 99.5)/100)
quantile(cumul_return.c[,sim.param$sample.size],  probs = c(0.5, 1, 5, 10, 25, 50, 75, 90, 95, 99, 99.5)/100)

#  mean, median and standard deviation EOP
mean(cumul_return[,sim.param$sample.size])
mean(cumul_return.c[,sim.param$sample.size])
median(cumul_return[,sim.param$sample.size])
median(cumul_return.c[,sim.param$sample.size])
sd(cumul_return[,sim.param$sample.size])
sd(cumul_return.c[,sim.param$sample.size])

#  mean, median and standard deviation EOP/2
mean(cumul_return[,sim.param$sample.size/2])
mean(cumul_return.c[,sim.param$sample.size/2])
median(cumul_return[,sim.param$sample.size/2])
median(cumul_return.c[,sim.param$sample.size/2])
sd(cumul_return[,sim.param$sample.size/2])
sd(cumul_return.c[,sim.param$sample.size/2])


## ====================================================
## UTILITY SCORE
## ====================================================

#parameters
lambda  <- 3
mu     <- mean(cumul_return[,sim.param$sample.size])/100
sigma2 <- (sd(cumul_return[,sim.param$sample.size])/100)^2
# quadratic utility function
U      <- (mu - 1/2 * lambda * sigma2) * 100
U
