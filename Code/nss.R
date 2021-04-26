library(readxl)
library(plotly) # 3d yield curve

#data loading
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # sets working directory wherever you place the folder with code
setwd("../Data") # moves to directory with data
parameters  <- read_excel("nss_par_data.xlsx", sheet = "parameters_past20y") 
parameters  <- as.data.frame(parameters)
spread      <- read_excel("nss_par_data.xlsx", sheet = "spread") 
spread      <- as.data.frame(spread$Spread)

#data glance
head(parameters)

# define function -- Nelson-Siegel-Svensson
nelson_siegel_svensson_q <-function(n,beta0,beta1,beta2,beta3,tau1,tau2){
  beta0 + beta1*(1-exp(-n/(4*tau1)))/(n/(4*tau1)) + beta2*((1-exp(-n/(4*tau1)))/(n/(4*tau1)) - exp(-n/(4*tau1)))+beta3*((1-exp(-n/(4*tau2)))/(n/(4*tau2)) - exp(-n/(4*tau2)))
}

# pre-allocate
yields_est  <- matrix(NA,80,80)
#estimate yields using NSS
for (j in 1:80) {
  for (i in 8:80) { 
    yields_est[j,i] <- nelson_siegel_svensson_q(i,parameters[j,]$BETA0,parameters[j,]$BETA1,parameters[j,]$BETA2,parameters[j,]$BETA3,parameters[j,]$TAU1,parameters[j,]$TAU2)
  }}
yields_est

# adjustement for first 3 quarters
for (i in 1:80) {
  for (k in 1:7) {
    yields_est[i,k] <-  yields_est[i,8] - spread[i,]*(8-k)/7
  }
}



#### 3D YIELD CURVE ####
## historical ##
# create our 3d surface yield curve
yields_est %>%
  # convert to numeric matrix
  data.matrix() %>% 
  # draw 3d surface
  plot_ly(
    x=c(1:80),
    y=parameters$Date,
    z=.,
    type="surface"
  ) %>%
  plotly::layout(
    scene=list(
      xaxis=list(title="term"),
      yaxis=list(title="year"),
      zaxis=list(title="yield")
    )
  )

# Returns

#20Y TBOND cumulative returns
# returns preparation
returns20prep <- matrix(NA,80,1)
for (i in 1:80) {
  returns20prep[i] <- yields_est[i,81-i]
}
# returns 
returns_20 <- matrix(NA, 1, 80)
for (i in 1:80) {
  returns_20[i] <- exp(-80 * (returns20prep[i]/400-returns20prep[1]/400) + i* returns20prep[i]/400)-1
}
returns_20 <- returns_20 * 100 
returns_20 <- as.vector(returns_20)

#10Y TBOND cumulative returns
# returns preparation
returns10prep <- matrix(NA,80,1)
for (j in 0:1) {
  for (i in (1+j*40):((j+1)*40)) {
    returns10prep[i] <- yields_est[i,(((j+1)*40)+1)-i]
  }
}
# returns until first maturity
returns_10 <- matrix(NA, 1, 80)
for (i in 1:40) {
  returns_10[i] <- exp(-40 * (returns10prep[i]/400-returns10prep[1]/400) + i* returns10prep[i]/400)-1
}
# returns from second maturity
returns_10_add <- matrix(NA, 1, 40)
for (j in 1:40) {
  returns_10_add[j] <- (1+(exp(-40 * (returns10prep[j+40]/400-returns10prep[1+40]/400) + j* returns10prep[j+40]/400)-1))*(1+returns_10[40])
}

returns_10[41:80] <- (returns_10_add-1) 
returns_10 <- returns_10*100
returns_10 <- as.vector(returns_10)

#correctness check
((exp(10*yields_est[1,40]/100))*((exp(10*yields_est[41,40]/100)))-1)*100 - returns_10[80]

#2Y TNOTE cumulative returns
# returns preparation
returns2prep <- matrix(NA,80,1)
for (j in 0:9) {
  for (i in (1+j*8):((j+1)*8)) {
    returns2prep[i] <- yields_est[i,(((j+1)*8)+1)-i]
  }
}
# returns until first maturity
returns_2 <- matrix(NA, 1, 80)
for (i in 1:8) {
  returns_2[i] <- exp(-8 * (returns2prep[i]/400-returns2prep[1]/400) + i* returns2prep[i]/400)-1
}
# returns intra maturities
for (k in 1:9) {
  returns_2_add <- matrix(NA, 1, 8)
  for (j in 1:8) {
    returns_2_add[j] <- (1+(exp(-8 * (returns2prep[j+k*8]/400-returns2prep[1+k*8]/400) + j* returns2prep[j+k*8]/400)-1))*(1+returns_2[k*8])
  }
  returns_2[(k*8+1):((k+1)*8)] <- (returns_2_add-1)
}
returns_2 <- returns_2*100
returns_2 <- as.vector(returns_2)

#correctness check
check <- (exp(2*yields_est[1,8]/100))
for (i in 1:9) {
  check <- check * (exp(2*yields_est[1+8*i,8]/100))
}
(check-1)*100 - returns_2[80]

### time-series and plots ###

TNOTE2Y  <- ts(returns_2, start = c(2001,2), end = c(2021,1), frequency = 4)
TBOND10Y <- ts(returns_10, start = c(2001,2), end = c(2021,1), frequency = 4)
TBOND20Y <- ts(returns_20, start = c(2001,2), end = c(2021,1), frequency = 4)


#plot time series
par(mar = c(4,5,2.5,2))
leg.txt <- c("2Y T-Note", "10Y T-Bond", "20Y T-Bond")
ts.plot(TNOTE2Y,TBOND10Y ,TBOND20Y, col = c("red", "green", "magenta"), gpars = list(axes=FALSE,frame=TRUE, ann=FALSE))
axis(2, cex.axis=1.25)
axis(1, cex.axis=1.25)
title(xlab="Year", cex.lab=1.25)
title(ylab="% return", cex.lab=1.25)
legend("topleft", bty="n", lty=c(1,1), col=c("red", "green", "magenta"),
       legend=leg.txt, cex = 1.25)




