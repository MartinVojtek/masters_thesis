library(readxl)
library(stats)
library(matrixStats)
#data loading
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # sets working directory wherever you place the folder with code
setwd("../Data") # moves to directory with data
returns     <- read_excel("masterdata.xlsx", sheet = "quarterly_adjusted") 
returns     <- as.data.frame(returns)
CPI         <- read_excel("US_CPI.xlsx", sheet = "Q_CPI")
CPI         <- as.data.frame(CPI)
#data glance
head(returns)
head(CPI)

#convert to time series
Bill   <- ts(returns$`Bill`, start = c(2001,2), end = c(2021,1), frequency = 4) # c(1992,2) for full data
Short  <- ts(returns$`Short`, start = c(2001,2), end = c(2021,1), frequency = 4) 
Middle <- ts(returns$`Middle`, start = c(2001,2), end = c(2021,1), frequency = 4) 
Long   <- ts(returns$`Long`, start = c(2001,2), end = c(2021,1), frequency = 4) 

#plot time series
leg.txt <- c("Bill", "Short", "Middle", "Long")
ts.plot(Bill,Short,Middle,Long, col = c("blue", "red", "green", "magenta"), xlab="Year", ylab="Percentage") 
legend("topleft", bty="n", lty=c(1,1), col=c("blue", "red", "green", "magenta"),
       legend=leg.txt)

# calculation of cumulative returns
return_prep      <- returns
return_prep[2:5] <- returns[2:5]/100+1
start_horizon    <- nrow(return_prep[2]) - 80 # to make investment horizon equal to 20 years

cumul_his_returns <- matrix(NA, 80, 4)
for (i in (start_horizon+1):nrow(return_prep[2])) {
  cumul_his_returns[i-start_horizon,] <- (colProds(as.matrix(return_prep[(start_horizon+1):i,2:5]), method = "direct")-1)*100
}

# transform back to timeseries
Bill_cum    <- ts(cumul_his_returns[,1], start = c(2001,2), end = c(2021,1), frequency = 4) # c(1992,2) for full data
Short_cum   <- ts(cumul_his_returns[,2], start = c(2001,2), end = c(2021,1), frequency = 4) 
Middle_cum  <- ts(cumul_his_returns[,3], start = c(2001,2), end = c(2021,1), frequency = 4) 
Long_cum    <- ts(cumul_his_returns[,4], start = c(2001,2), end = c(2021,1), frequency = 4) 

#plot cumulative nominal returns
leg.txt <- c("Bill", "Short", "Middle", "Long")
ts.plot(Bill_cum,Short_cum,Middle_cum,Long_cum, col = c("blue", "red", "green", "magenta"), xlab="Year", ylab="Percentage") 
legend("topleft", bty="n", lty=c(1,1), col=c("blue", "red", "green", "magenta"),
       legend=leg.txt)

# real returns - inflation adjusted

# last 20 years of CPI inflation
start_horizon_inflation <- nrow(CPI) - 80 # to make inflation horizon equal to 20 years
inflation <- CPI[(start_horizon_inflation+1):nrow(CPI),3]
# pre allocate real returns
real_returns <- returns[(start_horizon+1):nrow(returns[2]),]
# calculate real quarterly returns
real_returns[,2] <- ((1+real_returns$Bill/100)/(1+inflation/4))-1
real_returns[,3] <- ((1+real_returns$`Short Index`/100)/(1+inflation/4))-1
real_returns[,4] <- ((1+real_returns$`Middle Index`/100)/(1+inflation/4))-1
real_returns[,5] <- ((1+real_returns$`Long Index`/100)/(1+inflation/4))-1

return_prep_real <- real_returns
return_prep_real[2:5] <- real_returns[2:5]+1

cumul_his_real_returns <- matrix(NA, 80, 4)
for (i in 1:nrow(return_prep_real[2])) {
  cumul_his_real_returns[i,] <- (colProds(as.matrix(return_prep_real[1:i,2:5]), method = "direct")-1)*100
}

# transform back to timeseries
Bill_cum_real    <- ts(cumul_his_real_returns[,1], start = c(2001,2), end = c(2021,1), frequency = 4) # c(1992,2) for full data
Short_cum_real   <- ts(cumul_his_real_returns[,2], start = c(2001,2), end = c(2021,1), frequency = 4) 
Middle_cum_real  <- ts(cumul_his_real_returns[,3], start = c(2001,2), end = c(2021,1), frequency = 4) 
Long_cum_real    <- ts(cumul_his_real_returns[,4], start = c(2001,2), end = c(2021,1), frequency = 4) 

# plot cumulative real returns
leg.txt <- c("Bill", "Short", "Middle", "Long")
ts.plot(Bill_cum_real,Short_cum_real,Middle_cum_real,Long_cum_real, col = c("blue", "red", "green", "magenta"), gpars = list(axes=FALSE,frame=TRUE, ann=FALSE))
axis(2, cex.axis=1.25)
axis(1, cex.axis=1.25)
title(xlab="Year", cex.lab=1.25)
title(ylab="% cumulative returns", cex.lab=1.25)
legend("topleft", bty="n", lty=c(1,1), col=c("blue", "red", "green", "magenta"),
       legend=leg.txt, cex = 1.25)
# EOP returns
Bill_cum_real[length(Bill_cum_real)]
Short_cum_real[length(Short_cum_real)]
Middle_cum_real[length(Middle_cum_real)]
Long_cum_real[length(Long_cum_real)]