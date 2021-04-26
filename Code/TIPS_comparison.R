library(readxl) # read excel file
library(astsa)  # SARIMA
library(lmtest) # Granger test
#data loading
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # sets working directory wherever you place the folder with code
setwd("../Data") # moves to directory with data
data        <- read_excel("2yTIPSvs3mATIPS.xlsx", sheet = "to_r") 
data        <- as.data.frame(data)
#data glance
head(data)

dataATIPS   <- ts(data$`artificialTIPS`, start = c(1999,1), end = c(2021,1), frequency = 12)
dataTIPS    <- ts(data$`TIPSY02`, start = c(1999,1), end = c(2021,1), frequency = 12)

leg.txt     <- c("3-month artificial TIPS", "2-year TIPS")
ts.plot(dataATIPS,dataTIPS, col = c("blue", "red"), gpars = list(axes=FALSE,frame=TRUE, ann=FALSE))
axis(2, cex.axis=1.25)
axis(1, cex.axis=1.25)
title(xlab="Year", cex.lab=1.25)
title(ylab="% Yield", cex.lab=1.25)
legend("topright", bty="n", lty=c(1,1), col=c("blue", "red"),
       legend=leg.txt, cex = 1.25)

#testing for statistical significance of difference between the time series
t.test(dataATIPS,dataTIPS)
t.test(dataATIPS_priorGFC,dataTIPS_priorGFC)
grangertest(dataATIPS,dataTIPS, order = 1) 
grangertest(dataATIPS ~ dataTIPS, order = 1)

sarima(dataATIPS,1,0,0,details = FALSE)
sarima(dataTIPS,1,0,0,details = FALSE)






