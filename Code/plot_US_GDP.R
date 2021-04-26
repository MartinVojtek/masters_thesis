library(readxl)

#data loading
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # sets working directory wherever you place the folder with code
setwd("../Data") # moves to directory with data
GDPdata <- read_excel("US_GDP.xlsx", sheet = "GDPcovid") 
GDPdata <- as.data.frame(GDPdata)
#data glance
head(GDPdata)

#convert to time series
GDP_QoQ  <- ts(GDPdata$`QoQ`, start = c(1947,2), end = c(2020,4), frequency = 4) * 100
GDP_YoY  <- ts(GDPdata$`YoY`, start = c(1947,2), end = c(2020,4), frequency = 4) * 100

#plot time series
leg.txt <- c("QoQ growth", "YoY growth")
ts.plot(GDP_QoQ,GDP_YoY, col = c("blue", "red"), gpars = list(axes=FALSE,frame=TRUE, ann=FALSE))
axis(2, cex.axis=1.25)
axis(1, cex.axis=1.25)
title(xlab="Year", cex.lab=1.25)
title(ylab="% GDP growth*", cex.lab=1.25)
legend("topleft", bty="n", lty=c(1.25,1.25), col=c("blue", "red"),
       legend=leg.txt, cex = 1.25)



