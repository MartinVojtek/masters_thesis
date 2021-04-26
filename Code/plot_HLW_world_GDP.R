library(readxl)

#data loading
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # sets working directory wherever you place the folder with code
setwd("../Data") # moves to directory with data
data <- read_excel("HLW_GDP_in_world.xlsx") 
data <- as.data.frame(data)
#data glance
head(data)

#convert to time series
dataUS       <- ts(data$`US`, start = c(1972,4), end = c(2020,2), frequency = 4)
dataCanada   <- ts(data$`Canada`, start = c(1972,4), end = c(2020,2), frequency = 4)
dataEuroArea <- ts(data$`EuroArea`, start = c(1972,4), end = c(2020,2), frequency = 4)
dataUK       <- ts(data$`UK`, start = c(1972,4), end = c(2020,2), frequency = 4)

#plot time series
par(mar = c(4,5,2.5,2))
leg.txt <- c("United States", "Canada", "Euro Area", "UK")
ts.plot(dataUS,dataCanada ,dataEuroArea,dataUK, col = c("black", "blue", "red", "green"), gpars = list(axes=FALSE,frame=TRUE, ann=FALSE))
axis(2, cex.axis=1.25)
axis(1, cex.axis=1.25)
title(xlab="Year", cex.lab=1.25)
title(ylab="% GDP growth trend", cex.lab=1.25)
legend("topright", bty="n", lty=c(1,1), col=c("black", "blue", "red", "green"),
       legend=leg.txt, cex = 1.25)




