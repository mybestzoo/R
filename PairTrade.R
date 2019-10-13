#Utilize quantmod to load the security symbols
require(quantmod)
symbols <- c("USD/RUB", "USD/KRW")
getSymbols(symbols,src="oanda")

#define training set
startT  <- "2015-01-01"
endT    <- "2015-06-01"
rangeT  <- paste(startT,"::",endT,sep ="")
tRUB   <- USDRUB[,1][rangeT]
tKRW   <- USDKRW[,1][rangeT]

#define out of sample set
startO  <- "2015-01-02"
endO    <- "2015-06-01"
rangeO  <- paste(startO,"::",endO,sep ="")
oRUB   <- USDRUB[,1][rangeO]
oKRW   <- USDKRW[,1][rangeO]

#compute price differences on in-sample data
pdtRUB <- diff(tRUB)[-1]
pdtKRW <- diff(tKRW)[-1]

#build the model
model  <- lm(pdtRUB ~ pdtKRW - 1)

#extract the hedge ratio
hr     <- as.numeric(model$coefficients[1])

#spread price (in-sample)
spreadT <- tRUB - hr * tKRW

#compute statistics of the spread
meanT    <- as.numeric(mean(spreadT,na.rm=TRUE))
sdT      <- as.numeric(sd(spreadT,na.rm=TRUE))
upperThr <- meanT + 1 * sdT
lowerThr <- meanT - 1 * sdT

#visualize the in-sample spread + stats
plot(spreadT, main = "RUB vs. KRW spread (in-sample period)")
abline(h = meanT, col = "red", lwd =2)
abline(h = meanT + 1 * sdT, col = "blue", lwd=2)
abline(h = meanT - 1 * sdT, col = "blue", lwd=2)
