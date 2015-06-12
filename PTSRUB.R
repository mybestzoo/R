#Utilize quantmod to load the security symbols
require(quantmod)
symbols <- c("EUR/RUB", "USD/RUB")
start.date  <- "2015-03-01"
end.date    <- "2015-06-01"
getSymbols(symbols,from = start.date, to = end.date, src="oanda")
plot(EURRUB, ylim=range( c(EURRUB, USDRUB) ), main = "In search of cointegration: EUR & USD")
lines(USDRUB, col = 2)

#summary(lm(EURRUB~USDRUB))
#define the train and test periods
train.end <- "2015-05-01"
EUR.train <- window(EURRUB, start = start.date, end = train.end)
USD.train <- window(USDRUB, start = start.date, end = train.end)
EUR.test <- window(EURRUB, start = train.end, end = end.date)
USD.test <- window(USDRUB, start = train.end, end = end.date)

#build the model
pairsModel <- lm(EUR.train ~ USD.train)
summary(pairsModel)

#residuals in the model
plot(pairsModel$residuals, main="Residuals on test data")

#define borders for trades
border.top <- sd(pairsModel$residuals)
border.bottom <- -sd(pairsModel$residuals)
plot(pairsModel$residuals, main="Residuals on test data, now with borders")
abline(h = border.top, col = 3, lty = 2)
abline(h = border.bottom, col = 2, lty = 2)

#calculate returns
library("PerformanceAnalytics")
USD.ret <- lag(Return.calculate(USD.test), -1)
EUR.ret <- lag(Return.calculate(EUR.test), -1)

#calculate residuals
residuals <- EUR.test - (USD.test*pairsModel$coefficients[2] + pairsModel$coefficients[1])

#if residuals>border.top the return is (-EUR.ret + USD.ret)/2
pairsRet <- ifelse(residuals > border.top, (-EUR.ret + USD.ret)/2, 
                   ifelse(residuals < border.bottom, (EUR.ret - USD.ret)/2, 0))

#make report
Portf <- (USD.ret + EUR.ret)/2
str <- cbind(pairsRet, Portf)
colnames(str) <- c("Pairs trade", "50/50 portfolio")
charts.PerformanceSummary(str)

table.AnnualizedReturns(str)

maxDrawdown(str)
