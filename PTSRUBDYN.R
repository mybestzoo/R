#Utilize quantmod to load the security symbols
library(quantmod)
library(PerformanceAnalytics)
symbols <- c("EUR/RUB", "USD/RUB")
start.date  <- "2014-01-01"
end.date    <- "2015-06-01"
getSymbols(symbols,from = start.date, to = end.date, src="oanda")

#calculate the returns
USD.ret <- lag(Return.calculate(USDRUB), -1)
EUR.ret <- lag(Return.calculate(EURRUB), -1)

startTrade <- "2015-01-01"

tradingDays <- seq(as.Date(startTrade), as.Date(end.date), by = 'days')

#fill returns with zeros
returns <- xts(rep(0, length(tradingDays)), order.by = tradingDays)

#length for the statistics
length <- 100

for (i in seq_along(tradingDays)) { 
  # Step 1
  USD.model <- window(USDRUB, 
                      start = as.Date(tradingDays[i]) - length, 
                      end = tradingDays[i])
  EUR.model <- window(EURRUB, 
                      start = as.Date(tradingDays[i]) - length, 
                      end = tradingDays[i])
  # Step 2
  model <- lm(EUR.model ~ USD.model)
  # Step 3
  residual <- tail(model$residuals, 1)
  # Step 4
  border <- sd(model$residuals)
  # Step 5
  returns[i] <- ifelse(
    residual > border, 
    (-EUR.ret[tradingDays[i]] + USD.ret[tradingDays[i]])/2, 
    ifelse (
      residual < -border, 
      (EUR.ret[tradingDays[i]] - USD.ret[tradingDays[i]])/2, 
      0)
  )
}

returns <- na.omit(returns)
charts.PerformanceSummary(returns)

table.AnnualizedReturns(returns)

maxDrawdown(returns)


