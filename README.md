library(quantmod)
library(PerformanceAnalytics)
library(ggplot2)
library(zoo)
library(xts)


getSymbols(c("MSFT", "AAPL", "GOOG"), from = "2023-01-01", to = "2024-01-01")


prices <- merge(AAPL[,"AAPL.Adjusted"], 
                GOOG[,"GOOG.Adjusted"], 
                MSFT[,"MSFT.Adjusted"], 
                join = "inner")
colnames(prices) <- c("AAPL", "GOOG", "MSFT")


returns <- na.omit(diff(log(prices)))

portfolio_returns_numeric_vector <- rowMeans(returns)
portfolio_returns_eq_xts <- xts(portfolio_returns_numeric_vector, order.by = index(returns))
colnames(portfolio_returns_eq_xts) <- "Equal Weight Portfolio"

allreturns <- merge(portfolio_returns_eq_xts, returns)

anual_sndrt <- sd.annualized(allreturns)

print(anual_sndrt)

anual_shrp <- SharpeRatio.annualized(allreturns, Rf = 0)
print(anual_shrp)

plot(allreturns, main = "Daily Log Returns: Portfolio vs. Individual Stocks", legend.loc = "bottomleft", auto.legend = TRUE)


