#Veámos como optimizar este portafolio (maximizando S) en un caso en que solo vamos a cambiar w3,w4
rm(list = ls())

library(portfolioAnalytics)

precios <- NULL

tickers <- c("AMZN", "AAPL", "NFLX", "FB","SPY")

getSymbols(tickers, src="yahoo", from ="2021-01-01", to ="2021-11-17", periodicity = "daily",auto.assign=TRUE)[,c(4)]

