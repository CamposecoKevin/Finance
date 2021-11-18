#Librerias 
install.packages("foreach")
library(quantmod)
library(xts)
library(TTR)
library(zoo)
library(ggplot2)
library(tidyverse)

#Analisis de portafolio
library(iterators)
library(quadprog)
library(foreach)
library(PerformanceAnalytics)
library(PortfolioAnalytics)



#Descargar datos de empresas a analizar

tickers <- c("AMZN", "AAPL", "NFLX", "FB", "TSLA", "GOOGL", "SPY")       

portfolioPrices <- NULL

for(ticker in tickers) {
  portfolioPrices <- cbind(portfolioPrices, getSymbols.yahoo(ticker,
  from="2021-01-01", to = "2021-11-18", periodicity="daily",auto.assign=FALSE)[,6])
}


datos <- portfolioPrices

#Ver los datos descargados
view(portfolioPrices)


561290





