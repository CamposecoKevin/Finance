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
rm(list = ls())

precios <- NULL

tickers <- c("SPY", "AAPL", "NFLX", "FB", "TSLA")

for (ticker in tickers) {
    precios <- cbind(precios,getSymbols.yahoo(src="yahoo", from ="2021-01-01", to ="2021-11-16", periodicity = "daily", auto.assign=FALSE)[,6])
}


??getSymbols.yahoo


#Estos para identificar el archivo como nuevo







