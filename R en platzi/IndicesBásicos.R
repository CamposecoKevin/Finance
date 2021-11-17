library(quantmod)
library(xts)
library(zoo)
library(PerformanceAnalytics)
library(tidyverse)
library(tibble)

date<- "2019-3-2"

#LLMAR MUCHAS ACCIONES
tickers<- c("KOF", "GFNORTEO.MX", "TV", "WALMEX.MX", "BBD")


#ORDENAR 
#Error en auto.assing, aqui hay una error no se donde sale portafolioPrinces
portafolioPrices <- NULL

for (tickers in tickers) {
    portafolioPrices <-cbind(portafolioPrices,
                            getSymbols.yahoo(tickers,
                                             from="2019-3-2",
                                             periodicity = "daily", auto.assing=FALSE)[,6])
}
view(portfolio_bacon)

colnames(portafolioPrices)