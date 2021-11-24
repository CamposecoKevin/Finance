
# 1-Llamar Librerias a utilizar para analisis ---------------------------------------
library(xts)
library(zoo)
library(TTR)
library(quantmod)
library(ggplot2)
library(tidyverse)

library(iterators)
library(quadprog)
library(foreach)
library(PerformanceAnalytics)
library(PortfolioAnalytics)


library(knitr)
library(kableExtra)
library(dplyr)
library(ggplot2)


library(timeDate)
library(timeSeries)
library(fBasics)
library(fAssets)

library(fPortfolio)

# 2- Descargar empresas a analizar para portafolio ---------------------------

tickers <- c("AMZN", "AAPL", "NFLX", "FB", "TSLA", "GOOGL")       

portfolioPrices <- NULL

for(ticker in tickers) {
  portfolioPrices <- cbind(portfolioPrices, getSymbols.yahoo(ticker,
  from="2021-01-01", to = "2021-11-18", periodicity="daily",auto.assign=FALSE)[,6])
}


# 3-Visualización de los datos descargados ----------------------------------

#Pasar los nuevos datos a una nueva variable
Empresas <- portfolioPrices

#Ver los datos descargados
view(Empresas)

#Ver las primeras 10 filas
head(Empresas,10)

#ver las ultimas filas
tail(Empresas,10)



# Ver en gráficos nivel precios ---------------------------------------------------------

plot(Empresas, col=Empresas, xlab="Fecha", ylab="AdjClose"); title(main="Evolución S&P 5OO") 

  # 4- rendimientos de las empresas analizadas --------------------------------------------
(RetornoDePortafolio <- na.omit(ROC(Empresas)))

plot(RetornoDePortafolio, col="Blue", xlab="Fecha", ylab="Retornos"); title(main="Retornos del Portafolio")


# 5- Comparar con el indice SPY los rendimientos -----------------------------
#descargar datos de SPY
MarketIndiceSpy<- getSymbols.yahoo("SPY",
                                    from="2021-01-01", to = "2021-11-18", periodicity = "daily", auto.assign =FALSE)[,6]

# 6- rendimientos de las empresas analizadas --------------------------------------------
(RetornoDeMarket <- na.omit(ROC(MarketIndiceSpy)))

plot(RetornoDeMarket, col="Black", xlab="Fecha", ylab="Retornos"); title(main="Retornos del Portafolio")


# 7- Gráfica de los retornos ----------------------------------------------

#Retornors de SPY

RetornoDeMarket %>% chartSeries(TA = "addROC()", subset = "2021")


#retornos del portafolio


RetornoDePortafolio %>% chartSeries(TA = "addROC()", subset = "2021")


# 8- Analisis CAMP y Beta ---------------------------------------------------------
CAPM.beta(RetornoDePortafolio,RetornoDeMarket, 0.035/252)



# 9 Analisis SHARP --------------------------------------------------------
table.AnnualizedReturns(RetornoDePortafolio)



# 10- Optimización de portafolio ------------------------------------------
(portafolio1<-portfolio.spec(colnames(RetornoDePortafolio)))



(portafolio1 <- add.constraint(portafolio1, type = "weight_sum", min_sum = 1, max_sum =1))

distr <- pander(portafolio1$assets)


#con condiciones
(portafolio1 <- add.constraint(portafolio1, type = "box", min= .10, max = .40))
distrCodi <- pander(portafolio1$assets)











