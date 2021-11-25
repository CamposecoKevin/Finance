
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

plot(Empresas, main=""  ,col=Empresas, xlab="Fecha", ylab="AdjClose"); title(main="Evolución S&P 5OO") 

  # 4- rendimientos de las empresas analizadas --------------------------------------------
(RetornoDePortafolio <- na.omit(ROC(Empresas)))

plot(RetornoDePortafolio,col="Blue", xlab="Fecha", ylab="Retornos"); title(main="Retornos del Portafolio")


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




# Matrix de covarianzas ---------------------------------------------------

#Riesgos buscando la varianzas esta formula va para todos
(var(RetornoDePortafolio$AMZN.Adjusted)*100)
(var(RetornoDePortafolio$AAPL.Adjusted)*100)
(var(RetornoDePortafolio$NFLX.Adjusted)*100)
(var(RetornoDePortafolio$FB.Adjusted)*100)
(var(RetornoDePortafolio$TSLA.Adjusted)*100)
(var(RetornoDePortafolio$GOOGL.Adjusted)*100)

#matrix de covarianza
pander(Cov <- cov(RetornoDePortafolio)*100)

#matrix de correlación
pander(corr <- cor(RetornoDePortafolio) * 100)


#mapa de calor con pheatmap

install.packages("pheatmap")
library(pheatmap)

pheatmap(corr,
         display_numbers = TRUE,
         number_color = "black",
         cluster_cols = FALSE,
         cluster_rows = FALSE,
         fontsize_number = 8)


# - Optimización de portafolio ------------------------------------------
(portafolio1<-portfolio.spec(colnames(RetornoDePortafolio)))



(portafolio1 <- add.constraint(portafolio1, type = "weight_sum", min_sum = 1, max_sum =1))

distr <- pander(portafolio1$assets)


#con condiciones
(portafolio1 <- add.constraint(portafolio1, type = "box", min= .10, max = .40))
distrCodi <- pander(portafolio1$assets)




#optimización de markowitz
espcartera<-portfolioSpec()

setRiskFreeRate(espcartera)<- -0.001 ##Rentabilidad Activo Libre de Riesgo
setNFrontierPoints(espcartera) <- 20
constraints="LongOnly"

#calcular la frotera
Frontera <- portfolioFrontier(as.timeSeries(RetornoDePortafolio),spec=espcartera,constraints )
Frontera

#Grafica del portafolio 

frontierPlot(Frontera)
grid()
tangencyPoints(Frontera, pch = 19, col = "red", cex=2)
tangencyLines(Frontera, col="darkgreen", pch=19, cex=2)
minvariancePoints(Frontera, col="blue", pch=19, cex=2)
monteCarloPoints(Frontera, mCsteps=500, col="green", cex=0.001)



col <- qualiPalette(ncol(RetornoDePortafolio), "Dark2")
weightsPlot(Frontera, col=col)


pander(col)
