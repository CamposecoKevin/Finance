library(lubridate)
library(tidyquant)
library(ggplot2)
library(quantmod)
library(xts)
library(zoo)

#Clase No. 11

date <- "2019-3-2"

KOFClose<-getSymbols.yahoo("KOF", from = date,
                           auto.assign=FALSE)[,6]



#Visualización de datos
KOFClose

#Visualización de Redimientos Logaritmicos
KOFRets <- na.omit(dailyReturn(KOFClose, type = "log"))
chartSeries(KOFRets)

#Velas Japonesas para una empresa y decargar información
#de la empresa con tickers
getSymbols(Symbols = "BBF", src="yahoo",
           from= "2019-03-02",
           to = "2021-03-02")
barChart(BBF, theme = "white")


date = '2020-1-2' 
tickers = c('GFNORTEO.MX', 'TV', 'WALMEX.MX', 'BBD', 'KOF')       


for(ticker in tickers) {
    
    print(paste('Going to load data for', ticker, 'from', date, '...', sep=' '))
    stock_data = getSymbols.yahoo(
        ticker,
        from=date, 
        periodicity='daily',
        auto.assign=FALSE
    )
    
    adj_close_price = stock_data[,6]
    rets = na.omit(dailyReturn(adj_close_price, type='log'))
    chartSeries(rets)
    barChart(stock_data)
    
}


#clase no. 12

#VOLATILIDAD DE LOS PRECIOS A TRAVES DE UNA HISTOGRAMAS
#Ver la frecuencia de los precios ajustados de
#alguna acción

hist(BBF$BBF.Adjusted, breaks = 60, col = "green")



#CLASE NO. 13

#Cómo seleccionar los activos de un portafolio,
#bandas de bolinger, si las vandas se alejan hay valitilidad, y se acerca
#el mercado es más estable.

BBF%>% chartSeries(TA= 'addBBands();addVo();
                   addMACD()', subset='2020')



#clase no. 14
#Análisis detallado de volatilidad y tendencia



#clase no. 15
#Continuando con la selección de activos de un portafolio
BBF%>% chartSeries(TA= 'addBBands();addVo();
                   addMACD(); addRSI()', subset='2020')


BBF%>% chartSeries(TA= 'addBBands();addMACD(); 
                   addRSI();addATR()', subset='2020')
#El ATR (Indicador de Promedio del Rango Verdadero) es un indicador que nos permite ver más fácil la volatilidad de los precios.

#Es la línea azul, en el eje Y hay valores que se mueven entre 0 y 100. Los valores que están en 70 o por encima de 70 indican que existe sobrecompra o sobrevaloracion de la acción. Y los valores que están en 30 o por debajo de 30 indica que hay subestimación de la acción.
#En este caso $KOF el valor es de 25.7.
#Y el 14 es un indicador que evalua los últimos 14 periodos de tiempo para en nuestro caso que usamos daily significa que son los últimos 14 dias.




#clase no. 16
#¿Averso al riesgo o amante del riesgo? Entendiendo tu perfil como trader

# Trading: Consiste en hacer transacciones en la misma acción durante pocas semanas o pocos años, es adoptar una posición y estar ahí un tiempo.

#Swing Trading: Consiste en cambiar o hacer transacciones un par de días y hasta unas pocas semanas.

#Day Trading: Implica hacer transacciones en pocos segundos o durante un día.

#Scalping Trading: Consiste en hacer transacciones durante unos segundos o unos pocos minutos.

#Counter Trend: Entras a una transacción cuando el chart cambia de dirección.
#Trend: Entras a una transacción cuando estás seguro de que va a bajar o subir una acción.



#clase No. 17.

tickers<- c("FB", "AAPL", "AMZN", "NFLX")

wights <- c(.20,.20,.20,.20,.20)

library(xts)
library(zoo)
library(TTR)
library(quantmod)

#creación de portafolio de inversión

portafolioPrice<- NULL

for (tickers in tickers) {
    portafolioPrice<-cbind(portafolioPrice,
                           getSymbols.yahoo(tickers, from= "2019-3-1",
                        periodicity= "daily", auto.assign = FALSE)[,6])
    
}


portafolioreturns <- na.omit(ROC(portafolioPrice))
View(portafolioreturns)


#con la que se realizará la comparativa

benchkmarkprices<- getSymbols.yahoo("SPY",
                                    from = "2019-3-2", periodicity = "daily", auto.assign =FALSE)[,6]


#Comparación con el indice SPY de acuerdo a los rendimientos.
benchkmarkreturns <- na.omit(ROC(benchkmarkprices))
View(benchkmarkreturns)


#graficas
library(tidyverse)
library(dplyr)
library(PortfolioAnalytics)
library(foreach)
library(PerformanceAnalytics)

#Retornors de SPY
benchkmarkreturns %>% chartSeries(TA = "addROC()", subset = "2021")


#retornos del portafolio
portafolioreturns %>% chartSeries(TA = "addROC()", subset = "2021")



#clase 18.
#PARA MEDIR EL BETA

CAPM.beta(portafolioreturns,benchkmarkreturns, 0.035/252)


#SHARPE

table.AnnualizedReturns(portafolioreturns)


#CLASE NO. 19 Optimización del portafolio.

(portafolio<-portfolio.spec(colnames(portafolioreturns)))

(portafolio <- add.constraint(portafolio, type = "weight_sum", min_sum = 1, max_sum =1))


(portafolio <- add.constraint(portafolio, type = "box", min= .10, max = .40))
(portafolio <- add.objective(portafolio, type = "return", name = "mean"))
(portafolio <- add.objective(portafolio,type = "risk", name = "StdDev"))

portafolio$call


install.packages("GenSA")
install.packages("DEoptim")
install.packages("parallel")
library(DEoptim)
library(parallel)
library(GenSA)

OptiPortafolio <- optimize.portfolio(portafolioreturns, portafolio, optimize_method = "DEoptim")
View(OptiPortafolio)


??DEoptim


OptiPortafolio

?GenSA


aaplClose <- getSymbols.yahoo("AAPL", from="2019-3-5", auto.assign = F)[,4]

view(aaplClose)

#Clase 20: Back evaluacion de señales.

install.packages("Quandl")
install.packages("scales")
library(Quandl)
library(scales)
library(gridExtra)
library(TTR)
library(jsonlite)
library(xtable)
library(gtable)
library(grid)
library(dplyr)

library(tidyverse)
library(httr)
library(readxl)
library(lubridate)
library(reshape2)
library(quantmod)
library(xts)
library(zoo)


install.packages("pander")
library("pander")

options(scipen = 999)

retornos <- na.omit(ROC(Empresas))

#para asignar el nombre de columanas
ret <- c("AMZN", "AAPL", "NFLX", "FB", "TSLA", "GOOGL")

#cambiar el nombre para la data set retornos
colnames(retornos) <- ret

#analizando el sharpe
sharpe.indiv1 <- SharpeRatio(R= retornos, Rf=0, FUN= "StdDev")


#optimizar el portafolio

init.portfolio <- portfolio.spec(assets = colnames(retornos))


init.portfolio <-  add.constraint(portfolio = init.portfolio,type = "weight_sum", min_sum =1, max_sum =1)

Distribución <- pander(init.portfolio$assets) 

init.portfolio <-  add.constraint(portfolio = init.portfolio,type = "box",min=c(0.2,0.2,-1,-1),max=c(0.2,0.2,2,2))

init.portfolio <-  add.constraint(portfolio = init.portfolio,type = "box",min=c(0.2,0.2,-1,-1),max=c(0.2,0.2,2,2))

pander(head(sharpe.indiv1))

