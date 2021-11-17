library(quantmod)
library(xts)
library(zoo)
library(PerformanceAnalytics)
install.packages("tidyquant")

TV<-read.csv("TELEVISA.CSV")

#colocar la fecha de descarga
date<- "2019-1-15"
#descargar la información
tv<-getSymbols.yahoo("TV", from = date)
tvclose<-getSymbols.yahoo("TV", from = date, auto.assign =F)[,6]
#aqui ya podemos observar los datos
View(tvclose)

#VER RETORNOS DIARIOS
TVRets<-na.omit(dailyReturn(tvclose, type = "log"))
#aqui podemos ver los datos
View(TVRets)

#gráfica
chartSeries(TVRets)

#STIKER DE EMPRESAS A ENCONTRAR
#BRANDESCO "BBD"
#BANORTE "GFNORTEO.MX"
#FEMSA "KOF"
#WALLMARTMEXICO "WALMEX.MX"
#TELEVISA  "TLEVISACPO.MX"




