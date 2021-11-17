#ANALISIS DE LA ACCIÓN DE TESLA
library(quantmod)
library(tidyverse)
library(portfolioAnalytics)
#Obtener datos desde yahoo finance
getSymbols("TSLA", from = "2020-01-28", to = "2021-01-29", periodicity= "daily")
View(TSLA)

#Aqui solo busco quedarme con los datos de CIERRE
TSLA1<-getSymbols('TSLA', from = "2020-01-28", to = "2021-01-29", auto.assign = F)[,4]
View(TSLA1)

#para las cuatro acciones

TSLA<-getSymbols("TSLA", from = "2020-01-29", to = "2021-01-30", auto.assign = F)[,4]
RCL<-getSymbols("RCL", from = "2020-01-29", to = "2021-01-30", auto.assign = F)[,4]
PFE<-getSymbols("PFE", from = "2020-01-29", to = "2021-01-30", auto.assign = F)[,4]
AMZN<-getSymbols("AMZN", from = "2020-01-29", to = "2021-01-30", auto.assign = F)[,4]


#DESCARGANDO DATOS DE VARIAS ACCIONES
getSymbols(c("TSLA","RCL","PFE","AMZN"), from = "2020-01-29", to = "2021-01-30", periodicity= "daily")

## ver filas de la base de datos
head(TSLA, n =10)

## para ver la ultima filas

tail(TSLA, n = 10)


#Tasa de crecimiento
teslaroc=ROC(TSLA1, type='discret')
print(teslaroc)

#ELIMINAR LOS DATOS NUll EN LA CULUMNA
tslaprom<-teslaroc[!is.na(teslaroc$TSLA.Close)]
print(tslaprom)

#EXTRAER EL RENDIMIETNTO
promTSLA<-mean(tslaprom$TSLA.Close)

#PROMEDIO EN RENDIMIENTO DIARIO
tslaAño<-promTSLA*100
print(tslaAño)

#RENDIMIENTO ANUAL EN PORCENTAJE

primera_Fila<-print(TSLA1[1,])
ultima_fila<-print(TSLA1[254,])

Ranual<-(ultima_fila/primera_Fila)-1
#RENDIMIENTO ANUAL EN PORCENTAJE
RanualTsla<-(Ranual*100)
print(RanualTsla)

#RENDIMIENTO ULTIMO MES
## para ver la ultima filas

TSLAUM<-tail(TSLA, n = 22)
Rmensual<-((793.53/665.99)-1)*100
print(Rmensual)


