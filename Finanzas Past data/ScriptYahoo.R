library(quantmod)
?quantmod


#Tutorial Como importar datos de Yahoo Finance a RStudio

mdate="2001-01-03"
amazonprices=getSymbols('AMZ', from=mdate, auto.assign = F)

print(amazonprices)

#otro ejercicio
mdate1="2001-01-04"
amazonprices1=getSymbols('AMZ', from=mdate, auto.assign = F)[,4]
print(amazonprices1)

#Tasa de crecimiento
amazonroc=ROC(amazonprices1, type='discret')
print(amazonroc)

#Tasa de Rendimiento
amazonrend = periodReturn(amazonprices, period = 'monthly', type = 'log', subset = '2018')
print(amazonrend)

#ANALISIS DE LA ACCIÓN DE TESLA

#Obtener datos desde yahoo finance
getSymbols("TSLA", from = "2020-01-28", to = "2021-01-29", periodicity= "daily")
View(TSLA)

#Aqui solo busco quedarme con los datos de CIERRE
TSLA1<-getSymbols('TSLA', from = "2020-01-28", to = "2021-01-29", auto.assign = F)[,4]
View(TSLA1)

#Tasa de crecimiento
teslaroc=ROC(TSLA1, type='discret')
print(teslaroc)

#ELIMINAR LOS DATOS NUll EN LA CULUMNA
tslaprom<-teslaroc[!is.na(teslaroc$TSLA.Close)]
print(tslaprom)

#EXTRAER EL RENDIMIETNTO
promTSLA<-mean(tslaprom$TSLA.Close)
#PROMEDIO EN PORCENTAJE
tslaAño<-promTSLA*100
print(tslaAño)




chartSeries(TSLA, TA=NULL)

chartSeries(GSPC, subset ="last 3 months")




#instalacción
install.packages("quantmod")
library(quantmod)
library(TTR)
library(zoo)
library(ggplot2)


#descargar datos todos
getSymbols("AAPL")

#descargar cierta fecha
getSymbols("AAPL", from = "2005-01-02", to="2010-12-31")


#Creación de grafica
chartSeries(AAPL, theme = "white")


#para descargar datos desde google.

getSymbols("INTC", src = "google")
