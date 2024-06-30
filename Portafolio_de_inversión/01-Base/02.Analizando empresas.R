#instalacción
library(quantmod)
library(TTR)
library(zoo)
library(ggplot2)
library(PerformanceAnalytics)
library(dplyr)


#descargar datos todos
stickers=c("AAPL","TSLA","NVDA","MSFT","GOOG")

?quantmod

#descargar cierta fecha
getSymbols(stickers, from = "2023-06-27", to="2024-06-28")
AAPL2_years1<- AAPL$AAPL.Close


GOLD<- getSymbols("GC=F", from = "2023-06-27", to="2024-06-28")

#Precio de cierre de acciones
TSLA<- TSLA$TSLA.Close
NVDA <- NVDA$NVDA.Close
MSFT <- MSFT$MSFT.Close
GOOG <- GOOG$GOOG.Close

#Creación de grafica
chartSeries(AAPL2_years5, 
            theme = chartTheme("white"),
            name="AAPL",
            TA= list("addBBands(n = 10)",
                     "addVo()",
                     "addEMA(60)",
                     "addEMA(20, col = 2)"))

chartSeries(AAPL, 
            theme = chartTheme("white"),
            name="AAPL",
            TA= list("addBBands(n = 10)",
                     "addVo()",
                     "addEMA(60)",
                     "addEMA(20, col = 2)"))

#AAPLE DE ONE YEAR
chartSeries(GLD, 
            theme = chartTheme("white"),
            name="GLD",
            TA= list("addEMA(60, col=5)",
                     "addEMA(20, col = 2)"))


#Grafico con ggplt2, precio de la año de los 5 años
AAPL2 %>% 
  ggplot(aes(Index,AAPL.Close))+
  geom_line(colour = "blue")+
  labs(title = "Precios de las acciones de los 5 años de APPLE(AAPL)")+
  goem_line()


#para descargar datos desde google.

getSymbols("INTC", src = "google")