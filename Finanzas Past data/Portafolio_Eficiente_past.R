#Veámos como optimizar este portafolio (maximizando S) en un caso en que solo vamos a cambiar w3,w4
rm(list = ls())

library(portfolioAnalytics)

precios <- NULL

tickers <- c("AMZN", "AAPL", "NFLX", "FB")

for (nom in tickers) {
    precios <- cbind(precios,getSymbols(src="yahoo", from ="2016-01-02", to ="2019-06-01", periodicity = "daily", auto.assign=FALSE)[,c(4)])
}

options(scipen = 999)
retornados <- na.omit(ROC(precios))
ret<- c("AMZN", "AAPL", "NFLX", "FB")
colnames(retornados)<- ret

 
Sharper.indiv1<-SharpeRatio(R=retornados, Rf=0, FUN = "StDev") #Obtengamos el ratio de sharpe de cada activo

#Optimizamos ahora el portafolio


init.portafolio <- portafolio.spec(assets=colnames(retornados))

init.portafolio <- add.constraint(portfolio = init.portafolio,type = "weight_sum", min_sum=1, max_sum=1)