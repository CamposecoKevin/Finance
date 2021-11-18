
#Con el siguiente codigo se crea un archivo donde se va almacenando todos los archivos 
dir.create("R en platzi")

#este sirve para cambiar de dirección del directorio
setwd(paste0(getwd(),"/R en platzi"))

# revisar link https://r-coder.com/directorio-trabajo-r/


ates = "2019-3-2"
tickers = c ("GFNORTEO.MX", "TV", "WALMEX.MX","BBD")

portfolioprices = NULL 
for(ticker in tickers){
    portfolioprices = cbind(portfolioprices,
                            getSymbols.yahoo(ticker,
                                             from="2019-3-2",
                                             periodicity ="daily", auto.assign=FALSE)[,6])
}
view(portfolioprices)