library(xts)
library(zoo)
library(lubridate)
library(tidyquant)
library(ggplot2)
install.packages("lubridate")

#TELEVISA  "TLEVISACPO.MX"
#descargar datos desde yahoo finance
tv1<-tq_get("TLEVISACPO.MX", get = "stock.prices", from ="2019-01-01", to = "2020-08-01")

tvret<-tv1 %>%
    tq_transmute(select =adjusted, # el nombre de la columan a selecionar, precio ajustado
                 mutate_fun = periodReturn,
                 period="daily",   #calculo del retorno
                 col_rename = "televisa_returns") #renombramos la columan

#visualización de datos
View(tv1)

tvret %>%
    ggplot(aes(x=date, y = televisa_returns))+
    geom_line()+
    theme_classic()+
    labs(x="fecha", y ="retnor diario")+
    ggtitle("retorno diario de televisa")+
    scale_x_date(date_breaks = "years", date_labels = "%Y")+
    scale_y_continuous(breaks = seq(-0.5,0.6,0.05),
                       labels = scales::percent)
    
#ver los datos
str(tv1)   


#resument de los datos
summary(tv1)

#resument de los rendimientos
summary(tvret)

View(tvret)
    
#desviación estandar de los datos
sd(tv1$adjusted)

#desviación estandar de retornos
sd(tvret$televisa_returns)


#EXTRACCIÓN DE LA MEDIA, DESVIACIÓN ESTANDAR, Y COEFICIENTE
#la media de los precios
meanDatosTV<-mean(tv1$adjusted)
#desviación estandar
sddatostv<-sd(tv1$adjusted)

#coeficionete

coeficiente<-(sddatostv/meanDatosTV)*100

#ver los datos encontrados
meanDatosTV
sddatostv
coeficiente



#EMPRESA #WALLMARTMEXICO "WALMEX.MX"
tv2<-tq_get("WALMEX.MX", get = "stock.prices", from ="2020-01-01", to = "2020-08-01")

tvret2<-tv2 %>%
    tq_transmute(select =adjusted, # el nombre de la columan a selecionar, precio ajustado
                 mutate_fun = periodReturn,
                 period="daily",   #calculo del retorno
                 col_rename = "WALMEX_returns") #renombramos la columan

#visualización de datos
View(tv2)

tvret2 %>%
    ggplot(aes(x=date, y = WALMEX_returns))+
    geom_line()+
    theme_classic()+
    labs(x="fecha", y ="retnor diario")+
    ggtitle("retorno diario de WALMEX")+
    scale_x_date(date_breaks = "years", date_labels = "%Y")+
    scale_y_continuous(breaks = seq(-0.5,0.6,0.05),
                       labels = scales::percent)

#EMPRESA 3 #BRANDESCO "BBD"
tv3<-tq_get("BBD", get = "stock.prices", from ="2020-01-01", to = "2020-08-01")

tvret3<-tv3 %>%
    tq_transmute(select =adjusted, # el nombre de la columan a selecionar, precio ajustado
                 mutate_fun = periodReturn,
                 period="daily",   #calculo del retorno
                 col_rename = "BBD_returns") #renombramos la columan

#visualización de datos
View(tv3)

tvret3 %>%
    ggplot(aes(x=date, y = BBD_returns))+
    geom_line()+
    theme_classic()+
    labs(x="fecha", y ="retnor diario")+
    ggtitle("retorno diario de BBD")+
    scale_x_date(date_breaks = "years", date_labels = "%Y")+
    scale_y_continuous(breaks = seq(-0.5,0.6,0.05),
                       labels = scales::percent)

#EMPRESA 4 #BANORTE "GFNORTEO.MX"
tv4<-tq_get("GFNORTEO.MX", get = "stock.prices", from ="2020-01-01", to = "2020-08-01")

tvret4<-tv4 %>%
    tq_transmute(select =adjusted, # el nombre de la columan a selecionar, precio ajustado
                 mutate_fun = periodReturn,
                 period="daily",   #calculo del retorno
                 col_rename = "GFNORTEO.MX_returns") #renombramos la columan

#visualización de datos
View(tv4)

tvret4 %>%
    ggplot(aes(x=date, y = GFNORTEO.MX_returns))+
    geom_line()+
    theme_classic()+
    labs(x="fecha", y ="retnor diario")+
    ggtitle("retorno diario de GFNORTEO.MX")+
    scale_x_date(date_breaks = "years", date_labels = "%Y")+
    scale_y_continuous(breaks = seq(-0.5,0.6,0.05),
                       labels = scales::percent)

#EMPRESA 5#FEMSA "KOF"

tv5<-tq_get("KOF", get = "stock.prices", from ="2020-01-01", to = "2020-08-01")

tvret5<-tv5 %>%
    tq_transmute(select =adjusted, # el nombre de la columan a selecionar, precio ajustado
                 mutate_fun = periodReturn,
                 period="daily",   #calculo del retorno
                 col_rename = "KOF_returns") #renombramos la columan

#visualización de datos
View(tv5)

tvret5 %>%
    ggplot(aes(x=date, y = KOF_returns))+
    geom_line()+
    theme_classic()+
    labs(x="fecha", y ="retnor diario")+
    ggtitle("retorno diario de KOF")+
    scale_x_date(date_breaks = "years", date_labels = "%Y")+
    scale_y_continuous(breaks = seq(-0.5,0.6,0.05),
                       labels = scales::percent)















