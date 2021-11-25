#call the library for using analitic data 
library(tidyverse)
library(readxl)


#

pander(APPLeeff<- read_excel("financials.xlsx"))

plot(APPLeeff$Revenue, col= "Red", xlab= "Años", ylab="Millones en Ingresos",)

#Analizando los ingresos de una compañia
ggplot(APPLeeff, aes(x = APPLeeff$Años, y = APPLeeff$`Revenue-ingresos`)) +
  geom_line(
    color = 4,    # Color de la línea
    lwd = 1,      # Ancho de la línea
    linetype = 1  # El grueso de la liena
  )+
  geom_point()+
  geom_text(aes(label = APPLeeff$`Revenue-ingresos`, x = APPLeeff$Años + 0.7, y = (APPLeeff$`Revenue-ingresos`)))




#Analizando todas las variables de EEFF
(Apple_transpose <- as.data.frame(t(as.matrix(APPLeeff))))

#Saber cuantas fila sy columnas tiene el archivo
dim(Apple_transpose)



plot(Apple_transpose$)


#Anadir nombres de la tada frema
(nombrecolumnas<- APPLeeff[, 1])


# Analizando solo una variable de EEFF ------------------------------------


(Revent<- APPLeeff[1,])


(df_Revent <- data.frame(t(Revent[-1])))

hist(df_Revent)

library(ggplot2)





