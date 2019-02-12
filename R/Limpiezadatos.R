library(data.table)

multas <- read.csv('MultasMAD.csv', sep = ',', header = T, row.names = 'X')

#Arreglar la CALIFICACION PARA QUE SOLO SEAN 3 CATEGORIAS: LEVE, GRAVE Y MUY GRAVE
levels(multas$CALIFICACION)
levels(multas$CALIFICACION)[levels(multas$CALIFICACION)=="GRAVE     "] <- "GRAVE"
levels(multas$CALIFICACION)[levels(multas$CALIFICACION)=="LEVE      "] <- "LEVE"
levels(multas$CALIFICACION)[levels(multas$CALIFICACION)=="MUY GRAVE "] <- "MUY GRAVE"
#Cambiar el orden de los factores
multas$CALIFICACION <- ordered(multas$CALIFICACION, levels = c("LEVE", "GRAVE", "MUY GRAVE"))

multas$CALIFICACION = as.character(multas$CALIFICACION)
class(multas$CALIFICACION)
summary(multasprueb)

#Cambio a character a LUGAR
class(multas$LUGAR)
multas$LUGAR = as.character(multas$LUGAR)

#Cambiar los nombres de meses a numeros
class(multas$MES)
levels(multas$MES)
levels(multas$MES)[levels(multas$MES)=="DICIEMBRE"] <- 12
levels(multas$MES)[levels(multas$MES)=="ENERO"] <- 1
levels(multas$MES)[levels(multas$MES)=="FEBRERO"] <- 2
levels(multas$MES)[levels(multas$MES)=="NOVIEMBRE"] <- 11
levels(multas$MES)[levels(multas$MES)=="OCTUBRE"] <- 10
levels(multas$MES)[levels(multas$MES)=="SEPTIEMBRE"] <- 9

multas$MES = as.integer(multas$MES)

#Columna con mes y anio

library(zoo)
multas$FECHA <- as.yearmon(paste(multas$ANIO, multas$MES, sep = "-"))

#Modificar la hora
multasprueb <- head(multas, 50)
multasprueb$HORA = as.character.numeric_version(multasprueb$HORA)

multasprueb$HORA <- chartr(".", ":", multasprueb$HORA)
head(multasprueb)

multasprueb$HORAS <- as.POSIXct(multasprueb$HORA,format="%H:%M")


#HACER ESTO HASTA QUE SE CONVIERTA EL TEXTO DEL MES A NUMERO
multas$MES = as.numeric(multas$MES)
class(multas$CALIFICACION)
summary(multas)
str(multas$MES)
head(multas[multas$ANIO == 2014,][c('MES','CALIFICACION')])

#Podemos convertir a factor todo y cambiar 
#el valor de los valores de la columna y despues volverlo a convertir a character

multas[multas$COORDENADA_X > 0,]
