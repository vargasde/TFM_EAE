library(data.table)
library(dplyr)
library(tidyverse)
library(tidyr)
library(plyr)

multas <- read.csv('det_multas.csv', sep = ',', header = T)

#Arreglar la CALIFICACION PARA QUE SOLO SEAN 3 CATEGORIAS: LEVE, GRAVE Y MUY GRAVE
class(multas$CALIFICACION)
levels(multas$CALIFICACION)
multas$CALIFICACION <- str_trim(multas$CALIFICACION)
      #----------------------------------------------------------------------------------------
      # levels(multas$CALIFICACION'')[levels(multas$CALIFICACION)=="GRAVE     "] <- "GRAVE"
      # levels(multas$CALIFICACION)[levels(multas$CALIFICACION)=="LEVE      "] <- "LEVE"
      # levels(multas$CALIFICACION)[levels(multas$CALIFICACION)=="MUY GRAVE "] <- "MUY GRAVE"

class(multas$CALIFICACION)

#Cambio a character a LUGAR
class(multas$LUGAR)
multas$LUGAR <- str_trim(multas$LUGAR)
multas$LUGAR = as.character(multas$LUGAR)

# ------------------------------------------------------------------

# PRUEBAS PARA EL MES
  
  # verificar que 2014 tengo solo de Sep. en adelante
  #info <- multas %>%
    #filter(ANIO == 2014)
  
  #summary(info)
  
  #info <- distinct(info, MES)

# ------------------------------------------------------------------

# CAMBIO DE NOMBRE DE MES A NUMERO

multas$MES = as.character(multas$MES)
multas$MES <- gsub('ENERO', '1', multas$MES)
multas$MES <- gsub('FEBRERO', '2', multas$MES)
multas$MES <- gsub('SEPTIEMBRE', '9', multas$MES)
multas$MES <- gsub('NOVIEMBRE', '11', multas$MES)
multas$MES <- gsub('DICIEMBRE', '12', multas$MES)
multas$MES <- gsub('OCTUBRE', '10', multas$MES)
multas$MES <- gsub('SEPTIEMBRE', '9', multas$MES)
multas$MES = as.integer(multas$MES)

#----------------------------------------------------------------------------------------
      #CODIGO PARA AGREGAR COLUMNA DE MES Y ANIO
      #Columna con mes y anio
      #library(zoo) #Pasarlo a fecha y que quede todo con 1 del mes (evolucion temporal)
      #multas$FECHA <- as.yearmon(paste(multas$ANIO, multas$MES, sep = "-"))
#----------------------------------------------------------------------------------------

#Modificar la hora
    #multasprueb <- head(multas, 100000)

multas$HORA = as.character.numeric_version(multas$HORA)
multas$HORA <- chartr(".", ":", multas$HORA)
head(multas$HORA)

#----------------------------------------------------------------------------------------
      #CODIGO PARA PROBAR FORMULAS DE FECHA
      #ANIO <- multasprueb$ANIO
      #MES <- multasprueb$MES
      #HORA <- multasprueb$HORA
      
      
      # Create a data frame from the vectors
      #DATETIME <- data.frame(ANIO, MES, HORA)
      
      #head(DATETIME)
      #class(DATETIME$FECHA)

      # Unir fecha y hora como caracter
      #DATETIME$FECHA <- paste(DATETIME$ANIO, DATETIME$MES, 01, sep = '-')
      #DATETIME$FECHA <- paste(DATETIME$FECHA, DATETIME$HORA, sep = ' ')
      
      # Convierte a DATETIME o DATE
      #DATE
      #DATETIME$FECHA <- as.Date(DATETIME$FECHA, "%Y-%m-%d")
      #DATETIME
      #DATETIME$FECHA <- as.POSIXct(DATETIME$FECHA)
#------------------------------------------------------------------------------------------

# UNIR FECHA Y HORA COMO CHARACTER
multas$FECHA <- paste(multas$ANIO, multas$MES, 01, sep = '-') 
multas$FECHA <- paste(multas$FECHA, multas$HORA, sep = ' ')
head(multas$FECHA)

# Convierte a DATETIME o DATE
  #DATE
#DATETIME$FECHA <- as.Date(DATETIME$FECHA, "%Y-%m-%d")
  #DATETIME
multas$FECHA <- as.POSIXct(multas$FECHA)
class(multas$FECHA)
multas$HORA <- NULL

# ORDENAR DATA FRAME Y ASIGNAR PK A CADA MULTA
#    multas <- multas[order(multas$FECHA),]
#    multas$id_mul <- paste('m', 1:nrow(multas), sep = '')


#Convertir DESCUENTO a caracter
class(multas$DESCUENTO)
summary(multas$DESCUENTO)
multas$DESCUENTO <- gsub('SI', 1, multas$DESCUENTO)
multas$DESCUENTO <- gsub('NO', 0, multas$DESCUENTO)
multas$DESCUENTO = as.integer(multas$DESCUENTO)

# ARREGLAR NOMBRES DE DENUNCIANTES
class(multas$DENUNCIANTE)
levels(multas$DENUNCIANTE)
multas$DENUNCIANTE <- str_trim(multas$DENUNCIANTE)
#----------------------------------------------------------------------------------
   # levels(multas$DENUNCIANTE)[levels(multas$DENUNCIANTE)=="SER                 "] <- 'SER'
   # levels(multas$DENUNCIANTE)[levels(multas$DENUNCIANTE)=="POLICIA MUNICIPAL   "] <- 'POLICIA MUNICIPAL'
   # levels(multas$DENUNCIANTE)[levels(multas$DENUNCIANTE)=="SACE                "] <- 'SACE'

multas$DENUNCIANTE = as.character(multas$DENUNCIANTE)

# CREACION DE TABLA DENUNCIATES Y MERGE CON DETALLE DE MULTAS
denunciante <- distinct(multas, DENUNCIANTE)
denunciante$id <- paste('d', 1:nrow(denunciante), sep = '')
colnames(denunciante) <- c('denunciante','id_den')
denunciante <- denunciante[c(2,1)]
multas <- merge(multas, denunciante,by.x = "DENUNCIANTE", by.y = "denunciante")
colnames(multas)[17] <- 'denunciante'

# CREACION DE TABLA CALIFICACION
calificacion <- distinct(multas, CALIFICACION)
calificacion$id_cal <- paste('c', 1:nrow(calificacion), sep = '')
colnames(calificacion) <- c('calificacion','id_cal')
calificacion <- calificacion[c(2,1)]
multas <- merge(multas, calificacion, by.x = "CALIFICACION", by.y = "calificacion")
colnames(multas)[18] <- 'calificacion'


#--------------------------------------------------------------------------------------------------------------------------------
      
reasons <- count(multas, multas$HECHO.BOL)
colnames(reasons) <- c("Reason", 'count')
#reasons$Motivo <- (do.call('rbind', strsplit(as.character(reasons$Reason),': '))) #Contar el numero de elementos que devuelve split y si es igual a 1 dejarlo vacio
      
#n = 1
#for (i in reasons$Reason) {
#    reasons$elemcadena[n] <- length(strsplit(as.character(reasons$Reason), ": ")[[n]])
#  n = n+1
#}

#grepl("*BICICLETA*", reasons$Reason[78])

c = 1

for (i in 1:nrow(reasons)) {
  if(print(grepl("*ALCOHOL*|*ALCOHOLEMIA*|*EMBRIAGUEZ*|*ALCOHOL*", reasons$Reason[c]) == TRUE)) {
    reasons$Motivo[c] <- 'ALCOHOLEMIA'
  } else if(print(grepl("*CONTAMINACI�N*|*GASES*", reasons$Reason[c]) == TRUE)) {
    reasons$Motivo[c] <- 'VIOLAR RESTRICCIONES POR CONTAMINACION'
  } else if(print(grepl("*ESTACIONAR EN LUGAR PROHIBIDO*|*ESTACIONAR EN ZONA PROHIBIDA*", reasons$Reason[c]) == TRUE)) {
    reasons$Motivo[c] <- 'ESTACIONAMIENTO PROHIBIDO'
  } else if(print(grepl("*ESTACIONAR*", reasons$Reason[c]) == TRUE)) {
    reasons$Motivo[c] <- 'ESTACIONAMIENTO NEGLIGENTE'
  } else if(print(grepl("*MEDIDA*|*ABANDONA PUESTO*|*ABANDONO EL PUESTO*|*ABANDONO PUESTO*|
                        *ABANDONA EL PUESTO*|*ABANDONA EL VHO*|*ABANDONA VHO*|*ABANDONAR EL VHO*|*ABANDONAR VHO*|
                        *ABANDONAR EL PUESTO*|*ABANDONAR PUESTO*|*ABANDONA COCHE*", reasons$Reason[c]) == TRUE)) {
    reasons$Motivo[c] <- 'ESTACIONAMIENTO NEGLIGENTE'
  } else if(print(grepl("*SENTIDO CONTRARIO*|*CONTRARIA*|*DIRECCION PROHIBIDA*|*SENTIDO PROHIBIDO*|*DIRECCI�N PROHIBIDO*|*DIRECCI�N PROHIBIDA*", reasons$Reason[c]) == TRUE)) {
    reasons$Motivo[c] <- 'CIRCULACION EN SENTIDO CONTRARIO O PROHIBIDO'
  } else if(print(grepl("*VELOCIDAD*|*ACELER*|\\bMAS DE\\b|\\bCIRCULA A\\b", reasons$Reason[c]) == TRUE)) {
    reasons$Motivo[c] <- 'VELOCIDAD O ACELERACION INDEBIDA'
  } else if(print(grepl("*PRIORIDAD DE PASO*|*PREFERENCIA DE PASO*", reasons$Reason[c]) == TRUE)) {
    reasons$Motivo[c] <- 'PRIORIDAD DE PASO' 
  } else if(print(grepl("*SEMAFORO ROJO*|*SEMAFORO EN ROJO*|*FASE ROJA*|*REBASAR*SEMAFOROS*|*SALTA*SEMAFORO*|*SEMAFORO*", reasons$Reason[c]) == TRUE)) {
    reasons$Motivo[c] <- 'ILEGALIDAD EN SEMAFOROS'
  } else if(print(grepl("*REBASAR*|*ADELANTAR*", reasons$Reason[c]) == TRUE)) {
    reasons$Motivo[c] <- 'REBASAR INDEBIDAMENTE'
  } else if(print(grepl("*SE�AL*|*VIALES*|*CIRCULACI�N RESERVADA*", reasons$Reason[c]) == TRUE)) {
    reasons$Motivo[c] <- 'OMISION DE SENALIZACION VIAL O DE CONDUCCION'
  } else if(print(grepl("*OCUPANTE*", reasons$Reason[c]) == TRUE)) {
    reasons$Motivo[c] <- 'EXCESO DE OCUPANTES'
  } else if(print(grepl("*PANTALLA*|*TELEFONO*", reasons$Reason[c]) == TRUE)) {
    reasons$Motivo[c] <- 'USO DE DISPOSITIVO ELECTRONICO'
  } else if(print(grepl("\\bMOVIL\\b|*OTROS DISPOSITIVOS*", reasons$Reason[c]) == TRUE)) {
    reasons$Motivo[c] <- 'USO DE DISPOSITIVO ELECTRONICO'
  } else if(print(grepl("*NI�@*|*MENOR*", reasons$Reason[c]) == TRUE)) {
    reasons$Motivo[c] <- 'PELIGRO A MENORES'
  } else if(print(grepl("\\bKM\\b|\\bKMH\\b|\\bKMS\\b", reasons$Reason[c]) == TRUE)) {
    reasons$Motivo[c] <- 'VELOCIDAD O ACELERACION INDEBIDA'
  } else if(print(grepl("*CARGA*|*DESCARGA*|*ANIMALES*", reasons$Reason[c]) == TRUE)) {
    reasons$Motivo[c] <- 'TRANSPORTE DE PASAJEROS, OBJETOS O ANIMALES'
  } else if(print(grepl("*CINTUR*", reasons$Reason[c]) == TRUE)) {
    reasons$Motivo[c] <- 'CINTURON DE SEGURIDAD'
  } else if(print(grepl("*DROGA*", reasons$Reason[c]) == TRUE)) {
    reasons$Motivo[c] <- 'DROGAS'
  } else if(print(grepl("\\bCASCO\\b", reasons$Reason[c]) == TRUE)) {
    reasons$Motivo[c] <- 'EQUIPO PROTECTOR'
  } else if(print(grepl("*MARCHA ATR*", reasons$Reason[c]) == TRUE)) {
    reasons$Motivo[c] <- 'MARCHA ATRAS'
  } else if(print(grepl("*PEATONES*", reasons$Reason[c]) == TRUE)) {
    reasons$Motivo[c] <- 'OBSTACULIZACION A PEATONES'
  } else if(print(grepl("*ZIG ZAG*", reasons$Reason[c]) == TRUE)) {
    reasons$Motivo[c] <- 'CONDUCCION NEGLIGENTE ZIG ZAG'
  } else if(print(grepl("\\bPARAR\\b|\\bPARARSE\\b|\\bALTO\\b", reasons$Reason[c]) == TRUE)) {
    reasons$Motivo[c] <- 'OMISION DE SENALIZACION VIAL O DE CONDUCCION' #Indicar que esto puede ser porque para donde no debe o porque no para cuando le indican.
  } else if(print(grepl("*CAMI?N*", reasons$Reason[c]) == TRUE)) {
    reasons$Motivo[c] <- 'EXCESO DE PESO CAMION'
  } else if(print(grepl("\\bPERSONAS\\b", reasons$Reason[c]) == TRUE)) {
    reasons$Motivo[c] <- 'TRANSPORTE DE PASAJEROS, OBJETOS O ANIMALES'
  } else if(print(grepl("*CARRIL*", reasons$Reason[c]) == TRUE)) {
    reasons$Motivo[c] <- 'USO INDEBIDO DE CARRILES'
  } else if(print(grepl("*BICICLETA*|*MONOPATINES*", reasons$Reason[c]) == TRUE)) {
    reasons$Motivo[c] <- 'OTROS BICICLETAS'
  } else if(print(grepl("*MONOPATINES*", reasons$Reason[c]) == TRUE)) {
    reasons$Motivo[c] <- 'OTROS MONOPATINES'
  } else if(print(grepl("*MOTOCICLETA*|*CICLOMOTOR", reasons$Reason[c]) == TRUE)) {
    reasons$Motivo[c] <- 'OTROS MOTOCICLETA Y CICLOMOTOR'
    } else {
    reasons$Motivo[c] <- 'OTROS'
  }
  c = c + 1
}

a = 1

for (i in 1:nrow(reasons)) {
  if(print(grepl("*MONOPATIN*|\\bPATIN\\b", reasons$Reason[a]) == TRUE)) {
    reasons$Vehiculo_implicado[a] <- 'MONOPATIN, PATIN O SIMILAR'
  } else if(print(grepl("\\bDOS RUEDA\\b|\\bDOS RUEDAS\\b|\\bMOTO\\b|\\bCICLOMOTOR\\b|\\bMOTOCICLETA\\b", reasons$Reason[a]) == TRUE)) {
    reasons$Vehiculo_implicado[a] <- 'MOTOCICLETA'
  } else if(print(grepl("\\bBICI*|*CICLISTA*|*BICICLETA*", reasons$Reason[a]) == TRUE)) {
    reasons$Vehiculo_implicado[a] <- 'BICICLETA'
  } else {
    reasons$Vehiculo_implicado[a] <- 'AUTOMOTOR'
  }
  a = a + 1
}

# CREACION DE TABLA DE TIPO DE MULTA
tipo_mul <- count(reasons, reasons$Motivo)
tipo_mul$id_tip <- paste('t', 1:nrow(tipo_mul), sep = '')
tipo_mul[2] <- NULL
tipo_mul <- tipo_mul[c(2,1)]
colnames(tipo_mul) <- c('id_tipo','tipo_multa')


# CREACION DE TABLA DE VEHICULOS
vehiculos <- count(reasons, reasons$Vehiculo_implicado)
vehiculos$id_veh <- paste('v', 1:nrow(vehiculos), sep = '')
vehiculos[2] <- NULL
vehiculos <- vehiculos[c(2,1)]
colnames(vehiculos) <- c('id_veh','vehiculo')

# CREAR TABLA DE RAZONES DE MULTA CON CODIGO PARA HACER MERGE CON EL DETALLE
reasons_cod <- merge(reasons, vehiculos, by.x = "Vehiculo_implicado", by.y = "vehiculo")
reasons_cod <- merge(reasons_cod, tipo_mul, by.x = "Motivo", by.y = "tipo_multa")
reasons_cod[c(1,2,4)] <- NULL
colnames(reasons_cod)[1] <- 'HECHO.BOL'

# MERGE DE CODIGOS CON DETALLE DE MULTAS
multas <- merge(multas,reasons_cod, by.x = "HECHO.BOL", by.y = "HECHO.BOL")
det_multas <- merge(multas,reasons_cod, by.x = "HECHO.BOL", by.y = "HECHO.BOL") # Tarda aprox 11min
colnames(multas)[c(19,20)] <- c('vehiculo', 'tipo')

# ELIMINACION DE COLUMNAS
multas[c('DENUNCIANTE','CALIFICACION','HORA')] <- NULL
colnames(det_multas)[c(15,16)] <- c('DENUNCIANTE', 'CALIFICACION')

colnames(det_multas)[c(11,12,14,15,16,17,18)] <- c('velocidad_limite', 'velocidad_circulacion', 'tipo_multa',
                                                 'coordenada_x', 'coordenada_y', 'lonx', 'laty')

colnames(multas)[c(1,2,3,4,5,6,7,8,9,10,11,14)] <- c('hecho', 'lugar', 'mes', 'anio',
                                                     'monto', 'descuento', 'puntos', 'vel_limite',
                                                  'vel_circula', 'coordenada_x', 'coordenada_y', 'fecha')


colnames(det_multas)   
# BORRAR TABLAS DE PRUEBA
rm(DATETIME,multasprueb,multaspruebraz,reasons_cod)

multas_sample <- dplyr::sample_n(multas, 500000, replace = TRUE)
write.csv(multas_sample, 'SampleMultasPython.csv', row.names = FALSE)

det_multas <- multas[c(14,3,4,1,2,15,5,6,7,16,8,9,17,18,12,13,10,11)]

det_multas <- det_multas %>%
  arrange(fecha)

det_multas$id_mul <- 1:nrow(det_multas)
det_multas <- det_multas[c(19,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18)]

edit_coord <- det_multas[c(1,6,18,19)]
det_multas[c(18,19)] <- NULL

mapcodes <- write.csv('geocodegrave.csv', sep = ',', header = T,)
write.csv(edit_coord, file = "edit_coord.csv", row.names = FALSE)
write.csv(det_multas, file = "det_multas.csv", row.names = FALSE)
write.csv(calificacion, file = "calificacion.csv", row.names = FALSE)
write.csv(denunciante, file = "denunciante.csv", row.names = FALSE)
write.csv(tipo_mul, file = "tipo_mul.csv", row.names = FALSE)
write.csv(vehiculos, file = "vehiculos.csv", row.names = FALSE)

class(det_multas$coordenada_x)

#----------------------------------------------------------------------------------------------------------------

#MUESTRA DE LA TABLA
tipo_mul <- tipo_mul %>%
  filter(id_tipo != 't8')

#-------------------------------------------------------------------------------------------------

# CAMBIAR EL NOMBRE DEL TIPO DE MULTA DE ESTACIONAMIENTO Y APLICAR EL CAMBIO EN EL DETALLE DE MULTAS
tipo_mul$id_tipo <- str_replace(tipo_mul$tipo_multa, "ESTACIONAMIENTO NEGLIGENTE", "ESTACIONAMIENTO")
write.csv(tipo_mul, file = 'tipo_mul.csv', row.names = FALSE)
rm(tipo_mul)

sum(multas_mad$tipo_multa == 't24')
t8 <- multas_mad %>%
  filter(tipo_multa == 't8')
multas_mad$tipo_multa <- str_replace(multas_mad$tipo_multa, "t8", "t7")
