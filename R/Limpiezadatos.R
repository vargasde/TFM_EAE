library(data.table)
library(dplyr)

multas <- read.csv('MultasMAD.csv', sep = ',', header = T, row.names = 'X')

#Arreglar la CALIFICACION PARA QUE SOLO SEAN 3 CATEGORIAS: LEVE, GRAVE Y MUY GRAVE
levels(multas$CALIFICACION)
levels(multas$CALIFICACION)[levels(multas$CALIFICACION)=="GRAVE     "] <- "GRAVE"
levels(multas$CALIFICACION)[levels(multas$CALIFICACION)=="LEVE      "] <- "LEVE"
levels(multas$CALIFICACION)[levels(multas$CALIFICACION)=="MUY GRAVE "] <- "MUY GRAVE"

multas$CALIFICACION = as.character(multas$CALIFICACION)
class(multas$CALIFICACION)

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

# Unir fecha y hora como caracter
multas$FECHA <- paste(multas$ANIO, multas$MES, 01, sep = '-') 2016-12-01 17:30
multas$FECHA <- paste(multas$FECHA, multas$HORA, sep = ' ')
head(multas$FECHA)

# Convierte a DATETIME o DATE
  #DATE
#DATETIME$FECHA <- as.Date(DATETIME$FECHA, "%Y-%m-%d")
  #DATETIME
multas$FECHA <- as.POSIXct(multas$FECHA)
class(multas$FECHA)
class(multas$IFECHA)

#Convertir DESCUENTO a caracter
class(multas$DESCUENTO)
multas$DESCUENTO = as.character(multas$DESCUENTO)

#Arreglar los nombres de denunciantes
class(multas$DENUNCIANTE)
levels(multas$DENUNCIANTE)
levels(multas$DENUNCIANTE)[levels(multas$DENUNCIANTE)=="SER                 "] <- 'SER'
levels(multas$DENUNCIANTE)[levels(multas$DENUNCIANTE)=="POLICIA MUNICIPAL   "] <- 'POLICIA MUNICIPAL'
levels(multas$DENUNCIANTE)[levels(multas$DENUNCIANTE)=="SACE                "] <- 'SACE'
multas$DENUNCIANTE = as.character(multas$DENUNCIANTE)


#--------------------------------------------------------------------------------------------------------------------------------
      
reasons <- count(multas, multas$HECHO.BOL)
colnames(reasons) <- c("Reason", 'count')
reasons$Motivo <- (do.call('rbind', strsplit(as.character(reasons$Reason),': '))) #Contar el numero de elementos que devuelve split y si es igual a 1 dejarlo vacio
      
n = 1
for (i in reasons$Reason) {
    reasons$elemcadena[n] <- length(strsplit(as.character(reasons$Reason), ": ")[[n]])
  n = n+1
}

grepl("*BICICLETA*", reasons$Reason[78])

c = 1
for (i in 1:6242) {
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
  } else if(print(grepl("*SENTIDO CONTRARIO*|*CONTRARIA*|*DIRECCION PROHIBIDA*|*SENTIDO PROHIBIDO*|*DIRECCIÓN PROHIBIDO*", reasons$Reason[c]) == TRUE)) {
    reasons$Motivo[c] <- 'CIRCULACION EN SENTIDO CONTRARIO O PROHIBIDO'
  } else if(print(grepl("*VELOCIDAD*|*ACELER*|\\bMAS DE\\b|\\bCIRCULA A\\b", reasons$Reason[c]) == TRUE)) {
    reasons$Motivo[c] <- 'VELOCIDAD O ACELERACION INDEBIDA'
  } else if(print(grepl("*PRIORIDAD DE PASO*|*PREFERENCIA DE PASO*", reasons$Reason[c]) == TRUE)) {
    reasons$Motivo[c] <- 'PRIORIDAD DE PASO' 
  } else if(print(grepl("*SEMAFORO ROJO*|*SEMAFORO EN ROJO*|*FASE ROJA*|*REBASAR*SEMAFOROS*|*SALTA*SEMAFORO*|*SEMAFORO*", reasons$Reason[c]) == TRUE)) {
    reasons$Motivo[c] <- 'ILEGALIDAD EN SEMAFOROS'
  } else if(print(grepl("*REBASAR*|*ADELANTAR*", reasons$Reason[c]) == TRUE)) {
    reasons$Motivo[c] <- 'REBASAR INDEBIDAMENTE'
  } else if(print(grepl("*SEÑAL*|*VIALES*|*CIRCULACIÓN RESERVADA*", reasons$Reason[c]) == TRUE)) {
    reasons$Motivo[c] <- 'OMISION DE SEÁLIZACIÓN VIAL O DE CONDUCCIÓN'
  } else if(print(grepl("*OCUPANTE*", reasons$Reason[c]) == TRUE)) {
    reasons$Motivo[c] <- 'EXCESO DE OCUPANTES'
  } else if(print(grepl("*PANTALLA*|*TELEFONO*", reasons$Reason[c]) == TRUE)) {
    reasons$Motivo[c] <- 'USO DE DISPOSITIVO ELECTRONICO'
  } else if(print(grepl("\\bMOVIL\\b|*OTROS DISPOSITIVOS*", reasons$Reason[c]) == TRUE)) {
    reasons$Motivo[c] <- 'USO DE DISPOSITIVO ELECTRONICO'
  } else if(print(grepl("*NIÑ@*|*MENOR*", reasons$Reason[c]) == TRUE)) {
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
    reasons$Motivo[c] <- 'OMISION DE SEÁLIZACIÓN VIAL O DE CONDUCCIÓN' #Indicar que esto puede ser porque para donde no debe o porque no para cuando le indican.
  } else if(print(grepl("*CAMIÓN*", reasons$Reason[c]) == TRUE)) {
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

c = 1
for (i in reasons$Reason) {
  if(print(grepl("*MONOPATIN*|\\bPATIN\\b", reasons$Reason[c]) == TRUE)) {
    reasons$Vehiculo_implicado[c] <- 'MONOPATIN, PATIN O SIMILAR'
  } else if(print(grepl("\\bDOS RUEDA\\b|\\bDOS RUEDAS\\b|\\bMOTO\\b|\\bCICLOMOTOR\\b|\\bMOTOCICLETA\\b", reasons$Reason[c]) == TRUE)) {
    reasons$Vehiculo_implicado[c] <- 'MOTOCICLETA'
  } else if(print(grepl("\\bBICI*|*CICLISTA*|*BICICLETA*", reasons$Reason[c]) == TRUE)) {
    reasons$TVehiculo_implicado[c] <- 'BICICLETA'
  } else {
    reasons$TVehiculo_implicado[c] <- 'AUTOMOTOR'
  }
  c = c + 1
}



colnames(reasons)
reasons[2] <- NULL
colnames(reasons) <- c("HECHO.BOL", 'count', 'Motivo', 'Vehiculo_implicado')
multaspruebraz <- merge(multasprueb,reasons, by.x = "HECHO.BOL", by.y = "HECHO.BOL")
?order_by

colnames(reasons)
reasons[2] <- NULL
colnames(reasons) <- c("HECHO.BOL", 'count', 'Motivo', 'Vehiculo_implicado')
det_multas <- merge(multas,reasons, by.x = "HECHO.BOL", by.y = "HECHO.BOL")

head(det_multas)

Motivos <- count(reasons, reasons$Motivo)


#-------------------------------------------------------------------------------------------------