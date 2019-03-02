#TFM
#Importar data individual
# multas <- read.csv("MultasMAD.csv", header = TRUE, sep = ",", fill = FALSE, blank.lines.skip = FALSE)

#Path para ubicacion de archivos

folder<- "Users/Diego/OneDrive/Documents/Maestr?a/Clases/TFM/Codigos/TFM_EAE/Datos"

#Lista de archivos .csv a hacer read
file_list <- list.files(path=folder, pattern="*.csv")

# Loop para leer cada archivo y crear un dataframe con el mismo nombre del .csv
for (i in 1:length(file_list)){
  
  assign(file_list[i], 
      
         read.csv(paste(file_list[i]), header = TRUE, sep = ";", fill = TRUE, blank.lines.skip = TRUE)
         
  )}

#Juntar todas las tablas de multas de 12 columnas
Multas12v <- rbind(`201602_detalle.csv`, `201601_detalle.csv`, 
                `201512_detalle.csv`, `201511_detalle.csv`, `201510_detalle.csv`, 
                `201509_detalle.csv`, `201508_detalle.csv`, `201507_detalle.csv`, 
                `201506_detalle.csv`, `201505_detalle.csv`, `201504_detalle.csv`, 
                `201503_detalle.csv`, `201502_detalle.csv`, `201501_detalle.csv`, 
                `201411_12_detalle.csv`, `201409_10_detalle.csv`)

#Agregar dos columnas a la tabla creada
Multas12v$COORDENADA_X <- 'NA'
Multas12v$COORDENADA_Y <- 'NA'
head(Multas12v)

#Juntamos la nueva tabla con los archivos de 14 columnas
Multas <- rbind(Multas12v,`201809_detalle.csv`, `201808_detalle.csv`, `201807_detalle.csv`, 
                `201806_detalle.csv`, `201805_detalle.csv`, `201804_detalle.csv`, 
                `201803_detalle.csv`, `201802_detalle.csv`, `201801_detalle.csv`, 
                `201712_detalle.csv`, `201711_detalle.csv`, `201710_detalle.csv`, 
                `201709_detalle.csv`, `201708_detalle.csv`, `201707_detalle.csv`, 
                `201706_detalle.csv`, `201705_detalle.csv`, `201704_detalle.csv`, 
                `201703_detalle.csv`, `201702_detalle.csv`, `201701_detalle.csv`, 
                `201612_detalle.csv`, `201611_detalle.csv`, `201610_detalle.csv`, 
                `201609_detalle.csv`, `201608_detalle.csv`, `201607_detalle.csv`)

#Verificacion de un archivo con problema en el nombre
colnames(Multas12v)
colnames(`201603_detalle.csv`)

#Arreglamos columnas de archivo con problemas
colnames(`201603_detalle.csv`)[14] <- "COORDENADA_Y"
colnames(`201603_detalle.csv`)[13] <- "COORDENADA_X"

#Juntamos el archivo corregido con la tabla anterior
Multasmad <- rbind(Multas, `201603_detalle.csv`)

#Quitamos dos columnas que estan en los archivos de 2014 de abril a junio
`201604-06_detalle.csv` <- read.csv("201604-06_detalle.csv", header = TRUE, sep = ";", fill = FALSE, blank.lines.skip = FALSE)

#Agregamos los ultimos dataframes al consolidado
multas <- rbind(Multasmad, `201604-06_detalle.csv`)

#Escribimos el .csv final
write.csv(multas, "MultasMAD.csv")

