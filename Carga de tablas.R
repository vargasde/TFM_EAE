#TFM
#Importar data
a14m0910 <- read.csv("201606_detalle.csv", header = TRUE, sep = ";", fill = FALSE, blank.lines.skip = FALSE)
a14m1112 <- read.csv("201411_12_detalle.csv", header = TRUE, sep = ";", fill = TRUE)
a14 <- rbind(a14m0910, a14m1112)
pru <- a14[,c('HORA',"CALIFICACION","MES","DENUNCIANTE")]
head(pru, 5)
SEPTDEC14 <- pru[pru$CALIFICACION == c('LEVE', 'MUY GRAVE') & pru$MES == c('SEPIEMBRE', 'DICIEMBRE'), ]
head(SEPTDEC14, 5)
head(SEPTDEC14[order(SEPTDEC14$HORA),], 10)

rm(Multas)

folder <- "/Users/Diego/OneDrive/Documents/Maestría/Clases/TFM/Multas/Datos/"      

# path to folder that holds multiple .csv files

file_list <- list.files(path=folder, pattern="*.csv")
list = file_list
# create list of all .csv files in folder

# read in each .csv file in file_list and create a data frame with the same name as the .csv file

for (i in 1:length(file_list)){
  
  assign(file_list[i], 
         
         read.csv(paste(file_list[i]), header = TRUE, sep = ";", fill = TRUE, blank.lines.skip = TRUE)
         
  )}
?read.csv
#Juntar todas las tablas de multas
Multas12v <- rbind(`201602_detalle.csv`, `201601_detalle.csv`, 
                `201512_detalle.csv`, `201511_detalle.csv`, `201510_detalle.csv`, 
                `201509_detalle.csv`, `201508_detalle.csv`, `201507_detalle.csv`, 
                `201506_detalle.csv`, `201505_detalle.csv`, `201504_detalle.csv`, 
                `201503_detalle.csv`, `201502_detalle.csv`, `201501_detalle.csv`, 
                `201411_12_detalle.csv`, `201409_10_detalle.csv`)

#Falta agregar columnas nuevas

write.csv(file_list, file = "Tablafull")

Multas12v$COORDENADA_X <- 'NA'
Multas12v$COORDENADA_Y <- 'NA'

Multas <- rbind(Multas12v,`201809_detalle.csv`, `201808_detalle.csv`, `201807_detalle.csv`, 
                `201806_detalle.csv`, `201805_detalle.csv`, `201804_detalle.csv`, 
                `201803_detalle.csv`, `201802_detalle.csv`, `201801_detalle.csv`, 
                `201712_detalle.csv`, `201711_detalle.csv`, `201710_detalle.csv`, 
                `201709_detalle.csv`, `201708_detalle.csv`, `201707_detalle.csv`, 
                `201706_detalle.csv`, `201705_detalle.csv`, `201704_detalle.csv`, 
                `201703_detalle.csv`, `201702_detalle.csv`, `201701_detalle.csv`, 
                `201612_detalle.csv`, `201611_detalle.csv`, `201610_detalle.csv`, 
                `201609_detalle.csv`, `201608_detalle.csv`, `201607_detalle.csv`)

colnames(Multas12v)
colnames(`201603_detalle.csv`)

#Falta arreglar el 2016 03

colnames(`201603_detalle.csv`)[14] <- "COORDENADA_Y"
colnames(`201603_detalle.csv`)[13] <- "COORDENADA_X"

Multasmad <- rbind(Multas, `201603_detalle.csv`)

#Falta agregar 2016 del 04 al 06

`201604-06_detalle.csv` <- read.csv("201604-06_detalle.csv", header = TRUE, sep = ",", fill = FALSE, blank.lines.skip = FALSE)

#Ahora si queda!

colnames(Multasmad)
colnames(`201604-06_detalle.csv`)
colnames(`201604-06_detalle.csv`)[14] <- "COORDENADA_Y"
colnames(`201604-06_detalle.csv`)[13] <- "COORDENADA_X"

multas <- rbind(Multasmad, `201604-06_detalle.csv`)

