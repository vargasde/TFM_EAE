install.packages('tidyverse')
library(tidyverse)
library(tidyr)
library(plyr)
address <- distinct(address, LUGAR)
address$direccion <- paste(address$LUGAR, ', Madrid', sep = '')

multas2 <- multas %>%
  filter(calificacion == 'c3')
address2 <- multas2[6]
address2 <- distinct(address2, LUGAR)

# Geocoding a csv column of "addresses" in R

#load ggmap
library(ggmap)
register_google('AIzaSyDJT1IiUfMgeHV1nDnZHo2fNVBtcdl3qqY')

# Initialize the data frame
geocoded <- data.frame(stringsAsFactors = FALSE)

for(i in 1:nrow(address2)) {
  # Print("Working...")
  result <- geocode(address2$direccion[i], output = "latlona", source = "google")
  address2$lon[i] <- as.numeric(result[1])
  address2$lat[i] <- as.numeric(result[2])
  address2$geoAddress[i] <- as.character(result[3])
}

for (i in 1:nrow(address2)) {
  if(print(stringr::str_count(address2$geoAddress[i], pattern = ',') == 1)) {
    address2$lon[i] <- NA
    address2$lat[i] <- NA
  } else if(print(stringr::str_detect(address2$geoAddress[i], 'madrid') == TRUE)) {
  } else {
    address2$lon[i] <- NA
    address2$lat[i] <- NA
  }
}

address3 <- address2 %>%
  filter(geoAddress == 'paseo de santa maría de la cabeza, 41, 28045 madrid, spain')

for(i in 1:nrow(address3)) {
  result2 <- geocode(address3$direccion[i], output = "latlona", source = "google")
  address3$lon[i] <- as.numeric(result2[1])
  address3$lat[i] <- as.numeric(result2[2])
}

address_fin <- dplyr::bind_rows(address2, address3)

geocodes <- address_fin %>%
  filter(!is.na(lon))

geocodes <- geocodes[c(1,3,4)]
mapcodes <- distinct(geocodes, LUGAR, .keep_all = TRUE)

mapcodes <- read.csv('geocodegrave.csv', sep = ',', header = T)
class(mapcodes$LUGAR)
mapcodes$LUGAR = as.character(mapcodes$LUGAR)
multas <- dplyr::left_join(multas, mapcodes, by = "LUGAR")
write.csv(multas, 'MultasMAD_Python.csv', row.names=FALSE)

#--------------------------------------------------------------------------------------------


geocodeformat <- na.omit(edit_coord)
geocodeformat$lonx <- gsub("[^0-9]", "", geocodeformat$lonx)
geocodeformat$laty <- gsub("[^0-9]", "", geocodeformat$laty)

geocodeformat$lonx <- sub("(.{6})(.*)", "\\1.\\2", geocodeformat$lonx)
geocodeformat$laty <- sub("(.{7})(.*)", "\\1.\\2", geocodeformat$laty)

geocodeformat <- unite(geocodeformat, codes, lonx, laty, sep = "/")

geocodeformat2 <- distinct(geocodeformat, lugar, .keep_all = TRUE)

geocodeformat2 <- separate(geocodeformat2, col = codes, into = c("east", "north"), sep = "/")

geocodeformat2$zone <- 30

geocodeformat2 <- geocodeformat2[c(1,2,4,3,5)]

write.csv(geocodeformat2, 'convertcoord.csv', row.names=FALSE)
coordenadas <- read.csv('convertcoord.csv', header = TRUE, sep = ',', dec = '.')

coordenadas$lugar <- str_trim(coordenadas$lugar)
coordenadas <- coordenadas[c(2, 5, 6)]
colnames(coordenadas)[c(2, 3)] <- c('coordenada_x', 'coordenada_y')

coordenadas_det <- det_multas[c(6, 16, 17)]
coordenadas_det <- na.omit(coordenadas_det)
coordenadas_det$lugar <- str_trim(coordenadas_det$lugar)
coordenadas_det <- distinct(coordenadas_det, lugar, .keep_all = TRUE)

coordenadas_xy <- dplyr::union(coordenadas_det, coordenadas)
coordenadas_xy <- distinct(coordenadas_xy, lugar, .keep_all = TRUE)

det_multas <- det_multas[(1:15)]
det_multas$lugar <- str_trim(det_multas$lugar)

multas_mad <- dplyr::left_join(det_multas, coordenadas_xy, by = c('lugar'), all.x=T)

prueba <- multas_mad %>%
  filter(lugar == 'MIGUEL SERVET 1')
rm(prueba)

sum(!is.na(multas_mad$coordenada_x))-sum(!is.na(multas$coordenada_x))
sum(!is.na(multas_mad$coordenada_x))+sum(is.na(multas_mad$coordenada_x))
