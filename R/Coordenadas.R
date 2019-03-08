install.packages('tidyverse')
library(tidyverse)
library(tidyr)
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

geocodeformat <- multas %>%
  arrange(coordenada_x)
colnames(geocodeformat)
geocodeformat <- geocodeformat[c(10,11,12,13)]

geocodeformat2 <- geocodeformat %>%
  filter(!is.na(coordenada_x))

geocodeformat2 <- unite(geocodeformat2, codes, coordenada_x, coordenada_y, sep = "/")

geocodeformat2 <- distinct(geocodeformat2, codes, .keep_all = TRUE)

geocodeformat2 <- separate(geocodeformat2, col = codes, into = c("east", "north"), sep = "/")

geocodeformat2$zone <- 32

geocodeformat2 <- geocodeformat2[c(2,1,5,3,4)]

geocodeformat3 <- geocodeformat2[c(1,2,3)]

geocodeformat3 <- separate(geocodeformat3, col = north, into = c("north", "bad"), sep = ",")

geocodeformat3$north = as.integer(geocodeformat3$north)
geocodeformat3$east = as.integer(geocodeformat3$east)
class(geocodeformat3$east)

write.csv(geocodeformat3, 'convertcoord.csv', row.names=FALSE)

for(i in 1:nrow(geocodeformat2)) {
  # Print("Working...")
  result <- geocode(geocodeformat2$codes[i], output = "latlona", source = "google")
  address2$lon_x[i] <- as.numeric(result[1])
  address2$lat_y[i] <- as.numeric(result[2])
  address2$geoAddress[i] <- as.character(result[3])
}

