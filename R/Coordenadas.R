multas <- read.csv("MultasMAD.csv", header = TRUE, sep = ",", fill = FALSE, blank.lines.skip = FALSE, nrows = 50)

summary(multas)

multas$Mad <- paste(multas$LUGAR, ' Madrid', sep = '')
summary(multas)

# Geocoding a csv column of "addresses" in R

#load ggmap
library(ggmap)
register_google('AIzaSyBiZ83peM_TO8AczB7cpjKyV0NsRv851ec')

# Initialize the data frame
geocoded <- data.frame(stringsAsFactors = FALSE)
result <- geocode(multas$Mad[1:nrow(multas)], output = "latlona", source = "google")