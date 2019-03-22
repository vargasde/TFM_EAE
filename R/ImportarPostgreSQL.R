install.packages('DBI')
library(RPostgres)
library(DBI)
library(data.table)
library(dplyr)
library(stringr)

pw <- {
  "osna8tzv"
}

#GCP
gcp <- dbConnect(RPostgres::Postgres()
                 , host='35.228.88.227'
                 , port='5432'
                 , dbname='tfmeae'
                 , user='vargasde'
                 , password=pw)

#AWS
aws <- dbConnect(RPostgres::Postgres(), 
                   host='multastfm.cjdbvyorvhjj.us-east-2.rds.amazonaws.com', 
                   port='5432', 
                   dbname='postgres', 
                   user='vargasde1', 
                   password=pw)

# Probar que existan las tablas

dbExistsTable(aws, "multas")

# Borrar password

rm(pw) # removes the password

# IMPORT DATA ---------------------------------------------------------------------------------

dbExistsTable(con, "multas")

# Importar data de las tablas

dbWriteTable(aws, "denunciante", denunciante, row.names=FALSE, append=TRUE)
dbWriteTable(aws, "tipo_multa", tipo_mul, row.names=FALSE, append=TRUE)
dbWriteTable(aws, "vehiculo", vehiculos, row.names=FALSE, append=TRUE)
dbWriteTable(aws, "calificacion", calificacion, row.names=FALSE, append=TRUE)

x <- 1
y <- 1

while (x < 3567890) {
  for (i in x:(x+999999)) {
    assign(paste('multas_',y, sep = ''), multas_mad[x:(x+999999),])
    x <- (x+999999)+1
    y <- y+1
  }
}

tail(multas_11, 50)

multas_11 <- multas_11[1:259646,]
 
dbWriteTable(aws, "multas", multas_11, row.names=FALSE, append=TRUE)

