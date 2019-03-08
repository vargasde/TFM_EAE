install.packages('DBI')
library(RPostgres)
library(DBI)
library(data.table)
library(dplyr)

pw<- {
  "osna8tzv"
}

con <- dbConnect(RPostgres::Postgres()
                 , host='35.228.88.227'
                 , port='5432'
                 , dbname='tfmeae'
                 , user='vargasde'
                 , password=pw)

# Probar que existan las tablas

dbExistsTable(con, "multas")

# IMPORT DATA ---------------------------------------------------------------------------------

rm(pw) # removes the password

# Probar que existan las tablas

dbExistsTable(con, "multas")

# Importar data de las tablas

dbWriteTable(con, "denunciante", denunciante, row.names=FALSE, append=TRUE)
dbWriteTable(con, "tipo_multa", tipo_mul, row.names=FALSE, append=TRUE)
dbWriteTable(con, "vehiculo", vehiculos, row.names=FALSE, append=TRUE)
dbWriteTable(con, "calificacion", calificacion, row.names=FALSE, append=TRUE)
dbWriteTable(con, "multas", det_multas10, row.names=FALSE, append=TRUE)

multas2 <- head(det_multas)
det_multas10 <- det_multas[9000001:nrow(det_multas),]

rm(det_multasfin, reasons_cod, multas2)
colnames(multas2)
colnames(det_multas)

# MANIPULATION-------------------------------------------------------------------------------------

query 
