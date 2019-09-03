# Probar el poder de las visualizaciones de dbplyr
library(dbplyr)
library(DBI)
library(odbc)

# Hacemos la conexion a SQL
con <- dbConnect(odbc(),
                 Driver = "SQL Server",
                 Server = "40.87.53.140",
                 Database = "PyxisFinale",
                 UID = "alejandro.peralta",
                 PWD = "Pyxy54dm1n2018",
                 Encoding = "latin-9"
                 )
# Leer tabla
expo <- dbReadTable(con, "Exportaciones_Bienes")


# Llamamos una tabla 
expo_db <- dbSendQuery(con, 
                       "SELECT Fecha, Nit, IDpais, IDvia, Poscaran, IDdepto, Unidad, Kgneto, Fobusd, Fobcop
                       FROM Exportaciones_Bienes;")
