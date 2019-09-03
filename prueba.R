# Prueba Graficar datos totales

# Liberias
library(data.table)
library(lubridate)
library(odbc)
library(dplyr)
library(sqldf)

# Jerarquia
jerarquia <- read.csv("\\Users\\PC\\Desktop\\Armenia\\rutas.csv")

# Conexion a SQL
conex <- dbConnect(odbc(),
                   Driver = "SQL Server",
                   Server = "40.87.53.140",
                   Database = "PyxisFinale",
                   UID = "alejandro.peralta",
                   PWD = "Pyxy54dm1n2018",
                   Encoding = "latin-9")

# Traer Afiliados
afiliados <- dbGetQuery(conex, "SELECT * FROM Afiliados_Quindio")

# Convertir en data.table
afiliados <- data.table(afiliados)
jerarquia <- data.table(jerarquia)

# Crear columna de edad
afiliados[, edad := (year(today()) - year(fecha_nac)) * 12
] [, edad := (edad + (month(today()) - month(fecha_nac))) / 12
]

edad <- afiliados$edad
genero <- afiliados$id_genero

# Ver las actividades generales
jerarquia[edad >= minimo & edad < maximo & genero == generoID,
          .N,
          by = .(seccion, programa, actividad)
          ] -> resultado

View(resultado)
