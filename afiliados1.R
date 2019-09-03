# Script para auxiliar DAX

# Librerias
library(readxl)
library(readr)
library(data.table)
library(tidyr)
library(lubridate)
library(odbc)

# Quitar la notacion cientifica
options(scipen = 999)

# Subir data a SQL
conex <- dbConnect(odbc(),
                   Driver = "SQL Server",
                   Server = "40.87.53.140",
                   Database = "PyxisFinale",
                   UID = "alejandro.peralta",
                   PWD = "Pyxy54dm1n2018",
                   Encoding = "latin-9"
)

# Escritura
afiliados <- dbGetQuery(conex,
                        "SELECT * FROM Afiliados_Quindio"
                        )

# Convertir en data.table
afiliados <- data.table(afiliados)

# Añadir columna de indice
afiliados$indice <- 1:nrow(afiliados)

# Tabla de afiliados
datos_afiliados <- afiliados

# Deducir a lo util
afiliados <- afiliados[, c("indice", "fecha_nac", "id_genero")]

# Transformar y deducir la edad
afiliados[, anno := (year(today()) - year(ymd(fecha_nac))) * 12
          ] [, mes := month(today()) - month(ymd(fecha_nac))
          ] [, annodec := (anno + mes) / 12
          ] [, dec := (annodec - as.integer(annodec)) * 12
          ] [, c("anno", "mes", "dec") := list(as.integer(annodec),
                                               dec,
                                               NULL)
          ]

# Traer los datos de las actividades
rutas <- read_xlsx("\\Users\\PC\\Desktop\\Armenia\\Condicion_Ruta.xlsx",
                   sheet = 1,
                   col_names = TRUE
                   )

# Volver a data.table
rutas <- data.table(rutas)

# Vectores de asignacion
minimo <- c("primin" = 0, "infan" = 6, "adole" = 12, "juven" = 18,
            "adult" = 29, "vejez" = 60, "famil" = 0)

maximo <- c("primin" = 6, "infan" = 12, "adole" = 18, "juven" = 29,
            "adult" = 60, "vejez" = 150, "famil" = 150)

# Deducir a lo util
rutas <- melt(rutas, 
              measure.vars = c("f", "m"),
              variable.name = "genero",
              value.name = "id_genero"
              )

# Limpieza
rutas <- rutas[!is.na(id_genero)]

# Añadir columna de indice
rutas$indice <- 1:nrow(rutas)

rutas[, c("minimo", "maximo") := list(minimo[rutaID], maximo[rutaID])
     ] [, c("inicial", "final") := list(ifelse(unidad == 1,
                                               edadinicial,
                                               edadinicial / 12),
                                        ifelse(unidad == 1,
                                               edadfinal,
                                               edadfinal / 12)
                                        )
        ]

# Definir la tabla de jerarquia
jerarquia <- rutas[, c(1:2, 4:7, 11:20, 23)]

# Definir la tabla de condiciones
rutas <- rutas[, c(22:27)]

# Tablas de las condiciones -------------------------------------------

# Condicion Primer Infancia
primin <- rutas[minimo == 0 & maximo == 6]
afprimin <- afiliados[anno >= 0 & anno < 6]

# Condicion Infancia
infan <- rutas[minimo == 6 & maximo == 12]
afinfan <- afiliados[anno >= 6 & anno < 12]

# Condicion Adolescencia
adole <- rutas[minimo == 12 & maximo == 18]
afadole <- afiliados[anno >= 12 & anno < 16]

# Condicion Juventud
juven <- rutas[minimo == 18 & maximo == 29]
afjuven <- afiliados[anno >= 18 & anno < 29]

# Condicion Adultes
adult <- rutas[minimo == 29 & maximo == 60]
afadult <- afiliados[anno >= 29 & anno < 60]

# Condicion Vejez
vejez <- rutas[minimo == 60 & maximo == 150]
afvejez <- afiliados[anno >= 60 & anno < 150]

# Fin de tablas condiciones -------------------------------------------





