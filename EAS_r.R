# Script para la transformacion de las encuestas anuales
# Librerias# Librerias
# SCRIPT REVISADO
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(stringi)
library(haven)
library(odbc)
library(readr)
library(readxl)
library(purrr)

# Desactivar la notacion cientifica
options(scipen = 999)

# Lista de archivos
archivos <- "\\Users\\PC\\Desktop\\Archivos Brutos\\Encuestas Anuales" %>%
  paste0("\\Servicios") %>%
  list.files(pattern = ".sav", full.names = TRUE)
nombres <- str_match(archivos, "/(.*?).sav")[, 2]

# Importar los datos
importados <- archivos %>%
  map(read_sav, user_na = TRUE)
names(importados) <- nombres

# Funcion para procesar los datos
procesar <- function(datos, periodo) {
  
  # Asignacion de las fechas
  fecha <- periodo %>%
    str_match("\\d+") %>%
    paste("01", "01", sep = "-")
  
  # Limpieza de los encabezados
  names(datos) <- names(datos) %>%
    str_to_lower() %>%
    str_trim(side = "both")
  
  # Limpieza de los valores duplicados en los encabezados
  names(datos)[names(datos) == "seccion"] <- "correla"
  seccion <- str_which(names(datos), "seccion")
  names(datos)[seccion] <- "seccion"
  insertot <- str_which(names(datos), "insertot")
  names(datos)[insertot] <- "insertot"
  presperm <- str_which(names(datos), "preesperm")
  names(datos)[presperm] <- "presperm"
  presmision <- str_which(names(datos), "preesmision")
  names(datos)[presmision] <- "presmision"
  prestemp <- str_which(names(datos), "preestemp")
  names(datos)[prestemp] <- "prestemp"
  division <- str_which(names(datos), "division")
  names(datos)[division] <- "division"
  
  # Asifgnacion de nuevas columnas y formato de columnas
  datos <- datos %>%
    mutate(fecha = ymd(fecha)) %>%
    mutate_at(vars(-fecha), as.character)
  
}

# Aplicar proceso a todas las bases
procesados <- importados %>%
  map2(nombres, procesar)

# Union de todas las tablas en una sola
eas <- procesados %>%
  bind_rows() %>%
  mutate_at(vars(-fecha, -division, -seccion, -correla), as.numeric) %>%
  mutate(seccion = replace(seccion, seccion == "", 0),
         seccion = replace_na(seccion, 0),
         division = replace_na(division, 0),
         correla = paste(seccion, division, sep = "-"))

# Importar la tabla de la correlativa ciiu
correlativa <- "\\Users\\PC\\Desktop\\Archivos Brutos\\Encuestas Anuales" %>%
  paste0("\\Servicios\\correlativa_eas.xlsx") %>%
  read_excel(sheet = 1, col_types = "text", col_names = TRUE)

# Asignar correlativa a la base de datos
eas <- eas %>%
  left_join(correlativa, by = "correla") %>%
  select(idnoremp, correla, ciiu4, everything())

# Conexion a SQL
conex <- dbConnect(odbc(),
                   Driver = "SQL Server",
                   Server = "40.87.53.140",
                   Database = "PyxisFinale",
                   UID = "alejandro.peralta",
                   PWD = "Pyxy54dm1n2018",
                   Encoding = "latin-9")

# Crear la tabla
dbWriteTable(conex,
             "Encuesta_Anual_Servicios",
             eas,
             overwrite = TRUE)

# Extracción de los datos para EMA -------
eas_mas <- eas %>%
  filter(fecha >= ymd("2015-01-01")) %>%
  select(fecha, ciiu4, valagre, sulperm, sultemp, presperm, prestemp,
         potperm, pottcde) %>%
  mutate_at(vars(-fecha), replace_na, 0) %>%
  group_by(fecha, ciiu4) %>%
  summarise_all(sum)

# Extraccion a csv
write_csv(eas_mas,
          "\\Users\\PC\\Desktop\\EQMAP\\eas_mas.csv",
          na = "")





