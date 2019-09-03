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
library(purrr)

# Desactivar la notacion cientifica
options(scipen = 999)

# Lista de archivos
archivos <- "\\Users\\PC\\Desktop\\Archivos Brutos\\Encuestas Anuales" %>%
  paste0("\\Industria") %>%
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
  ciiu <- str_which(names(datos), "ciiu")
  names(datos)[ciiu] <- "ciiu"
  
  # Asifgnacion de nuevas columnas y formato de columnas
  datos <- datos %>%
    mutate(fecha = ymd(fecha)) %>%
    mutate_at(vars(-fecha), as.character) %>%
    mutate_at(vars(-fecha), as.numeric)
  
}

# Aplicar proceso a todas las bases
procesados <- importados %>%
  map2(nombres, procesar)

# Union de todas las tablas en una sola
eam <- procesados[c(9:17)] %>%
  bind_rows()

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
             "EAM",
             eam,
             append = TRUE)

# Extracción de los datos para EMA -------
eam_mas <- eam %>%
  filter(fecha >= ymd("2015-01-01")) %>%
  select(fecha, ciiu, valagri, salarper, pressper,
         persocu, pertem3, remutemp) %>%
  mutate_at(vars(-fecha), replace_na, 0) %>%
  group_by(fecha, ciiu) %>%
  summarise_all(sum)

# Extraccion a csv
write_csv(eam_mas,
          "\\Users\\PC\\Desktop\\EQMAP\\eam_mas.csv",
          na = "")







