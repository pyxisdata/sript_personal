# Script para la transformacion de las encuestas anuales
# Librerias
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
  paste0("\\Comercio") %>%
  list.files(pattern = ".sav", full.names = TRUE)
nombres <- str_match(archivos, "/(.*?).sav")[, 2]

# Importar los datos
importados <- archivos %>%
  map(read_sav, user_na = TRUE)
names(importados) <- nombres

# Funcion para procesar los datos
procesar <- function(datos, periodo) {
  
  # Adicion de la columna de fecha
  fecha <- periodo %>%
    str_match("\\d+") %>%
    paste("01", "01", sep = "-")
  
  # Limpiar los encabezados
  names(datos) <- names(datos) %>%
    str_to_lower() %>%
    str_trim(side = "both")
  
  # Reemplazar valores duplicados en los encabezados
  idnoremp <- str_which(names(datos), "idnoremp")
  names(datos)[idnoremp] <- "idnoremp"
  idoj <- str_which(names(datos), "idoj")
  names(datos)[idoj] <- "idoj"
  
  # Aplicar nuevas columnas y conversion de datos
  datos <- datos %>%
    mutate(fecha = ymd(fecha)) %>%
    mutate_at(vars(-fecha), as.character) %>%
    mutate_at(vars(-fecha), as.numeric)
  
}

# Aplicar proceso a todas las bases
procesados <- importados %>%
  map2(nombres, procesar)

# Union de todas las tablas en una sola
eac <- procesados %>%
  bind_rows()

# Importar la tabla de la correlativa ciiu
correlativa <- "\\Users\\PC\\Desktop\\Archivos Brutos\\Encuestas Anuales" %>%
  paste0("\\Comercio\\correlativa_eac.xlsx") %>%
  read_excel(sheet = 1, col_types = "numeric", col_names = TRUE)

# Asignar correlativa a la base de datos
eac <- eac %>%
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
             "Encuesta_Anual_Comercio",
             eac,
             append = TRUE)

# Extracción de los datos para EMA -------
eac_mas <- eac %>%
  filter(fecha >= ymd("2015-01-01")) %>%
  select(fecha, ciiu4, personom, directo, remtemp, sueplan, preplan,
         agrega, venta) %>%
  mutate_at(vars(-fecha), replace_na, 0) %>%
  group_by(fecha, ciiu4) %>%
  summarise_all(sum)

# Extraccion a csv
write_csv(eac_mas,
          "\\Users\\PC\\Desktop\\EQMAP\\eac_mas.csv",
          na = "")





