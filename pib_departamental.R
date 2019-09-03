# Script para la transformacion de PIB Departamental
# Librerias
library(tidyverse)
library(readxl)
library(lubridate)
library(odbc)

# Eliminar notacion cientifica
options(scipen = 999)

# Direccion de los datos
archivos <- 
  "\\Users\\PC\\Desktop\\Archivos Brutos\\PIB\\PIB Departamental\\" %>%
  list.files(full.names = TRUE, pattern = "2018")

# Lectura de los archivos
# Datos por actividad economica
importados <- archivos %>%
  map2(c(2, 3), function(x, y) {
         x <- read_excel(x,
                         sheet = y,
                         col_names = FALSE,
                         col_types = "text")
         })
names(importados) <- c("corrientes", "constantes")

# Transformacion de los datos
# Segun departamento
# Funcion de limpieza
limpiar <- function(x, anno) {
  
  limpio <- x %>%
    mutate(nombre = str_to_lower(.[[2]]),
           nombre = replace_na(nombre, 0),
           codigo = str_to_lower(.[[1]]),
           codigo = ifelse(nombre == "colombia",
                           100,
                           codigo)) %>%
    filter(!is.na(codigo)) %>%
    select(-1, -2) %>%
    select(codigo, nombre, everything()) %>%
    filter(!str_detect(codigo,
                       "divipola|fuente|liminar|pp|actualizado|base 2015")) %>%
    mutate(rama = ifelse(nombre == "0",
                         codigo,
                         NA)) %>%
    select(codigo, nombre, rama, everything()) %>%
    fill(rama) %>%
    filter(nombre != "0") %>%
    select(-nombre) %>%
    mutate(rama = case_when(str_detect(rama, "agricultura") ~ 1,
                            str_detect(rama, "canteras") ~ 2,
                            str_detect(rama, "manufactureras") ~ 3,
                            str_detect(rama, "suministro") ~ 4,
                            str_detect(rama, "construcci") ~ 5,
                            str_detect(rama, "comercio") ~ 6,
                            str_detect(rama, "comunicaciones") ~ 7,
                            str_detect(rama, "financieras") ~ 8,
                            str_detect(rama, "inmobiliarias") ~ 9,
                            str_detect(rama, "profesionales") ~ 10,
                            str_detect(rama, "defensa") ~ 11,
                            str_detect(rama, "entretenimiento") ~ 12,
                            str_detect(rama, "agregado") ~ 13,
                            str_detect(rama, "impuestos") ~ 14,
                            str_detect(rama, "interno") ~ 15))
  
  # Encontrar la columna vacia 
  col_na <- detect_index(limpio,
                         function(x) {sum(is.na(x)) == length(x)})
  
  # Filtrar por columnas y cambiar el tipo de dato
  limpio <- limpio %>%
    select(1:(col_na - 1)) %>%
    mutate_at(vars(1, 2), as.integer) %>%
    mutate_at(vars(-1, -2), as.numeric)
  
  # Encabezados de fecha
  fechas <- seq.Date(ymd("2005-01-01"), 
                     ymd(paste(anno, "01", "01", sep = "-")),
                     by = "year")
  
  # Asignar encabezados
  names(limpio)[c(-1, -2)] <- as.character(fechas)
  
  # Cambio de estructura
  limpio <- limpio %>%
    gather("fecha", "valor", -codigo, -rama) %>%
    mutate(fecha = ymd(fecha),
           valor = replace_na(valor, 0))
  
}

# Limpieza del archivo
# Base a precios corrientes
limpios <- importados %>%
  map(limpiar, anno = "2018") %>%
  map(function(x) {
    
    x <- x %>%
    # Agrupar por ramas
    # Calcular los ajustes al total
    spread(rama, valor) %>%
    mutate(`16` = `13` - rowSums(select(., `1`:`12`)),
           `17` = `15` - (`13` + `14`),
           `13` = `16`,
           `15` = `17`) %>%
    select(-`16`, -`17`) %>%
    gather("idrama", "valor", -codigo, -fecha) %>%
    # Agrupar por departamentos
    # Calcular los ajustes al total
    spread(codigo, valor) %>%
    mutate(`101` = `100` - rowSums(select(., `5`:`99`)),
           `100` = `101`) %>%
    select(-`101`) %>%
    gather("iddepto", "valor", -idrama, -fecha) %>%
    mutate_at(vars(idrama, iddepto), as.integer)
    
    }) %>%
  # Asignar columnas de tipo de base
  map_at(vars(1), mutate, idbase = 1) %>%
  map_at(vars(2), mutate, idbase = 2)

# Conexion a SQL Server
conex <- dbConnect(odbc(),
                   Driver = "SQL Server",
                   Server = "40.87.53.140",
                   Database = "PyxisFinale",
                   UID = "alejandro.peralta",
                   PWD = "Pyxy54dm1n2018",
                   Encoding = "latin-9")

# Tabla de PIB base 2005 en sql
dbWriteTable(conex, 
             "pib_departamental_2015",
             limpios[[1]],
             append = TRUE)
dbWriteTable(conex, 
             "pib_departamental_2015",
             limpios[[2]],
             append = TRUE)
