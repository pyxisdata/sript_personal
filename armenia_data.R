# Script para presicion en Armenia 
# Librerias
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(readxl)

# Carpeta
carpeta <- "\\Users\\PC\\Desktop\\Armenia\\"
# Importar datos --------------------------------------------------------------
# Base de datos de afiliados Quindio
afiliados <- read_csv(paste0(carpeta, "afiliados.csv"), 
                      col_types = "cccccccciiicci",
                      locale = locale(encoding = "ISO-8859-1"))
# Actividades por tipo de genero y edad
cruce <- read_csv(paste0(carpeta, "cruce_estandar.csv"),
                  col_types = "diiddd")
# Archivo con las caracteristicas de las actividades
actividades <- read_csv(paste0(carpeta, "actividades.csv"),
                        col_types = "iccciccccciciciciciiiccdddccicddicc",
                        locale = locale(encoding = "ISO-8859-1"))
# Archivo con los periodos del pronostico
periodos <- read_csv(paste0(carpeta, "periodos.csv"),
                     col_types = "dcdiid")
# Parametricas
tabla_geografia <- read_excel(paste0(carpeta, "Parametricas_Armenia.xlsx"),
                              sheet = 1)
tabla_entidad <- read_excel(paste0(carpeta, "Parametricas_Armenia.xlsx"),
                            sheet = 2)
tabla_edad <- read_excel(paste0(carpeta, "Parametricas_Armenia.xlsx"),
                         sheet = 3)
tabla_genero <- read_excel(paste0(carpeta, "Parametricas_Armenia.xlsx"),
                           sheet = 4)
tabla_regimen <- read_excel(paste0(carpeta, "Parametricas_Armenia.xlsx"),
                            sheet = 5)

# Limpieza de los datos -------------------------------------------------------
# Añadir las llaves a la tabla de periodos
periodos <- periodos %>%
  mutate(fecha = mdy(fecha),
         edad = replace(edad, is.na(edad), 0),
         base = as.numeric(paste0(generoID, regimenID, actual)),
         general = as.numeric(paste0(generoID, edad))) %>%
  select(-generoID, -regimenID)
# Añadir la llave a la tabla de cruce
cruce <- cruce %>%
  mutate(general = as.numeric(paste0(generoID, edad))) %>%
  select(-edad, -generoID)
# Calcular la edad en la tabla de afiliados y filtrar por afiliados activos
afiliados <- afiliados %>%
  filter(afiliadoID == 3) %>%
  mutate(fecha_nac = mdy(fecha_nac),
         anno = year(today()) - year(fecha_nac),
         mes = 4 - month(fecha_nac),
         edad = (anno * 12) + mes,
         edad = round(edad / 12, 2),
         anno = as.integer(edad),
         mes = round(edad - anno, 2),
         mes = (mes * 12) / 100,
         edad = round(anno + mes, 2),
         base = as.numeric(paste0(generoID, regimenID, edad)),
         nombre = paste(apellido_2, apellido_1, nombre_2, nombre_1)) %>%
  filter(edad <= 120) %>%
  select(-anno, -mes, edad, -modalidadID, afiliadoID, zonaID,
         -apellido_2, -apellido_1, -nombre_2, -nombre_1)
# Dejar unicamente las columnas para utilizar en actividades
actividades <- actividades %>%
  select(-rutaID, -ruta, -min, -max, -costo_reds, -uniras)

# Filtros ---------------------------------------------------------------------
identificacion <- unique(afiliados$personaID)
municipio <- unique(tabla_geografia$munpioID)
regimen <- unique(tabla_regimen$regimenID)
entidad <- unique(tabla_entidad$entidadID)
genero <- unique(tabla_genero$generoID)
