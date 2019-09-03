# Proceso de transformacion mensual
# Exportaciones

# Librerias
library(readr)
library(readxl)
library(dplyr)
library(purrr)
library(lubridate)
library(stringr)
library(odbc)

# Archivo de exportaciones
archivos <- "\\Users\\PC\\Desktop\\Archivos Brutos\\Exportaciones" %>%
  paste0("\\2009") %>%
  list.files(full.names = TRUE)

# Nombres de los archivos
nombres <- str_match(archivos, "M1(.*?).AVA")[, 2] %>%
  paste0("M", .)

# Tabla de las longitudes de los campos de exportaciones
campos <- "\\Users\\PC\\Desktop\\Archivos Brutos\\Exportaciones" %>%
  paste0("\\longitud_expo.csv") %>%
  read_csv(col_names = TRUE, col_types = cols(.default = "c")) %>%
  mutate(longitud = as.integer(longitud))

# Importar archivos
# Lectura de los archivos de exportaciones
# Funcion de importacion
importar <- function(data, largo, encabezado) {
  
  expo <- data %>%
    read_fwf(fwf_widths(largo),
             trim_ws = FALSE,
             col_types = cols(.default = "c"),
             locale = locale(encoding = "ISO-8859-1"))
  
  names(expo) <- encabezado
  expo
  
}

# # Ejecutar funcion para cada uno de los a?os
exportaciones <- archivos %>%
  map(importar, 
      largo = campos[["longitud"]],
      encabezado = campos[["nombre"]])

names(exportaciones) <- nombres

# Procesamiento de los archivos
# Funcion de procesamiento y limpieza de los tipos de dato
procesar <- function(data) {
  
  data %>%
  # Variables que deben ser enteros
  mutate_at(vars(fecha, articulos, idoficina, idaduana,
                 idident, idtipousu, idusuario, idtipoexpo,
                 idmunpioexpo, idpaisdestn, iddeclar, idsalidan,
                 iddeptoproc, fechadeclarant, fechadeclarant2, 
                 idvia, nacvia, idregimen, idmodalexp, formpago,
                 idcertificado, sistemaespec, iddeptoorig,
                 idunidadn, idadmonadu, fechaembarque, 
                 numdeclaracion2),
            as.integer) %>%
  # Variables que deben ser numeros con decimales
  mutate_at(vars(unidades, kgbruto, kgneto, fobusd, fobcop, valoragre,
                 flete, seguro, otrosgastos),
            as.numeric) %>%
  # Variables texto
  mutate_at(vars(nitexpo, idpaisdestc, ciudaddest, idautoriz, idsalidac,
                 declarant, iddeclarant, idmodalimp, idmoneda, idembarque,
                 iddato, idexpotran, posicionaranc, idunidadc, 
                 numdeclaracion, nomexpo, direccionexpo, nitdeclar,
                 nomdeclar, nomimpo, direccionimpo), 
            function(x) {
              x <- x %>% 
                str_trim(side = "both") %>%
                str_squish() %>%
                str_to_upper()
            })
  
}

# Ejecutar el proceso de la limpieza en los datos importados
procesados <- exportaciones %>%
  map(procesar)

# Conexion a sql server
conex <- dbConnect(odbc(),
                   Driver = "SQL Server",
                   Server = "40.87.53.140",
                   Database = "PyxisFinale",
                   UID = "alejandro.peralta",
                   PWD = "Pyxy54dm1n2018")

# Exportar datos a sql server
exportar <- function(conexion, data, nombre) {
  
  dbWriteTable(conexion,
               "Exportaciones_Bienes_Col",
               data,
               append = TRUE)
  
  print(paste("Terminado", nombre))
  
}

# Exportar a la tabla
procesados %>%
  map2(names(.), 
       exportar,
       conexion = conex)




