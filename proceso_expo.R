# Script para el procesamiento y limpieza de las bases de comercio exterior
# Exportaciones

# Librerias
library(readr)
library(readxl)
library(dplyr)
library(purrr)
library(lubridate)
library(stringr)

# Lista de direcciones de exportaciones
archivos <- "\\Users\\PC\\Desktop\\Archivos Brutos\\Exportaciones" %>%
  paste0(c("\\2009", "\\2010", "\\2011", "\\2012","\\2013",
           "\\2014", "\\2015", "\\2016", "\\2017", "\\2018")) %>%
  map(list.files, full.names = TRUE)

names(archivos) <- c("2009", "2010", "2011", "2012", "2013",
                     "2014", "2015", "2016", "2017", "2018")

# Tabla de las longitudes de los campos de exportaciones
campos <- "\\Users\\PC\\Desktop\\Archivos Brutos\\Exportaciones" %>%
  paste0("\\longitud_expo.csv") %>%
  read_csv(col_names = TRUE, col_types = cols(.default = "c")) %>%
  mutate(longitud = as.integer(longitud))

# Directorios de exportadores
directorio <- "\\Users\\PC\\Desktop\\Archivos Brutos\\Comex" %>%
  paste0("\\Directorios Exportadores") %>%
  list.files(full.names = TRUE, pattern = ".xlsx") %>%
  .[c(9:12)] %>%
  map(read_xlsx, sheet = 1, col_names = FALSE, col_types = "text") %>%
  map(filter, !is.na(...2)) %>%
  map(select, 2, 3) %>%
  map(slice, -1) %>%
  map(function(x) {
    names(x) <- c("nitexpo", "nombre")
    x
    }) %>%
  map(mutate, 
      nitexpo = str_trim(nitexpo, side = "both"),
      nitexpo = str_to_upper(nitexpo),
      nombre = str_to_upper(nombre),
      nombre = str_trim(nombre, side = "both")) %>%
  map2(c("2015-01-01", "2016-01-01", "2017-01-01", "2018-01-01"),
       function(x, y) {
         x <- x %>%
           mutate(fecha = ymd(y))
       }) %>%
  bind_rows()

# Tabla de partidas arancelarias
partidas <- "\\Users\\PC\\Desktop\\Archivos Brutos\\Comex" %>%
  paste0("\\partidas_arancel.csv") %>%
  read_csv(col_names = FALSE, col_types = cols(.default = "c"),
           locale = locale(encoding = "ISO-8859-1")) %>%
  filter(!is.na(.[[2]])) %>%
  slice(-1) %>%
  select(1, 3) %>%
  mutate(subpartida = str_trim(.[[1]], side = "both"),
         subpartida = str_replace_all(subpartida, " ", ""),
         nombre = str_trim(.[[2]], side = "both"),
         len = nchar(subpartida)) %>%
  select(subpartida, nombre) %>%
  distinct(subpartida, .keep_all = TRUE)

# Lectura de los archivos de exportaciones
# A?os 2015 a 2018
# Funcion de importacion
importar <- function(data, largo, nombres) {
  
  expo <- data %>%
    map(read_fwf,
        fwf_widths(largo),
        trim_ws = FALSE,
        col_types = cols(.default = "c"),
        locale = locale(encoding = "ISO-8859-1")) %>%
    bind_rows()
  
  names(expo) <- nombres
  expo
  
}

# Ejecutar funcion para cada uno de los a?os
exportaciones <- archivos[[1]] %>%
  map(importar, 
      largo = campos[["longitud"]],
      nombres = campos[["nombre"]]) %>%
  bind_rows()

# Procesamiento de los archivos
procesados <- exportaciones %>%
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

# NIT de la base de comercio exterior
nit_expo <- procesados %>%
  select(idident, nitexpo) %>%
  distinct() %>%
  mutate(len = nchar(nitexpo),
         cond1 = str_detect(nitexpo, "[^\\d]"),
         cond2 = str_detect(nitexpo, "^0{1,3}[^0]"),
         nitexpo2 = ifelse(cond1 == FALSE & cond2 == TRUE,
                           as.numeric(nitexpo),
                           nitexpo),
         len2 = nchar(nitexpo2),
         nitexpo3 = as.numeric(nitexpo2),
         nitexpo4 = ifelse(is.na(nitexpo3), nitexpo2, nitexpo3),
         len3 = nchar(nitexpo4),
         cond3 = str_detect(nitexpo4, "^[8-9]"),
         nitexpo5 = ifelse(len3 == 10 & cond3 == TRUE,
                           str_sub(nitexpo4, end = -2),
                           nitexpo4),
         len4 = nchar(nitexpo5)) %>%
  select(nitexpo, nitexpo5) %>%
  rename(nitexpo2 = nitexpo5) %>%
  distinct(nitexpo2, .keep_all = TRUE)

View(
  nit_expo %>%
    group_by(nitexpo) %>%
    summarise(n = n_distinct(idident))
)





































# Proceso de la transformacion y limpieza de los NIT y subpartidas
# Limpieza de los NIT
# Directorio procesado
directorio_p <- directorio %>%
  group_by(nitexpo) %>%
  summarise(nombre = last(nombre, order_by = fecha)) %>%
  ungroup() %>%
  mutate(len = nchar(nitexpo),
         cond = str_detect(nitexpo, "^[8-9]"),
         nitexpo2 = ifelse(len == 10 & cond == TRUE,
                           str_sub(nitexpo, end = -2),
                           nitexpo),
         len2 = nchar(nitexpo2)) %>%
  select(nitexpo, nitexpo2, nombre) %>%
  distinct(nitexpo2, .keep_all = TRUE)



# Union 
union <- nit_expo %>%
  left_join(directorio_p, by = "nitexpo2") %>%
  select(nitexpo.x, nitexpo2, nombre) %>%
  rename(nitexpo = nitexpo.x) %>%
  mutate(nombre = ifelse(is.na(nombre), "SIN IDENTIFICAR", nombre))


# Exportar valores
# Directorio
write_csv(union, 
          paste0("\\Users\\PC\\Desktop\\Archivos Brutos\\Exportaciones",
                 "\\directorio_expo.csv"),
          na = "")

# Datos de exportaciones
write_csv(procesados, 
          paste0("\\Users\\PC\\Desktop\\Archivos Brutos\\Exportaciones",
                 "\\procesados_1518.csv"),
          na = "")
# Limpiar memoria
rm(exportaciones, directorio, importar)

# Proceso de las subpartidas arancelarias
partidas_expo <- procesados %>%
  select(posicionaranc) %>%
  distinct() %>%
  left_join(partidas, by = c("posicionaranc" = "subpartida"))

