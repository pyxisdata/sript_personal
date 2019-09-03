# Script para el procesamiento y limpieza de las bases de comercio exterior
# Importaciones
# Librerias
library(readr)
library(readxl)
library(dplyr)
library(purrr)
library(lubridate)
library(stringr)

# Quitar la notacion cientifica
options(scipen = 999)

# Lista de direcciones de importaciones
archivos <- "\\Users\\PC\\Desktop\\Archivos Brutos\\Importaciones" %>%
  paste0(c("\\2015", "\\2016", "\\2017", "\\2018")) %>%
  map(list.files, full.names = TRUE)

names(archivos) <- c("2015", "2016", "2017", "2018")

# Tabla de las longitudes de los campos de exportaciones
campos <- "\\Users\\PC\\Desktop\\Archivos Brutos\\Importaciones" %>%
  paste0("\\longitud_impo.csv") %>%
  read_csv(col_names = TRUE, col_types = cols(.default = "c")) %>%
  mutate(longitud = as.integer(longitud))

# Directorios de exportadores
directorio <- "\\Users\\PC\\Desktop\\Archivos Brutos\\Comex" %>%
  paste0("\\Directorios Importadores") %>%
  list.files(full.names = TRUE, pattern = ".xlsx") %>%
  .[c(9:12)] %>%
  map(read_xlsx, sheet = 1, col_names = FALSE, col_types = "text") %>%
  map(filter, !is.na(...2)) %>%
  map(select, 2, 3) %>%
  map(slice, -1) %>%
  map(function(x) {
    names(x) <- c("nitimpo", "nombre")
    x
  }) %>%
  map(mutate, 
      nitimpo = str_trim(nitimpo, side = "both"),
      nitimpo = str_to_upper(nitimpo),
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
# Años 2015 a 2018
# Funcion de importacion
importar <- function(data, largo, nombres) {
  
  impo <- data %>%
    map(read_fwf,
        fwf_widths(largo),
        trim_ws = FALSE,
        col_types = cols(.default = "c"),
        locale = locale(encoding = "ISO-8859-1")) %>%
    bind_rows()
  
  names(impo) <- nombres
  impo
  
}

# Ejecutar funcion para cada uno de los años
importaciones <- archivos %>%
  map(importar, 
      largo = campos[["longitud"]],
      nombres = campos[["nombre"]]) %>%
  bind_rows()

# Procesamiento de los archivos
procesados <- importaciones %>%
  # Variables que deben ser enteros
  mutate_at(vars(fecha, idaduana, idpaisorig, idpaisproc, idpaiscomp,
                 iddeptodest, idvia, bandera, otrosder, ciudadimpo,
                 idactividad, idadmonadu, idpaisimpo, idimport,
                 digv),
            as.integer) %>%
  # Variables que deben ser numeros con decimales
  mutate_at(vars(kgbruto, kgneto, unidades, fobusd, flete, cifusd,
                 cifcop, impuesto, valaduana, valajuste, iva, totiva,
                 seguro, otrosgastos, arancelporcen, derechoaranc),
            as.numeric) %>%
  # Variables texto
  mutate_at(vars(idregimen, idacuerdo, idunidad, posicionaranc, idimpo, 
                 ciudadexpo, otrosporcen, otrosbase, ingreso, idingreso,
                 iddeptoimpo, nitimpo, nomimpo), 
            function(x) {
              x <- x %>% 
                str_trim(side = "both") %>%
                str_squish() %>%
                str_to_upper()
            })

# Proceso de la transformacion y limpieza de los NIT y subpartidas
# Limpieza de los NIT
# Directorio procesado
directorio_p <- directorio %>%
  group_by(nitimpo) %>%
  summarise(nombre = last(nombre, order_by = fecha)) %>%
  ungroup() %>%
  mutate(len = nchar(nitimpo),
         cond = str_detect(nitimpo, "^[8-9]"),
         nitimpo2 = ifelse(len == 10 & cond == TRUE,
                           str_sub(nitimpo, end = -2),
                           nitimpo),
         len2 = nchar(nitimpo2)) %>%
  select(nitimpo, nitimpo2, nombre) %>%
  distinct(nitimpo2, .keep_all = TRUE)


View(
  directorio_p %>%
    group_by(nombre) %>%
    summarise(n = n_distinct(nitimpo2))
)


# NIT de la base de comercio exterior
nit_impo <- procesados %>%
  select(nitimpo) %>%
  distinct() %>%
  mutate(len = nchar(nitimpo),
         nitimpo2 = as.numeric(nitimpo),
         nitimpo2 = as.character(nitimpo2),
         len2 = nchar(nitimpo2),
         cond1 = str_detect(nitimpo2, "^[8-9]"),
         nitimpo3 = case_when(len2 == 10 & cond1 == TRUE ~
                                str_sub(nitimpo2, end = -2),
                              len2 > 11 & cond1 == FALSE ~
                                str_sub(nitimpo2, end = 10),
                              len2 > 11 & cond1 == TRUE ~
                              str_sub(nitimpo2, end = 9),
                              TRUE ~ nitimpo2),
         len3 = nchar(nitimpo3)) %>%
  select(nitimpo, nitimpo3) %>%
  rename(nitimpo2 = nitimpo3) %>%
  distinct(nitimpo2, .keep_all = TRUE)

# Union 
union <- nit_impo %>%
  left_join(directorio_p, by = "nitimpo2") %>%
  select(nitimpo.x, nitimpo2, nombre) %>%
  rename(nitimpo = nitimpo.x) %>%
  mutate(nombre = ifelse(is.na(nombre), "SIN IDENTIFICAR", nombre))


# Exportar valores
# Directorio
write_csv(union, 
          paste0("\\Users\\PC\\Desktop\\Archivos Brutos\\Importaciones",
                 "\\directorio_impo.csv"),
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