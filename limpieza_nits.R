# Script para limpieza de los NITS de coemrcio exterior
# Librerias
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(stringdist)
library(stringi)
library(purrr)
library(readxl)
library(readr)
library(odbc)
library(dbplyr)

# Quitar la notacion cientifica
options(scipen = 999)

# Carpeta de archivos
# Directorios
# Exportadores
archivos_exp <- "\\Users\\PC\\Desktop\\Archivos Brutos\\Comex\\" %>%
  paste0("Directorios Exportadores\\") %>%
  list.files(full.names = TRUE, pattern = ".xlsx")
nombres_exp <- str_match(archivos_exp, "es\\\\(.*?)\\.xlsx")[, 2]

# Importadores
archivos_imp <- "\\Users\\PC\\Desktop\\Archivos Brutos\\Comex\\" %>%
  paste0("Directorios Importadores\\") %>%
  list.files(full.names = TRUE, pattern = ".xlsx")
nombres_imp <- str_match(archivos_imp, "es\\\\(.*?)\\.xlsx")[, 2]

# Importar archivos exportadores
importados_exp <- archivos_exp %>%
  map(read_xlsx, sheet = 1, col_names = FALSE)
names(importados_exp) <- nombres_exp

# Importar archivos importadores
importados_imp <- archivos_imp %>%
  map(read_xlsx, sheet = 1, col_names = FALSE)
names(importados_imp) <- nombres_imp

# Limpiar memoria
rm(nombres_exp, nombres_imp)

# Funcion para aplicar limpieza
limpiar <- function(archivo, periodo) {
  
  fecha <- paste(str_match(periodo, "\\d+"), "01", "01", sep = "-")
  
  archivo <- archivo %>%
    filter(!is.na(.[2])) %>%
    select(2, 3) %>%
    slice(2:n()) %>%
    mutate(nombre = tolower(.[[2]]),
           nombre = str_replace(nombre, "\"", " "),
           nombre = str_trim(nombre),
           nombre = str_squish(nombre),
           nombre = stri_trans_general(nombre, "Latin-ASCII"),
           nit = str_trim(.[[1]]),
           nit = ifelse(nchar(nit) > 9 & 
                          (str_sub(nit, end = 1) %in% c("9", "8")), 
                        str_sub(nit, 1, 9),
                        nit)) %>%
    select(nit, nombre) %>%
    mutate(fecha = ymd(fecha))
  
}

# Limpiar archivos
# Exportadores
limpios_exp <- importados_exp %>% 
  map2(names(.), limpiar) %>%
  bind_rows()
# Importadores
limpios_imp <- importados_imp %>% 
  map2(names(.), limpiar) %>%
  bind_rows()
 
# Verificacion de nits
# Exportadores
directorio_exp <- limpios_exp %>%
  group_by(nit) %>%
  summarize(nombre = last(nombre, order_by = fecha)) %>%
  mutate(nit = replace(nit, nit == ".", "0"))
# Importadores
directorio_imp <- limpios_imp %>%
  group_by(nit) %>%
  summarize(nombre = last(nombre, order_by = fecha))

# Limpiar memoria
rm(limpios_exp, limpios_imp)

# Importar valores unicos de los NIT de comercio exterior
# Exportaciones
unico_expo <- paste0(getwd(), "/nit_expo.csv") %>%
  read_csv(col_types = "c")
# Importaciones
unico_impo <- paste0(getwd(), "/nit_impo.csv") %>%
  read_csv(col_types = "c")















