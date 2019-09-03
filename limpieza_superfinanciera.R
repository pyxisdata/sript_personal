# Script para limpieza de los estados financieros de superfinanciera
# Librerias
library(dplyr)
library(tidyr)
library(lubridate)
library(readxl)
library(stringr)
library(stringi)
library(readr)

# Eliminar notacion cientifica
options(scipen = 999)

# Listado de archivos de entidades financieras
archivos <- paste0(getwd(), "/Superfinanciera") %>%
  list.files(full.names = TRUE, pattern = ".xls") %>%
  .[!str_detect(., "directorio")]

nombres <- archivos %>%
  str_match("00(.*?)n.xls") %>%
  .[, 2] %>%
  map_if(str_detect(., "011"), str_replace, "^.{4}", "bancos_") %>%
  map_if(str_detect(., "041"), str_replace, "^.{4}", "financ_") %>%
  map_if(str_detect(., "021"), str_replace, "^.{4}", "corpor_") %>%
  map_if(str_detect(., "321"), str_replace, "^.{4}", "cooper_") %>%
  unlist()

# Funcion para importar los archivos
# Ejecutar la importacion
importados <- archivos %>%
  map(read_xls, sheet = 1, col_names = FALSE, col_types= "text")
names(importados) <- nombres

# Limpieza de los datos
# Funcion para limpiar los datos
limpiar <- function(archivo, periodo) {
  # Definir la fecha de cada periodo
  fecha <- str_match(periodo, "\\d+") %>%
    paste("01", "01", sep = "-")
  tipo <- str_match(periodo, "[^\\d+]+")
  # Condicion para asignar el tipo de la entidad financiera
  if (str_detect(tipo, "bancos")) { tipo <- 1 } 
  else if (str_detect(tipo, "corpor")) { tipo <- 2 }
  else if (str_detect(tipo, "financ")) { tipo <- 4 }
  else if (str_detect(tipo, "cooper")) { tipo <- 32 }
  # Datos
  data <- archivo %>%
    filter(!is.na(.[3]))
  data[1, 1:2] <- c("cuenta", "nom_cuenta")
  names(data) <- data[1, ] %>%
    unlist() %>%
    str_to_lower()
  data <- data %>%
    slice(-1) %>%
    mutate(tipo = tipo,
           fecha = ymd(fecha),
           nom_cuenta = str_to_lower(nom_cuenta),
           cuenta = as.double(cuenta)) %>%
    gather("nombre", "valor", -cuenta, -nom_cuenta, -tipo, -fecha) %>%
    mutate(valor = as.double(valor),
           valor = replace_na(valor, 0),
           nombre = word(nombre, 1, sep = "-"),
           nombre = as.integer(nombre))
}

# Ejecutar la limpieza
limpios <- importados %>%
  map2(nombres, limpiar) %>%
  map(distinct)

# Seleccion de los valores necesarios
seleccion <- limpios %>%
  map(filter, cuenta %in% c(100000, 300000, 410000, 510000, 570000)) %>%
  map(select, -nom_cuenta) %>%
  map(mutate, cuenta = case_when(cuenta == 100000 ~ 1,
                                 cuenta == 300000 ~ 3,
                                 cuenta == 410000 ~ 41,
                                 cuenta == 510000 ~ 51,
                                 cuenta == 570000 ~ 54)) %>%
  map(spread, cuenta, valor) %>%
  map(mutate, `33` = `41` - `51`) %>%
  map(select, -`51`) %>%
  bind_rows() %>%
  mutate(codigo = paste(tipo, nombre, sep = "_")) %>%
  select(-tipo, -nombre) 

# Importar el directorio de entidades vigiladas
directorio <- getwd() %>%
  paste0("/Superfinanciera/directorio_superfinanciera.xls") %>%
  read_xls(sheet = 1, col_names = FALSE, col_types = "text") %>%
  filter(!is.na(.[[3]])) %>%
  select(1, 2, 4, 6, 11)
names(directorio) <- c("tipo", "cod", "nombre", "nit", "munpio")

directorio <- directorio %>%
  slice(-1) %>%
  filter(tipo %in% c(1, 2, 4, 32)) %>%
  mutate(codigo = paste(tipo, cod, sep = "_"),
         nit = word(nit, 1, sep = "-"),
         munpio = word(munpio, 1, sep = " ")) %>%
  select(-tipo, -cod)

# Encontrar los NIT de las empresas
# Valores unicos de los codigos
entidades <- seleccion %>%
  select(codigo) %>%
  distinct() %>%
  left_join(directorio, by = "codigo") %>%
  filter(!is.na(nit)) %>%
  mutate(tipo = word(codigo, 1, sep = "_"),
         ciiu = case_when(tipo == 1 ~ 6412,
                          tipo == 2 ~ 6421,
                          tipo == 4 ~ 6422,
                          tipo == 32 ~ 6424),
         munpio = case_when(munpio == "Bogotá" ~ 11001,
                            munpio == "Medellín" ~ 5001,
                            munpio == "Cali" ~ 76001,
                            munpio == "Chía" ~ 25175,
                            munpio == "Bucaramanga" ~ 68001,
                            munpio == "Popayán" ~ 19001,
                            munpio == "Barranquilla" ~ 8001,
                            munpio == "Envigado" ~ 5266,
                            munpio == "Bello" ~ 5088)) %>%
  select(-tipo)

# Asignar el nit a la base de limpios
seleccion <- seleccion %>%
  left_join(entidades, by = "codigo") %>%
  select(-codigo, -nombre, -munpio, -ciiu) %>%
  filter(!is.na(nit))

# Seleccion de empresas consecutivas
consecutivas <- seleccion %>%
  group_by(nit) %>%
  summarise(n = n_distinct(fecha)) %>%
  filter(n == 4) %>%
  select(-n) %>%
  unlist()
# Filtrar la base de seleccion para sacar las no consecutivas
seleccion <- seleccion %>%
  filter(nit %in% consecutivas)
  
# Tamaño de las empresas
# Vector del salario minimo
smmlv <- tibble(fecha = ymd("2015-01-01", "2016-01-01",
                            "2017-01-01", "2018-01-01"),
                valor = c(644350, 689455, 737717, 781242))

# Separacion de los activos unicos de las empresas
# Definir el tamaño de las empresas basado en el tamaño de los activos
activos <- seleccion %>%
  select(fecha, nit, `1`) %>%
  left_join(smmlv, by = "fecha") %>%
  mutate(`1` = as.numeric(`1`),
         tamano = case_when(`1` <= (valor * 500) ~ 1,
                            `1` > (valor * 500) & 
                              `1` <= (valor * 5000) ~ 2,
                            `1` > (valor * 5000) & 
                              `1` <= (valor * 30000) ~ 3,
                            `1` > (valor * 30000) ~ 4)) %>%
  distinct(nit, fecha, tamano)

# Asignar el tamaño de los activos a las empresas
seleccion <- seleccion %>%
  left_join(activos, by = c("nit", "fecha"))

# Eliminar columna de codigo
entidades <- entidades %>%
  select(-codigo)
  

# Escribir los archivos 
write_csv(seleccion,
          paste0(getwd(), "/Limpio/ef_superfinanciera.csv"),
          na = "")
write_csv(entidades,
          paste0(getwd(), "/Limpio/dir_superfinanciera.csv"),
          na = "")
