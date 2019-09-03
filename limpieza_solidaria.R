# Limpieza de los estados financieros de las entidades
# Supersolidaria
# Librerias
library(readr)
library(purrr)
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(stringdist)
library(lubridate)
library(stringi)

# Eliminar la notación cientifica
options(scipen = 999)

# Listado de los archivos
archivos <- paste0(getwd(), "/Supersolidaria") %>%
  list.files(full.names = TRUE) %>%
  .[!str_detect(., "cuentas|directorio")]
nombres <- str_match(archivos[c(1, 3, 5, 7)], "ria/(.*?).xlsx")[, 2]

# Fase 1: Importar 
# Funcion de importacion empresas copmpletas 
# Ejecutar la importacion
importados <- archivos[c(1, 3, 5, 7)] %>% 
  map(read_xlsx, sheet = 1, col_names = FALSE, col_types = "text")
names(importados) <- nombres

# Fase 2: Limpieza
# Funcion de limpieza 
limpiar <- function(archivo, periodo) {
  fecha <- paste(str_match(periodo, "\\d+"), "01", "01", sep = "-")
  data <- archivo %>%
    filter(!is.na(.[[2]]))
  names(data) <- unlist(data[1, ])
  data <- data %>%
    slice(-1) %>%
    select(2, 3, 4, 7, 10, 11, str_which(names(.), "100000"):ncol(.)) %>%
    gather("cuenta", "valor", -c(1:6))
  names(data) <- c("cod_entidad", "nombre", "nit", "ciiu", "depto",
                   "munpio", "cuenta", "valor")
  data
  data <- data %>%
    mutate(nit = str_replace_all(nit, "-", ""),
           nit = str_sub(nit, end = -2),
           valor = as.double(valor),
           valor = replace_na(valor, 0),
           fecha = ymd(fecha)) %>%
    mutate_at(vars(nombre, depto, munpio), str_to_lower) %>%
    mutate_at(vars(nombre, depto, munpio), str_trim) %>%
    mutate_at(vars(nombre, depto, munpio), str_squish)
}

# Ejecutar la limpieza 
limpios <- importados %>%
  map2(c("2015", "2016", "2017", "2018"), limpiar) %>%
  map(distinct) %>%
  map(filter, nombre != "-")

# Seleccion de los valores necesarios par EQMAP
seleccion <- limpios %>%
  map_at(vars(1), 
         filter, 
         cuenta %in% c(100000, 300000, 410000, 610000,
                       510000, 520000, 580000)) %>%
  map_at(vars(-1), 
         filter, 
         cuenta %in% c(100000, 300000, 410000, 610000,
                       510000, 540000)) %>%
  map(mutate, cuenta = case_when(cuenta == 100000 ~ 1,
                                 cuenta == 300000 ~ 3,
                                 cuenta == 410000 ~ 41,
                                 cuenta == 610000 ~ 61,
                                 cuenta == 510000 ~ 51,
                                 cuenta == 540000 ~ 52,
                                 cuenta == 520000 ~ 52,
                                 cuenta == 580000 ~ 54)) %>%
  map(mutate, cuenta = as.integer(cuenta)) %>%
  map(spread, cuenta, valor)

# Seleccion de la informacion del directorio
directorio <- seleccion %>%
  map(select, nit, nombre, ciiu, depto, munpio, fecha) %>%
  bind_rows() %>%
  group_by(nit) %>%
  summarise(nom = last(nombre, order_by = fecha),
            ciiu = last(ciiu, order_by = fecha),
            depto = last(depto, order_by = fecha),
            munpio = last(munpio, order_by = fecha))

# Fase 4: Importar los valores de impuestos faltantes 2016-2018
# Importar los valores de los impuestos de aquellas empresas que no aparecen
importar <- function(archivo, periodo) {
  if(periodo %in% c("2017", "2018")) {
    data1 <- read_excel(archivo,
                        sheet = 1,
                        col_names = FALSE,
                        col_types = "text")
    data2 <- read_excel(archivo,
                        sheet = 2,
                        col_names = FALSE,
                        col_types = "text")
    data <- bind_rows(data1, data2)
  } else {
    data <- read_excel(archivo,
                       sheet = 1,
                       col_names = FALSE,
                       col_types = "text")
  }
}

# Ejecutar la importacion
impuestos <- archivos[c(4, 6, 8)] %>%
  map2(c("2016", "2017", "2018"), importar)
names(impuestos) <- c("E2", "E3", "E4")

# Segunda fase: Limpieza
# Funcion de limpieza 
limpiar <- function(archivo, periodo) {
  fecha <- paste(str_match(periodo, "\\d+"), "01", "01", sep = "-")
  data <- archivo %>%
    filter(!is.na(.[[2]])) %>%
    filter(!str_detect(.[[1]], "CODIGO"))
  names(data) <- c("cod_entidad", "cuenta", "valor")
  data <- data %>%
    mutate(cod_entidad = as.integer(cod_entidad),
           cod_entidad = as.character(cod_entidad),
           cuenta = as.integer(cuenta),
           valor = as.double(valor),
           valor = replace(valor, is.na(valor), 0),
           fecha = ymd(fecha))
}

# Ejecutar la limpieza
impuestos_limpios <- impuestos %>% 
  map2(c("2016", "2017", "2018"), limpiar) %>%
  map(filter, cuenta == 523500) %>%
  map(mutate, cuenta = replace(cuenta, cuenta == 523500, 54)) %>%
  map2(seleccion[c(-1)], function(x, y) {
    y <- y %>%
      select(fecha, cod_entidad, nit)
    x <- x %>%
      left_join(y, by = c("fecha", "cod_entidad"))
  })


# Importar los directorios 
importar <- function(archivo, periodo) {
  if(periodo %in% c("2017", "2018")) {
    data <- read_excel(archivo,
                        sheet = 3,
                        col_names = FALSE,
                        col_types = "text")
  } else {
    data <- read_excel(archivo,
                       sheet = 2,
                       col_names = FALSE,
                       col_types = "text")
  }
}

# Importar los directorios secundarios
codigos <- archivos[c(4, 6, 8)] %>%
  map2(c("2016", "2017", "2018"), importar) %>%
  map2(c("2016", "2017", "2018"), function(x, anno) {
    fecha <- paste(anno, "01", "01", sep = "-")
    data <- x %>%
      slice(-1:-4) %>%
      select(2, 5) %>%
      mutate(fecha = ymd(fecha))
    names(data) <- c("cod_entidad", "nit", "fecha")
    data <- data %>%
      mutate(nit = str_replace_all(nit, "-", ""),
             nit = str_sub(nit, end = -2))
  })

# Añadir nits a los impuestos
impuestos_limpios <- impuestos_limpios %>%
  map2(codigos, function(x, y) {
    x <- x %>%
      left_join(y, by = c("fecha", "cod_entidad"))
  }) %>%
  map_at(vars(1), filter, !is.na(nit.x)) %>%
  map_at(vars(3),
         mutate, 
         nit.y = replace(nit.y, is.na(nit.y), "830009190")) %>%
  map(select, -nit.y, -cod_entidad, -cuenta) %>%
  map(rename, nit = nit.x)

# Limpiar memoria
rm(codigos, impuestos)

# Añadir impuestos a las base de selecccion
seleccion[c(2, 3, 4)] <- seleccion[c(2, 3, 4)] %>%
  map2(impuestos_limpios, function(x, y) {
    x <- x %>%
      left_join(y, by = c("fecha", "nit")) %>%
      rename(`54` = valor) %>%
      mutate(`54` = replace_na(`54`, 0))
  })

# Eliminar columnas del directorio
# Añadir columna de utilidad operacional
seleccion <- seleccion %>%
  map(select, -cod_entidad, -nombre, -ciiu, -depto, -munpio) %>%
  map(mutate, `33` = `41` - `61` - `51` - `52`) %>%
  map(select, -`61`, -`51`, -`52`) 

# Averiguar empresas con valores nulos o no validos
# Determinar entidades que valores igual a 0 a ser excluidas
# Funcion para determinar valores en 0
excluir <- function(archivo) {
  entidades <- archivo %>%
    filter_at(vars(`1`, `3`, `41`, `33`), any_vars(.==0)) %>%
    select(nit) %>%
    unlist()
}

# Lista de los NIT excluidos 
excluidos <- seleccion %>%
  map(excluir)

# Filtrar los datos de seleccion para sacar las entidades excluidas
seleccion <- seleccion %>%
  map2(excluidos, function(x, y) {
    x <- x %>%
      filter(!nit %in% y)
  }) %>%
  bind_rows()

# Averiguar el tamaño de las empresas
# Vector del los smmlv
smmlv <- tibble(fecha = ymd("2015-01-01", "2016-01-01",
                            "2017-01-01", "2018-01-01"),
                salario = c(644350, 689455, 737717, 781242))
# Por activos
activos <- seleccion %>%
  select(nit, fecha, `1`) %>%
  left_join(smmlv, by = "fecha") %>%
  mutate(`1` = as.numeric(`1`),
         tamano = case_when(`1` <= (salario * 500) ~ 1,
                            `1` > (salario * 500) & 
                              `1` <= (salario * 5000) ~ 2,
                            `1` > (salario * 5000) & 
                              `1` <= (salario * 30000) ~ 3,
                            `1` > (salario * 30000) ~ 4)) %>%
  select(nit, fecha, tamano)

# Añadir tamaños a la base de seleccion
seleccion <- seleccion %>%
  left_join(activos, by = c("nit", "fecha"))

# Limpiar memoria
rm(smmlv, activos, impuestos_limpios)

# Averiguar empresas que reportaron de forma consecutivo
excluidos <- seleccion %>%
  group_by(nit) %>%
  summarise(n = n_distinct(fecha)) %>%
  filter(n != 4) %>%
  select(nit) %>%
  unlist()

# Eliminar las compañias que no pertenecen a la base homogenea
seleccion <- seleccion %>%
  filter(!nit %in% excluidos)


directorio <- directorio %>%
  mutate(len = nchar(ciiu),
         ciiu = replace(ciiu, ciiu == "-", 1312),
         ciiu = ifelse(len == 3, paste0("0", ciiu), ciiu),
         len = nchar(ciiu)) %>%
  select(-len)
  

# Limpiar memoria
rm(excluidos)

# Base finalizada homogenea de empresas para EQMAP
write_csv(seleccion, 
          paste0(getwd(), "/Limpio/ef_supersolidaria.csv"),
          na = "")
write_csv(directorio, 
          paste0(getwd(), "/Limpio/dir_supersolidaria.csv"),
          na = "")


  



















  
