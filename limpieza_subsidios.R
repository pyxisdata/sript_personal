# Scriot para la limpieza de subsidios 
# Librerias
library(purrr)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(readxl)
library(readr)
library(stringi)
library(rvest)
library(stringdist)

# Eliminar notacion cientifica
options(scipen = 999)

# Archivos con los estados financieros 
# Importar unicamente los que tinene formato xlsx
archivos <- paste0(getwd(), "/Supersubsidios") %>%
  list.files(full.names = TRUE, pattern = ".xlsx")

# Extraer los nombres eliminando el path y el formato
nombres <- str_match(archivos, "os/(.*?)\\.xlsx")[, 2]

# Importar los archivos de excel con los estados financieros
# Importar los archivos 
importados <- map(archivos, read_xlsx, sheet = 1, col_names = FALSE)
names(importados) <- nombres

# Limpieza para los datos entrew 2016 y 2018
# Funcion de limpieza 2016-2018
limpiar <- function(archivo, periodo) {
  anno <- word(periodo, 2, sep = "_")
  # Condicion para eliminar la primera columna de los BG
  if (str_detect(periodo, "BG")) {
    archivo <- archivo %>%
      select(-1)
  } else { archivo }
  archivo <- archivo %>%
    mutate(filtro = ifelse(is.na(.[[3]]) & !is.na(.[[2]]), 0, .[[3]])) %>%
    filter(!is.na(filtro)) %>%
    select(-filtro)
  # Aplicar los nombres
  names(archivo) <- unlist(archivo[2, ])
  names(archivo)[c(1:2, ncol(archivo))] <- c("cod_cuenta", "nom_cuenta",
                                             "total")
  archivo <- archivo %>%
    select(-total) %>%
    filter(!is.na(.[2])) %>%
    mutate(fecha = paste(anno, "01", "01", sep = "-"),
           fecha = ymd(fecha),
           cod_cuenta = str_trim(cod_cuenta, side = "both"),
           n = nchar(cod_cuenta)) %>%
    filter(n <= 4) %>%
    select(-n) %>%
    gather("nombre", "valor", -cod_cuenta, -nom_cuenta, -fecha) %>%
    mutate(valor = as.double(valor),
           valor = replace_na(valor, 0)) %>%
    mutate_at(vars(nombre, nom_cuenta), str_to_lower) %>%
    mutate_at(vars(nombre, nom_cuenta),
              stri_trans_general, "Latin-ASCII") %>%
    mutate_at(vars(nombre, nom_cuenta), str_trim) %>%
    mutate_at(vars(nombre, nom_cuenta), str_squish)
}

# Limpiear todas las bases
limpios <- importados[-c(1, 5)] %>%
  map2(names(.), limpiar)

# Limpieza de las bases de 2015
# Limpieza del ER 2015
ER_2015 <- importados[[5]] %>%
  filter(!is.na(.[3])) %>%
  select(2:4, ncol(importados[[5]])) %>%
  filter(!is.na(.[3])) %>%
  fill(1) %>%
  select(2, 3, 1, 4) %>%
  mutate(fecha = paste("2015", "01", "01", sep = "-"),
         fecha = ymd(fecha),
         n = nchar(.[[1]])) %>%
  filter(n <= 4) %>%
  select(-n)
names(ER_2015) <- c("cod_cuenta", "nom_cuenta", "nombre", "valor", "fecha")
ER_2015 <- ER_2015 %>%
  mutate(valor = as.double(valor),
         valor = replace_na(valor, 0)) %>%
  mutate_at(vars(nombre, nom_cuenta), str_to_lower) %>%
  mutate_at(vars(nombre, nom_cuenta),
            stri_trans_general, "Latin-ASCII") %>%
  mutate_at(vars(nombre, nom_cuenta), str_trim) %>%
  mutate_at(vars(nombre, nom_cuenta), str_squish)

# Añadir a la lista de limpios
limpios[[7]] <- ER_2015

# Limpieza del BG 2015
BG_2015 <- importados[[1]] %>%
  select(-1, -ncol(.)) %>%
  mutate(filtro = ifelse(is.na(.[[3]]) & !is.na(.[[2]]), 0, .[[3]])) %>%
  filter(!is.na(filtro)) %>%
  select(-filtro)

# Generacion de los encabezados para poder filtrar solo los totales
encabezados <- tibble(nombres = unlist(BG_2015[2, ]),
                      saldo = unlist(BG_2015[4, ])) %>%
  fill(nombres) %>%
  mutate(union = paste(nombres, saldo, sep = "_")) %>%
  select(union) %>%
  unlist()
encabezados[1:2] <- c("cod_cuenta", "nom_cuenta")
names(BG_2015) <- encabezados

# Segunda parte de la limpieza BG 2015
BG_2015 <- BG_2015 %>%
  select(1:2, contains("Total")) %>%
  filter(!is.na(.[2])) %>%
  mutate(cod_cuenta = str_trim(cod_cuenta, side = "both"),
         n = nchar(cod_cuenta)) %>%
  filter(n <= 4) %>%
  select(-n) %>%
  gather("nombre", "valor", -c(1:2)) %>%
  mutate(nombre = word(nombre, 1, sep = "_"),
         fecha = paste("2015", "01", "01", sep = "-"),
         fecha = ymd(fecha),
         valor = as.double(valor),
         valor= replace_na(valor, 0)) %>%
  mutate_at(vars(nombre, nom_cuenta), str_to_lower) %>%
  mutate_at(vars(nombre, nom_cuenta),
            stri_trans_general, "Latin-ASCII") %>%
  mutate_at(vars(nombre, nom_cuenta), str_trim) %>%
  mutate_at(vars(nombre, nom_cuenta), str_squish)

# Añadir a datos limpios
limpios[[8]] <- BG_2015
names(limpios)[c(7, 8)] <- c("ER_2015", "BG_2015")

# Remover duplicados
limpios <- limpios %>%
  map(distinct) %>%
  map(mutate,
      nombre = str_replace_all(nombre, ":|-", ""),
      nombre = str_squish(nombre))

# Limpiar memoria
rm(BG_2015, ER_2015, encabezados)

# Agrupar EF por año
agrupados <- list()
agrupados[[1]] <- bind_rows(limpios[c(7, 8)])
agrupados[[2]] <- bind_rows(limpios[c(1, 4)])
agrupados[[3]] <- bind_rows(limpios[c(2, 5)])
agrupados[[4]] <- bind_rows(limpios[c(3, 6)])
names(agrupados) <- c("2015", "2016", "2017", "2018")

# Limpiar memoria
rm(limpios)

# Cambiar la dimension de los valores
agrupados <- agrupados %>%
  map_at(vars(1, 2), mutate, valor = valor * 1000)

# Agregar informacion de las empresas
empresas <- agrupados %>%
  map(select, nombre) %>%
  map(distinct) %>%
  bind_rows() %>%
  distinct()



# Fase 5: Informacion completa de las empresas
# Importar las direcciones de las cajas de compensacion
cajas <- "\\Users\\PC\\Desktop\\EQMAP\\Supersubsidios\\links_cajas.csv" %>%
  read_csv(col_names = FALSE, col_types = "c") %>%
  unlist()
# Funcion para extraer datos cada pagina web de las cajas de compensacion
extraer <- function(x) {
  texto <- read_html(x) %>%
    html_nodes("section") %>%
    html_text()
  texto_1 <- str_match(texto[3], "Nombre de la CCF:(.*?) twitter")[2]
  nombre <- word(texto_1, 1, sep = "Nombre del Director")
  nit <- str_match(texto_1, "Nit CCF:(.*?) Codigo de la CCF")[2]
  municipio <- str_match(texto_1, "Ciudad o Sede:(.*?) Mail CCF")[2]
  tibble(nombre = nombre, nit = nit, municipio = municipio)
}

# Ejecutar funcion para todas las cajas
# Limpiar para homgeneizar los nombres
directorio <- map(cajas, extraer) %>%
  bind_rows() %>%
  mutate_at(vars(nombre, municipio), str_to_lower) %>%
  mutate_at(vars(nombre, municipio, nit), str_trim) %>%
  mutate_at(vars(nombre, municipio), str_squish) %>%
  mutate_at(vars(nombre, municipio), str_to_lower) %>%
  mutate_at(vars(nombre, municipio), 
            stri_trans_general, "Latin-ASCII") %>%
  mutate_at(vars(nombre, nit), 
            str_replace_all, "[[:punct:]]", "") %>%
  mutate_at(vars(nombre, municipio), str_squish) %>%
  mutate(ciiu = 6520,
         nit = str_sub(nit, end = -2),
         nit = replace(nit, nit==86004484, 860044840),
         municipio = replace(municipio, municipio == "-", "sin definir")) %>%
  # Informacion de la caja faltante
  rbind(c("caja de compensacion familiar del magdalena cajamag",
          891780093, 47001, 6520))

# Limpiar memoria
rm(cajas, extraer)

# Nombres unicos de la base de datos
nit <- empresas %>%
  mutate(union = 1) %>%
  full_join(mutate(directorio, union = 1), by = "union") %>%
  select(-union) %>%
  mutate(dist =  stringdist(nombre.x, nombre.y, method = "cosine")) %>%
  group_by(nombre.x) %>%
  summarise(valor = min(dist),
            nombre.y = first(nombre.y, order_by = dist),
            nit = first(nit, order_by = dist)) %>%
  arrange(desc(valor)) %>%
  mutate(nit = replace(nit, str_detect(nombre.x, "cajacopi"), 890102044),
         nit = replace(nit, str_detect(nombre.x, "cofrem"), 892000146)) %>%
  select(-2, -3) %>%
  rename(nombre = nombre.x)

# Asignar nit a la base de datos
agrupados <- agrupados %>%
  map(left_join, nit, by = c("nombre")) %>%
  map(select, -nombre)

# Limpiar memoria
rm(nit, empresas)

# Seleccion de las cuentas a utilizar en el EQMAP
# Filtro por cuentas
seleccion <- agrupados %>%
  map(filter, cod_cuenta %in% c(1, 3, 41, 61, 51, 54)) %>%
  map(select, -nom_cuenta) %>%
  map(spread, cod_cuenta, valor) %>%
  map(mutate_at, vars(-fecha, -nit), replace_na, 0) %>%
  map(mutate, `33` = `41` - `61` - `51`) %>%
  map(select, -`61`, -`51`)

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
rm(smmlv, activos)

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

# Limpiar memoria
rm(excluidos)


# Base finalizada homogenea de empresas para EQMAP
write_csv(seleccion, 
          paste0(getwd(), "/Limpio/ef_supersubsidios.csv"),
          na = "")
write_csv(directorio, 
          paste0(getwd(), "/Limpio/dir_supersubsidios.csv"),
          na = "")
