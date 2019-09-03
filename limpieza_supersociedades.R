# Script para la preparacion de los Estados Financieros
# Supersociedades
# Librerias
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)
library(readxl)
library(stringr)
library(stringdist)
library(purrr)
library(stringi)

# Carpeta con los archivos de los estados financieros
archivos <- paste0(getwd(), "/Supersociedades") %>%
  list.files(full.names = TRUE) %>%
  .[!str_detect(., ".zip|.csv")]

# Nombres de los archivos importados
nombres <- str_match(archivos, "dades/(.*?)\\.")[, 2]

# Funcion para importar los EF entre 2015 y 2016
# Importar solo archivos de texto
importar <- function(archivo) {
  data <- read_tsv(archivo, 
                   col_types = cols(.default = "c"),
                   col_names = TRUE,
                   locale = locale(encoding = "ISO-8859-1"))
}

# Importar archivos texto del balance general NIIF
# SOlo importar archivos txt
importados <- map(archivos[c(1, 2, 3, 12, 13, 14)], importar)
names(importados) <- nombres[c(1, 2, 3, 12, 13, 14)]

# Limpiar las bases de datos para que queden homogeneas
# Funcion para la limpieza de los datos extraidos 2015 y 2016 (NIIF)
limpiar <- function(archivo, periodo) {
  names(archivo) <- str_to_lower(names(archivo))
  fecha <- paste(periodo, "01", "01", sep = "-")
  data <- archivo %>%
    select(-3, -8, -9, -10) %>%
    gather("cuenta", "valor", -c(1:6)) %>%
    filter(!str_detect(cuenta, "sinopsis|resumen")) %>%
    mutate(fecha = ymd(fecha),
           valor = as.double(valor),
           valor = replace(valor, is.na(valor), 0))
  names(data) <- c("nit", "nombre", "depto", "ciiu", "periodo",
                   "fecha_corte", "cuenta", "valor", "fecha")
  data <- data %>%
    mutate(ciiu = str_extract(ciiu, "\\d+")) %>%
    mutate_at(vars(nombre, depto), str_to_lower)
}

# Ejecutar la limpieza
limpios <- importados[-c(1, 4)] %>%
  map2(c("2015", "2016", "2015", "2016"), limpiar) %>%
  map(filter, str_detect(fecha_corte, "12/31")) %>%
  map(select, -fecha_corte) %>%
  # Eliminar los periodos sin valores
  map_at(vars(1), filter, periodo == "2015-dic-31") %>%
  map_at(vars(2), filter, periodo == "2016-dic-31") %>%
  map_at(vars(3), filter, periodo != "2015-dic-31") %>%
  map(select, -periodo) %>%
  # Remover valores duplicados
  map_at(vars(1, 3),
         mutate,
         depto = case_when(nit == "805001194" ~ "bogota d.c.",
                           nit == "830104796" ~ "atlantico",
                           TRUE ~ depto)) %>%
  map_at(vars(2, 4),
         mutate,
         depto = case_when(nit == "830082556" ~ "cundinamarca",
                           nit == "900447022" ~ "antioquia",
                           nit == "900559327" ~ "risaralda",
                           TRUE ~ depto)) %>%
  map_at(vars(2, 4), filter, nit != "999999999") %>%
  map(distinct) %>%
  # Agrupar los valores sin contar el periodo
  map(group_by_at, vars(-valor)) %>%
  map(summarise, valor = sum(valor)) %>%
  map(as_tibble)

# Limpieza EF de 2015 bajo codificacion anterior no NIIF
# Balance general y estado de resultados
limpiar <- function(archivo, periodo) {
  names(archivo) <- str_to_lower(names(archivo))
  fecha <- paste(periodo, "01", "01", sep = "-")
  data <- archivo %>%
    select(-5, -7) %>%
    gather("cuenta", "valor", -c(1:5)) %>%
    mutate(fecha = ymd(fecha),
           valor = as.double(valor),
           valor = replace(valor, is.na(valor), 0))
  names(data) <- c("nit", "nombre", "munpio", "depto", "ciiu",
                   "cuenta", "valor", "fecha")
  data <- data %>%
    mutate(ciiu = str_extract(ciiu, "\\d+")) %>%
    mutate_at(vars(nombre, depto, munpio), str_to_lower)
}

# Ejecutar la limpieza y agregar a los datos limpios
limpios[c(5, 6)] <- importados[c(1, 4)] %>%
  map2(c("2015", "2015"), limpiar)
names(limpios)[c(5, 6)] <- nombres[c(1, 12)]

# Añadir columnas de identificacion de estados financieros
# Diferenciar entre NIIF y COLGAAP
limpios <- limpios %>%
  map_at(vars(1, 3), mutate, tipo = 1) %>%
  map_at(vars(5, 6), mutate, tipo = 2)

# Agrupar por estados financieros
agrupados <- list()
agrupados[[1]] <- bind_rows(limpios[c(1, 3, 5, 6)])
agrupados[[2]] <- bind_rows(limpios[c(2, 4)])
names(agrupados) <- c("EF_2015", "EF_2016") 

# Econtrar empresas con incoherencias en 2015
# Duplicados y remover incompletos
exclusion <- agrupados[[1]] %>%
  group_by(nit) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  filter(n %in% c(327)) %>%
  select(nit) %>%
  unlist()
# Eliminar empresas con incoherencias
agrupados[[1]] <- agrupados[[1]] %>%
  filter(!(nit %in% exclusion & tipo == 2)) %>%
  filter(!(nit %in% c("900127139", "900571155"))) %>%
  select(-tipo)

# Limpiar memoria
rm(limpios, exclusion)

# Ajustar valor de los anteriores periodos en seleccion
agrupados <- agrupados %>%
  map(mutate, valor = valor * 1000)

# Seleccion de las cuentas para el EQMAP
seleccion <- agrupados %>%
  map_at(vars(1),
         filter,
         cuenta %in% c("total activo",
                       "total de activos",
                       "total patrimonio",
                       "patrimonio total",
                       "41 ingresos operacionales",
                       "ingresos de actividades ordinarias",
                       "utilidad operacional",
                       "ganancia (pérdida) por actividades de operación",
                       "54 menos: impuesto de renta y complementarios",
                       "ingreso (gasto) por impuestos")) %>%
  map_at(vars(2),
         filter,
         cuenta %in% c("total de activos",
                       "patrimonio total",
                       "ingresos de actividades ordinarias",
                       "ganancia (pérdida) por actividades de operación",
                       "ingreso (gasto) por impuestos")) %>%
  # Cambiar nombres de las cuentas por codigos
  map_at(vars(1),
         mutate,
         cuenta = case_when(str_detect(cuenta, "activo") ~ 1,
                            str_detect(cuenta, "patrimonio") ~ 3,
                            str_detect(cuenta, "impuesto") ~ 54,
                            str_detect(cuenta, "ingresos") ~ 41,
                            str_detect(cuenta, "operaci") ~ 33)) %>%
  map_at(vars(2),
         mutate,
         cuenta = case_when(str_detect(cuenta, "activos") ~ 1,
                            str_detect(cuenta, "patrimonio") ~ 3,
                            str_detect(cuenta, "ordinarias") ~ 41,
                            str_detect(cuenta, "ganancia") ~ 33,
                            str_detect(cuenta, "impuestos") ~ 54)) %>%
  map(spread, cuenta, valor)

# Empresas en 2015 y 2016
empresas_1516 <- seleccion %>%
  map_at(vars(1), select, nit, nombre, depto, munpio, ciiu, fecha) %>%
  map_at(vars(2), select, nit, nombre, depto, ciiu, fecha)

# Remover columnas de informacion de directorio
seleccion <- seleccion %>%
  map_at(vars(1), select, -nombre, -depto, -munpio, -ciiu) %>%
  map_at(vars(2), select, -nombre, -depto, -ciiu)

# Estados financieros entre 2017 y 2018
# Funcion para importar los EF entre 2017 y 2018
importar <- function(archivo, periodo, tipo) {
  tipo <- 
  if (periodo == "2017") {
    if (tipo == 1) {
      n <- 2
    } else if (tipo == 2) {
      n <- 3
    }
  } else if (periodo == "2018") {
    if (tipo == 1) {
      n <- 3
    } else if (tipo == 2) {
      n <- 4
    }
  }
  data <- read_xlsx(archivo, 
                    sheet = n,
                    col_types = "text",
                    col_names = TRUE)
}

# Ejecutar la importacion 
# Balances generales (ESF)
importados[c(7:14)] <- archivos[c(4:11)] %>%
  map2(c("2017", "2017", "2017", "2017", "2018", "2018", "2018", "2018"),
       importar, tipo = 1)
names(importados)[7:14] <- str_replace(nombres[c(4:11)], "EF", "BG")
# Estados de resultados
importados[c(15:22)] <- archivos[c(4:11)] %>%
  map2(c("2017", "2017", "2017", "2017", "2018", "2018", "2018", "2018"),
       importar, tipo = 2)
names(importados)[15:22] <- str_replace(nombres[c(4:11)], "EF", "ER")

# Funcion para limpiar bases entre 2017 y 2018
limpiar <- function(archivo, periodo) {
  names(archivo) <- str_to_lower(names(archivo))
  if (str_detect(periodo, "2017")) {
    data <- archivo
  } else {
    data <- archivo %>% select(-3)
  }
  anno <- str_extract(periodo, "\\d+")
  data <- data %>%
    gather("cuenta", "valor", -c(1:3)) %>%
    mutate(cuenta = str_to_lower(cuenta)) %>%
    filter(!str_detect(cuenta, "sinopsis|resumen")) %>%
    mutate(fecha = paste(anno, "01", "01", sep = "-"),
           anno = NULL,
           fecha = ymd(fecha),
           valor = as.numeric(valor),
           valor = replace_na(valor, 0))
  names(data) <- c("nit", "fecha_corte", "periodo", "cuenta",
                   "valor", "fecha")
  data
}

# Limpieza de los archivos entre 2017 y 2018
limpios <- importados[c(7:22)] %>%
  map2(names(importados)[7:22], limpiar) %>%
  map_if(str_detect(names(.), "2018"), filter, fecha_corte == "43465") %>%
  map_if(str_detect(names(.), "2018"), filter, periodo == "Periodo Actual") %>%
  map_if(str_detect(names(.), "2017"), filter, fecha_corte == "43100") %>%
  map_if(str_detect(names(.), "BG_2017"), filter, periodo == "2017-dic-31") %>%
  map_if(str_detect(names(.), "ER_2017"), filter, periodo != "2016") %>%
  # Remover valores incorrectos
  map_at(vars(10), filter, !(nit == "900531210" & periodo != "2017")) %>%
  map(distinct) %>%
  # Remover columnas utilizadas
  map(select, -fecha_corte, -periodo) %>%
  # Agrupar los valores sin contar el periodo
  map(group_by_at, vars(-valor)) %>%
  map(summarise, valor = sum(valor)) %>%
  map(as_tibble)

# Agrupar estados financieros por periodos
agrupados[[3]] <- bind_rows(limpios[c(1, 2, 3, 4, 9, 10, 11, 12)])
agrupados[[4]] <- bind_rows(limpios[c(5, 6, 7, 8, 13, 14, 15, 16)])
names(agrupados)[3:4] <- c("EF_2017", "EF_2018")

# Remover empresas con EF incompletos
agrupados[[3]] <- agrupados[[3]] %>%
  filter(!nit %in% c("900485245", "900900140")) %>%
  distinct() %>%
  filter(!(nit == "900450226" & str_detect(cuenta, "niif") &
             valor == 0),
         !(nit == "900450226" & cuenta =="ingresos financieros" &
             valor == 0),
         !(nit == "900450226" & cuenta == "costos financieros" 
           & valor == 1366641))

# Limpiar memoria
rm(limpios)

# Ajustar los valores
agrupados[c(3, 4)] <- agrupados[c(3, 4)] %>%
  map(mutate, valor = valor * 1000)

# Seleccion de las cuentas a utilizar en EQMAP
seleccion[c(3, 4)] <- agrupados[c(3, 4)] %>%
  map_at(vars(1),
         filter,
         cuenta %in% c("activos",
                       "patrimonio",
                       "ingresos de actividades ordinarias",
                       "ganancia (pérdida) por actividades de operación",
                       "gasto (ingreso) por impuestos, operaciones continuadas")
         ) %>%
  map_at(vars(2),
         filter,
         cuenta %in% c("total de activos",
                       "patrimonio total",
                       "ingresos de actividades ordinarias",
                       "ganancia (pérdida) por actividades de operación",
                       "ingreso (gasto) por impuestos")
         ) %>%
  # Cambiar nombres de las cuentas por codigos
  map_at(vars(1),
         mutate,
         cuenta = case_when(str_detect(cuenta, "activo") ~ 1,
                            str_detect(cuenta, "patrimonio") ~ 3,
                            str_detect(cuenta, "impuesto") ~ 54,
                            str_detect(cuenta, "ordinarias") ~ 41,
                            str_detect(cuenta, "operaci") ~ 33)) %>%
  map_at(vars(2),
         mutate,
         cuenta = case_when(str_detect(cuenta, "activos") ~ 1,
                            str_detect(cuenta, "patrimonio") ~ 3,
                            str_detect(cuenta, "ordinarias") ~ 41,
                            str_detect(cuenta, "ganancia") ~ 33,
                            str_detect(cuenta, "impuestos") ~ 54)) %>%
  map(spread, cuenta, valor)
names(seleccion)[c(3:4)] <- c("EF_2017", "EF_2018")

# Limpiar memoria
rm(empresas_1516)

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
          paste0(getwd(), "/Limpio/ef_supersociedades.csv"),
          na = "")

# Directorio de empresas
empresas <- agrupados[c(1, 2)] %>%
  map(select, -cuenta, -valor) %>%
  map(distinct) %>%
  map(mutate_at,
      vars(-nit, -ciiu, -fecha), stri_trans_general, "Latin-ASCII") %>%
  bind_rows()

# Importar los directorios de 2017-2018
importar <- function(x, periodo) {
  if (periodo == "2017") {
    n <- 1
  } else {
    n <- 2
  }
  data <- read_excel(x,
                     sheet = n,
                     col_names = TRUE,
                     col_types = "text")
}

# Importar directorios de cada archivo
directorio <- archivos[4:11] %>%
  map2(c("2017", "2017", "2017", "2017", "2018", "2018", "2018", "2018"),
       importar) %>%
  map2(c("2017", "2017", "2017", "2017", "2018", "2018", "2018", "2018"),
       function(x, periodo) {
         names(x) <- str_to_lower(names(x))
         fecha <- paste(periodo, "01", "01", sep = "-")
         x <- x %>%
           select(str_which(names(.),
                            "nit|departamento|ciudad|social de|industrial")) %>%
           select(-str_which(names(.), "judicial")) %>%
           mutate(fecha = ymd(fecha))
         names(x) <- c("nit", "nombre", "ciiu", "depto", "munpio", "fecha")
         x <- x %>%
           mutate_at(vars(nombre, depto, munpio), str_to_lower) %>%
           mutate(ciiu = str_match(ciiu, "\\d+")[, 1],
                  munpio = word(munpio, end = -2, sep = "-"),
                  munpio = str_replace_all(munpio, "-", " ")) %>%
           mutate_at(vars(nombre, depto, munpio),
                     stri_trans_general, "Latin-ASCII")
  }) %>%
  bind_rows() %>%
  bind_rows(empresas) %>%
  mutate(fecha = ymd(fecha)) %>%
  # Agrupacion
  group_by(nit) %>%
  summarise(nom = last(nombre, order_by = fecha),
            ciu = last(ciiu, order_by = fecha),
            dept = last(depto, order_by = fecha),
            munp = last(munpio, order_by = fecha)) %>%
  mutate(dept = case_when(munp == "bogota d.c." ~ "bogota d.c.",
                          TRUE ~ dept),
         munp = case_when(dept == "bogota d.c." ~ "bogota d.c.",
                          TRUE ~ munp)) %>%
  # Empresa sin CIIU
  mutate(ciu = replace(ciu, nit == "900630475", 4111))

# Directorio final supersociedades
write_csv(directorio, 
          paste0(getwd(), "/Limpio/dir_supersociedades.csv"),
          na = "")

