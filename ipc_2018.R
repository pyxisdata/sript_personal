# Script de inflacion
# Librerias
library(dplyr)
library(tidyr)
library(lubridate)
library(readxl)
library(stringr)
library(stringi)
library(purrr)
library(readr)

# Eliminar la notacion cientifica
options(scipen = 000)

# Importar archivos
setwd("\\Users\\PC\\Desktop\\Archivos Brutos\\IPC\\Indices")

# Funcion para reemplazar los municipios por codigos divipola
divipola <- function(x) {
  x <- x %>%
    str_to_lower() %>%
    stri_trans_general("Latin-ASCII") %>%
    replace(str_detect(., "bogota"), 11001) %>%
    replace(str_detect(., "medellin"), 5001) %>%
    replace(str_detect(., "cali"), 76001) %>%
    replace(str_detect(., "barranquilla"), 8001) %>%
    replace(str_detect(., "bucaramanga"), 68001) %>%
    replace(str_detect(., "otras"), 100000) %>%
    replace(str_detect(., "cartagena"), 13001) %>%
    replace(str_detect(., "cucuta"), 54001) %>%
    replace(str_detect(., "pereira"),  66001) %>%
    replace(str_detect(., "villavicencio"), 50001) %>%
    replace(str_detect(., "manizales"), 17001) %>%
    replace(str_detect(., "ibague"), 73001) %>%
    replace(str_detect(., "pasto"), 52001) %>%
    replace(str_detect(., "santa marta"), 47001) %>%
    replace(str_detect(., "neiva"), 41001) %>%
    replace(str_detect(., "armenia"), 63001) %>%
    replace(str_detect(., "valledupar"), 20001) %>%
    replace(str_detect(., "monteria"), 23001) %>%
    replace(str_detect(., "popayan"), 19001) %>%
    replace(str_detect(., "tunja"), 15001) %>%
    replace(str_detect(., "sincelejo"), 70001) %>%
    replace(str_detect(., "riohacha"), 44001) %>%
    replace(str_detect(., "florencia"), 18001)
}

# Ponderaciones
# Ponderaciones a 5 digitos de los productos
pond <- list.files(pattern = "ponderaciones") %>%
  read_excel(sheet = 1,
             col_types = "text",
             col_names = FALSE)
names(pond) <- c("id_munpio", "tipo", "id_coicop", "nombre",
                 "id_nivel", "ponderador")

# Ponderaciones a 5 digitos de las ciudades
pond_ciudades <- pond %>%
  filter(tipo == "Total") %>%
  filter(str_detect(id_nivel, "Total")) %>%
  filter(id_munpio != "Nacional") %>%
  mutate(id_munpio = divipola(id_munpio),
         id_nivel = 1,
         ponderador = round(as.numeric(ponderador) / 100, 5)) %>%
  select(id_munpio, id_nivel, ponderador)

# Transformacion de las ponderaciones de los productos
pond_trans <- pond %>%
  filter(tipo %in% c("División", "Subclase")) %>%
  filter(id_munpio != "Nacional") %>%
  mutate(id_munpio = divipola(id_munpio),
         id_nivel = 1,
         ponderador = round(as.numeric(ponderador) / 100, 5)) %>%
  filter(!(id_munpio %in% c(11001, 5001, 76001, 8001, 68001,
                            13001, 54001, 66001, 50001, 17001,
                            52001, 41001, 23001) &
           tipo == "División")) %>%
  select(-tipo, -nombre) %>%
  left_join(pond_ciudades, by = c("id_munpio", "id_nivel")) %>%
  rename(pond_coicop = ponderador.x,
         pond_ciudad = ponderador.y)

# Limpiar memoria
rm(pond, pond_ciudades)

# Lista de los archivos de los indices
# Indices de subclase por ciudades
subclase <- list.files(pattern = "subclase") %>%
  map(read_excel,
      sheet = 1,
      col_types = "text",
      col_names = TRUE) %>%
  bind_rows()
names(subclase) <- c("anno", "mes", "id_nivel", "id_coicop",
                     "id_munpio", "indice")
  
# Transformacion de los indices de subclase
subclase_trans <- subclase %>%
  mutate(mes = case_when(mes == "Ene" ~ 1,
                         mes == "Feb" ~ 2,
                         mes == "Mar" ~ 3,
                         mes == "Abr" ~ 4,
                         mes == "May" ~ 5,
                         mes == "Jun" ~ 6,
                         mes == "Jul" ~ 7,
                         mes == "Ago" ~ 8,
                         mes == "Sep" ~ 9,
                         mes == "Oct" ~ 10,
                         mes == "Nov" ~ 11,
                         mes == "Dic" ~ 12),
         id_nivel = 1,
         id_coicop = word(id_coicop, 1, sep = " - "),
         id_munpio = divipola(id_munpio),
         indice = round(as.numeric(indice), 5),
         fecha = ymd(paste(anno, mes, 1, sep = "-"))) %>%
  select(-anno, -mes) %>%
  select(fecha, everything())

# Limpiar memoria
rm(subclase)

# Indices de division por ciudades
division <- list.files(pattern = "division") %>%
  map(read_excel,
      sheet = 1,
      col_types = "text",
      col_names = TRUE) %>%
  bind_rows()
names(division) <- c("anno", "mes", "id_nivel", "id_coicop",
                     "id_munpio", "indice")

# Transformacion de los indices de division
division_trans <- division %>%
  mutate(mes = case_when(mes == "Ene" ~ 1,
                         mes == "Feb" ~ 2,
                         mes == "Mar" ~ 3,
                         mes == "Abr" ~ 4,
                         mes == "May" ~ 5,
                         mes == "Jun" ~ 6,
                         mes == "Jul" ~ 7,
                         mes == "Ago" ~ 8,
                         mes == "Sep" ~ 9,
                         mes == "Oct" ~ 10,
                         mes == "Nov" ~ 11,
                         mes == "Dic" ~ 12),
         id_nivel = 1,
         id_coicop = word(id_coicop, 1, sep = " - "),
         id_munpio = divipola(id_munpio),
         indice = round(as.numeric(indice), 5),
         fecha = ymd(paste(anno, mes, 1, sep = "-"))) %>%
  select(-anno, -mes) %>%
  select(fecha, everything()) %>%
  filter(!id_munpio %in% c(11001, 5001, 76001, 8001, 68001,
                           13001, 54001, 66001, 50001, 17001,
                           52001, 41001, 23001))

# Limpiar memoria
rm(division)

# Indices totales por ciudades
total <- list.files(pattern = "totcanasta_ciudades") %>%
  map(read_excel,
      sheet = 1,
      col_types = "text",
      col_names = TRUE) %>%
  bind_rows()
names(total) <- c("anno", "mes", "id_nivel", "id_munpio", "indice")

# Transformacion de los indices totales por ciudades
total_trans <- total %>%
  mutate(mes = case_when(mes == "Ene" ~ 1,
                         mes == "Feb" ~ 2,
                         mes == "Mar" ~ 3,
                         mes == "Abr" ~ 4,
                         mes == "May" ~ 5,
                         mes == "Jun" ~ 6,
                         mes == "Jul" ~ 7,
                         mes == "Ago" ~ 8,
                         mes == "Sep" ~ 9,
                         mes == "Oct" ~ 10,
                         mes == "Nov" ~ 11,
                         mes == "Dic" ~ 12),
         id_nivel = 1,
         id_munpio = divipola(id_munpio),
         indice = round(as.numeric(indice), 5),
         fecha = ymd(paste(anno, mes, 1, sep = "-"))) %>%
  select(-anno, -mes) %>%
  select(fecha, everything())

# Limpiar memoria
rm(total)

# Indices totales nacional
nacional <- list.files(pattern = "totcanasta_nacional") %>%
  read_excel(sheet = 1,
             col_types = "text",
             col_names = TRUE)
names(nacional) <- c("anno", "mes", "id_nivel", "indice")

# Transformacion de los indices nacional
nacional_trans <- nacional %>%
  mutate(mes = case_when(mes == "Ene" ~ 1,
                         mes == "Feb" ~ 2,
                         mes == "Mar" ~ 3,
                         mes == "Abr" ~ 4,
                         mes == "May" ~ 5,
                         mes == "Jun" ~ 6,
                         mes == "Jul" ~ 7,
                         mes == "Ago" ~ 8,
                         mes == "Sep" ~ 9,
                         mes == "Oct" ~ 10,
                         mes == "Nov" ~ 11,
                         mes == "Dic" ~ 12),
         id_nivel = 1,
         indice = round(as.numeric(indice), 5),
         fecha = ymd(paste(anno, mes, 1, sep = "-"))) %>%
  select(-anno, -mes) %>%
  select(fecha, everything())

# Limpiar memoria
rm(nacional)

# Union de las bases de indices
indices <- list(subclase_trans, division_trans) %>%
  bind_rows() %>%
  left_join(pond_trans, by = c("id_nivel", "id_coicop", "id_munpio")) %>%
  mutate(indice_comp = round(indice * pond_coicop, 5))

# Totales artificales
total_artificial <- indices %>%
  group_by(fecha, id_nivel, id_munpio) %>%
  summarise(indice_comp = round(sum(indice_comp), 5)) %>%
  left_join(total_trans, by = c("fecha", "id_nivel", "id_munpio")) %>%
  ungroup() %>%
  mutate(diff = round(indice - indice_comp, 5),
         id_coicop = "00000000",
         ponderador = round(diff / indice, 5),
         indice2 = round(diff / ponderador, 5),
         indice2 = replace(indice2, indice2 %in% c(Inf, -Inf), 0),
         indice2 = replace_na(indice2, 0)) %>%
  select(-indice_comp, -indice, -diff) %>%
  rename(indice = indice2)

# Indices conjuntos
indices_conjuntos <- list(subclase_trans,
                          division_trans,
                          total_artificial) %>%
  bind_rows() %>%
  left_join(select(pond_trans, -pond_ciudad),
            by = c("id_nivel", "id_coicop", "id_munpio")) %>%
  mutate(pond_coicop = ifelse(is.na(ponderador), pond_coicop, ponderador)) %>%
  select(-ponderador) %>%
  left_join(distinct(pond_trans, id_munpio, id_nivel, pond_ciudad),
            by = c("id_nivel", "id_munpio")) %>%
  mutate(indice_comp = round(indice * pond_coicop * pond_ciudad, 5))

# Segundo totales artificales
total_artificial_segundo <- indices_conjuntos %>%
  group_by(fecha, id_nivel) %>%
  summarise(indice_comp = round(sum(indice_comp), 5)) %>%
  ungroup() %>%
  left_join(nacional_trans, by = c("fecha", "id_nivel")) %>%
  mutate(diff = round(indice - indice_comp, 5),
         id_munpio = "00000",
         id_coicop = "00000000",
         pond_coicop = 1,
         ponderador = round(diff / indice, 5),
         indice2 = round(diff / ponderador, 5),
         indice2 = replace(indice2, indice2 %in% c(Inf, -Inf), 0)) %>%
  select(-indice_comp, -indice, -diff) %>%
  rename(indice = indice2,
         pond_ciudad = ponderador)

# Indices finales
indices_procesados <- indices_conjuntos %>%
  select(-indice_comp) %>%
  bind_rows(total_artificial_segundo) %>%
  # Intercambio de codigo coicop
  mutate(id_coicop = case_when(id_coicop == "01000000" ~ "01110100",
                               id_coicop == "02000000" ~ "02110100",
                               id_coicop == "03000000" ~ "03120100",
                               id_coicop == "04000000" ~ "04130100",
                               id_coicop == "05000000" ~ "05110100",
                               id_coicop == "06000000" ~ "06110300",
                               id_coicop == "07000000" ~ "07110100",
                               id_coicop == "08000000" ~ "08210200",
                               id_coicop == "09000000" ~ "09110100",
                               id_coicop == "10000000" ~ "10110400",
                               id_coicop == "11000000" ~ "11110100",
                               id_coicop == "12000000" ~ "12110100",
                               TRUE ~ id_coicop))
          
# Paso opcional a la hora de ejecutar el codigo
# Verificacion
verificacion <- indices_procesados %>%
  mutate(indice_comp = round(indice * pond_coicop * pond_ciudad, 5)) %>%
  group_by(fecha, id_nivel) %>%
  summarise(indice_comp = round(sum(indice_comp), 5)) %>%
  ungroup() %>%
  left_join(nacional_trans, by = c("fecha", "id_nivel")) %>%
  mutate(diff = round(indice - indice_comp, 5))

# Exportar bases de datos de IPC terminada y finalizada
write_csv(indices_procesados,
          "ipc_historico.csv",
          na = "")
