# Limpieza de gazeta

# Librerias
library(stringr)
library(lubridate)
library(dplyr)
library(purrr)
library(readxl)
library(tidyr)

# Carpeta
folder <- "\\Users\\PC\\Desktop\\"
file <- "Excel_Report (5).xlsx"

# Importar
data <- read_xlsx(paste(folder, file, sep = ""),
                  sheet = 1,
                  col_names = FALSE,
                  col_types = "text")

# Numero de gaceta
gaceta <- as.numeric(data[[1]][3])

# Limpieza eliminar nulos, columnas innecesarias y añadir partes de la fecha
data %>%
  filter(!is.na(X__4)) %>%
  mutate(X__2 = gsub("[^[:alnum:] ]", "", X__2)) %>%
  select(-c(3, 5, 10, 14, 16)) %>%
  mutate(dia = as.numeric(substr(X__4, 1, 2)),
         mes_letra = trimws(substr(X__4, 4, 7), "both"),
         mes_numero = mes_letra,
         año = as.numeric(substr(X__4, 8, 11))) -> data2

# Numero de los meses
numero <- setNames(c(1:12), 
                   c("ene", "feb", "mar", "abr", "may", "jun",
                     "jul", "ago", "sep", "oct", "nov", "dic"))

# Nombre largo de los meses
nombre <- setNames(c("enero", "febrero", "marzo", "abril", "mayo",
                     "junio", "julio", "agosto", "septiembre",
                     "octubre", "noviembre", "diciembre"),
                   c("ene", "feb", "mar", "abr", "may", "jun",
                     "jul", "ago", "sep", "oct", "nov", "dic"))

# Reemplazar nombres y numeros
data2$mes_letra[] <- nombre[unlist(data2$mes_letra)]
data2$mes_numero[] <- numero[unlist(data2$mes_numero)]

# Enocntrar el que tiene el mayor numero de particiones
lista <- str_split(string = data2$X__9, pattern = ",")
maximo <- max(map_dbl(lista, length))

# Segunda limpieza, refinar la fecha, hacer unpivot y añadir gaceta
data2 %>%
  mutate(fecha = mdy(paste(mes_numero, dia, año, sep = "-"))) %>%
  separate(X__9, c(paste0("col", 1:maximo)), sep = ",", extra = "merge"
           ) %>%
  gather(numero, categoria, c(paste0("col", 1:maximo))) %>%
  filter(!is.na(categoria)) %>%
  select(-numero) %>%
  mutate(gaceta = gaceta) -> data3

# Definir los encabezados
nombres <- unlist(data3[1, ])
cabezas <- names(data3)

# Añadir encabezados
nombres <- ifelse(is.na(nombres),
                  cabezas,
                  nombres)

names(data3) <- tolower(nombres)

# Eliminar primera fila
data3 %>%
  slice(2:n()) -> data3

# Nombres de las columnas finales
names(data3)[c(17, 18)] <- c("categoria", "gaceta")

# Exportar a csv
write.csv(data3, 
          paste(folder, gaceta, ".csv", sep = ""),
          row.names = FALSE,
          na = "" 
          )





