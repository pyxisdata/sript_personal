# Script para la limpieza de las empresas
# Librerias
library(dplyr)
library(tidyr)
library(lubridate)
library(readxl)
library(readr)
library(purrr)
library(stringr)

# Carpeta
carpeta <- "\\Users\\PC\\Desktop\\EQMAP\\Superfinanciera\\RNVE\\"
archivos <- list.files(carpeta, full.names = TRUE, pattern = ".xlsx")
nombres <- str_match(archivos, "RNVE\\\\(.*?)_financials")[, 2]

# Lectura de los archivos
datos_importados <- map(archivos, read_xlsx, sheet = 1, col_names = FALSE)
names(datos_importados) <- nombres

# Limpieza
limpiar <- function(df) {
  
  nombre <- df[[6, 1]]
  df <- df %>%
    filter(!is.na(.[2])) %>%
    slice(4:n()) %>%
    slice(1:(n() - 6))
  names(df) <- unlist(df[1, ])
  df <- df %>%
    slice(2:n()) %>%
    mutate(nom_cuenta = .[[1]],
           nom_cuenta = str_trim(nom_cuenta),
           nom_cuenta = str_squish(nom_cuenta),
           nom_cuenta = str_to_lower(nom_cuenta)) %>%
    gather("fecha", "valor", `2018`:`2015`) %>%
    mutate(valor = as.double(valor),
           empresa = nombre,
           fecha = ymd(paste(fecha, "01", "01", sep = "-"))) %>%
    select(-1)
  df
}
  





