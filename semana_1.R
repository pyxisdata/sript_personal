# Script Estados Financieros
# Librerias
library(readr)
library(tidyverse)

# Carpeta de trabajo
carpeta <- "\\Users\\PC\\Desktop\\Poryecto Semana\\"

# Archivos
archivos <- list.files(carpeta, pattern = ".txt", full.names = TRUE)

# Importar 
BG <- read_delim(archivos[1],
                 delim = "¬", 
                 col_types = paste(rep("c", 77), collapse = ""),
                 locale = locale(encoding = "latin-9"))


# Cuales fueron los Activos totales de todas las empresas
BG_2 <- BG %>%
  gather("Cuentas", "Valores", -1:-9)

          