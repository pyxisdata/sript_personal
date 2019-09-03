# Formato 351

# Librerias
library(dplyr)
library(tidyr)
library(odbc)
library(lubridate)
library(readxl)
library(stringr)

# Carpeta
folder <- "\\Users\\PC\\Desktop\\Archivos Brutos\\Fondos\\Formato 351"
folder_list <- list.files(folder, pattern = ".xls", full.names = TRUE)

# Funcion de lecturas
read_format <- function(x) {
  
  files <- list()
  names <- c()
  for (i in seq_along(x)) {
    data <- read_xls(x[i], sheet = 2, col_names = TRUE)
    year <- str_sub(word(x[i], 2, sep = "/"), 1, 4)
    month <- str_sub(word(x[i], 2, sep = "/"), 5, 6)
    data$fecha_m <- ymd(paste(year, month, "01", sep = "-"))
    files[[i]] <- data
    name <- paste("m", year, month, sep = "_")
    names[i] <- name
  }
  
  names(files) <- names
  assign("format", files, envir = .GlobalEnv)
}

# Lectura de archivos
read_format(folder_list)

# Unificar
data <- bind_rows(format)

