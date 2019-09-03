# Script para la transformacion del IPP Mensual

# Librerias
library(dplyr)
library(tidyr)
library(lubridate)
library(odbc)
library(readxl)

# Quitar la notacion cientifica
options(scipen = 999)

# Nombre del archivo 
folder <- "\\Users\\PC\\Desktop\\Archivos Brutos\\IPP\\"
file <- paste(folder, "Ponderaciones_ipp_15.xls", sep = "")

# Leer archivos
read_pnd <- function(file) {
  
  sheets <- excel_sheets(file)
  
  data_list <- list()
  
  for (i in seq_along(sheets)) {
    
    data <- read_excel(file, 
                       sheet = sheets[i],
                       col_names = FALSE)
    
    data_list[[i]] <- data
    
  }
  
  names(data_list) <- sheets
  
  assign("pnd_list", data_list, envir = .GlobalEnv)  
  
}

# Leer todos los datos 
read_pnd(file)

# Transformacion de la data ----------------------------------------------
refin_pnd <- function(list) {
  
  refin_list <- list()
  
  numbers <- c(1, 2, 3, 4, 5, 10, 9, 11, 12)

  for (i in seq_along(list)) {
  
    list[[i]] %>%
      filter(!is.na(X__2)) %>%
      slice(2:n()) %>%
      fill(X__1) %>%
      mutate(X__1 = tolower(X__1),
            codes = replace(X__1, grepl("tot", X__1), 9),
            codes = replace(codes, grepl("ecc", X__1), 1),
            codes = replace(codes, grepl("ivi", X__1), 2),
            codes = replace(codes, grepl("rup", X__1), 3),
            codes = replace(codes, grepl("las", X__1), 4),
            codes = replace(codes, grepl("cpc", X__1), 5),
            X__1 = codes,
            codes = NULL,
            id_base = numbers[i]) %>%
      select(-3) %>%
      mutate_at(vars(-2), as.character) %>%
      mutate_at(vars(-2), as.numeric) -> data
  
    names(data) <- c("id_nivel", "id_ciiu4", "pnd", "id_base")
  
    refin_list[[i]] <- data
  
  }

  assign("refin_list", refin_list, envir = .GlobalEnv)

}

# Refinar archivos de la lista
refin_pnd(pnd_list)

# Conexion a SQL
conex <- dbConnect(odbc(),
                   Driver = "SQL Server",
                   Server = "40.87.53.140",
                   Database = "PyxisFinale",
                   UID = "alejandro.peralta",
                   PWD = "Pyxy54dm1n2018",
                   Encoding = "latin-9")


for (i in seq_along(refin_list)) {
  
  dbWriteTable(conex,
               "Ponderadores_IPP",
               refin_list[[i]],
               append = TRUE)
  
}



