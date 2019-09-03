# Script para la Encuesta Anual Manufacturera

# Librerias
library(dplyr)
library(lubridate)
library(haven)
library(tidyr)
library(odbc)
library(readr)
library(purrr)

# Sin notacion cientifica para evadir el Inf
options(scipen = 999)

# Carpeta
folder <- "\\Users\\PC\\Desktop\\Archivos Brutos\\Encuestas Anuales\\Industria\\"

# Lista de archivos 
file_list <- grep(".sav",
                  list.files(folder, full.names = TRUE),
                  value = TRUE
)

# Leer los archivos 
read_eam <- function(x) {
  
  eam_list <- list()
  
  names_list <- vector("character", length = length(x))
  
  for (i in seq_along(x)) {
    
    data <- read_sav(file = x[i], user_na = TRUE)
    
    date <- paste(parse_number(x[i]), "01", "01", sep = "-")
    
    data %>%
      mutate(fecha = ymd(date)) %>%
      mutate_at(vars(-fecha), as.character) %>%
      mutate_at(vars(-fecha), as.numeric) -> data
    
    names(data) <- trimws(tolower(names(data)), which = "both")
    
    ciiu <- grep("ciiu", tolower(names(data)))
    
    names(data)[ciiu] <- "ciiu"
    
    name <- paste("eam", parse_number(x[i]),sep = "_")
    
    eam_list[[i]] <- data
    
    names_list[i] <- name
    
  }
  
  names(eam_list) <- names_list
  
  assign("eam_list", eam_list, envir = .GlobalEnv)
  
}

# Creacion de la lista
read_eam(file_list)

# Creacion de la lista final
eam_final <- bind_rows(eam_list)

# Conexion a SQL
conex <- dbConnect(odbc(),
                   Driver = "SQL Server",
                   Server = "40.87.53.140",
                   Database = "PyxisFinale",
                   UID = "alejandro.peralta",
                   PWD = "Pyxy54dm1n2018",
                   Encoding = "latin-9"
)

# Crear la tabla
dbWriteTable(conex,
             "EAM2",
             eam_final,
             append = TRUE
)
