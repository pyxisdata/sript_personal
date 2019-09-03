# Script para la Encuesta Anual de Servicios

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
folder <- "\\Users\\PC\\Desktop\\Archivos Brutos\\Encuestas Anuales\\Servicios\\"

# Lista de archivos 
file_list <- grep(".sav",
                  list.files(folder, full.names = TRUE),
                  value = TRUE
)

# Leer los archivos 
read_eas <- function(x) {
  
  eas_list <- list()
  
  names_list <- vector("character", length = length(x))
  
  for (i in seq_along(x)) {
    
    data <- read_sav(file = x[i], user_na = TRUE)
    
    date <- paste(parse_number(x[i]), "01", "01", sep = "-")
    
    section <- grep("seccion", tolower(names(data)))
    
    data %>%
      mutate(fecha = ymd(date)) %>%
      mutate_at(vars(-fecha, -section), as.numeric) -> data
    
    names(data) <- trimws(tolower(names(data)), which = "both")
    
    div <- grep("division", names(data))
    
    names(data)[div] <- "division"
    
    logi <- TRUE %in% grepl("subseccion", names(data))
    
    if (logi == TRUE) {
      
      login <- grep("subseccion", names(data))
      
      data %>%
        select(-seccion) -> data
      
    } else {
      
    }
    
    section <- grep("seccion", names(data))
    
    names(data)[section] <- "seccion"
    
    name <- paste("eas", parse_number(x[i]),sep = "_")
    
    eas_list[[i]] <- data
    
    names_list[i] <- name
    
  }
  
  names(eas_list) <- names_list
  
  assign("eas_list", eas_list, envir = .GlobalEnv)
  
}

# Creacion de la lista
read_eas(file_list)

# Creacion de la lista final
eas_final <- bind_rows(eas_list)

# Transformacion final
eas_final %>%
  mutate(insertot = ifelse(is.na(insertot),
                                 insertot_numera1,
                                 insertot),
         presperm = ifelse(is.na(presperm),
                                 preesperm,
                                 presperm),
         presmision = ifelse(is.na(presmision),
                                   preesmision,
                                   presmision),
         prestemp = ifelse(is.na(prestemp),
                                 preestemp,
                                 prestemp)
  ) %>%
  select(-insertot_numera1, -preestemp, -preesmision, -preesperm) -> eas_final


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
             "EAS",
             eas_final,
             append = TRUE
)