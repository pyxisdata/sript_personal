# Script para la Encuesta Anual de Comercio

# Librerias
library(dplyr)
library(lubridate)
library(haven)
library(tidyr)
library(odbc)
library(readr)
library(purrr)

# Carpeta
folder <- "\\Users\\PC\\Desktop\\Archivos Brutos\\Encuestas Anuales\\Comercio\\"

# Lista de archivos 
file_list <- grep(".sav",
                  list.files(folder, full.names = TRUE),
                  value = TRUE
                  )

# Leer los archivos 
read_eac <- function(x) {
  
  eac_list <- list()
  
  names_list <- vector("character", 15)
  
  for (i in seq_along(x)) {
    
    data <- read_sav(file = x[i], user_na = TRUE)
  
    date <- paste(parse_number(x[i]), "01", "01", sep = "-")
  
    data %>%
      mutate(fecha = ymd(date)) %>%
      mutate_at(vars(-fecha), as.numeric) -> data
    
    names(data) <- trimws(tolower(names(data)), which = "both")
  
    name <- paste("eac", parse_number(x[i]),sep = "_")
    
    eac_list[[i]] <- data
    
    names_list[i] <- name
    
  }
  
  names(eac_list) <- names_list
  
  assign("eac_list", eac_list, envir = .GlobalEnv)
  
}

# Creacion de la lista
read_eac(file_list)

# Creacion de la lista final
eac_final <- bind_rows(eac_list)

# Transformacion final
eac_final %>%
  mutate(idnoremp = ifelse(is.na(idnoremp),
                           idnoremp_publ,
                           idnoremp),
         idnoremp = ifelse(is.na(idnoremp),
                           idnoremp_pub,
                           idnoremp),
         idnoremp = ifelse(is.na(idnoremp),
                           idnoremp1,
                           idnoremp),
         idoj1 = ifelse(is.na(idoj1),
                        idoj,
                        idoj1)
         ) %>%
  select(-idoj, -idnoremp1, -idnoremp_pub, -idnoremp_publ) -> eac_final


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
             "EAC",
             eac_final,
             append = TRUE
)

