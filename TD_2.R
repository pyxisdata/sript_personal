# Script para las tasas y Desembolsos

# Librerias
library(dplyr)
library(tidyr)
library(lubridate)
library(purrr)
library(readxl)

# Quitar la notacion cientifica
options(scipen = 999)

# Carpetas
folder <- paste("\\Users\\PC\\Desktop\\Archivos Brutos\\",
                "Superfinanciera\\TD_Semanales\\2018\\",
                sep = ""
)

# Lista
folder_list <- list.files(folder, full.names = TRUE)

# Lectura de los archivos 
read_td <- function(files) {
  
  data_list <- list()
  names <- c()
  
  for (i in seq_along(files)) {
    
    data <- read_excel(files[i], 
                       sheet = "Base",
                       col_names = FALSE,
                       trim_ws = TRUE
    )
    
    name <- substr(files[i], 91, 96)
    
    data_list[[i]] <- data
    names[i] <- name
    
  }
  
  names(data_list) <- names
  
  year <- substr(folder_list[1], 98, 99)
  
  list_name <- paste("raw_", year, sep = "")
  
  assign(list_name, data_list, envir = .GlobalEnv)
  
}

# Leer los archivos
read_td(folder_list)

# Limpieza
clean_td <- function(list) {
  
  data_list <- list()
  names <- c()
  
  for (i in seq_along(list)) {
    
    data <- list[[i]]
    
    names(data) <- c("fecha", "id_tipo", "id_entidad", 
                     "id_valor", "id_uc", "id_tuc", "valor")
    
    data %>%
      slice(2:n()) %>%
      mutate(fecha = date(fecha)) %>%
      mutate_at(vars(-fecha), as.character) %>%
      mutate_at(vars(-fecha), as.numeric) -> data
      
    data_list[[i]] <- data
    
  }
    
  data_full <- bind_rows(data_list)
  
  year <- year(data_full$fecha[1])
  
  data_name <- paste("final", year, sep = "_")
  
  assign(data_name, data_full, envir = .GlobalEnv)
    
}

# Refinar los archivos
clean_td(raw_18)

