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
                      sheet = 1,
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

data <- raw_18[[1]]

data %>%
  filter(!is.na(X__4)) %>%
  as.matrix() %>%
  t() %>%
  as.data.frame() %>%
  fill(V1, V2) %>%
  mutate(V1 = paste(V1, V2, V3, sep = "_")) %>%
  select(-2:-3) %>%
  as.matrix() %>%
  t() %>% 
  as.data.frame() -> dataf

names(dataf) <- unlist(dataf[1, ])

dataf %>%
  slice(2:n()) %>%
  gather(value, name, -1:-3) -> dataf
