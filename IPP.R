# Script para la transformacion del IPP Mensual

# Librerias
library(dplyr)
library(tidyr)
library(lubridate)
library(odbc)
library(readxl)

# Quitar la notacion cientifica
options(scipen = 999)

# Formacion de las fechas 
today <- Sys.Date()
t_month <- month(today) - 1
t_year <- year(today)

# Nombres de los meses 
months <- c("ene", "feb", "mar", "abr", "may", "jun",
            "jul", "ago", "sep", "oct", "nov", "dic")

url_month <- months[t_month]
fix_url <- paste("http://www.dane.gov.co/files/investigaciones/",
                 "boletines/ipp/anexo_ipp_", 
                 sep = "")
ext_url <- ".xlsx"
url <- paste(fix_url, url_month, substr(t_year, 3, 4), ext_url, 
             sep = "")

# Nombre del archivo 
folder <- "\\Users\\PC\\Desktop\\Archivos Brutos\\IPP\\"
file <- paste(folder, "IPP", "_", url_month, "_", t_year, ext_url,
              sep = "")

# Descarga del archivo
download.file(url, file, mode = "wb")

# Funcion para traer la data del archivo de IPP
read_ipp <- function(file) {
  
  sheets <- excel_sheets(file)
  
  data_sheets <- paste(c("1.1", "2.1", "3.1", "4.1", "5.1", "6.1", "7.1", 
                         "8.1", "9.1", "10.1", "11.1", "12.1", "13.1"),
                       collapse = "|")
  
  numbers <- grep(data_sheets, sheets)
  
  data_list <- list()
  
  for (i in seq_along(numbers)) {
    
    data <- read_excel(file, 
                       sheet = numbers[i],
                       col_names = FALSE)
    
    data_list[[i]] <- data

  }
  
  names(data_list) <- c(paste("ipp", 1:13, sep = "_"))
  
  assign("raw_list", data_list, envir = .GlobalEnv)  
  
}

# Leer todos los datos 
read_ipp(file)

# Funcion para hacer las fechas
make_dates <- function(date) {
  
  initial <- ymd("2014-12-01")
  
  range <- length(seq(from = initial,
                      to = date,
                      by = "month")
                  )
  
  dates <- seq(initial, 
               by = "month",
               length.out = range - 1)
  
  assign("dates", dates, envir = .GlobalEnv)
  
}

# Fechas
make_dates(today)

# Transformacion de la data ----------------------------------------------
refin_ipp <- function(lista) {
  
  refin_list <- list()
  
  for (i in seq_along(lista)) {
    
  data <- lista[[i]] %>%
    filter(!is.na(.[[2]])) %>%
    slice(-1) %>%
    fill(...1) %>%
    mutate(nom = tolower(.[[1]]),
           codes = replace(nom, grepl("tot", nom), 9),
           codes = replace(codes, grepl("ecc", nom), 1),
           codes = replace(codes, grepl("ivi", nom), 2),
           codes = replace(codes, grepl("rup", nom), 3),
           codes = replace(codes, grepl("las", nom), 4),
           codes = replace(codes, grepl("cpc", nom), 5),
           nom = codes,
           codes = NULL) %>%
    select(-1, -3) %>%
    select(nom, everything())
  names(data) <- c("id_nivel", "id_ciiu4", as.character(dates))
  data %>%
    gather(fecha, indice, -id_nivel, -id_ciiu4) %>%
    mutate(id_nivel = as.numeric(id_nivel),
           id_ciiu4 = as.character(id_ciiu4),
           fecha = ymd(fecha),
           indice = as.numeric(indice),
           id_base = i) -> data
  
  refin_list[[i]] <- data

  }
  
  assign("refin_list", refin_list, envir = .GlobalEnv)
  
}

# Refinar archivos de la lista
refin_ipp(raw_list)

# Conexion a SQL
conex <- dbConnect(odbc(),
                   Driver = "SQL Server",
                   Server = "40.87.53.140",
                   Database = "PyxisFinale",
                   UID = "alejandro.peralta",
                   PWD = "Pyxy54dm1n2018",
                   Encoding = "latin-9")

dbGetQuery(conex, "DELETE FROM IPP")

for (i in seq_along(refin_list)) {

  dbWriteTable(conex,
               "IPP",
               refin_list[[i]],
               append = TRUE)
  print("terminado")
  
}

