# Script para transformacion de Industria Trimestral - MTMR

# Liberias
library(dplyr)
library(tidyr)
library(readxl)
library(lubridate)
library(odbc)

# La fecha de hoy para calcular el trimestre de descarga 
today <- Sys.Date()
quart <- quarter(today) - 1 
year <- year(today)

quart_num <- c("I", "II", "III", "IV")

# Archivo de descarga 
fix_url <- paste("http://www.dane.gov.co/files/investigaciones/boletines/",
                 "mtmr/Indices_regiones_",
                 sep = "")

quart_url <- paste("Trim", quart_num[quart], "_", year,
                   sep = "")

ext_url <- ".xls"

url <- paste(fix_url, quart_url, ext_url, 
             sep = "")

# Nombre del archivo 
folder <- "\\Users\\PC\\Desktop\\Archivos Brutos\\Industria Trimestral\\"
  
file <- paste(folder, "mtmr", "_", quart_num[quart], "_", year, ext_url,
              sep = "")

# Descargar el archivo
download.file(url, file, mode = "wb")

# Leer toda las pestañas necesarias
read_mtmr <- function(file) {
  
  # Lista de pestañas
  sheets <- excel_sheets(file)
  
  # Patrones para las busquedas de las pestañas
  sheet_patt <- paste(c("bogot", "medell", "cali", "costa", "eje", "santand"),
                      collapse = "|")
  
  index <- grep(sheet_patt, tolower(sheets))
  
  mtmr <- list()
  
  for (i in seq_along(index)) {
    
    data <- read_xls(file, 
                     sheet = index[i],
                     col_names = FALSE)
    
    mtmr[[i]] <- data
    
  }
  
  names(mtmr) <- c("bogota", "medellin", "cali", "costa", "eje", "santand")
  
  assign("raw_data", 
         mtmr,
         envir = .GlobalEnv)

}

# Lectura de todos los archivos
read_mtmr(file)

# Transformar la data dentro de la lista de bases 
refin_mtmr <- function(list) {
  
  mtmr2 <- list()
  
  for(i in seq_along(list)) {
  
    data <- list[[i]]
    
    data %>%
      filter(!is.na(X__2)) %>%
      slice(2:n()) %>% 
      mutate_at(vars(-X__4), as.character) %>%
      mutate_at(vars(-X__4), as.numeric) %>%
      mutate(fecha = ymd(paste(X__1, X__2, 1, 
                              sep = "-")
                        )
      ) %>%
      select(fecha, 3:7, -4) %>%
      gather(medida, indice, -fecha, -X__3) %>%
      mutate(medida = ifelse(medida == "X__5",
                            2,
                            ifelse(medida == "X__6",
                                    4,
                                    5)
                            ),
            id_munpio = i,
            fecha = ymd(fecha)
      ) -> data
    
    names(data) <- c("fecha", "id_ciiu3do", "id_medida", "indice", "id_region")
    
    mtmr2[[i]] <- data
    
  }
  
  names(mtmr2) <- c("bogota", "medellin", "cali", "costa", "eje", "santand")
  
  assign("refined_data", 
         mtmr2,
         envir = .GlobalEnv)
  
}

# Archivos terminados
refin_mtmr(raw_data)

# Conexion a SQL
conex <- dbConnect(odbc(),
                   Driver = "SQL Server",
                   Server = "40.87.53.140",
                   Database = "PyxisFinale",
                   UID = "alejandro.peralta",
                   PWD = "Pyxy54dm1n2018",
                   Encoding = "latin-9"
                   )

# Vaciar data
dbGetQuery(conex, "DELETE FROM MTMR")

# Escribimos las tablas
for (i in seq_along(refined_data)) {
  
  dbWriteTable(conex, 
               "MTMR",
               refined_data[[i]], 
               append = TRUE)
             
}

  



