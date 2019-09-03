# Script para SIPSA

# Librerias
library(dplyr)
library(tidyr)
library(odbc)
library(readxl)
library(lubridate)
library(stringr)

# Espacio de Trabajo 
setwd("\\Users\\PC\\Desktop\\Archivos Brutos\\SIPSA")

# Definimos URL
fixurl <- "http://www.dane.gov.co/files/investigaciones/agropecuario/sipsa/Sem_"
exturl <- ".xlsx"

# Definimos Fechas
today <- Sys.Date()
yearurl <- year(today)

week <- "06oct__12oct_"

# Nombre de la URL
url <- paste(fixurl, week, yearurl, exturl, sep = "")

# Descargar el archivo
download.file(url, 
              paste(week, exturl, sep = ""),
              mode = "wb"
              )

# Fecha del archivo
opmes <- c("ene", "feb", "mar", "abr", "may", "jun", "jul", "ago", "sep", 
           "oct", "nov", "dic") 

mesR <- c(paste("0", c(1:9), sep = ""), c(10:12))

opdia <- c(paste("0", c(1:9), sep = ""), c(10:31))

fechad <- c()

# Fechas del DANE
for (mes in opmes) {
  for (dia in opdia) {
    fechad <- c(fechad, paste(dia, mes, sep = "")
                )
  }
}

fechas <- c()

# Fechas reales
for (mes in mesR) {
  for (dia in opdia) {
    fechas <- c(fechas, paste(yearurl, mes, dia, sep = "-")
                )
  }
}

nomI <- substr(week, 1, 5)
nomF <- substr(week, 8, 12)

dateI <- fechas[grep(nomI, fechad)]
dateF <- fechas[grep(nomF, fechad)]

# Lista de pestañas
pest <- excel_sheets(paste(week, exturl, sep = ""))

# Lectura de todas las pestañas
for (i in pest) {
  assign(paste("p", i,
               sep = ""
               ), 
  read_xlsx(paste(week, exturl, sep = ""),
            sheet = i,
            col_names = FALSE
            )
  )
}

# Encabezados
nomcol <- c("Producto", "Mercado", "Pmin", "Pmax", "Pmed")

# Lista de los archivos
plist <- objects(pattern = "p1")

# Trasformar todas las pestañas la tiempo
for (base in plist) {
  data <- eval(parse(text = base))
  data %>%
    filter(!is.na(.[2])) %>%
    select(-6:-7) -> data
  names(data) <- nomcol
  data %>%
    slice(2:n()) %>%
    mutate(FechaIN = as.Date(dateI, format = "%Y-%m-%d"), 
           FechaFN = as.Date(dateF, format = "%Y-%m-%d")
           ) -> data
  assign(base, data)
}

p1.1 %>%
  mutate(Ciudad = ifelse(grepl("\\(", Mercado),
                         word(Mercado, 1, sep = "\\("),
                         ifelse(grepl("\\,", Mercado),
                                word(Mercado, 1, sep = "\\,"),
                                ifelse(grepl("C.,", Mercado),
                                       word(Mercado, 1, sep = "C.,"),
                                       NA
                                       )
                                )
                         ),
         Mercadito = ifelse(grepl("\\(", Mercado),
                            word(Mercado, 2, sep = "\\("),
                            ifelse(grepl("\\,", Mercado),
                                   word(Mercado, 2, sep = "\\,"),
                                   ifelse(grepl("C.,", Mercado),
                                          word(Mercado, 2, sep = "C.,"),
                                          NA
                                          )
                                   )
                            ),
         Mercado = Mercadito,
         Mercadito = NULL,
         Ciudad = trimws(Ciudad, which = "both"),
         Mercado = trimws(Mercado, which = "both")
         ) -> prueba

grepl("\\)", prueba$Mercado)
replace(prueba$Mercado, )
sub("\\)", "", prueba$Mercado)


