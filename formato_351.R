# Script de los formatos del portafolio
# Librerias
library(dplyr)
library(tidyr)
library(readxl)
library(readr)
library(stringr)

# Archivos 
archivos <- paste0(getwd(), "/Portafolio") %>%
  list.files(full.names = TRUE)

# Funcion para importar la hoja correcta del archivo excel
importar <- function(archivo) {
  
  # Ubicacion de la hoja del formato 351
  hoja <- str_which(excel_sheets(archivo), "351")
  # Importar la hoja
  archivo <- read_excel(archivo, 
                        sheet = hoja,
                        col_names = TRUE,
                        col_types = "text")
}

# Importar los archivos en una lista
importados <- archivos %>%
  map(importar)

# Funcion para preparar los datos
limpiar <- function(archivo) {
  
  nombres <- names(archivo) %>%
    str_to_lower() %>%
    stri_trans_general("Latin-ASCII") %>%
    str_replace_all("_", "") %>%
    as_tibble() %>%
    mutate(titulo = case_when(str_detect(.[[1]], "entidad") ~ "noment",
                              str_detect(.[[1]], "corte") ~ "fecha",
                              str_detect(.[[1]], "subtipo") ~ "codsubpat",
                              str_detect(.[[1]], "tipo patrim") ~ "codtipat",
                              str_detect(.[[1]], "codigo patrim") ~ "codpat",
                              str_detect(.[[1]], "nombre patrim") ~ "nompat",
                              str_detect(.[[1]], "captura") ~ "coducap",
                              str_detect(.[[1]], "renglon") ~ "renglon",
                              str_detect(.[[1]], "tipoid") ~ "codtiemi",
                              str_detect(.[[1]], "id emisor") ~ "codemi",
                              str_detect(.[[1]], "razon") ~ "razemi",
                              str_detect(.[[1]], "inversion") ~ "clasinv",
                              str_detect(.[[1]], "descripcion") ~ "desinv",
                              str_detect(.[[1]], "nemotecnico") ~ "nemo",
                              str_detect(.[[1]], "fecha vencim") ~ "fechavt",
                              str_detect(.[[1]], "fecha compr") ~ "fechacomp",
                              str_detect(.[[1]], "moneda") ~ "codmon",
                              str_detect(.[[1]], "residual") ~ "valres",
                              str_detect(.[[1]], "nominal") ~ "valnom",
                              str_detect(.[[1]], "acciones") ~ "accion",
                              str_detect(.[[1]], "tasa facial") ~ "tasaf",
                              str_detect(.[[1]], "compra") ~ "valcompusd",
                              str_detect(.[[1]], "spread") ~ "valtasa",
                              str_detect(.[[1]], "mercado") ~ "valmer",
                              str_detect(.[[1]], "negoc") ~ "tasanegoc",
                              str_detect(.[[1]], "dias") ~ "diasvt",
                              str_detect(.[[1]], "inter") ~ "intercapt",
                              str_detect(.[[1]], "custodio") ~ "isisn")) %>%
    select(titulo) %>%
    unlist()
  # Asignar nombres
  names(archivo) <- nombres
  # Procesamiento
  archivo <- archivo %>%
    mutate_at(vars(codtipat, codpat, codsubpat, coducap,
                   renglon, codtiemi),
              as.integer) %>%
    mutate_at(vars(valnom, valres, accion, valcompusd, 
                   valtasa, valmer, tasanegoc, intercapt),
              as.numeric) %>%
    mutate_at(vars(noment, nompat, razemi, clasinv, desinv,
                   nemo, codmon, tasaf, isisn),
              str_to_lower) %>%
    mutate_at(vars(noment, nompat, razemi, clasinv, desinv,
                   nemo, codmon, tasaf, isisn),
              str_squish) %>%
    # Columna de entidades
    mutate(noment = case_when(str_detect(noment, "protecci") ~ "1",
                              str_detect(noment, "colfondo") ~ "2",
                              str_detect(noment, "old mutual") ~ "3",
                              str_detect(noment, "porvenir") ~ "4"),
           # Columna de patrimonio
           nompat = case_when(str_detect(nompat, "ley 50") ~ "8",
                              str_detect(nompat, "ley 50") ~ "8",
                              TRUE ~ nompat))
  
}

# Aplicar la limpieza 
limpios <- importados %>%
  map(limpiar)
  
  


