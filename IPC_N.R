# Script para limpieza de IPC

library(dplyr)
library(lubridate)
library(odbc)
library(readxl)
library(stringr)
library(tidyr)

# Carpeta de los archivos
carpeta <- "\\Users\\PC\\Desktop\\Archivos Brutos\\IPC\\IPC_2018"

# Lista de la carpeta
lista_carpeta <- list.files(carpeta, full.names = TRUE)

# Funcion de lectura
read_ipc <- function(x) {

  lista <- list()
  nombres <- c()
  meses <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun",
             "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
  
 ingresos <- setNames(c(1:5),
                       c("Pobres", "Vulnerables",
                         "Clase Media", "Ingresos Altos",
                         "Total Nivel de Ingresos"))
  
  geografia <- setNames(c(169, 5001, 8001, 11001, 13001, 15001, 17001, 18001,
                          19001, 20001, 23001, 41001, 44001, 47001, 50001,
                          52001, 54001, 63001, 66001, 68001, 70001, 73001,
                          76001, 100000),
                        c("NACIONAL", "MEDELLÍN", "BARRANQUILLA",
                          "BOGOTÁ, D.C.", "CARTAGENA DE INDIAS", "TUNJA",
                          "MANIZALES", "FLORENCIA", "POPAYÁN", "VALLEDUPAR",
                          "MONTERÍA", "NEIVA", "RIOHACHA", "SANTA MARTA",
                          "VILLAVICENCIO", "PASTO", "CÚCUTA", "ARMENIA",
                          "PEREIRA", "BUCARAMANGA", "SINCELEJO", "IBAGUÉ",
                          "CALI", "OTRAS AREAS URBANAS"))
  
  for (i in seq_along(x)) {
    
    # Lectura
    data <- read_xls(x[i], sheet = 1, col_names = TRUE)
    
    # Mayuscula
    data %>%
      mutate(`Cobertura Geografica` = toupper(`Cobertura Geografica`)) -> data
    
    # Creacion de la fecha
    mes <- grep(data[1, 2], meses)
    anno <- data[1, 1]
    fecha <- ymd(paste(anno, mes, 1, sep = "-"))
    
    # Limpieza
    data %>%
      mutate(fecha = fecha, # Añadir fecha
             Mes = NULL,
             Año = NULL
             ) %>%
      # Separar columna de subclase
      separate(Subclase, c("codigo", "nombre"), sep = " - ") %>%
      mutate(nombre = NULL) -> data
    
    # Reemplazar valores estilo BUSCARV
    data$`Nivel de ingresos`[] <- ingresos[unlist(data$`Nivel de ingresos`)]
    data$`Cobertura Geografica`[] <- geografia[unlist(data$`Cobertura Geografica`)]
    
    # Nuevos nombres de las columnas
    names(data) <- c("ingresoID", "subclaseID", "munpioID", "indice", "fecha")
    
    data %>% # COnvertir a numerico
      mutate_at(vars(-fecha), as.numeric) -> data
    
    # Añadir valores a las listas
    lista[[i]] <- data
    nombre <- str_sub(x[i], -10, -5)
    nombres[[i]] <- nombre
    
  }
  
  names(lista) <- nombres
  assign("ipc_lista", lista, envir = .GlobalEnv)
  
}

# Lectura
read_ipc(lista_carpeta)

# Unir todo en una sola tabla
ipc <- bind_rows(ipc_lista)

write.csv(ipc, "\\Users\\PC\\Desktop\\ipc.csv", row.names = FALSE, na = "")






