# Script para limpieza de Supersociedades

library(dplyr)
library(lubridate)
library(tidyr)
library(readr)
library(readxl)
library(stringr)

# Ubicacion de los archivos
carpeta <- "\\Users\\PC\\Desktop\\Archivos Brutos\\Estados Financieros\\Supersociedades\\"
lista <- list.files(carpeta, full.names = TRUE, pattern = "txt")

# Funcion de lectura
read_ef <- function(x) {
  
  lista <- list()
  nombres <- c()
  
  for (i in seq_along(x)) {
    data <- read_delim(x[i], delim = "¬", locale = locale(encoding = "latin-9"))
    data <- data.frame(data)
    data %>% 
      mutate(fecha = ymd(paste(str_sub(x[i], -8, -5), 1, 1, sep = "-"))) -> data
    nombre <- str_sub(x[i], -11, -5)
    lista[[i]] <- data
    nombres[i] <- nombre
    
  }
  
  names(lista) <- nombres
  assign("EF", lista, envir = .GlobalEnv)
}

# Lectura
read_ef(lista)

# Carpeta 2017
carpeta_2017 <- "\\Users\\PC\\Desktop\\Archivos Brutos\\Estados Financieros\\Supersociedades\\2017\\"
lista_2017 <- list.files(carpeta_2017, full.names = TRUE)

# Funcion de lectura 2017
read_efs <- function(x) {
  
  lista <- list()
  nombres <- c()
  names <- c("Estado de Situación Financiera", "Estado de Resultados Integrales")
  snames <- c("BG", "ER")
  locacion <- grepl("Individuales", x)
  tipo <- grepl("Plena", x)
  
  for (i in seq_along(names)) {
      
    data <- read_xlsx(x, sheet = names[i])
    data <- data.frame(data)
    if (locacion == TRUE & tipo == TRUE) { 
      data %>% mutate(PUNTO_ENTRADA = 10) -> data 
    } else if (locacion == TRUE & tipo == FALSE) { 
      data %>% mutate(PUNTO_ENTRADA = 40) -> data
    } else if (locacion == FALSE & tipo == TRUE) {
      data %>% mutate(PUNTO_ENTRADA = 20) -> data
    } else if (locacion == FALSE & tipo == FALSE) {
      data %>% mutate(PUNTO_ENTRADA = 50) -> data  
    }
    
    data %>% mutate(fecha = ymd("2017-01-01")) -> data
      
    nombre <- paste(snames[i], str_sub(x, -25, -10), sep = "_")
    lista[[i]] <- data
    nombres[i] <- nombre
      
  }
  
  names(lista) <- nombres
  assign(str_sub(x, -25, -10), lista, envir = .GlobalEnv)
}

# Lectura
EF_2017 <- lapply(lista_2017, read_efs)

# Listados de orden
EROA_2017 <- c(27, 1, 26, 3, 2, 4:13, 15, 14, 16, 21:24, 18, 25, 19, 20, 17)
EF_2017[[1]][[2]] <- EF_2017[[1]][[2]][EROA_2017]
EROB_2017 <- c(29, 1, 28, 3, 2, 4:13, 15, 14, 16, 23:26, 20, 18:19, 27, 21:22, 17)
EF_2017[[2]][[2]] <- EF_2017[[2]][[2]][EROB_2017]
EROC_2017 <- c(21, 1, 20, 3, 2, 4:12, 13, 14:19)
EF_2017[[3]][[2]] <- EF_2017[[3]][[2]][EROC_2017]
EROD_2017 <- c(22, 1, 21, 3, 2, 4:14, 16:19, 15, 20)
EF_2017[[4]][[2]] <- EF_2017[[4]][[2]][EROD_2017]

# Crear listado unificado de variables
ERA_2017 <- paste("v", c(39, 1, 3, 6, 7, 12:25, 27:28, 30:31, 34:36, 38), sep = "")
ERB_2017 <- paste("v", c(39, 1, 3, 6, 7, 12:25, 27:28, 30:36, 38), sep = "")
ERC_2017 <- paste("v", c(39, 1, 3, 6, 7, 12:20, 22, 24:25, 27:29, 34), sep = "")
ERD_2017 <- paste("v", c(39, 1, 3, 6, 7, 12:20, 22, 24:25, 27:29, 32, 34), sep = "")

# Asignar nombres
names(EF_2017[[1]][[2]]) <- ERA_2017
names(EF_2017[[2]][[2]]) <- ERB_2017
names(EF_2017[[3]][[2]]) <- ERC_2017
names(EF_2017[[4]][[2]]) <- ERD_2017

# Unir 2017 Estados de Resultados
ER_2017 <- bind_rows(list(EF_2017[[1]][[2]], EF_2017[[2]][[2]],
                          EF_2017[[3]][[2]], EF_2017[[4]][[2]]))
ER_2017 %>%
  mutate(v7 = ymd(v7)) %>%
  mutate_at(vars(-1:-5), as.numeric) -> ER_2017

# Listados de orden Balance General
BGOA_2017 <- c(60, 1, 59, 3, 2, 4:14, 16, 15, 17:30, 32:58, 31)
EF_2017[[1]][[1]] <- EF_2017[[1]][[1]][BGOA_2017]
BGOB_2017 <- c(62, 1, 61, 3, 2, 4:14, 16, 15, 17:18, 21, 19, 20, 22:32, 34:60, 33)
EF_2017[[2]][[1]] <- EF_2017[[2]][[1]][BGOB_2017]
BGOC_2017 <- c(66, 1, 65, 3, 2, 4:21, 23, 22, 24:33, 35:64, 34)
EF_2017[[3]][[1]] <- EF_2017[[3]][[1]][BGOC_2017]
BGOD_2017 <- c(69, 1, 68, 3, 2, 4:20, 24, 21, 26, 25, 27, 22:23, 28:34, 35:36, 38:67, 37)
EF_2017[[4]][[1]] <- EF_2017[[4]][[1]][BGOD_2017]

# Crear listado unificado de variables
BGA_2017 <- paste("v", c(93, 1, 3, 6, 7, 13:17, 20:25, 27:28, 31:33, 37, 40, 43:48,
                         50, 55:56, 58:60, 63:65, 67, 70:76, 78:80, 82:92),
                  sep = "")
BGB_2017 <- paste("v", c(93, 1, 3, 6, 7, 13:17, 20:25, 27:28, 31:34, 36:37, 40, 
                         43:48, 50, 55:56, 58:60, 63:65, 67, 70:76, 78:80, 82:92),
                  sep = "")
BGC_2017 <- paste("v", c(93, 1, 3, 6, 7, 13:16, 18:25, 27, 29:33, 37, 39:40, 43:48, 50,
                         55:56, 58:65, 67, 70:80, 82:92),
                  sep = "")
BGD_2017 <- paste("v", c(93, 1, 3, 6, 7, 13:16,18:25, 27, 29:33, 35, 37, 39:48, 50, 
                         55:56, 58:65, 67, 70:80, 82:92), 
                  sep = "")

# Asignar nombres
names(EF_2017[[1]][[1]]) <- BGA_2017
names(EF_2017[[2]][[1]]) <- BGB_2017
names(EF_2017[[3]][[1]]) <- BGC_2017
names(EF_2017[[4]][[1]]) <- BGD_2017

# Unir 2017
BG_2017 <- bind_rows(list(EF_2017[[1]][[1]], EF_2017[[2]][[1]],
                          EF_2017[[3]][[1]], EF_2017[[4]][[1]]))
BG_2017 %>%
  mutate(v7 = ymd(v7)) %>%
  mutate_at(vars(-1:-5), as.numeric) -> BG_2017

# ----------- 2015 - 2016 ------------------------------------------------
# Listados de orden
BGO_2015 <- c(78, 1:17, 18:24, 26, 25, 27:28, 31, 29, 30, 32, 33, 34:43, 45:77, 44)
EF[[1]] <- EF[[1]][BGO_2015]
BGO_2016 <- c(92, 1:91)
EF[[2]] <- EF[[2]][BGO_2016]
ERO_2015 <- c(34, 1:21, 23, 22, 24, 29, 30:31, 32, 25, 26, 33, 28, 27)
EF[[3]] <- EF[[3]][ERO_2015]
ERO_2016 <- c(37, 1:36)
EF[[4]] <- EF[[4]][ERO_2016]

# Listados unificado de variables
BG_2015 <- paste("v", c(93, 1:17, 20:28, 31:34, 36:37, 40, 43:48, 50:53, 55:60,
                        63:64, 66:76, 78:92), sep = "")
BG_2016 <- paste("v", c(93, 1:91), sep = "")
ER_2015 <- paste("v", c(39, 1:25, 27:28, 30, 32:34, 36:37), sep = "")
ER_2016 <- paste("v", c(39, 1:36), sep = "")

# Asignar nombres
names(EF[[1]]) <- BG_2015
names(EF[[2]]) <- BG_2016
names(EF[[3]]) <- ER_2015
names(EF[[4]]) <- ER_2016

# Unir no 2017
BG_N2017 <- bind_rows(list(EF[[1]], EF[[2]]))
BG_N2017 %>%
  mutate(v3 = as.numeric(str_sub(v3, 1, 2))) %>%
  mutate_at(vars(-1:-10), as.numeric) -> BG_N2017

ER_N2017 <- bind_rows(list(EF[[3]], EF[[4]]))
ER_N2017 %>%
  mutate(v3 = as.numeric(str_sub(v3, 1, 2))) %>%
  mutate_at(vars(-1:-10), as.numeric) -> ER_N2017

# Union total
BG <- bind_rows(list(BG_N2017, BG_2017))
ER <- bind_rows(list(ER_N2017, ER_2017))
