# Script para añadir periodos a las edades
# Librerias
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)

# Carpeta
carpeta <- "\\Users\\PC\\Desktop\\Armenia\\"
# Archivo con las condiciones de edad y genero de las actividades
condiciones <- read_csv(paste0(carpeta, "condiciones.csv"),
                        col_types = "iiddiidd")
# Archivo con los valores del UPC
upc <- read_csv(paste0(carpeta, "upc.csv"),
                col_types = "iiid")
# Limpieza de tablas
upc <- upc %>%
  mutate(cruce_base = paste0(regimenID, generoID, edad),
         cruce_base = as.integer(cruce_base)) %>%
  select(cruce_base, valor)
# Limpiez de condiciones
condiciones <- condiciones %>%
  mutate(union = 1,
         ina = as.integer(inicial),
         fia = as.integer(final),
         inm = round((inicial - ina), 2) * 100,
         fim = round((final - fia), 2) * 100,
         di = (ina * 12) + inm,
         df = (fia * 12) + fim,
         periodos = round(df - di, 0),
         proporcion = 1 / periodos) %>%
  select(jerarquiaID, generoID, inicial, final, costo,
         valor, periodos, proporcion, di, df, union)

# Dataframe con los vectores anteriores
edades <- tibble(anno = rep(0:120, each = length(0:11)),
                     mes = rep(0:11, length(0:120))) %>%
  mutate(mesdec = mes / 100,
         annodec = round(anno + mesdec, 2),
         union = 1,
         edadmes = (anno * 12) + mes)

# Funcion para añadir pronostico de edades
add_edad <- function(datos, hoy, inicio, fin) {
  
  # Partes de la fecha actual
  hoy <- ymd(hoy)
  inicio <- ymd(inicio)
  fin <- ymd(fin)
  
  # Periodos a pronosticar
  atras <- length(seq(inicio, hoy, by = "month")) - 1
  adelante <- length(seq(hoy, fin, by = "month")) - 1
  
  # Dataframe vacio atras
  lista_atras <- data.frame(anterior = 1:nrow(datos))
  
  # Periodos hacia atras
  for (i in 1:atras) {
    actual <- (datos$anno * 12) + datos$mes
    actual <- actual - i
    actual <- round(actual / 12, 2)
    entero <- as.integer(actual)
    decimal <- (actual - entero) * 12
    actual <- round(entero + (decimal / 100), 2)
    lista_atras[[i + 1]] <- actual
  }
  
  # Reversa de columnas
  lista_atras <- rev(lista_atras)
  
  # Dataframe vacio adelante
  lista_adelante <- data.frame(posterior = 1:nrow(datos))
  
  # Periodos hacia adelante
  for (i in 1:adelante) {
    actual <- (datos$anno * 12) + datos$mes
    actual <- actual + i
    actual <- round(actual / 12, 2)
    entero <- as.integer(actual)
    decimal <- (actual - entero) * 12
    actual <- round(entero + (decimal / 100), 2)
    lista_adelante[[i + 1]] <- actual
  }
  
  # Limpieza
  datos <- datos %>% select(annodec)
  lista_atras <- lista_atras %>% select(-anterior)
  lista_adelante <- lista_adelante %>% select(-posterior)
  
  # Unir con la base actual
  tabla <- cbind(datos, lista_atras, datos, lista_adelante)
  # Lista de fechas para encabezados
  fechas <- seq(inicio, fin, by = "month")
  names(tabla) <- c("actual", as.character(fechas))
  # Negativos como NA
  tabla[tabla < 0] <- NA
  # Unpivot
  tabla <- tabla %>%
    gather("fecha", "edad", -actual) %>%
    mutate(fecha = ymd(fecha),
           edad = as.numeric(edad),
           actual = as.numeric(actual)) %>%
    as_tibble()
  # Asignar
  assign("periodos", tabla, envir = .GlobalEnv)
}

# Ejecutar
add_edad(edades, 
         hoy = "2019-05-01",
         inicio = "2019-01-01",
         fin = "2023-12-01")

# Añadir columnas de regimen y genero
periodos <- periodos %>%
  mutate(f = 1, 
         m = 2) %>%
  gather("genero", "generoID", -actual, -fecha, -edad) %>%
  select(-genero) %>%
  mutate(c = 1, 
         s = 2) %>%
  gather("regimen", "regimenID", -actual, -fecha, -edad, -generoID) %>%
  select(-regimen) %>%
  mutate(cruce_jerarquia = paste0(generoID, edad),
         cruce_jerarquia = as.double(cruce_jerarquia),
         anno = as.integer(edad),
         cruce_base = paste0(regimenID, generoID, anno),
         cruce_base = as.integer(cruce_base)) %>%
  left_join(upc, by = "cruce_base") %>%
  mutate(cruce_base = paste0(regimenID, generoID, actual),
         cruce_base = as.double(cruce_base)) %>%
  select(-generoID, -regimenID, -anno) %>%
  rename(valor_upc = valor)
# Tabla general
general <- edades %>%
  full_join(condiciones, by = "union") %>%
  filter(annodec >= inicial & annodec < final) %>%
  mutate(periodo = round(df - edadmes, 0),
         proporcion = 1 / periodo) %>%
  mutate(cruce_jerarquia = paste0(generoID, annodec),
         cruce_jerarquia = as.double(cruce_jerarquia)) %>%
  select(annodec, costo, valor, proporcion, periodos, cruce_jerarquia,
         jerarquiaID) %>%
  right_join(periodos, by = "cruce_jerarquia") %>%
  filter(!is.na(cruce_jerarquia)) %>%
  select(cruce_base, fecha, actual, valor, costo, proporcion, periodos,
         jerarquiaID, valor_upc) %>%
  mutate(valor_upc = valor_upc * 65610.6)
  
# Exportar
write_csv(general,
          "\\Users\\PC\\Desktop\\general.csv",
          na = "")



  
  
  
