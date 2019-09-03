# Identificacion de valores repetidos
# Librerias
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)

# Tabla de actividades por edad y genero
cruce <- 
  read_csv("\\Users\\PC\\Desktop\\Armenia\\cruce_estandar.csv",
           col_types = "diiid")
edades <- unique(cruce$cruce_general)
# Limpieza
cruce <- cruce %>%
  mutate(cruce_general = as.double(paste0(generoID, edad)))

# Funcion para agrupar las listas de los iguales
identificar <- function(datos) {
  
  # La tabla de seleccion permite ver en cuantos grupos estan los datos
  seleccion <- datos %>%
    group_by(cruce_general) %>%
    summarise(num = n()) %>%
    group_by(num) %>%
    arrange(num)
  # Numero de grupos
  grupos <- unique(seleccion$num)
  # Lista de almacenamiento
  tablas <- list()
  # Seleccion
  for (i in seq_along(grupos)) {
    # Filtro de seleccion
    filtro <- seleccion %>%
      filter(num == grupos[i])
    # Verificador realiza la tabla necesaria
    verificador <- datos %>%
      filter(cruce_general %in% unique(filtro$cruce_general)) %>%
      group_by(cruce_general) %>%
      arrange(cruce_general, jerarquiaID) %>%
      mutate(indice = 1:grupos[i]) %>%
      spread(cruce_general, jerarquiaID)
    # Almacenar en la lista
    tablas[[i]] <- verificador
  }
  
  assign("resultados", tablas, envir = .GlobalEnv)
}

# Eejecutar
identificar(cruce)

# Funcion para verificar la igualdad
verificar <- function(datos) {
  
  lista <- list()
  for (i in seq_along(datos)) {
  
  nombres <- names(datos[[i]])
  data <- datos[[i]] %>%
    t() %>%
    as.data.frame() %>%
    as.tbl() %>%
    slice(2:n()) %>%
    mutate(edad = nombres[-1]) %>%
    unite(actividades, -edad) %>%
    group_by(actividades)
  lista[[i]] <- data
  
  }
  lista <- bind_rows(lista)
  unicos <- unique(lista$actividades)
  grupos <- list()
  for (i in seq_along(unicos)) {
    grupo <- lista %>%
      filter(actividades == unicos[i]) %>%
      mutate(edad = as.numeric(edad))
    grupo$identidad <- min(grupo$edad)
    grupos[[i]] <- grupo
  }
  lista <- bind_rows(grupos)
  assign("igualdades", grupos, envir = .GlobalEnv)
  assign("igualdades_df", lista, envir = .GlobalEnv)
}  

verificar(resultados)    

# Aisgnar a cada edad su identidad
identidades <- cruce %>%
  left_join(igualdades_df,
            by = c("cruce_general" = "edad")) %>%
  select(-actividades, -cruce_general)

identidades_2 <- identidades %>%
  group_by(identidad, jerarquiaID) %>%
  summarise(n = n()) %>%
  select(-n)

write_csv(identidades, "\\Users\\PC\\Desktop\\cruce_unico.csv")
    
    
    
  




