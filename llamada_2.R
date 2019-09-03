# Script para limpieza de los estados financieros de superfinanciera
# Librerias
library(dplyr)
library(tidyr)
library(lubridate)
library(readxl)
library(stringr)
library(stringi)
library(readr)

# Eliminar notacion cientifica
options(scipen = 999)
# Carpeta
carpeta <- "\\Users\\PC\\Desktop\\Proyecto Semana\\Superfinanciera\\Bancos\\"
archivos <- list.files(carpeta, full.names = TRUE, pattern = "n.xls")

# Funcion para importar los archivos
importar <- function(archivos) {
  # Lista vacia para guardar archivos
  lista <- list()
  # Vector de nombres
  nombres <- c()
  # Para cada archivo realizar:
  for (i in seq_along(archivos)) {
    # Importar con la funcion de excel, solo primera pagina
    data <- read_excel(archivos[i], sheet = 1, col_names = FALSE)
    nombre <- str_match(archivos[i], "112(.*?)n\\.xls")[2]
    nombre <- paste0("EF_", nombre)
    # Guardar nombres y archivos
    lista[[i]] <- data
    nombres[i] <- nombre
  }
  # Aplicar nombres
  names(lista) <- nombres
  assign("datos_importados", lista, envir = .GlobalEnv)
}
# Ejecutar la importacion
importar(archivos)

# Limpieza de los datos
limpiar <- function(archivos) {
  # Lista vacia
  lista <- list()
  # Para cada archivo importado aplicar:
  for (i in seq_along(archivos)) {
    data <- archivos[[i]]
    data <- data %>%
      filter(!is.na(.[3]))
  names(data) <- unlist(data[1, ])
  names(data)[1:2] <- c("codigo", "cuenta")
  anno <- str_match(names(archivos)[i], "\\d+")
  data <- data %>%
    slice(2:n()) %>%
    gather("nombre", "valor", -c(1:2)) %>%
    separate(nombre, c("empresaID", "empresa"), sep = "-") %>%
    mutate(codigo = as.integer(codigo),
           cuenta = str_to_title(cuenta),
           empresaID = as.integer(empresaID),
           valor = as.double(valor),
           empresa = str_to_title(empresa),
           fecha = paste(anno, "01", "01", sep = "-"),
           fecha = ymd(fecha))
  lista[[i]] <- data
  }
  # Nombrar la lista
  names(lista) <- names(archivos)
  lista <- bind_rows(lista)
  assign("datos_limpios", lista, envir = .GlobalEnv)
}
# Ejecutar la limpieza
limpiar(datos_importados)
# Lectura del archivo del DIVIPOLA
divipola <- read_csv("\\Users\\PC\\Desktop\\Proyecto Semana\\Divipola.csv",
                     col_types = "icic----",
                     locale = locale(encoding = "ISO-8859-1"))
# Limpieza de DIVIPOLA
divipola <- divipola %>%
  mutate(nomdepto = tolower(nomdepto),
         nommunpio = tolower(nommunpio),
         nomdepto = stri_trans_general(nomdepto, "Latin-ASCII"),
         nommunpio = stri_trans_general(nommunpio, "Latin-ASCII"),
         nomdepto = replace(nomdepto,
                            nomdepto == "-", "sin definir"),
         nommunpio = replace(nommunpio,
                             nommunpio == "-", "sin definir"))
# Lectura del archivo de los nits
lista_entidades <- read_excel(paste0(carpeta, "lista_bancos.xls"),
                              sheet = 1, col_names = FALSE)
# Limpieza de la lista de entidades
lista_entidades <- lista_entidades %>%
  filter(!is.na(.[4]),
         !is.na(.[3])) %>%
  select(2, 4, 5, 10)
names(lista_entidades) <- c("empresaID", "empresa", "nit", "ubicacion")
lista_entidades <- lista_entidades %>% 
  slice(2:n()) %>%
  mutate(nit = str_sub(nit, 1, 9),
         empresa = str_to_title(empresa),
         empresaID = as.integer(empresaID),
         nit = as.integer(nit)) %>%
  separate(ubicacion, c("municipio", "departamento"), sep = "-") %>%
  mutate(departamento = NULL,
         municipio = str_trim(tolower(municipio)),
         municipio = stri_trans_general(municipio, "Latin-ASCII"))
# Valores unicos DIVIPOLA
unico_divipola <- divipola %>%
  select(nommunpio, idmunpio) %>%
  distinct(idmunpio, .keep_all = TRUE) %>%
  mutate(union = 1)
# Asignar codigos de DIVIPOLA
lista_entidades <- lista_entidades %>%
  left_join(unico_divipola, by = c("municipio" = "nommunpio")) %>%
  select(empresaID, empresa, nit, idmunpio)
# Verificar nombre y NIT
verificacion <- datos_limpios %>%
  group_by(empresaID, empresa, fecha) %>%
  distinct(empresaID, empresa) %>%
  left_join(lista_entidades, by = "empresaID") %>%
  group_by(empresaID) %>%
  summarise(nit = nit[which.max(fecha)],
            nombre = empresa.x[which.max(fecha)],
            munpioID = idmunpio[which.min(fecha)])
# Aplicar NIT y nombres a los datos limpios 
datos_limpios <- datos_limpios %>%
  left_join(verificacion, by = "empresaID") %>%
  mutate(empresa = nombre) %>%
  select(-nombre, -empresa, -cuenta, -empresaID, -munpioID)
# Escribir los archivos 
write_csv(datos_limpios,
          "\\Users\\PC\\Desktop\\ef_bancos.csv",
          na = "")
write_csv(verificacion,
          "\\Users\\PC\\Desktop\\empresas_bancos.csv",
          na = "")




