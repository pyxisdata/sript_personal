# Script para la limpieza de los EF de Supersolidaria
# Librerias 
library(readr)
library(readxl)
library(tidyverse)
library(lubridate)
library(stringi)
library(stringdist)

# Quitar la notacion cientifica
options(scipen = 999)

# Origen de los datos
carpeta <- "\\Users\\PC\\Desktop\\EQMAP\\Supersolidaria\\"
archivos <- list.files(carpeta, full.names = TRUE, pattern = ".xlsx")

# Funcion para importar archivos
importar <- function(lista) {
  
  # Lista vacia para guardar los archivos importados
  indice <- list()
  # Vector para guardar los nombres
  nombres <- c()
  # Para cada archivo de la lista se importa y se guarda en indice
  for (i in seq_along(lista)) {
    data <- read_excel(lista[i], sheet = 1, col_names = FALSE)
    indice[[i]] <- data
    # Nombres de los archivos 
    nombre <- str_match(lista[i], "ia\\\\(.*?)\\.xlsx")[2]
    nombres[i] <- nombre
  }
  # Escribe el indice en el ambiente global
  names(indice) <- nombres
  assign("datos_importados", indice, envir = .GlobalEnv)
}

# Importandos los archvos con la funcion
importar(archivos)

# Limpieza de los datos importados
limpiar <- function(lista, textos) {
  
  # Lista vacia para guardar los archivos limpios
  indice <- list()
  # Vector para guardar los nombres
  nombres <- c()
  # Para cada archivo sucio, se aplican los siguientes pasos de limpieza
  for (i in seq_along(lista)) {
    # Eliminar las filas con NA en la columna 2 
    data <- lista[[i]] %>%
      filter(!is.na(.[2]))
    # Asignar la primera fila como encabezado
    names(data) <- unlist(data[1, ]) 
    # Limpieza
    data <- data %>%
      # Eliminar la primera fila
      slice(2:n()) %>% 
      # Exraer solo los numeros de la columna NIT
      mutate(NIT = as.numeric(gsub("[^\\d]+", "", NIT, perl = TRUE)),
             # Eliminar el ultimo digito de la columna NIT
             NIT = str_sub(NIT, 1, -2)) %>%
      # Se selecciona con la ubicacion de las columnas
      select(grep("ENTIDAD|NIT|CIIU|MUNICIPIO|DEPARTAMENTO", names(data)),
             grep("100000", names(data)): ncol(data),
             -grep("TIPO|CODIGO", names(data))) %>%
      # Se realiza un unpivot en las columnas de numeros
      gather("cuenta", "valores", -1:-5)
    # Extraer el numero de los nombres de los datos
    anno <- parse_number(textos)[i]
    # Añadir la columna de fecha utilizando el numero extraido
    data <- data %>% 
      mutate(fecha = paste(anno, "01", "01", sep = "-"),
             # Cambios en los tipos de columna
             NIT = as.integer(NIT),
             CIIU = as.integer(CIIU),
             ENTIDAD = str_to_title(ENTIDAD),
             valores = as.double(valores),
             fecha = ymd(fecha),
             NIT = replace(NIT, is.na(NIT), 0L),
             CIIU = replace(CIIU, is.na(CIIU), 0L),
             ENTIDAD = replace(ENTIDAD, ENTIDAD == "-", "Sin identificar"))
    # Añadir datos a la lista
    indice[[i]] <- data
    # Nombres de los archivos 
    nombre <- str_match(textos[i], "ia\\\\(.*?)\\.xlsx")[2]
    nombres[i] <- nombre
  }
  # Añadir los nombres
  names(indice) <- nombres
  # Unir todas las tablas en una sola
  indice <- bind_rows(indice)
  # Nombres en minuscula
  names(indice) <- tolower(names(indice))
  # Limpieza de los caracteres
  indice <- indice %>%
  mutate(departamento = tolower(departamento),
         municipio = tolower(municipio),
         # Cambiar la codificacion para eliminar los acentos
         departamento = stri_trans_general(departamento, "Latin-ASCII"),
         municipio = stri_trans_general(municipio, "Latin-ASCII"),
         # Eliminar los espacios en blanco al inicio y al final
         departamento = str_trim(departamento, side = "both"),
         municipio = str_trim(municipio, side = "both"),
         # Reemplazar "-" con sin definir
         departamento = replace(departamento,
                                departamento == "-", "sin definir"),
         municipio = replace(municipio,
                             municipio == "-", "sin definir"))
  # Asignacion de los datos limpios al ambiente glabial
  assign("datos_limpios", indice, envir = .GlobalEnv)
}
# Limpiando datos con la funcion
limpiar(datos_importados, archivos)

# Importar Division Politico Administartiva (DIVIPOLA)
divipola <- 
  read_csv("\\Users\\PC\\Desktop\\EQMAP\\Divipola.csv",
           col_types = "icic----",
           locale = locale(encoding = "latin-9"))
# Limpiar divipola antes de hacer la relacion (mismos pasos anteriores)
divipola <- divipola %>%
  mutate(nomdepto = tolower(nomdepto),
         nommunpio = tolower(nommunpio),
         nomdepto = stri_trans_general(nomdepto, "Latin-ASCII"),
         nommunpio = stri_trans_general(nommunpio, "Latin-ASCII"),
         nomdepto = replace(nomdepto,
                            nomdepto == "-", "sin definir"),
         nommunpio = replace(nommunpio,
                             nommunpio == "-", "sin definir"))
# Sacar los datos unicos de ambos lados para reducir el tamaño de las pruebas
unico_datos <- datos_limpios %>%
  select(departamento) %>%
  distinct(departamento) %>%
  mutate(union = 1)
unico_divipola <- divipola %>%
  select(nomdepto, iddepto) %>%
  distinct(nomdepto, .keep_all = TRUE) %>%
  mutate(union = 1)
# ----------------------------------------------------------------------------
# Union de las tablas mediante busqueda inexacta
igualar <- unico_datos %>%
  # Union cruzada de las tablas de valores unicos
  full_join(unico_divipola, 
            by = c("union")) %>%
  # Estimacion de las diferencias utilizando 3 metodos de stringdist
  select(-union) %>%
  mutate(dist1 = stringdist(departamento, nomdepto, method = "jw", p = 0.01),
         dist2 = stringdist(departamento, nomdepto, method = "jw", p = 0.02),
         dist3 = stringdist(departamento, nomdepto, method = "jw", p = 0.03),
         dist4 = stringdist(departamento, nomdepto, method = "jw", p = 0.04),
         media = (dist1 + dist2 + dist3 + dist4) / 4) %>%
  group_by(departamento) %>%
  # Agrupar por departamento y conocer el valor del minimo de la distancia
  summarise(minimo = min(media),
            nomdepto = nomdepto[which.min(media)],
            iddepto = iddepto[which.min(media)]) %>%
  arrange(desc(minimo))

# vector de reemplazo
vector_r <- setNames(igualar$iddepto, igualar$departamento)
# Hacer el reemplazo
datos_limpios$departamento <- vector_r[datos_limpios$departamento]
# ----------------------------------------------------------------------------
unico_datos <- datos_limpios %>%
  select(municipio, departamento) %>%
  distinct(municipio, departamento) %>%
  mutate(union = 1,
         md = paste0(municipio, departamento)) %>%
  select(-municipio, -departamento)
unico_divipola <- divipola %>%
  select(nommunpio, idmunpio, iddepto) %>%
  distinct(idmunpio, .keep_all = TRUE) %>%
  mutate(union = 1,
         mdv = paste0(nommunpio, iddepto)) %>%
  select(-nommunpio, -iddepto)
# Union de las tablas mediante busqueda inexacta
igualar <- unico_datos %>%
  # Union cruzada de las tablas de valores unicos
  full_join(unico_divipola, 
            by = c("union")) %>%
  # Estimacion de las diferencias utilizando 3 metodos de stringdist
  select(-union) %>%
  mutate(dist1 = stringdist(md, mdv, method = "jw", p = 0.01),
         dist2 = stringdist(md, mdv, method = "jw", p = 0.02),
         dist3 = stringdist(md, mdv, method = "jw", p = 0.03),
         dist4 = stringdist(md, mdv, method = "jw", p = 0.04),
         media = (dist1 + dist2 + dist3 + dist4) / 4) %>%
  group_by(md) %>%
  # Agrupar por departamento y conocer el valor del minimo de la distancia
  summarise(minimo = min(media),
            mdv = mdv[which.min(media)],
            idmunpio = idmunpio[which.min(media)]) %>%
  arrange(desc(minimo)) %>%
  mutate(idmunpio = replace(idmunpio, md == "cubarral50", 50223),
         idmunpio = replace(idmunpio, md == "el tablon52", 52258),
         idmunpio = replace(idmunpio, md == "cartagena13", 13001),
         idmunpio = replace(idmunpio, md == "tumaco52", 52835),
         idmunpio = replace(idmunpio, md == "ubate25", 25843),
         idmunpio = replace(idmunpio, md == "mariquita73", 73443),
         idmunpio = replace(idmunpio, md == "pto nare(lamagdalena)5", 5585),
         idmunpio = replace(idmunpio, md == "buga76", 76111))
# Nueva columna datos limpios
datos_limpios <- datos_limpios %>%
  mutate(municipio = paste0(municipio, departamento))
# vector de reemplazo
vector_r <- setNames(igualar$idmunpio, igualar$md)
# Hacer el reemplazo
datos_limpios$municipio <- vector_r[datos_limpios$municipio]
# -------------------------------------------------------------------
# Nombres de las entidades
# Limpieza de los valores cambiantes en el tiempo:
# Nombre, CIIU
lista_empresas <- datos_limpios %>%
  group_by(nit) %>%
  summarise(nombre = entidad[which.max(fecha)],
            ciiuID = ciiu[which.max(fecha)],
            munpioID = municipio[which.max(fecha)])
# Evaluaciones de calidad
# Revision de NA
View(
   lista_empresas %>%
    filter(is.na(nombre))
  )
# Revision para que cada NIT tenga un unico nombre
View(
  lista_empresas %>%
    group_by(nit) %>%
    summarise(nombres = n_distinct(nombre))
)
# Evaluacion final de los datos_limpios
datos_limpios <- datos_limpios %>%
  select(nit, cuenta, valores, fecha)
# Filtro opcional
datos_limpios <- datos_limpios %>%
  filter(fecha >= ymd("2015-01-01"))
# Exportar datos 
write_csv(datos_limpios,
          "\\Users\\PC\\Desktop\\ef_supersolidaria.csv",
          na = "")
write_csv(lista_empresas,
          "\\Users\\PC\\Desktop\\empresas_solidarias.csv",
          na = "")






















