# Preparación de los datos de estados financieros de los fondos de pensiones
# Estados financieros NIIF desde 2015 hasta la actualidad
# Libreria
library(magrittr)
library(readxl)
library(tibble)
library(stringr)
library(purrr)
library(dplyr)
library(lubridate)
library(tidyr)


# Lista de los archivos
archivos <- paste0(getwd(), "/Estados Financieros") %>%
  list.files(full.names = TRUE, pattern = ".xls")
# Importar los datos 
# Funcion para importar 
# Importar todas las hojas y unirlas en una sola tabla
# Programacion Funcional
importar <- function(data) {
  # Lista de las hojas excluyendo las hojas vacias
  lista <- excel_sheets(data) %>%
    str_to_lower() %>%
    .[!str_detect(., "hoja")] %>%
    str_which(., .)
  # Aplicar map para importar cada hoja a la vez
  hojas <- data %>%
    # Funcion anonima para leer cada hoja por separado
    map2(lista, function(x, y) {
      data <- read_excel(x, 
                         sheet = y,
                         col_names = FALSE,
                         col_types = "text")
    })
}
# Importar archivos
importados <- map(archivos, importar)
# Funcion para limpiar los archivos
limpiar <- function(data) {
  # Generar la fecha
  periodo <- data[[1]][[3]]
  anno <- word(periodo, -1, sep = " ")
  mes <- word(periodo, -3, sep = " ") %>%
    str_to_lower() %>%
    str_sub(end = 3)
  # Vector de meses
  meses <- c("ene", "feb", "mar", "abr", "may", "jun",
             "jul", "ago", "sep", "oct", "nov", "dic")
  mes <- str_which(meses, mes)
  # Generar fecha uniendo anno, mes y dia
  fecha <- paste(anno, mes, "01", sep = "-")
  # Preparacion de los datos
  data <- data %>%
    # Remover las filas iniciales
    filter(!is.na(.[[3]]))
  # Preparacion de los encabezados
  encabezados <- data %>%
    slice(1:2) %>%
    # Añadir valores unicos 
    rownames_to_column() %>%
    gather("columna", "dato", -rowname) %>%
    spread(rowname, dato) %>%
    mutate(columna = str_match(columna, "\\d+"),
           columna = as.integer(columna)) %>%
    # Ordenar deacuerdo al numero de la columna
    arrange(columna) %>%
    fill(2:3) %>%
    mutate_all(str_to_lower) %>%
    # Eliminar columnas de totales
    filter(!str_detect(.[[3]], "total") | is.na(.[[3]])) %>%
    mutate(`1` = str_match(`1`, "\\d+"),
           nombre = paste(`1`, `2`, sep = "_"),
           # Asignar nombres a las dos primeras columnas en NA
           nombre = case_when(columna == 1 ~ "cuenta",
                              columna == 2 ~ "nombre",
                              TRUE ~ nombre)) %>%
    select(columna, nombre)
  # Filtrar por las columnas seleccionadas
  data <- data %>%
    slice(-c(1:2)) %>%
    select(as.integer(encabezados[[1]]))
  # Asignar nombres
  names(data) <- encabezados[[2]]
  # Preparacion final
  data <- data %>%
    select(-nombre) %>%
    mutate_at(vars(-cuenta), as.double) %>%
    mutate(cuenta = as.integer(cuenta),
           fecha = ymd(fecha)) %>%
    gather("entidad", "valor", -fecha, -cuenta) %>%
    separate(entidad, c("idfondo", "entidad"), sep = "_") %>%
    mutate(idfondo = as.integer(idfondo) / 1000)
  # Reemplazar nombres de entidades por codigos
  # Vector de entidades
  entidades <- data %>%
    select(entidad) %>%
    distinct() %>%
    arrange(entidad) %>%
    mutate(codigo = case_when(str_detect(entidad, "colf") ~ 1,
                              str_detect(entidad, "alterna") ~ 3,
                              str_detect(entidad, "old") ~ 2,
                              str_detect(entidad, "porve") ~ 4,
                              str_detect(entidad, "protec") ~ 5))
  # Reemplazar columna mediante vector de reemplazo
  entidades <- setNames(entidades[["codigo"]], entidades[["entidad"]])
  # Reemplazar utilizando el match con vector
  data[["entidad"]] <- entidades[data[["entidad"]]]
  data
}
# Limpiar archivos
limpios <- importados %>%
  map(function(x) {
    data <- map(x, limpiar)
  }) %>% 
  # Unir achivos en tablas
  map(bind_rows) %>%
  bind_rows()


# Exportar valores terminados



  


