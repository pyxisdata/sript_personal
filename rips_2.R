# Script para la limpieza de los datos RIPS mensual (1 solo archivo)
# Librerias
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)
library(readxl)
library(purrr)
library(stringr)
# Eliminar la notacion cientifica
options(scipen = 999)

# Carpeta
carpeta <- "\\Users\\PC\\Desktop\\"
archivos <- list.files(carpeta, full.names = TRUE, pattern = "vida_4")
# Pestañas en el excel
hojas <- excel_sheets(archivos[1])

# Lectura de los archivos
datos_importados <- map(hojas, read_excel,
                        path = archivos[1],
                        col_names = FALSE)
# Añadir los nombres
names(datos_importados) <- hojas

# Limpieza de los archivos
limpiar <- function(archivo) {
  # Año de los archivos
  anno <- archivo[[1, 2]]
  # Limpieza
  archivo <- archivo %>%
    slice(6:n())
  # Seleccionar las primeras filas como datos de los encabezados
  medida <- archivo[1, ]
  genero <- archivo[2, ]
  regimen <- archivo[3, ]
  atencion <- archivo[4, ]
  # Nombres de las primeras 6 columnas
  iniciales <- c("mes", "municipioID", "diagnosticoID",
                 "procedimientoID", "edad", "personalID")
  # Limpieza de los encabezados
  encabezados <- rbind(atencion, regimen, genero, medida) %>%
    t() %>%
    as.data.frame() %>%
    as.tbl() %>%
    fill(1:4) %>%
    slice(7:n()) %>%
    mutate_all(as.character) %>%
    mutate(V1 = word(V1, 1, sep = "-"),
           V2 = word(V2, 1, sep = "-")) %>%
    mutate_all(str_trim) %>%
    mutate(V3 = case_when(V3 == "FEMENINO" ~ 1,
                          V3 == "MASCULINO" ~ 2,
                          V3 == "NO DEFINIDO" ~ 3,
                          V3 == "NR - NO REPORTADO" ~ 4),
          V3 = replace_na(V3, 3),
          V4 = case_when(V4 == "Número Personas Atendidas" ~ 1,
                          V4 == "Número de Atenciones" ~ 2,
                          V4 == "Valor Consulta" ~ 3,
                          V4 == "Costo Procedimiento" ~ 4),
          V5 = paste(V1, V2, V3, V4, sep = "_")) %>%
    select(V5)
  # Nombrar la base
  names(archivo) <- c(iniciales, unlist(encabezados))
  # Limpieza final
  archivo <- archivo %>%
    slice(5:n()) %>%
    gather("variables", "valores", -c(1:6)) %>%
    filter(!is.na(valores)) %>%
    separate(variables,
             c("atencionID", "regimenID", "generoID", "medidaID"),
             sep = "_") %>%
    mutate_at(vars(municipioID, diagnosticoID, procedimientoID,
                   personalID), word, 1, sep = "-") %>%
    mutate_all(str_trim)
  # Vector de meses para el reemplazo
  meses <- c("Enero" = 1, "Febrero" = 2, "Marzo" = 3, "Abril" = 4,
             "Mayo" = 5, "Junio" = 6, "Julio" = 7, "Agosto" = 8,
             "Septiembre" = 9, "Octubre" = 10, "Noviembre" = 11,
             "Diciembre" = 12)
  # Reemplazar los nombres de los meses con numeros
  archivo$mes <- meses[archivo$mes]
  # Añadir la columna de fecha
  archivo <- archivo %>%
    mutate(fecha = paste(anno, mes , "01", sep = "-"),
           fecha = ymd(fecha)) %>%
    mutate_at(vars(-fecha, -diagnosticoID, -procedimientoID, -valores),
              as.integer) %>%
    mutate(valores = as.double(valores)) %>%
    spread(medidaID, valores) 
  # Nombres finales
  nombres <- names(archivo)
  nombres[11:14] <- c("personas", "atenciones", 
                      "valor_consulta", "costo_procedimiento")
  names(archivo) <- nombres
  # Datos finales
  archivo
}
# Limpiar los archivos al tiempo
datos_limpios <- map(datos_importados, limpiar)
datos_limpios <- bind_rows(datos_limpios)
# Exportar datos
write_csv(datos_limpios, "\\Users\\PC\\Desktop\\salud_vida_2018.csv")

  
  
  
  
  
         