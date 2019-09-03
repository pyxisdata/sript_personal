# Libreria
library(dplyr)
library(readxl)
library(readr)

# Carpeta
carpeta <- "\\Users\\PC\\Desktop\\Policia Nacional\\Homicidios"
archivos <- list.files(carpeta, full.names = TRUE)

# Lectura 
importar <- function(x) {
  datos <- list()
  for (i in seq_along(x)) {
    data <- read_excel(x[i], sheet = 1, col_names = FALSE)
    # Transformacion
    data %>%
      filter(!is.na(X__2)) -> data
    names(data) <- toupper(unlist(data[1, ]))
    names(data) <- names(read_excel(x[9], sheet = 1, col_names = FALSE))
    data %>%
      slice(2:n()) %>%
      mutate_all(as.character) -> data
    # Asignacion
    datos[[i]] <- data
  }
  nombres <- paste("homicidios", parse_number(x), sep = "_")
  names(datos) <- nombres
  assign("datos", datos, envir = .GlobalEnv)
}

importar(archivos)

homicidios <- bind_rows(datos)
names(homicidios) <- c("fecha", "depto", "munpio", "dia", "hora", "barrio",
                       "zona", "lugar", "arma", "movil_a", "movil_v", "edad",
                       "genero", "estado_civil", "empleado", "profesion",
                       "escolaridad", "cod_dane", "cantidad")

# Exportar
write.csv(homicidios, 
          "\\Users\\PC\\Desktop\\Policia Nacional\\Homicidios\\homicidio_total.csv",
          row.names = FALSE)



