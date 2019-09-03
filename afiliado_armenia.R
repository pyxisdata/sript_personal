# 
# Librerias
library(readxl)
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(odbc)

# Quitar la notacion cientifica
options(scipen = 999)

# Carpeta
folder <- "\\Users\\PC\\Desktop\\Armenia\\Afiliados\\"
files <- list.files(folder, ".TXT", full.names = TRUE)

# Importar los archivos
contr <- read_csv(files[1],
                  col_names = FALSE,
                  locale = locale(encoding = "latin-9"),
                  col_types = paste(rep("c", 20), collapse = "")
                  )

subs <- read_csv(files[2],
                 col_names = FALSE,
                 locale = locale(encoding = "latin-9"),
                 col_types = paste(rep("c", 31), collapse = "")
                 )

# Encabezados
names_contr <- c("id_identificacion", "id_persona", "apellido_1", "apellido_2",
                 "nombre_1", "nombre_2", "fecha_nac", "id_depto", "id_munpio",
                 "id_afiliado", "id_genero", "id_estado", "aa_1", "aa_2", 
                 "aa_3", "aa_4", "id_eps", "vacio", "fecha_af",
                 "fecha_archivo"
                 )

names_subs <- c("aa_1", "id_eps", "ident", "aa_2", "id_identificacion",
                "id_persona", "apellido_1", "apellido_2", "nombre_1",
                "nombre_2", "fecha_nac", "id_genero", "aa_4", "aa_5",
                "aa_6", "aa_7", "aa_8", "aa_9", "id_depto", "id_munpio",
                "id_zona", "fecha_sgss", "fecha_af", "aa_10", "aa_11",
                "aa_12", "aa_13", "id_modalidad", "id_afiliado",
                "fecha", "fecha_archivo"
                )

# Encabezados
names(contr) <- names_contr
names(subs) <- names_subs

# A?adir las columnas de clasificacion
contr$id_regimen <- 1
subs$id_regimen <- 2

# Refinar los datos
contr <- mutate_all(contr, as.character)
subs <- mutate_all(subs, as.character)

# Fecha del mes actual
date_n <- "2018-12-1"

# Unir las dos bases
afiliados <- bind_rows(subs, contr)

# Refinacion de los datos
afiliados %>%
  select(id_persona, id_identificacion, apellido_1, apellido_2, nombre_1,
         nombre_2, fecha_nac, id_eps, 
         id_genero, id_regimen, id_depto, id_munpio,
         id_zona, fecha_af, id_modalidad, id_afiliado, id_estado
         ) %>%
  mutate(fecha = ymd(date_n),
         fecha_nac = dmy(fecha_nac)) %>%
  mutate_at(vars(id_depto, id_munpio, id_regimen), as.numeric) %>%
  mutate(id_munpio = as.numeric(paste0(id_depto, id_munpio)),
         id_munpio = replace(id_munpio, id_munpio == 631, 63001),
         id_munpio = replace(id_munpio, id_munpio == 630, 63000),
         id_genero = ifelse(id_genero == "F", 1, 2),
         id_depto = NULL
         ) -> afiliados

# Asignar tipos de afiliados
tipo <- setNames(c(1:8), c("RE", "AF", "AC", "DE", "SU", "SM", "PL", "SD"))
afiliados$id_afiliado[] <- tipo[afiliados$id_afiliado]

names(afiliados) <- c("personaID", "identificacionID", "apellido_1",
                      "apellido_2", "nombre_1", "nombre_2", "fecha_nac",
                      "entidadID", "generoID", "regimenID", "munpioID",
                      "zonaID", "fecha_af", "modalidadID", "afiliadoID",
                      "estadoID", "fecha")

# CSV
write.csv(afiliados,
          "\\Users\\PC\\Desktop\\afiliados.csv",
          row.names = FALSE,
          na = "")  



# Subir data a SQL
conex <- dbConnect(odbc(),
                   Driver = "SQL Server",
                   Server = "40.87.53.140",
                   Database = "PyxisFinale",
                   UID = "alejandro.peralta",
                   PWD = "Pyxy54dm1n2018",
                   Encoding = "latin-9"
                   )

# Escritura
dbWriteTable(conex, 
             "Afiliados_G", 
             glob1, 
             append = TRUE
             )

# Funcion para sacar 100 ejemplos 
ejemplos <- function(per, act, n) {
  
  ejemp <- list()
  for (i in seq_along(1:n)) {
  # Sacar informacion de las personas
  per[per$id_persona == 
      unique(per$id_persona)[sample(1:length(unique(per$id_persona)),
                                                              1)],
                          ] -> data
  # Union
  final <- CJ.dt(data.table(data), act)
  final[id_genero == i.id_genero &
      edad >= inicial & edad < final, ] -> final

  # Vector numeros alatorios
  numeros <- sample(1:28, nrow(final), replace = TRUE)

  # A?adir fechas de dia
  final$dia <- ymd(paste(substr(final$fechas, 1, 4),
                   substr(final$fechas, 6, 8),
                   numeros,
                   sep = "-"))
  
  # Seleccion aleatoria
  seleccion <- sample(0:1, nrow(final), replace = TRUE)
  final$seleccion <- seleccion
  final[seleccion == 1] -> fin

  fin <- distinct(fin, jerarquiaID, .keep_all = TRUE)
  ejemp[[i]] <- fin
  print(paste("Toy en el ejemplo numero: ", i))
  }
  
  assign("ejemplos_totales", ejemp, envir = .GlobalEnv)
}

# Ejecutar
tic("inicio")
ejemplos(per = personas, act = actividades, n = 5000)
toc("termina de forma epica")

ejemplos_n <- rbindlist(ejemplos_totales)

write.csv(ejemplos_n, "\\Users\\PC\\Desktop\\ejemplos.csv",row.names = FALSE)


