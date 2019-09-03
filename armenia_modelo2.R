# Script para generar las tablas de resultados de Armenia
# Ejercicio con Circasia
# Librerias
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(stringi)
library(readr)
library(odbc)

# Datos de afiliados
afiliados <- paste0(getwd(), "/afiliados_0319.csv") %>%
  read_csv(col_names = TRUE, col_types = cols(.default = "c")) %>%
  # Filtrar solo los afiliados activos
  filter(afiliadoID == "2") %>%
  # Solo municipio de circasia
  filter(munpioID == "63190") %>%
  # Calcular la edad
  mutate(anno = 2019 - year(fechanac),
         mes = 4 - month(fechanac),
         edad = (anno * 12) + mes,
         edad = round(edad / 12, 2),
         anno = as.integer(edad),
         mes = round(edad - anno, 2),
         mes = (mes * 12) / 100,
         edad = round(anno + mes, 2),
         edad = as.character(edad)) %>%
  select(-anno, -mes)

# Datos de los periodos
periodos <- paste0(getwd(), "/periodos.csv") %>%
  read_csv(col_names = TRUE, col_types = cols(.default = "c")) %>%
  # Reemplazar valores nulos por 0
  mutate(edad = replace_na(edad, 0),
         edad = as.character(edad),
         upc_valor = as.numeric(upc_valor) * 65610.6)

# Datos de las condiciones 
condiciones <- paste0(getwd(), "/condiciones.csv") %>%
  read_csv(col_names = TRUE, col_types = cols(.default = "c")) %>%
  mutate_at(vars(ruta, intervencion, atencion, programa, actividad,
                 esquema, consulta, procedimiento, tipo, programar,
                 transicion, observacion, notas, complejidad),
            stri_trans_general, "Latin-ASCII") %>%
  mutate_at(vars(ruta, intervencion, atencion, programa, actividad,
                 esquema, consulta, procedimiento, tipo, programar,
                 transicion, observacion, notas, complejidad),
            str_to_lower) %>%
  mutate_at(vars(ruta, intervencion, atencion, programa, actividad,
                 esquema, consulta, procedimiento, tipo, programar,
                 transicion, observacion, notas, complejidad),
            str_to_title)


# Datos del cruce entre para actividades por persona
actividades <- paste0(getwd(), "/cruce_estandar.csv") %>%
  read_csv(col_names = TRUE, col_types = cols(.default = "c")) %>%
  mutate(periodo = 1 / as.numeric(proporcion),
         periodo = as.integer(periodo))


# Ejemplo de las tablas
# Base Agrupada
afiliados_agrupado <- afiliados %>%
  group_by(edad, generoID, regimenID, munpioID, entidadID) %>%
  summarise(personas = n()) %>%
  as_tibble()


# Base individual
actividades_afiliados <- afiliados %>%
  group_by(edad, generoID, regimenID) %>%
  summarise(personas = n()) %>%
  as_tibble() %>%
  left_join(periodos, by = c("edad" = "actual", "generoID", "regimenID")) %>%
  rename(actual = edad,
         edad = edad.y) %>%
  select(-upc_valor) %>%
  left_join(actividades, by = c("generoID", "edad")) %>%
  mutate_at(vars(generoID, regimenID, jerarquiaID), as.integer) %>%
  mutate_at(vars(actual, edad, proporcion), as.numeric) %>%
  mutate(fecha = mdy(fecha)) %>%
  rename(actividades = personas)




# Importar a CSV
write_csv(actividades_afiliados,
          paste0(getwd(), "/actividades_circasia.csv"),
          na = "")

write_csv(afiliados_agrupado,
          paste0(getwd(), "/agrupado_circasia.csv"),
          na = "")

write_csv(afiliados,
          paste0(getwd(), "/afiliados_circasia.csv"),
          na = "")

write_csv(periodos_agrupado,
          paste0(getwd(), "/periodos_circasia.csv"),
          na = "")

write_csv(condiciones,
          paste0(getwd(), "/condiciones_circasia.csv"),
          na = "")

periodos_agrupado <- periodos %>%
  group_by(edad, generoID, regimenID) %>%
  summarise(upc_mes = min(upc_valor)) %>%
  as_tibble()

# Exportar a SQL
# Conexion a SQL
conex <- dbConnect(odbc(),
                   Driver = "SQL Server",
                   Server = "40.87.53.140",
                   Database = "PyxisFinale",
                   UID = "alejandro.peralta",
                   PWD = "Pyxy54dm1n2018",
                   Encoding = "IS0-8959-1")

# Importar a SQL
dbWriteTable(conex,
             "actividades_afiliados",
             actividades_afiliados,
             append = TRUE)



