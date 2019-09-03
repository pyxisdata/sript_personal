# Prueba 1 Planeacion

# Librerias
library(tidyverse)
library(lubridate)

# Datos
# Base de datos afiliados_quindio
afiliados <- read_csv("\\Users\\PC\\Desktop\\Armenia\\afiliados.csv",
                      col_types = "cccccccciiicci",
                      locale = locale(encoding = "latin-9"))
# Actividades de la ruta de prevencion
actividades <- read_csv("\\Users\\PC\\Desktop\\Armenia\\actividades.csv",
                        col_types = "iccciccccciciciciciiicdcdddccicciiddiccd",
                        locale = locale(encoding = "latin-9"))
# Cruce estandar 
cruce <- read_csv("\\Users\\PC\\Desktop\\Armenia\\cruce_estandar.csv",
                  col_types = "diiid")

# Periodos de cada edad
periodos <- read_csv("\\Users\\PC\\Desktop\\Armenia\\periodos.csv",
                     col_types = "dcdiid")

# Refinacion de periodos
periodos %>%
  mutate(fecha = mdy(fecha),
         llave_a = as.numeric(paste0(regimenID, generoID, edad)),
         llave_c = as.numeric(paste0(generoID, edad_pronostico))) %>%
  select(-edad, -generoID, -regimenID) -> periodos

# Refinacion de cruce
cruce %>%
  mutate(llave_c = as.numeric(paste0(generoID, edad))) %>%
  select(llave_c, periodos, proporcion, jerarquiaID) -> cruce

# Refinacion de la base de afiliados
afiliados %>%
  filter(afiliadoID == 3) %>%
  mutate(anno = 2019 - year(mdy(fecha_nac)),
         mes = 3 - month(mdy(fecha_nac)),
         edad = round(((anno * 12) + mes) / 12, 2),
         anno = as.integer(edad),
         mes = round(edad - anno, 2),
         mes = round(mes * 12, 0),
         mes = as.integer(mes),
         edad = (mes / 100) + anno,
         edad = round(edad, 2)) %>%
  select(-zonaID, -modalidadID, -afiliadoID, -fecha_nac) %>%
  filter(edad <= 120) %>%
  mutate(llave_a = as.numeric(paste0(regimenID, generoID, edad)),
         llave_c = as.numeric(paste0(generoID, edad))
         ) -> afiliados

# Refinacion de las actividades
actividades %>%
  select(jerarquiaID, intervencion, programa, actividad, complejidad,
         programar, transicion, costo, valor) -> actividades

# -----------------------------------------------------------------------------
# Relaciones para el individual
afiliados %>%
  # Filtro para persona
  filter(personaID == "10001381") %>%
  select(personaID, llave_a, llave_c) %>%
  # Union con la tabla de periodos
  left_join(filter(periodos,
                   fecha >= ymd("2019-01-01") & fecha <= ymd("2019-12-01")),
            by = c("llave_a")) %>%
  # Union para tomar las actividades por periodo
  left_join(cruce, by = c("llave_c.y" = "llave_c")) %>%
  group_by(personaID, jerarquiaID) %>%
  summarise(conteo = n_distinct(jerarquiaID),
            porcentaje = (max(periodos) - min(periodos)) + 1,
            porcentaje = (1 / max(periodos) * porcentaje),
            ) %>%
  inner_join(filter(actividades,
                   programar == "Si" & transicion == "No"),
            by = "jerarquiaID"
            ) %>%
  select(-programar, -transicion) %>%
  mutate(costo_est = costo * porcentaje,
         valor_est = valor * porcentaje
         ) -> individual

sum(individual$conteo)
sum(individual$porcentaje)
sum(individual$costo)
sum(individual$costo_est)

# -----------------------------------------------------------------------------
# Relaciones para el agregado
afiliados %>%
  filter(munpioID == 63190) %>%
  group_by(entidadID, generoID, regimenID, edad, llave_a, llave_c) %>%
  summarise(personas = n()) %>%
  left_join(filter(periodos,
                   fecha >= ymd("2019-01-01") & fecha <= ymd("2019-12-01")),
            by = c("llave_a")) %>%
  left_join(cruce, by = c("llave_c.y" = "llave_c")) %>%
  group_by(llave_a, jerarquiaID, personas) %>%
  summarise(conteo = n_distinct(jerarquiaID),
            porcentaje = (max(periodos) - min(periodos)) + 1,
            porcentaje = (1 / max(periodos) * porcentaje)) %>%
  mutate(conteo = personas * conteo) %>%
  inner_join(filter(actividades,
                    programar == "Si" & transicion == "No"),
             by = "jerarquiaID"
             ) %>%
  select(-programar, -transicion) %>%
  mutate(costo_est = costo * porcentaje,
         valor_est = valor * porcentaje
  ) -> circasia

sum(circasia$conteo)
