# Generacion de la tabla Conjunta

# Librerias
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)

# Datos 
# Base de afiliados
afiliados <- 
  read_csv("\\Users\\PC\\Desktop\\Armenia\\afiliados.csv",
                      col_types = "cccccccciiicci",
                      locale = locale(encoding = "latin-9"))
# Tabla de fechas con pronosticos de edad
periodos <- 
  read_csv("\\Users\\PC\\Desktop\\Armenia\\periodos.csv",
           col_types = "dcdiid")
# Tabla de actividades por edad y genero
cruce <- 
  read_csv("\\Users\\PC\\Desktop\\Armenia\\cruce_estandar.csv",
           col_types = "diiid")
# Tabla de actividades
actividades <- 
  read_csv("\\Users\\PC\\Desktop\\Armenia\\actividades.csv",
           col_types = "iccciccccciciciciciiicdcdddccicciiddiccd",
           locale = locale(encoding = "latin-9"))

# Transformacion
# Generacion de las categorias unicas en afiliados
categorias <-
  afiliados %>%
  filter(afiliadoID == 3) %>%
  select(munpioID, entidadID, regimenID, generoID, fecha_nac) %>%
  mutate(fecha_nac = mdy(fecha_nac),
         anno = 2019 - year(fecha_nac),
         mes = 4 - month(fecha_nac),
         edad = (anno * 12) + mes,
         edad = round(edad / 12, 2),
         anno = as.integer(edad),
         mes = round(edad - anno, 2),
         mes = (mes * 12) / 100,
         edad = round(anno + mes, 2),
         cruce_base = paste0(regimenID, generoID, edad)) %>%
  filter(edad <= 120) %>%
  group_by(cruce_base, munpioID, entidadID) %>%
  summarise(personas = n())
  
# Generacion de los cruces en periodos
periodos <- 
  periodos %>%
  filter(!is.na(edad)) %>%
  mutate(cruce_base = paste0(regimenID, generoID, actual),
         cruce_jerarquia = paste0(generoID, edad),
         fecha = mdy(fecha)) %>%
  select(cruce_base, fecha, cruce_jerarquia)
# Generacion de los cruces en Cruce
cruce <-
  cruce %>%
  mutate(cruce_jerarquia = paste0(generoID, edad)) %>%
  select(-generoID, -edad)
# Limpieza de las actividades
actividades <- 
  actividades %>%
  select(jerarquiaID, valor, costo)

# Relacion entre las bases 
general <- 
  categorias %>%
  left_join(periodos, by = "cruce_base") %>%
  left_join(cruce, by = "cruce_jerarquia") %>%
  left_join(actividades, by = "jerarquiaID")

# Pruebas
resultado <- 
  general %>%
  filter(fecha >= ymd("2019-01-01"),
         fecha <= ymd("2019-12-01")) %>% 
  group_by(jerarquiaID, cruce_base) %>%
  summarise(act = mean(personas),
            min = min(proporcion),
            per = n())

sum(resultado$act)

# Escribir la tabla
write_csv(general,
          "\\Users\\PC\\Desktop\\general.csv",
          na = "")









            

