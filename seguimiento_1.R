# Script para base de pruebas de seguimiento 
# Librerias
library(tidyverse)
library(lubridate)

# Importamos los datos
afiliados <- read_csv("\\Users\\PC\\Desktop\\Armenia\\afiliados.csv",
                      col_types = "cccccccciiicci")
cruce <- read_csv("\\Users\\PC\\Desktop\\Armenia\\cruce_estandar.csv",
                  col_types = "diiid")
periodos <- read_csv("\\Users\\PC\\Desktop\\Armenia\\periodos.csv",
                     col_types = "dcdii")
actividades <- read_csv("\\Users\\PC\\Desktop\\Armenia\\actividades.csv",
                        col_types = "iccciccccciciciciciiicdcdddccicciiddiccd")

# Calculo de periodos
periodos <- periodos %>%
  mutate(fecha = mdy(fecha),
         edad = replace(edad, is.na(edad), 0),
         cruce_r = as.numeric(paste0(generoID, regimenID, actual)),
         cruce_g = as.numeric(paste0(generoID, edad))
         )

# Calculo de cruce
cruce <- cruce %>%
  mutate(cruce_g = as.numeric(paste0(generoID, edad)))

# Calculo actividades
actividades <- actividades %>%
  mutate(cruce_j = paste0(actividadID, generoID))

# Calculo de afiliados
afiliados_2 <- afiliados %>%
  filter(afiliadoID == 3) %>%
  mutate(fecha_nac = mdy(fecha_nac),
         anno = year(today()) - year(fecha_nac),
         mes = 4 - month(fecha_nac),
         edad = (anno * 12) + mes,
         edad = round(edad / 12, 2),
         anno = as.integer(edad),
         mes = round(edad - anno, 2),
         mes = (mes * 12) / 100,
         edad = round(anno + mes, 2),
         cruce_r = as.numeric(paste0(generoID, regimenID, edad))) %>%
  filter(edad <= 120)

# Union Afiliados - Cruce - Periodos - Actividades
persona_1 <- afiliados_2 %>% 
  filter(personaID == "71652102") %>%
  select(personaID, cruce_r) %>%
  left_join(filter(periodos,
                   fecha >= ymd("2019-01-01") & fecha <= ymd("2019-12-01")), 
            by = "cruce_r") %>%
  select(-generoID, -regimenID, -actual) %>%
  left_join(cruce, by = "cruce_g") %>%
  semi_join(filter(actividades,
                   transicion == "No" & programar == "Si"),
            by = "jerarquiaID") %>%
  select(personaID, fecha, jerarquiaID) %>%
  sample_n(20) %>%
  distinct(jerarquiaID, .keep_all = TRUE)

# Prueba con el FOR
lista <- list()

for (i in 1:5000) {
  data <- afiliados_2 %>% 
    filter(personaID == personaID[[i]]) %>%
    select(personaID, cruce_r) %>%
    left_join(filter(periodos,
                     fecha >= ymd("2019-01-01") & fecha <= ymd("2019-12-01")), 
              by = "cruce_r") %>%
    select(-generoID, -regimenID) %>%
    left_join(cruce, by = "cruce_g") %>%
    semi_join(filter(actividades,
                     transicion == "No" & programar == "Si"),
              by = "jerarquiaID") %>%
    select(personaID, fecha, jerarquiaID) %>%
    sample_n(20) %>%
    distinct(jerarquiaID, .keep_all = TRUE)
  lista[[i]] <- data
  print(paste0("Mi chavo estoy en el numero: ", i))
}

# Union en un dataframe
seguimiento <- bind_rows(lista)


  





