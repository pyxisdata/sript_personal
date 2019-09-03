# Script para obtener reportes diarios de los registros de las empresas
# Librerias
# Conexion a google drive
library(googledrive)
library(googlesheets)
library(readr)
# Procesamiento de los datos
library(lubridate)
library(dplyr)
library(stringr)
library(stringi)
library(purrr)
# Graficacion
library(ggplot2)
library(DT)

# Conectarse a la hoja de calculo de google
hoja_calculo <- gs_key("1iKJj0l3Gix8Zcmg6-2M0CYS4m_ZEJclbxd4F38QU7cg")

# Importar los datos de la hoja de calculo de registros
registros <- hoja_calculo %>%
  gs_read(ws = "registros")

# Procesamiento de los registros
reporte <- registros %>%
  filter(Tipo != 0) %>%
  mutate(id = 1:nrow(.)) %>%
  select(id, everything()) %>%
  mutate_at(vars(Empresa, Nombre1, Apellido1, Nombre2, Apellido2,
                 Replegal, Cargo), 
            stri_trans_general, "Latin-ASCII") %>%
  mutate_at(vars(Nombre1, Apellido1, Nombre2, Apellido2, Cargo), 
            str_to_title) %>%
  mutate(Tipo = case_when(Tipo == 1 ~ "Aprobado",
                          Tipo == 2 ~ "Revisión",
                          Tipo == 3 ~ "Cancelado"),
         Fecha = word(Fecha, 1),
         Fecha = ymd(Fecha),
         Empresa = str_to_upper(Empresa),
         Carta = ifelse(!is.na(Carta),
                        "Si",
                        NA),
         Carta = ifelse(Replegal == "No" & is.na(Carta),
                        "No",
                        Carta))

# Reporte publico
reporte_publico <- reporte %>%
  select(-ID, -CIIU, -Formulario) %>%
  rename(ID = id) %>%
  mutate(Nombre = ifelse(Replegal == "Si",
                         Nombre1,
                         Nombre2),
         Apellido = ifelse(Replegal == "Si",
                           Apellido1,
                           Apellido2)) %>%
  select(-Nombre1, -Nombre2, -Apellido1, -Apellido2, -Cargo, -Carta) %>%
  select(ID, Tipo, Fecha, Empresa, NIT, Municipio, Departamento,
         Replegal, Nombre, Apellido, Telefono, Correo)

fecha <- reporte %>%
  filter(Tipo == "Aprobado") %>%
  group_by(Fecha) %>%
  summarise(Aprobados = n())

depto <- reporte %>%
  filter(Tipo == "Aprobado") %>%
  group_by(Departamento) %>%
  summarise(Aprobados = n())

ciudad <- reporte %>%
  filter(Tipo == "Aprobado") %>%
  group_by(Municipio) %>%
  summarise(Aprobados = n())



write_csv(registros, 
          paste0(getwd(), "/reporte_diario.csv"))