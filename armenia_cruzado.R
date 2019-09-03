# Script para presicion en Armenia 
# Librerias
library(dplyr)
library(tidyr)
library(readr)

# Carpeta
carpeta <- "\\Users\\PC\\Desktop\\Armenia\\"
# Importar datos
# Archivo con edades y meses de 0 a 121 años
edades <- read_csv(paste0(carpeta, "edades.csv"),
                   col_types = "iidd")
# Archivo con las condiciones de edad y genero de las actividades
condiciones <- read_csv(paste0(carpeta, "condiciones.csv"),
                        col_types = "iiddiidd---")
# Archivo con las caracteristicas de las actividades
actividades <- read_csv(paste0(carpeta, "actividades.csv"),
                        col_types = "---------------------c-c------------i---")
# Archivo con los periodos
periodos <- read_csv(paste0(carpeta, "periodos.csv"),
                     col_types = "dcd---")
# Añadir la columna para permitir la union cruzada
edades <- edades %>%
  mutate(union = 1,
         edadmes = (anno * 12) + mes)
condiciones <- condiciones %>%
  mutate(union = 1,
         ina = as.integer(inicial),
         fia = as.integer(final),
         inm = round((inicial - ina), 2) * 100,
         fim = round((final - fia), 2) * 100,
         di = (ina * 12) + inm,
         df = (fia * 12) + fim,
         periodos = round(df - di, 0),
         proporcion = 1 / periodos) %>%
  select(jerarquiaID, generoID, inicial, final, costo,
         valor, periodos, proporcion, di, df, union)
# Realizar la union cruzada
relacion <- edades %>%
  full_join(condiciones, by = "union") %>%
  filter(annodec >= inicial,
         annodec < final) %>%
  mutate(periodo = round(df - edadmes, 0),
         proporcion = 1 / periodo) %>%
  select(annodec, generoID, jerarquiaID, costo, valor, proporcion)
# Prueba de los valores
personal <- relacion %>%
  filter(annodec >= 55.09,
         annodec <= 56.08,
         generoID == 2) %>%
  group_by(jerarquiaID) %>%
  summarise(costo = mean(costo),
            valor = mean(valor),
            cantidad = 1,
            estimacion = max(proporcion)) %>%
  left_join(actividades, by = "jerarquiaID") %>%
  filter(programar == "Si",
         transicion == "No")

