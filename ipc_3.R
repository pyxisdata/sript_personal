# Librerias
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(stringi)

# Opciones
options(scipen = 999)

# Ponderadores
ponderadores <- "\\Users\\PC\\Desktop\\ponderaciones13.csv" %>%
  read_csv(col_names = TRUE, col_types = cols(.default = "c")) %>%
  mutate(ponderadores = as.double(ponderadores),
         ponderadores = ponderadores / 100,
         ponderadores = round(ponderadores, 5)) %>%
  spread(cod_municipio, ponderadores) %>%
  mutate_at(vars(-1, -2), replace_na, 0)
# Parametros
parametros <- "\\Users\\PC\\Desktop\\parametros_ipc.csv" %>%
  read_csv(col_names = TRUE,
           col_types = cols(.default = "c"),
           locale = locale(encoding = "ISO-8859-1")) 

# Union
union <- parametros %>%
  left_join(ponderadores, by = c("subclaseID" = "cod_subclase"))
# Ponderadores subclase
subclase <- union %>%
  group_by(subclase, subclaseID) %>%
  summarise_at(vars(`11001`:`8001`), sum) %>%
  as_tibble() %>%
  gather("idmunpio", "valor", -c(1:2))
# Ponderadores de clase
clase <- union %>%
  group_by(clase, claseID) %>%
  summarise_at(vars(`11001`:`8001`), sum )%>%
  as_tibble() %>%
  gather("idmunpio", "valor", -c(1:2))
# Ponderadores de grupo
grupo <- union %>%
  group_by(grupo, grupoID) %>%
  summarise_at(vars(`11001`:`8001`), sum) %>%
  as_tibble() %>%
  gather("idmunpio", "valor", -c(1:2))
# Ponderadores de division
division <- union %>%
  group_by(division, divisionID) %>%
  summarise_at(vars(`11001`:`8001`), sum) %>%
  as_tibble() %>%
  gather("idmunpio", "valor", -c(1:2))

# Nueva union
union_final <- parametros %>%
  left_join(subclase, by = c("subclaseID", "subclase")) %>%
  left_join(clase, by = c("claseID", "clase", "idmunpio")) %>%
  left_join(grupo, by = c("grupoID", "grupo", "idmunpio")) %>%
  left_join(division, by = c("divisionID", "division", "idmunpio")) %>%
  rename(valor_subclase = valor.x,
         valor_clase = valor.y,
         valor_grupo = valor.x.x,
         valor_division = valor.y.y)

# Tabla para exportar
write_csv(union_final,
          "\\Users\\PC\\Desktop\\parametros_ipc_final.csv",
          na = "")

