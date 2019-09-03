# Directorio 
library(readr)

carpeta <- getwd()
directorio_data <- read_csv(paste0(carpeta, "/directorio.csv"))
directorio <- directorio_data %>%
  group_by(NIT) %>%
  summarise(n = n(),
            nombre = last(NOMBRE_COMERCIAL),
            munpio = last(MUNI_ID_MPIO),
            depto = last(MUNI_ID_DPTO)) %>%
  mutate(munpio = paste0(depto, munpio)) %>%
  distinct(NIT, .keep_all = TRUE) %>%
  rename(nit = NIT) %>%
  select(nit, nombre, munpio) %>%
  mutate(nit = as.character(nit))


