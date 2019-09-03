# Libreria
library(dplyr)
library(readr)

# Datos afiliados
bdua <- read_csv("\\Users\\PC\\Desktop\\Armenia\\bdua_febrero19.csv",
                 col_types = "cici-iii",
                 locale = locale(encoding = "latin-9"))

# Transformacion
bdua <- bdua %>%
  filter(estado == "Activo" & edad <= 120) %>%
  mutate(cruce_r = as.integer(paste0(generoID, regimenID, edad))) %>%
  select(-estado)

# Proceso de summario
bdua_2 <- bdua %>%
  group_by(cruce_r, munpioID, entidadID) %>%
  summarise(personas = sum(valor))

         