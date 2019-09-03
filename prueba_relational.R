# Prueba con bases de datos relacionales

# Libreria
library(tidyverse)
library(nycflights13)

# Tablas
View(airlines) # Aerolineas
View(airports) # Aeropuertos
View(planes) # Aviones
View(weather) # Clima
View(flights) # Vuelos

# Identificacion de llaves
# LLave unica
planes %>%
  count(tailnum) %>%
  filter(n > 1)
# Llave multiple
weather %>%
  count(year, month, day, hour, origin) %>%
  filter(n > 1)
# No hay llave
flights %>%
  mutate(serial = row_number()) -> flights # Se añade una llave serial

# Combinaciones
flights2 <- flights %>% 
  select(year:day, hour, origin, dest, tailnum, carrier)

# Left Join
flights2 %>%
  select(-origin, -dest) %>%
  left_join(airlines, by = "carrier") %>%
  count(name)

# Calcular el top 
flights2 %>%
  count(dest, sort = TRUE) %>%
  head(10)


