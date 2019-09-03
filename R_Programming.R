# The R Programming Environment

# Crash Course on R 
# Asignacion
x <- 1
print(x)
# Vector
x <- 1:30
print(v)
# Numero
x <- 1453
x <- 1453L # L convierte el numero en entero
# Vectores
x <- c(TRUE, FALSE)
x <- c("oveja", "avestruz")
# Inicializar vectores
x <- vector("numeric", length = 9)
# Combinando vectores de diferentes tipos (sucede coercion)
x <- c(1.7, "a")
x <- c(TRUE, 2)
x <- c("a", TRUE)
# Coercion a proposito
x <- 1:9
as.integer(x) # Conversion a entero
as.character(x) # Conversion a texto
as.logical(x) # Conversion a logico
as.complex(x) # Conversion a complejo
# Matrices
m <- matrix(nrow = 2, ncol = 3)
print(m)
dim(m) # Dimensiones
attributes(m) # Atributos
m <- matrix(1:9, nrow = 3, ncol = 3) # Organizado por columna
m <- matrix(1:9, nrow = 3, ncol = 3, byrow = TRUE) # Organizado por fila
m <- 1:9
dim(m) <- c(9, 1) # Añadir dimensiones a vectores
print(m)
# Combinacion para crear matrices
a <- 1:9
b <- 0:8
cbind(a, b) # Union por columna
rbind(a, b) # Union por fila
# Listas
lista <- list(55, "cinco", TRUE, 1+1)
print(lista)
lista <- vector("list", length = 9) # Lista iniciliazada
# Factores
y <- factor(c("si", "si", "no", "no", "no", "no"))
print(y)
table(y) # Conteo de cada etiqueta
unclass(y)
y <- factor(c("si", "si", "no", "no", "no", "no"),
            levels = c("si", "no")) # orden
print(y)
# Valores faltantes
x <- NA
x <- NaN
# data frames
df <- data.frame(hola = 1:9, chao = c(T, T, T, T, F, F, F, F, F))
print(df)
nrow(df) # Numero de filas
ncol(df) # Numero de columnas
# Nombres
x <- 1:3
names(x) <- c("pato", "pato", "ganso") # Asignar nombres
names(x) # Ver nombres
m <- matrix(1:6, ncol = 2)
dimnames(m) <- list(c("pan1", "pan2", "pan3"),
                    c("huevo1", "huevo2")) # Nombres de filas y columnas matriz
print(m)
names(df) # Ver nombres del data frame
row.names(df) # Nombres de las filas del data frame
# Atributos
attributes(df) # Muestra todos los atributos del objeto

# Importancia de Tidy Data (data ordenada o limpia)
library(tidyr)
library(dplyr)

VADeaths %>% # Datos sucios
  tbl_df() %>% # Transformar en tibble 
  mutate(age = row.names(VADeaths)) %>% # usar nombres de fila como columna
  gather(key, death_rate, -age) %>% # hacer unpivot
  separate(key, c("urban", "gender"), sep = " ") %>% # separar columna
  mutate(age = factor(age), 
         urban = factor(urban),
         gender = factor(gender)) # Transformar columnas a factor

# Lectura de datos tabulares
library(readr)
teams <- read_csv(paste0("\\Users\\PC\\Desktop\\R Programming\\",
                         "data\\team_standings.csv"),
                  col_types = "cc")
# URL de los datos web
url <- paste0("http://rammb.cira.colostate.edu/research/",
              "tropical_cyclones/tc_extended_best_track_",
              "dataset/data/ebtrk_atlc_1988_2015.txt")
# Longitud de cada columna
longitud <- c(7, 10, 2, 2, 3, 5, 5, 6, 4, 5, 4, 4, 5, 3, 4, 3, 3, 3,
              4, 3, 3, 3, 4, 3, 3, 3, 2, 6, 1)
# Nombres de las columnas
nombres <- c("storm_id", "storm_name", "month", "day",
             "hour", "year", "latitude", "longitude",
             "max_wind", "min_pressure", "rad_max_wind",
             "eye_diameter", "pressure_1", "pressure_2",
             paste("radius_34", c("ne", "se", "sw", "nw"), sep = "_"),
             paste("radius_50", c("ne", "se", "sw", "nw"), sep = "_"),
             paste("radius_64", c("ne", "se", "sw", "nw"), sep = "_"),
             "storm_type", "distance_to_land", "final")
# Lectura
ext_tracks <- read_fwf(url, 
                       fwf_widths(longitud, nombres),
                       na = "-99")
ext_tracks[1:3, 1:9]



