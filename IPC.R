# IPC Mensual

library(odbc)
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)
library(purrr)

# Quitar notacion cientifica
options(scipen = 999)

# Leer el archivo historico
folder <- "\\Users\\PC\\Desktop\\Archivos Brutos\\IPC\\"

# Archivos en la carpeta
file <- list.files(folder, full.names = TRUE)

# Leer la data
ipc <- read_csv(file = file,
                col_names = TRUE)

# Transformar 
ipc %>% 
  gather(clase, indice, -FECHA, -COD_CLASIFICACION, -COD_IPC) %>%
  separate(clase, c("id_munpio", "id_ingreso"), sep = "_") %>% 
  mutate(codes = replace(id_ingreso, id_ingreso == "T", 9),
         codes = replace(codes, codes == "B", 1),
         codes = replace(codes, codes == "M", 2),
         codes = replace(codes, codes == "A", 3),
         id_ingreso = codes,
         codes = NULL,
         codes = replace(COD_CLASIFICACION, COD_CLASIFICACION == "T", 9),
         codes = replace(codes, codes == "G", 1),
         codes = replace(codes, codes == "S", 2),
         codes = replace(codes, codes == "C", 3),
         codes = replace(codes, codes == "GB", 4),
         COD_CLASIFICACION = codes,
         codes = NULL,
         id_ipc = as.numeric(COD_IPC),
         id_clasificacion = as.numeric(COD_CLASIFICACION),
         fecha = mdy(FECHA),
         id_munpio = as.numeric(id_munpio),
         id_ingreso = as.numeric(id_ingreso),
         indice = as.numeric(indice),
         COD_IPC = NULL,
         COD_CLASIFICACION = NULL,
         FECHA = NULL) -> data

# Transformacion de la base rellenando los datos
data2 <- data

munpio <- paste(c(15001, 18001, 19001, 20001, 17001, 41001, 47001,
                  63001, 70001, 73001, 88001), collapse = "|")

ingreso <- paste(c(9, 1, 2, 3), collapse = "|")

grupo <- paste(c(1000000, 2000000, 3000000, 4000000, 5000000, 6000000,
                 7000000, 8000000, 9000000), collapse = "|")

basico <- paste(c(1110100, 2110100, 3110100, 4110100, 5110100, 6110100,
                  7110100, 8110100, 9110100), collapse = "|")

data2 %>%
  mutate(indexm = ifelse(grepl(munpio, id_munpio),
                         1, 
                         0),
         indexi = ifelse(grepl(ingreso, id_ingreso),
                         1,
                         0),
         indexg = ifelse(grepl(grupo, id_ipc),
                         1,
                         0),
         indexb = ifelse(grepl(basico, id_ipc),
                         1,
                         0),
         index1 = indexm + indexi + indexg,
         index2 = indexm + indexi + indexb,
         indice1 = ifelse(index1 == 3, 
                          indice, 
                          NA)) %>% 
  fill(indice1) %>%
  mutate(indiceX = ifelse(index2 == 3,
                          indice1,
                          indice),
         indice = indiceX) %>%
  select(fecha, id_munpio, id_ingreso, id_ipc, id_clasificacion, indice
         ) -> dataf
  
# Conexion a SQL
conex <- dbConnect(odbc(),
                   Driver = "SQL Server",
                   Server = "40.87.53.140",
                   Database = "PyxisFinale",
                   UID = "alejandro.peralta",
                   PWD = "Pyxy54dm1n2018",
                   Encoding = "latin-9")
  
dbWriteTable(conex,
             "IPC",
             dataf,
             append = TRUE)



  