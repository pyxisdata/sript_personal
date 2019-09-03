# Script de Importaciones Final

# Librerias
library(lubridate)
library(dplyr)
library(readr)
library(odbc)

# Definimos la variable de la carpeta
direct <- "\\Users\\PC\\Desktop\\Archivos Brutos\\Importaciones\\"
folder <- "2018"
wd <- paste(direct, folder, sep = "")

# Definimos las fechas
mtoday <- month(Sys.Date()) - 2
mlist <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
ytoday <- as.character(year(Sys.Date()))

# Definimos el archivo
p1file <- "M1"
p2file <- paste(mlist[mtoday], 
                substr(ytoday, 3, 4),
                sep = ""
                )
extfile <- ".ASU"
nomfile <- paste(p1file,
                 p2file,
                 extfile,
                 sep = ""
                 )

# Ubicacion final
file <- paste(wd, "\\", nomfile, sep = "")

# Definir los nombres y anchos de columnas
cformat <- fwf_cols("Fecha" = 4,
                    "IDaduana" = 2,
                    "IDpaisorg" = 3,
                    "IDpaispro" = 3,
                    "IDpaiscom" = 3,
                    "Deptodest" = 2,
                    "IDvia" = 2,
                    "Band" = 3,
                    "IDregim" = 4,
                    "IDacuer" = 3,
                    "Kgbruto" = 13,
                    "Kgneto" = 13,
                    "Unidad" = 13,
                    "IDund" = 3,
                    "Poscaran" = 18,
                    "FOBusd" = 11,
                    "Flete" = 11,
                    "CIFusd" = 13,
                    "CIFcop" = 15,
                    "Impven" = 14,
                    "Otroder" = 14,
                    "IDimport" = 1,
                    "Ciudimport" = 29,
                    "Ciudexport" = 29,
                    "Actcono" = 4,
                    "IDadmadu" = 2,
                    "Valaduana" = 11,
                    "Valajust" = 11,
                    "Baseiva" = 14,
                    "Porotro" = 8,
                    "Baseotro" = 14,
                    "Totiva" = 14,
                    "Seguro" = 13,
                    "Otrosgs" = 13,
                    "Lugingre" = 15,
                    "IDingreso" = 3,
                    "Deptoimport" = 2,
                    "IDpaisimport" = 3,
                    "IDtipo" = 2,
                    "Poraran" = 8,
                    "Nit" = 16,
                    "Digito" = 1,
                    "Nomimport" = 60,
                    "Deraran" = 14
                    )

# Se definen los datos (c = texto, n = numero)
ctype <- cols("c",
              "n",
              "n",
              "n",
              "n",
              "n",
              "n",
              "n",
              "c",
              "c",
              "n",
              "n",
              "n",
              "c",
              "n",
              "n",
              "n",
              "n",
              "n",
              "n",
              "n",
              "c",
              "c",
              "c",
              "c",
              "n",
              "n",
              "n",
              "n",
              "n",
              "n",
              "n",
              "n",
              "n",
              "c",
              "c",
              "n",
              "c",
              "n",
              "n",
              "c",
              "c",
              "c",
              "n"
              )

# Leer el archivo
data <- read_fwf(file,
                 col_positions = cformat,
                 col_types = ctype,
                 locale = locale(encoding = "latin-9") 
                 )

# Hacemos la fecha
date <- paste(ytoday, mlist[mtoday], "01", sep = "-")

# Transformacion del archivo
data %>%
  mutate(Nit3 = ifelse(is.na(as.numeric(Nit)), # Si es numero se convierte, si es texto no
                       Nit,
                       as.numeric(Nit)
                       ),
  Len = nchar(Nit3), # Longitud de los nits trasnformados
  Nit4 = ifelse(Len == 10 & (substr(Nit3, 1, 1) == 9 | substr(Nit3, 1, 1) == 8), 
                substr(Nit3, 1, 9),
                Nit3
                ), # Si mide 10 y empieza por 8 o 9 es posible que tenga digito, asi que se quita
  Nit = Nit4,
  Nit3 = NULL,
  Len = NULL,
  Nit4 = NULL, # Eliminamos las columnas creadas temporalmente 
  Fecha = as.Date(date, format = "%Y-%m-%d")
  ) -> data

# Hacemos la conexion a SQL
con <- dbConnect(odbc(),
                 Driver = "SQL Server",
                 Server = "40.87.53.140",
                 Database = "PyxisFinale",
                 UID = "alejandro.peralta",
                 PWD = "Pyxy54dm1n2018",
                 Encoding = "latin-9" # Codificacion para que importe ñ y tildes
                 )

# Importamos la tabla hacia la tabla de importaciones 
dbWriteTable(con,
             "Importaciones_Colombia_Bienes",
             data,
             append = TRUE # Añadir nuevas filas a una tabla existente
             )
