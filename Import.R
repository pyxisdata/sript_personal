# Script de la base de Importaciones Final 

# Librerias
library(data.table)
library(readr)
library(odbc)

# Definimos carpetas de archivos
bigfolder <- "\\Users\\PC\\Desktop\\Archivos Brutos\\Importaciones\\"
foldername <- "2012"
workfolder <- paste(bigfolder, foldername, sep = "")

# Lista de archivos de en la carpeta de trabajo
wflist <- list.files(workfolder)

# Conjunto de nombres de los meses 
fileid <- c(paste("m", c(1:12), sep = "_"))

# Definimos los nombres de los archivos 
filename <- c(paste(workfolder, "\\", wflist, sep = ""))

# Definir los nombre de las columnas y el ancho fijo de sus columnas
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

# Leemos los archivos al tiempo y le anexamos las fechas
for (i in 1:length(filename)) {
  data <- read_fwf(filename[i], 
                   col_positions = cformat,
                   col_types = ctype,
                   locale = locale(encoding = "latin-9")
                   )
  data <- data.table(data)
  data[, Fecha := as.Date(paste("20", Fecha, "01", sep = ""),
                          format = "%Y%m%d"
                          )]
  assign(fileid[i], data)
}

# Lista de las bases leidas
dblist <- objects(pattern = "m_")

# Hacemos la transformacion de los Nits
for (db in dblist) {
  data <- eval(parse(text = db))
  data[, NitN := ifelse(is.na(as.numeric(Nit)),
                        Nit,
                        as.numeric(Nit))
       ] [, Len := nchar(NitN)
          ] [, NitP := ifelse(Len == 10 & (substr(NitN, 1, 1) == 9 | substr(NitN, 1, 1) == 8),
                              substr(NitN, 1, 9),
                              NitN)
             ] [, Nit := NitP
                ] [, c("NitN", "Len", "NitP") := NULL
                   ]
}

# Conexion a SQL
conex <- dbConnect(odbc(),
                   Driver = "SQL Server",
                   Server = "40.87.53.140",
                   Database = "PyxisFinale",
                   UID = "alejandro.peralta",
                   PWD = "Pyxy54dm1n2018",
                   Encoding = "latin-9"
                   )

# Importamos la tabla hacia Exportaciones
for (db in dblist) {
  data <- eval(parse(text = db))
  dbWriteTable(conex,
               "Importaciones_Colombia_Bienes",
               data,
               append = TRUE
               )
}

# Fin de la transformacion

dbGetQuery(conex, "SELECT Fecha, COUNT(*) FROM Exportaciones_Colombia_Bienes GROUP BY Fecha")

dbGetQuery(conex, "DELETE FROM Exportaciones_Colombia_Bienes WHERE Fecha BETWEEN '2010-01-01' AND '2010-12-01'")


# Maximos
for(col in names(m_1)) {
  print(m_1[, max(nchar(col))])
}


Largos <- cbind(lapply(m_1, 
                       function(x) max(nchar(x),
                                       na.rm = TRUE)
                       )
                )
Largos