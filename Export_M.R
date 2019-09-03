## Script de la transformacion de Exportaciones Mensual

# Librerias
library(data.table) # Transformacion de datos mas rapida para grandes conjuntos de datos
library(readr) # Leer rapidamente los archivos planos de ancho fijo
library(odbc) # Mejor conexion de datos y carga muy rapida a SQL
library(lubridate)

# Definimos carpetas de archivos
bigfolder <- "\\Users\\PC\\Desktop\\Archivos Brutos\\Exportaciones\\"
foldername <- "Mensual"
workfolder <- paste(bigfolder, foldername, sep = "")

# Lista de archivos de en la carpeta de trabajo
wflist <- list.files(workfolder)
wflist <- grep(".AVA", wflist, value = TRUE)

# Conjunto de nombres de los meses 
fileid <- c(paste("m", substr(wflist, 3, 6), sep = "_"))

# Definimos los nombres de los archivos 
filename <- c(paste(workfolder, "\\", wflist, sep = ""))

# Definir los nombre de las columnas y el ancho fijo de sus columnas
cformat <- fwf_cols("Fecha" = 6,
                    "Numart" = 4,
                    "IDofic" = 2,
                    "IDaduana" = 2,
                    "IDident" = 1,
                    "Nit" = 12,
                    "IDtipo" = 2,
                    "IDusuar" = 5,
                    "IDexport" = 2,
                    "Munexport" = 5,
                    "IDpaisdestn" = 3,
                    "IDpaisdestc" = 3,
                    "Ciuddest" = 20,
                    "IDautor" = 16,
                    "IDdeclar" = 1,
                    "IDsalidan" = 2,
                    "IDsalidac" = 3,
                    "Deptoproc" = 2,
                    "Declant" = 14,
                    "Fechadecla" = 6,
                    "Numdeclant" = 14,
                    "Fechadecla2" = 6,
                    "IDmodalimp" = 4,
                    "IDmoneda" = 3,
                    "IDvia" = 1,
                    "Nacvia" = 3,
                    "IDregim" = 1,
                    "IDmodalexp" = 3,
                    "Formpago" = 1,
                    "IDembarque" = 1,
                    "IDdato" = 1,
                    "IDcertf" = 1,
                    "Sisespec" = 1,
                    "IDexpoT" = 1,
                    "Poscaran" = 18,
                    "Deptorig" = 2,
                    "IDundn" = 2,
                    "IDundc" = 3,
                    "Unidad" = 15,
                    "Kgbruto" = 15,
                    "Kgneto" = 15,
                    "FOBusd" = 15,
                    "FOBcop" = 20,
                    "Valagre" = 15,
                    "Flete" = 15,
                    "Seguro" = 15,
                    "Otrosgs" = 15,
                    "IDadmadu" = 2,
                    "Fechaembarque" = 6,
                    "Numdef" = 13,
                    "Numdef2" = 8,
                    "Nomexport" = 60,
                    "Direxport" = 60,
                    "Nit2" = 12,
                    "Nomexport2" = 60,
                    "Nomimport" = 60,
                    "Dirpaisdest" = 80
)

# Se definen los datos (c = texto, n = numerico)
ctype <- cols("c", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c", 
              "c", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c",
              "c", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c",
              "c", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c")

# Leemos los archivos al tiempo y le anexamos las fechas
for (i in 1:length(filename)) {
  data <- read_fwf(filename[i], 
                   col_positions = cformat,
                   col_types = ctype,
                   locale = locale(encoding = "latin-9")
  )
  data <- data.table(data)
  data[, Fecha := as.Date(paste(Fecha, "01", sep = ""),
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
          ] [, NitP := ifelse(IDident == 2 & Len == 10,
                              substr(NitN, 1, 9),
                              NitN)
             ] [, Nit := NitP
                ] [, c("NitN", "Len", "NitP") := NULL
                   ] [, c("Numart",
                          "IDaduana",
                          "IDident",
                          "IDtipo",
                          "IDexport",
                          "Munexport",
                          "IDpaisdestn",
                          "IDdeclar",
                          "IDsalidan",
                          "Deptoproc",
                          "Fechadecla",
                          "Fechadecla2",
                          "IDvia",
                          "IDregim",
                          "Formpago",
                          "IDcertf",
                          "Sisespec",
                          "Poscaran",
                          "Deptorig",
                          "IDundn",
                          "Unidad",
                          "Kgbruto",
                          "Kgneto",
                          "FOBusd",
                          "FOBcop",
                          "Valagre",
                          "Flete",
                          "Seguro",
                          "Otrosgs",
                          "Fechaembarque",
                          "Numdef2"
                   ) := .(as.numeric(Numart),
                          as.numeric(IDaduana),
                          as.numeric(IDident),
                          as.numeric(IDtipo),
                          as.numeric(IDexport),
                          as.numeric(Munexport),
                          as.numeric(IDpaisdestn),
                          as.numeric(IDdeclar),
                          as.numeric(IDsalidan),
                          as.numeric(Deptoproc),
                          as.numeric(Fechadecla),
                          as.numeric(Fechadecla2),
                          as.numeric(IDvia),
                          as.numeric(IDregim),
                          as.numeric(Formpago),
                          as.numeric(IDcertf),
                          as.numeric(Sisespec),
                          as.numeric(Poscaran),
                          as.numeric(Deptorig),
                          as.numeric(IDundn),
                          as.numeric(Unidad),
                          as.numeric(Kgbruto),
                          as.numeric(Kgneto),
                          as.numeric(FOBusd),
                          as.numeric(FOBcop),
                          as.numeric(Valagre),
                          as.numeric(Flete),
                          as.numeric(Seguro),
                          as.numeric(Otrosgs),
                          as.numeric(Fechaembarque),
                          as.numeric(Numdef2)
                   )
                   ]
}

## Fin de la transformacion

# Fechas para la busqueda en SQL
sqldates <- paste("20", substr(dblist, 5, 6), "-", substr(dblist, 3, 4), "-", "01",
                  sep = "")

# Conexion a SQL
conex <- dbConnect(odbc(),
                   Driver = "SQL Server",
                   Server = "40.87.53.140",
                   Database = "PyxisFinale",
                   UID = "alejandro.peralta",
                   PWD = "Pyxy54dm1n2018",
                   Encoding = "latin-9"
)

# Lista de fechas de SQL
expolist <- dbGetQuery(conex, 
                       "SELECT DISTINCT Fecha FROM Exportaciones_Colombia_Bienes;")
expolist <- expolist$Fecha

# Condicion para importar las tablas hacia importaciones
for (i in 1:length(sqldates)) {
  if (sqldates[i] %in% expolist) {
    truequery <- paste("DELETE FROM Exportaciones_Colombia_Bienes WHERE Fecha = '", sqldates[i], "';",
                       sep = "")
    dbGetQuery(conex, 
               truequery)
      data <- eval(parse(text = dblist[i]))
        dbWriteTable(conex,
                     "Exportaciones_Colombia_Bienes",
                     data,
                     append = TRUE
                     )
        } else {
          data <- eval(parse(text = dblist[i]))
          dbWriteTable(conex,
                       "Exportaciones_Colombia_Bienes",
                       data,
                       append = TRUE
                       )
  }
}

for (i in seq_along(dblist)) {
  data <- eval(parse(text = dblist[i]))
  dbWriteTable(conex,
               "Exportaciones_Colombia_Bienes",
               data,
               append = TRUE)
  print(paste0("terminado ", dblist[i]))
}

  
  
## Fin del Script
