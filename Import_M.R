# Script de la transformacion de Importaciones Mensual

# Librerias
library(data.table) # Transformacion de datos mas rapida para grandes conjuntos de datos
library(readr) # Leer rapidamente los archivos planos de ancho fijo
library(odbc) # Mejor conexion de datos y carga muy rapida a SQL
library(lubridate) # Manejar mas facilmente las fechas

# Definimos carpetas de archivos
folder <- "\\Users\\PC\\Desktop\\Archivos Brutos\\Importaciones\\Mensual\\"

# Lista de archivos de en la carpeta de trabajo
folderlist <- list.files(folder)
folderlist <- grep(".ASU", folderlist, value = TRUE)

# Conjunto de nombres de los meses 
monthid <- c(paste("m", substr(folderlist, 3, 6), sep = "_"))

# Definimos los nombres de los archivos 
filename <- c(paste(folder, "\\", folderlist, sep = ""))

# Leemos los archivos al tiempo y le anexamos las fechas
rd_impo <- function(file) {
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
  # Se definen los datos (todo texto para que la lectura sea mas limpia)
  ctype <- cols("c", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c", 
                "c", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c",
                "c", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c"
                )
  # Lectura de los archivos
  for (i in seq_along(file)) {
  data <- read_fwf(file[i], 
                   col_positions = cformat,
                   col_types = ctype,
                   locale = locale(encoding = "latin-9")
                   )
  data <- data.table(data)
  data[, Fecha := as.Date(paste("20", Fecha, "01", sep = ""),
                          format = "%Y%m%d"
                          )]
  assign(monthid[i], data, envir = .GlobalEnv)
  }
}

# Lectura del archivo
rd_impo(filename)

# Lista de las bases leidas
dblist <- objects(pattern = "m_")

# Funcion para hacer la transformacion de los Nits
trans_impo <- function(file) {
  # Hacemos la transformacion de los Nits
  for (i in seq_along(dblist)) {
    data <- eval(parse(text = dblist[i]))
    data[, NitN := ifelse(is.na(as.numeric(Nit)),
                          Nit,
                          as.numeric(Nit))
        ] [, Len := nchar(NitN)
            ] [, NitP := ifelse(Len == 10 & (substr(NitN, 1, 1) == 9 | substr(NitN, 1, 1) == 8),
                                substr(NitN, 1, 9),
                                NitN)
              ] [, Nit := NitP
                  ] [, c("NitN", "Len", "NitP") := NULL
                    ] [, c("IDaduana",
                           "IDpaisorg",
                           "IDpaispro",
                           "IDpaiscom",
                           "Deptodest",
                           "IDvia",
                           "Band",
                           "Kgbruto",
                           "Kgneto",
                           "Unidad",
                           "Poscaran",
                           "FOBusd",
                           "Flete",
                           "CIFusd",
                           "CIFcop",
                           "Impven",
                           "Otroder",
                           "IDadmadu",
                           "Valaduana",
                           "Valajust",
                           "Baseiva",
                           "Porotro",
                           "Baseotro",
                           "Totiva",
                           "Seguro",
                           "Otrosgs",
                           "Deptoimport",
                           "IDtipo",
                           "Poraran",
                           "Digito",
                           "Deraran"
                    ) := .(as.numeric(IDaduana),
                           as.numeric(IDpaisorg),
                           as.numeric(IDpaispro),
                           as.numeric(IDpaiscom),
                           as.numeric(Deptodest),
                           as.numeric(IDvia),
                           as.numeric(Band),
                           as.numeric(Kgbruto),
                           as.numeric(Kgneto),
                           as.numeric(Unidad),
                           as.numeric(Poscaran),
                           as.numeric(FOBusd),
                           as.numeric(Flete),
                           as.numeric(CIFusd),
                           as.numeric(CIFcop),
                           as.numeric(Impven),
                           as.numeric(Otroder),
                           as.numeric(IDadmadu),
                           as.numeric(Valaduana),
                           as.numeric(Valajust),
                           as.numeric(Baseiva),
                           as.numeric(Porotro),
                           as.numeric(Baseotro),
                           as.numeric(Totiva),
                           as.numeric(Seguro),
                           as.numeric(Otrosgs),
                           as.numeric(Deptoimport),
                           as.numeric(IDtipo),
                           as.numeric(Poraran),
                           as.numeric(Digito),
                           as.numeric(Deraran)
                    )]
    assign(paste("fin", monthid[i], sep = "_"), data, envir = .GlobalEnv)
  }
}

# Fin de la transformacion
trans_impo(dblist)

# Lista de las bases terminadas
finallist <- objects(pattern = "fin")

# Conexion a SQL
conex <- dbConnect(odbc(),
                   Driver = "SQL Server",
                   Server = "40.87.53.140",
                   Database = "PyxisFinale",
                   UID = "alejandro.peralta",
                   PWD = "Pyxy54dm1n2018",
                   Encoding = "latin-9"
)



# Funcion para importar las tablas hacia importaciones
sql_impo <- function(conex, final) {
  # Fechas para la busqueda en SQL
  dates <- paste("20", substr(final, 5, 6), "-", substr(final, 3, 4), "-", "01",
                    sep = "")
  # Lista de fechas de SQL
  impolist <- dbGetQuery(conex, 
                       "SELECT DISTINCT Fecha FROM Importaciones_Colombia_Bienes;")
  impolist <- impolist$Fecha

  for (i in seq_along(dates)) {
    if (dates[i] %in% impolist) {
      truequery <- paste("DELETE FROM Importaciones_Colombia_Bienes WHERE Fecha = '", dates[i], "';",
                       sep = "")
      dbGetQuery(conex, 
                 truequery)
      data <- eval(parse(text = final[i]))
      dbWriteTable(conex,
                   "Importaciones_Colombia_Bienes",
                   data,
                   append = TRUE
      )
    } else {
      data <- eval(parse(text = final[i]))
      dbWriteTable(conex,
                   "Importaciones_Colombia_Bienes",
                   data,
                   append = TRUE
      )
    }
  }
}

# Importar
sql_impo(conex, finallist)
# Importar version B
dbWriteTable(conex, "Importaciones_Colombia_Bienes", fin_m_0319, append = TRUE)
