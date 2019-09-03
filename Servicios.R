# Transformacion de Servicios Mensual y Servicios Bogota

# Traemos las librerias
library(data.table)
library(lubridate)
library(readxl)
library(odbc)

# Ubicacion de los historicos 
folder <- "\\Users\\PC\\Desktop\\Archivos Brutos\\Servicios Mensual\\"
tnal <- "Historico_MTS.xlsx"
tbta <- "Historico_MTSB.xlsx"

# Leer los archivos
datat <- read_excel(paste(folder, tnal, sep = ""),
                    col_names = FALSE
                    )

datab <- read_excel(paste(folder, tbta, sep = ""),
                    col_names = FALSE
                    )

# Transformacion en data.table
datat <- data.table(datat)
datab <- data.table(datab)

# Eliminamos los NA de la columna 2
nalf <- na.omit(datat, cols = 2)
bogf <- na.omit(datab, cols = 2)

# Eliminamos la fila 2
nalf <- nalf[c(1, 3:.N), ]
bogf <- bogf[c(1, 3:.N), ]

# Vectores para las fechas
year <- c(2007:2018)
trim <- c("01", "02", "03", "04")
dates <- c()

# Se hacen las fechas
for (i in year) {
  for (j in trim) {
    dates <- c(dates, 
               paste(i, j, "01", sep = "-")
               )
  }
}

# Convertir el vector de fechas en formato fecha
datesn <- dates[1:(dim(nalf)[1] - 2)]
datesb <- dates[21:(dim(bogf)[1] + 18)]

# Añadimos las fechas
nalf <- nalf[, 1 := c(NA, NA, datesn)]
bogf <- bogf[, 1 := c(NA, NA, datesb)]

# Trasponemos las bases
nalf <- data.table(t(nalf))
bogf <- data.table(t(bogf))

# Asignar la primera columna como encabezado
names(nalf) <- unlist(nalf[1, ])
names(bogf) <- unlist(bogf[1, ])

# Eliminar primera fila
nalf <- nalf[2:.N]
bogf <- bogf[2:.N]

# Renombrar columnas
names(nalf)[1:2] <- c("Servicio", "Medida")
names(bogf)[1:2] <- c("Servicio", "Medida")

# Rellenar primera columna
nalf <- nalf[, Servicio := Servicio[1], by = cumsum(!is.na(Servicio))]
bogf <- bogf[, Servicio := Servicio[1], by = cumsum(!is.na(Servicio))]

# Tablas de referencia
refn1 <- data.table(Servicio = unique(nalf$Servicio),
                    S1 = match(unique(nalf$Servicio), unique(nalf$Servicio))
                    )

refn2 <- data.table(Medida = unique(nalf$Medida),
                    M1 = match(unique(nalf$Medida), unique(nalf$Medida))
                    )

refb1 <- data.table(Servicio = unique(bogf$Servicio),
                    S1 = match(unique(bogf$Servicio), unique(bogf$Servicio))
                    )

refb2 <- data.table(Medida = unique(bogf$Medida),
                    M1 = match(unique(bogf$Medida), unique(bogf$Medida))
                    )

# Buscarv
nalf <- merge(refn1, nalf, by = "Servicio")
nalf <- merge(refn2, nalf, by = "Medida")
bogf <- merge(refb1, bogf, by = "Servicio")
bogf <- merge(refb2, bogf, by = "Medida")

# Eliminar las columnas no necesarias
nalf <- nalf[, c("Medida", "Servicio") := NULL]
bogf <- bogf[, c("Medida", "Servicio") := NULL]

# Hacemos Unpivot
nalf <- melt(nalf, id = 1:2, measure = 3:dim(nalf)[2], "Fecha", "Indice")
bogf <- melt(bogf, id = 1:2, measure = 3:dim(bogf)[2], "Fecha", "Indice")

# Nombres de las tablas finales
cnom <- c("IDmedida", "IDservicio", "Fecha", "Indice")
names(nalf) <- cnom
names(bogf) <- cnom

# Convertir indices en numero y fechas en fecha
nalf <- nalf[, (cnom) := .(as.numeric(IDmedida),
                           as.numeric(IDservicio),
                           as.Date(Fecha, format = "%Y-%m-%d"),
                           as.numeric(Indice)
                           )
             ]

bogf <- bogf[, (cnom) := .(as.numeric(IDmedida),
                           as.numeric(IDservicio),
                           as.Date(Fecha, format = "%Y-%m-%d"),
                           as.numeric(Indice)
                           )
             ]

# Hacemos la conexion a SQL
con <- dbConnect(odbc(),
                 Driver = "SQL Server",
                 Server = "40.87.53.140",
                 Database = "PyxisFinale",
                 UID = "alejandro.peralta",
                 PWD = "Pyxy54dm1n2018",
                 Encoding = "latin-9"
                 )

# Escribimos las tablas
dbWriteTable(con, 
             "MTS_Historico", nalf, 
             append = TRUE
             )

dbWriteTable(con, 
             "MTSB", bogf, 
             append = TRUE
             )


