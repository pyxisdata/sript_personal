# Transformacion de Servicios Mensual

# Traemos las librerias
library(data.table)
library(lubridate)
library(readxl)
library(odbc)

# Carpeta de trabajo
folder <- "\\Users\\PC\\Desktop\\Archivos Brutos\\Servicios Mensual\\"

# Definimos el mes y el año actual
tdate <- Sys.Date()
tmonth <- month(tdate)
tyear <- as.character(year(tdate))

# Vector con los nombres de meses 
nommonth = c("enero", "febrero", "marzo", "abril", "mayo", "junio", "julio",
             "agosto", "septiembre", "octubre", "noviembre", "diciembre")

# Definimos el retraso del EMS
monthf <- tmonth - 2

# Definimos las partes de las url
fixurl <- paste("http://www.dane.gov.co/files/investigaciones/boletines/",
                "ems/anexos_ems_",
                sep = "")
monthurl <- nommonth[monthf]
yearurl <- substr(tyear, 3, 4)
exturl <- ".xls"

# Definimos las url 
url <- paste(fixurl, monthurl, "_", yearurl, exturl,
                 sep = ""
)

# Definir nombres de lo descargado
file <- paste(folder, "EMS", "_", monthurl, "_", tyear,
              exturl,
              sep = ""
)

# Descargar los archivos
download.file(url,
              file,
              mode = "wb"
)

# Lista de las pestañas
sheetL <- excel_sheets(file)

# Ubicacion de pestaña de los indices
indexsheet <- grep("A13", sheetL)

# Leer los archivos
data <- read_excel(file,
                   sheet = indexsheet,
                   col_names = FALSE
                   )

# Transformacion en data.table
data <- data.table(data)

# Eliminamos los NA de la columna 2
dataf <- na.omit(data, cols = 2)

# Eliminamos la fila 2
dataf <- dataf[c(1, 3:.N), ]

# Vectores para las fechas
year <- c(2017:2020)
month<- c(paste("0", 1:9, sep = ""), 10:12)
dates <- c()

# Se hacen las fechas
for (i in year) {
  for (j in month) {
    dates <- c(dates, 
               paste(i, j, "01", sep = "-")
               )
  }
}

# Convertir el vector de fechas en formato fecha
datesm <- dates[1:(dim(dataf)[1] - 2)]

# Añadimos las fechas
dataf <- dataf[, 1 := c(NA, NA, datesm)]

# Trasponemos las bases
dataf <- data.table(t(dataf))

# Asignar la primera columna como encabezado
names(dataf) <- unlist(dataf[1, ])

# Eliminar primera fila
dataf <- dataf[2:.N]

# Renombrar columnas
names(dataf)[1:2] <- c("Servicio", "Medida")

# Rellenar primera columna
dataf <- dataf[, Servicio := Servicio[1], by = cumsum(!is.na(Servicio))]

# Tablas de referencia
refd1 <- data.table(Servicio = unique(dataf$Servicio),
                    S1 = match(unique(dataf$Servicio), unique(dataf$Servicio))
                    )

refd2 <- data.table(Medida = unique(dataf$Medida),
                    M1 = match(unique(dataf$Medida), unique(dataf$Medida))
                    )

# Buscarv
dataf <- merge(refd1, dataf, by = "Servicio")
dataf <- merge(refd2, dataf, by = "Medida")

# Eliminar las columnas no necesarias
dataf <- dataf[, c("Medida", "Servicio") := NULL]

# Hacemos Unpivot
dataf <- melt(dataf, id = 1:2, 
              measure = 3:dim(dataf)[2], 
              "Fecha", "Indice")

# Nombres de las tablas finales
cnom <- c("IDmedida", "IDservicio", "Fecha", "Indice")
names(dataf) <- cnom

# Convertir indices en numero y fechas en fecha
dataf <- dataf[, (cnom) := .(as.numeric(IDmedida),
                             as.numeric(IDservicio),
                             as.Date(Fecha, format = "%Y-%m-%d"),
                             as.numeric(Indice)
                             )
               ]

# Hacemos la conexion a SQL
conex <- dbConnect(odbc(),
                   Driver = "SQL Server",
                   Server = "40.87.53.140",
                   Database = "PyxisFinale",
                   UID = "alejandro.peralta",
                   PWD = "Pyxy54dm1n2018",
                   Encoding = "latin-9"
                   )

# Escribimos las tablas
dbWriteTable(conex, 
             "EMS", 
             dataf, 
             overwrite = TRUE
             )
