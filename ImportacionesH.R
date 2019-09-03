## Script de Importaciones Final - Historico

## Librerias
library(dplyr) ## Una delicia las columnas
library(readr) ## Que belleza para leer textos
library(odbc) ## Magia para subir datos a SQL
library(foreign) ## Leer los dta

## Definimos la variable de la carpeta
direct <- "\\Users\\PC\\Desktop\\Archivos Brutos\\Importaciones\\"
folder <- "2017"
wd <- paste(direct, folder, sep = "")

## Lista de archivos en la carpeta
flist <- list.files(wd)
flist <- flist[grepl(".dta", flist)]

## Nombres de los archivos
nomdta <- c()
for (i in 1:length(flist)) {
  nomdta[i] <- strsplit(flist[i], "\\.")[[1]][1]
}

## Definimos el archivo zip interno
fpath <- c()
for (i in 1:length(flist)) {
  fpath[i] <- paste(wd, "\\", flist[i], sep = "")
}

## Definimos lista de meses
mlist <- c("04", "08", "12", "01", "02", "07", "06", "03", "05", "11", "10", "09")

## Definimos nombres de columnas
cnames <- c("Fecha",
            "IDaduana",
            "IDpaisorg",
            "IDpaispro",
            "IDpaiscom",
            "Deptodest",
            "IDvia",
            "Band",
            "IDregim",
            "IDacuer",
            "Kgbruto",
            "Kgneto",
            "Unidad",
            "IDund",
            "Poscaran",
            "FOBusd",
            "Flete",
            "CIFusd",
            "CIFcop",
            "Impven",
            "Otroder",
            "IDimport",
            "Ciudimport",
            "Ciudexport",
            "Actcono",
            "IDadmadu",
            "Valaduana",
            "Valajust",
            "Baseiva",
            "Porotro",
            "Baseotro",
            "Totiva",
            "Seguro",
            "Otrosgs",
            "Lugingre",
            "IDingreso",
            "Deptoimport",
            "IDpaisimport",
            "IDtipo",
            "Poraran",
            "Nit",
            "Digito",
            "Nomimport",
            "Deraran"
            )

## Listamos los archivos
for (i in 1:length(fpath)) {
  data <- read.dta(fpath[i], 
                   convert.factors = FALSE
                   )
  data %>%
    mutate(Fecha = as.Date(paste(folder, mlist[i], "01", sep = "-"),
                           format = "%Y-%m-%d"
                           )
         ) %>%
    select(Fecha, 2:44) -> data
  names(data) <- cnames
  assign(paste(nomdta[i], "df", sep = "_"), data)
}

# Lista de los dataframes
dflist <- objects(pattern = "_df")

# Transformacion de los Nits
for (i in dflist) {
  data <- eval(parse(text = i))
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
           Nit4 = NULL, # Eliminamos las columnas creadas temporalmente,
           IDaduana = ifelse(IDaduana == "", NA, IDaduana),
           IDpaisorg = ifelse(IDpaisorg == "", NA, IDpaisorg),
           IDpaispro = ifelse(IDpaispro == "", NA, IDpaispro),
           IDpaiscom = ifelse(IDpaiscom == "", NA, IDpaiscom),
           Deptodest = ifelse(Deptodest == "", NA, Deptodest),
           IDvia = ifelse(IDvia == "", NA, IDvia),
           Band = ifelse(Band == "", NA, Band),
           IDregim = ifelse(IDregim == "", NA, IDregim),
           IDacuer = ifelse(IDacuer == "", NA, IDacuer),
           Kgbruto = ifelse(Kgbruto == "", NA, Kgbruto),
           Kgneto = ifelse(Kgneto == "", NA, Kgneto),
           Unidad = ifelse(Unidad == "", NA, Unidad),
           IDund = ifelse(IDund == "", NA, IDund),
           Poscaran = ifelse(Poscaran == "", NA, Poscaran),
           FOBusd = ifelse(FOBusd == "", NA, FOBusd),
           Flete = ifelse(Flete == "", NA, Flete),
           CIFusd = ifelse(CIFusd == "", NA, CIFusd),
           CIFcop = ifelse(CIFcop == "", NA, CIFcop),
           Impven = ifelse(Impven == "", NA, Impven),
           Otroder = ifelse(Otroder == "", NA, Otroder),
           IDimport = ifelse(IDimport == "", NA, IDimport),
           Ciudimport = ifelse(Ciudimport == "", NA, Ciudimport),
           Ciudexport = ifelse(Ciudexport == "", NA, Ciudexport),
           Actcono = ifelse(Actcono == "", NA, Actcono),
           IDadmadu = ifelse(IDadmadu == "", NA, IDadmadu),
           Valaduana = ifelse(Valaduana == "", NA, Valaduana),
           Valajust = ifelse(Valajust == "", NA, Valajust),
           Baseiva = ifelse(Baseiva == "", NA, Baseiva),
           Porotro = ifelse(Porotro == "", NA, Porotro),
           Baseotro = ifelse(Baseotro == "", NA, Baseotro),
           Totiva = ifelse(Totiva == "", NA, Totiva),
           Seguro = ifelse(Seguro == "", NA, Seguro),
           Otrosgs = ifelse(Otrosgs == "", NA, Otrosgs),
           Lugingre = ifelse(Lugingre == "", NA, Lugingre),
           IDingreso = ifelse(IDingreso == "", NA, IDingreso),
           Deptoimport = ifelse(Deptoimport == "", NA, Deptoimport),
           IDpaisimport = ifelse(IDpaisimport == "", NA, IDpaisimport),
           IDtipo = ifelse(IDtipo == "", NA, IDtipo),
           Poraran = ifelse(Poraran == "", NA, Poraran),
           Nit = ifelse(Nit == "", NA, Nit),
           Digito = ifelse(Digito == "", NA, Digito),
           Nomimport = ifelse(Nomimport == "", NA, Nomimport),
           Deraran = ifelse(Deraran == "", NA, Deraran)
           ) -> data
  assign(i, data)
}

# Lista de los dataframes de nuevo
dflist <- objects(pattern = "_df")


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
for (i in dflist) {
  data <- eval(parse(text = i))
  dbWriteTable(con,
             "Importaciones_Colombia_Bienes",
             data,
             append = TRUE # Añadir nuevas filas a una tabla existente
             )
}
