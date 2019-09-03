## Script de Importaciones Final - Historico

## Librerias
library(dplyr) ## Una delicia las columnas
library(readr) ## Que belleza para leer textos
library(odbc) ## Magia para subir datos a SQL
library(foreign) ## Leer los dta

## Definimos la variable de la carpeta
direct <- "\\Users\\PC\\Desktop\\Archivos Brutos\\Exportaciones\\"
folder <- "2018"
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
            "Numart",
            "IDofic",
            "IDaduana",
            "IDident",
            "Nit",
            "IDtipo",
            "IDusuar",
            "IDexport",
            "Munexport",
            "IDpaisdestn",
            "IDpaisdestc",
            "Ciuddest",
            "IDautor",
            "IDdeclar",
            "IDsalidan",
            "IDsalidac",
            "Deptoproc",
            "Declant",
            "Fechadecla",
            "Numdeclant",
            "Fechadecla2",
            "IDmodalimp",
            "IDmoneda",
            "IDvia",
            "Nacvia",
            "IDregim",
            "IDmodalexp",
            "Formpago",
            "IDembarque",
            "IDdato",
            "IDcertf",
            "Sisespec",
            "IDexpoT",
            "Poscaran",
            "Deptorig",
            "IDundn",
            "IDundc",
            "Unidad",
            "Kgbruto",
            "Kgneto",
            "FOBusd",
            "FOBcop",
            "Valagre",
            "Flete",
            "Seguro",
            "Otrosgs",
            "IDadmadu",
            "Fechaembarque",
            "Numdef",
            "Numdef2",
            "Nomexport",
            "Direxport",
            "Nit2",
            "Nomexport2",
            "Nomimport",
            "Dirpaisdest"
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
    Numart = ifelse(Numart == "", NA, Numart),
    IDofic = ifelse(IDofic == "", NA, IDofic),
    IDaduana = ifelse(IDaduana == "", NA, IDaduana),
    IDident = ifelse(IDident == "", NA, IDident),
    Nit = ifelse(Nit == "", NA, Nit),
    IDtipo = ifelse(IDtipo == "", NA, IDtipo),
    IDusuar = ifelse(IDusuar == "", NA, IDusuar),
    IDexport = ifelse(IDexport == "", NA, IDexport),
    Munexport = ifelse(Munexport == "", NA, Munexport),
    IDpaisdestn = ifelse(IDpaisdestn == "", NA, IDpaisdestn),
    IDpaisdestc = ifelse(IDpaisdestc == "", NA, IDpaisdestc),
    Ciuddest = ifelse(Ciuddest == "", NA, Ciuddest),
    IDautor = ifelse(IDautor == "", NA, IDautor),
    IDdeclar = ifelse(IDdeclar == "", NA, IDdeclar),
    IDsalidan = ifelse(IDsalidan == "", NA, IDsalidan),
    IDsalidac = ifelse(IDsalidac == "", NA, IDsalidac),
    Deptoproc = ifelse(Deptoproc == "", NA, Deptoproc),
    Fechadecla = ifelse(Fechadecla == "", NA, Fechadecla),
    Numdeclant = ifelse(Numdeclant == "", NA, Numdeclant),
    Fechadecla2 = ifelse(Fechadecla2 == "", NA, Fechadecla2),
    IDmodalimp = ifelse(IDmodalimp == "", NA, IDmodalimp),
    IDmoneda = ifelse(IDmoneda == "", NA, IDmoneda),
    IDvia = ifelse(IDvia == "", NA, IDvia),
    Nacvia = ifelse(Nacvia == "", NA, Nacvia),
    IDregim = ifelse(IDregim == "", NA, IDregim),
    IDmodalexp = ifelse(IDmodalexp == "", NA, IDmodalexp),
    Formpago = ifelse(Formpago == "", NA, Formpago),
    IDembarque = ifelse(IDembarque == "", NA, IDembarque),
    IDdato = ifelse(IDdato == "", NA, IDdato),
    IDcertf = ifelse(IDcertf == "", NA, IDcertf),
    Sisespc = ifelse(Sisespc == "", NA, Sisespc),
    IDexpoT = ifelse(IDexpoT == "", NA, IDexpoT),
    Poscaran = ifelse(Poscaran == "", NA, Poscaran),
    Deptorig = ifelse(Deptorig == "", NA, Deptorig),
    IDundn = ifelse(IDundn == "", NA, IDundn),
    IDundc = ifelse(IDundc == "", NA, IDundc),
    Unidad = ifelse(Unidad == "", NA, Unidad),
    Kgbruto = ifelse(Kgbruto == "", NA, Kgbruto),
    Kgneto = ifelse(Kgneto == "", NA, Kgneto),
    FOBusd = ifelse(FOBusd == "", NA, FOBusd),
    FOBcop = ifelse(FOBcop == "", NA, FOBcop),
    Valagre = ifelse(Valagre == "", NA, Valagre),
    Flete = ifelse(Flete == "", NA, Flete),
    Seguro = ifelse(Seguro == "", NA, Seguro),
    Otrosgs = ifelse(Otrosgs == "", NA, Otrosgs),
    IDadmadu = ifelse(IDadmadu == "", NA, IDadmadu),
    Fechaembarque = ifelse(Fechaembarque == "", NA, Fechaembarque),
    Numdef = ifelse(Numdef == "", NA, Numdef),
    Numdef2 = ifelse(Numdef2 == "", NA, Numdef2),
    Nomexport = ifelse(Nomexport == "", NA, Nomexport),
    Direxport = ifelse(Direxport == "", NA, Direxport),
    Nit2 = ifelse(Nit2 == "", NA, Nit2),
    Nomexport2 = ifelse(Nomexport2 == "", NA, Nomexport2),
    Nomimport = ifelse(Nomimport == "", NA, Nomimport),
    Dirpaisdest = ifelse(Dirpaisdest == "", NA, Dirpaisdest)
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
               "Exportaciones_Colombia_Bienes",
               data,
               append = TRUE # Añadir nuevas filas a una tabla existente
  )
}