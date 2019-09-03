# Script para Industria EMMET 
# Librerias
library(dplyr)
library(tidyr)
library(lubridate)
library(odbc)
library(readxl)

# Carpeta
carpeta <- "\\Users\\PC\\Desktop\\Archivos Brutos\\Industria Mensual\\"

# Partes de la fecha actual
hoy <- today()
mes <- month(hoy)
nmes <- ifelse(mes %in% 3:12, mes - 2, ifelse(mes == 1, 11, 10))
anno <- ifelse(mes %in% 3:12, year(hoy), year(hoy) - 1)
meses <- c("enero", "febrero", "marzo", "abril", "mayo", "junio", 
           "julio", "agosto", "septiembre", "octubre", "noviembre",
           "diciembre")
# Direccion del total nacional 
url <- paste("https://www.dane.gov.co/files/investigaciones/boletines/",
             "emmet/anexos_nacional_emmet_",
             meses[nmes],
             "_",
             anno,
             ".xlsx",
             sep = "")

# Direccion del territorial
urlt <- paste("https://www.dane.gov.co/files/investigaciones/boletines/",
              "emmet/anexos_territorial_emmet_",
              meses[nmes],
              "_",
              anno,
              ".xlsx",
              sep = "")
# Nombre del archivo nacional
archivo <- paste(carpeta, "EMMN", "_", meses[nmes], "_", anno, ".xlsx", 
                 sep = "")
# Nombre del archivo territorial
archivot <- paste(carpeta, "EMMT", "_", meses[nmes], "_", anno, ".xlsx", 
                  sep = "")
# Descargar el archivo nacional
download.file(url, archivo, mode = "wb")
# Descargar el archivo territorial
download.file(urlt, archivot, mode = "wb")

# Seleccionar pestaña nacional
hojas <- excel_sheets(archivo)
indices <- grep("2014", hojas)

# Seleccionar pestaña territorial
hojast <- excel_sheets(archivot)
indicest <- grep("Departamentos", hojast)

# Funcion para leer el archivo nacional
datos <- read_xlsx(archivo, sheet = indices, col_names = FALSE)
# Funcion para leer el archivo territorial
datost <- read_xlsx(archivot, sheet = indicest, col_names = FALSE)
# Nombres archivo nacional
nombres <- c("dominio", "anno", "mes", "sector",
             "pn", "pr", "vn", "vr", "e", "ep", "et", "te", "to", 
             "ts", "sp", "st", "se", "so", "ht")
names(datos) <- nombres
# Nombres archivo territorial
nombrest <- c("departamento", "anno", "mes", "descripcion",
              "pn", "pr", "vn", "vr", "e")
names(datost) <- nombrest
# Transformacion nacional
datos %>%
  filter(!is.na(anno)) %>%
  slice(2:n()) %>%
  mutate(dominio = replace(dominio, dominio == "T_IND", 10000),
         fecha = ymd(paste(anno, mes, 1, sep = "-"))
         ) %>%
  select(-anno, -mes, -sector) %>%
  gather("medida", "indice", -fecha, -dominio) -> data
# Transformacion territorial
datost %>%
  filter(!is.na(anno)) %>%
  slice(2:n()) %>%
  mutate(fecha = ymd(paste(anno, mes, 1, sep = "-"))) %>%
  select(-mes, -anno) %>%
  gather("medida", "indice", -fecha, -descripcion, -departamento) -> datat
# Vector de reemplazo de las medidas nacional
codigosm <- setNames(c(1:15), 
                    c("pn", "pr", "vn", "vr", "e", "ep", "et", "te", "to", 
                      "ts", "sp", "st", "se", "so", "ht"))
data$medida[] <- codigosm[data$medida]
# Vector de reemplazo de las medidas territorial
codigosd <- setNames(c(5, 11, 76, 25, 68, 13, 8, 17, 66, 19, 73, 15, 23, 100), 
                    c("Antioquia", "Bogotá, D.C", "Valle del Cauca",
                      "Cundinamarca", "Santander", "Bolívar", "Atlántico",
                      "Caldas", "Risaralda", "Cauca", "Tolima", "Boyacá",
                      "Córdoba", "Otros Departamentos"))
datat$departamento[] <- codigosd[datat$departamento]
codigosc <- setNames(c(10000, 1000, 1300, 1500, 1600, 1700, 2000, 2300, 2500,
                       2900, 3200, 1400), 
                     c("Total",
                       "Alimentos y bebidas",
                       "Textiles y confecciones",
                       "Curtido de cuero y calzado",
                       "Madera y muebles",
                       "Papel e imprentas",
                       "Sustancias y productos químicos, farmacéuticos, de caucho y plásticos",
                       "Minerales no metálicos",
                       "Productos metálicos",
                       "Vehículos de transporte, carrocerías, autopartes y otro equipo de transporte",
                       "Resto de industria",
                       "Textiles, confecciones y cuero"))
datat$descripcion[] <- codigosc[datat$descripcion]

codigosmt <- setNames(c(1:5), 
                     c("pn", "pr", "vn", "vr", "e"))
datat$medida[] <- codigosmt[datat$medida]
# Limpieza final nacional
data %>%
  mutate_at(vars(-fecha), as.numeric) -> data
# Limpieza final territorial
datat %>%
  mutate_at(vars(-fecha), as.numeric) -> datat
# Conexion a SQL
conex <- dbConnect(odbc(),
                   Driver = "SQL Server",
                   Server = "40.87.53.140",
                   Database = "PyxisFinale",
                   UID = "alejandro.peralta",
                   PWD = "Pyxy54dm1n2018",
                   Encoding = "latin-9")
# Carga de los datos nacional
dbWriteTable(conex,
             "EMMN",
             data,
             overwrite = TRUE)
# Carga de los datos territorial  
dbWriteTable(conex,
             "EMMET",
             datat,
             overwrite = TRUE)




