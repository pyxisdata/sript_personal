# Script para la transformacion del GEIH (con dplyr)

# Librerias
library(dplyr)
library(odbc)
library(zip)
library(lubridate)
library(haven)
library(purrr)

# Carpeta donde se encuentran todos los zip mensuales
folder <- "\\Users\\PC\\Desktop\\Archivos Brutos\\GEIH\\2018"

# Año de trabajo
year <- substr(folder, 40, 43)

# Lista de que contiene todos los archivos zip
folder_list <- list.files(folder, full.names = TRUE)
folder_list <- grep(".zip", folder_list, value = TRUE)

# Esta funcion lee cada uno de los archivos dentro del zip
import_geih <- function(zip, year) {
  
  # Nombres de los archivos incluidos en el zip
  file_names <- c("Pers", "Desoc", "Fuerza", "Inact", "Ocup", "Otros",
                  "Seman", "Hogares")
  
  # Se acorta a 3 caracteres los nombres de los meses
  month_names <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago",
                   "Sep", "Oct", "Nov", "Dic")
  
  # Mes del archivo
  file_month <- grep(substr(zip, 45, 47), 
                     month_names, 
                     value = TRUE)
  
  # Lista de los archivos internos 
  zip_files <- zip_list(zip)$filename
  
  # Ubicacion de los archivos de cabecera y resto dentro del archivo zip
  patt_c <- grep("becera", zip_files) 
  geo_c <- 1
  
  patt_r <- grep("esto", zip_files) 
  geo_r <- 2
  
  # Extraer los archivos de cabecera
  for (i in seq_along(file_names)) {
    
    # Lectura de los archivos
    data_c <- read_sav(unz(zip, zip_files[patt_c[i]]))
    data_r <- read_sav(unz(zip, zip_files[patt_r[i]]))
    
    # Encabezados en minuscula para hacer mejor match con los nombres
    names(data_c) <- tolower(names(data_c))
    names(data_r) <- tolower(names(data_r))
    
    # Se ajustan los nombres de las columnas a los historicamente acertados
    if (i == 1) {
      
      col_names <- c("directorio", "secuencia_p", "orden", "hogar", "regis",
                     "p6016", "p6020", "p6030s1", "p6030s3", "p6040", "p6050",
                     "p6070", "p6081", "p6081s1", "p6083", "p6083s1", "p6071",
                     "p6071s1", "p6090", "p6140", "p6150", "p6100", "p6110", 
                     "p6120", "p6125", "p6160", "p6170", "p6175", "p6210", 
                     "p6210s1", "p6220", "p6269", "clase", "esc", "fex_c_2011",
                     "mes", "dpto", "area"
                    )
    
    } else if (i == 2) {
      
      col_names <- c("directorio", "secuencia_p", "orden", "hogar", "regis",
                     "clase", "p7250", "p7260", "p7280", "p7280s1", "p7310",
                     "p7320", "p7350", "p7350s1", "p7360", "p9460", "p7390",
                     "p7390s1", "p7420", "p7420s1", "p7420s2", "p7420s3",
                     "p7420s4", "p7420s5", "p7420s6", "p7420s7", "p7420s7a1",
                     "p7420s8", "p7422", "p7422s1", "p1806", "p1519", "p1883",
                     "oficio1", "oficio2", "rama2d_d", "dsi", "dscy",
                     "rama4d_d", "fex_c_2011", "mes", "dpto", "area"
                    )
      
    } else if (i == 3) {
      
      col_names <- c("directorio", "secuencia_p", "orden", "hogar", "regis",
                     "clase", "p6290", "p6290s1", "p6230", "p6240", "p6240s1",
                     "p6250", "p6260", "p6270", "p6280", "p6300", "p6310",
                     "p6310s1", "p6320", "p6330", "p6340", "p6350", "p6351",
                     "raband", "ft", "fex_c_2011", "mes", "dpto", "area"
                     )
      
    } else if (i == 4) {
      
      col_names <- c("directorio", "secuencia_p", "orden", "hogar", "regis",
                     "clase", "p7430", "p7440", "p7450", "p7450s1", "p7452",
                     "p7454", "p7456", "p7458", "p7458s1", "p7460", "p7470",
                     "p7472", "p7472s1", "p744", "p1884", "p1807", "p6921",
                     "ini", "fex_c_2011", "mes", "dpto", "area"
                     )
      
    } else if (i == 5) {
      
      col_names <- c("directorio", "secuencia_p", "orden", "hogar", "regis",
                     "clase", "p388", "p6440", "p6450", "p6460", "p6460s1",
                     "p6400", "p6410", "p6410s1", "p6422", "p6424s1", "p6424s2",
                     "p6424s3", "p6426", "p6430s1", "p6480", "p6480s1", "p9440",
                     "p6500", "p6510", "p6510s1", "p6510s2", "p6590", "p6590s1",
                     "p6600", "p6600s1", "p6610", "p6610s1", "p6620",
                     "p6620s1", "p6585s1", "p6585s1a1", "p6585s1a2", "p6585s2",
                     "p6585s2a1", "p6585s2a2", "p6585s3", "p6585s3a1",
                     "p6585s3a2", "p6585s4", "p6585s4a1", "p6585s4a2", "p6545",
                     "p6545s1", "p6545s2", "p6580", "p6580s1", "p6580s2",
                     "p6630s1", "p6630s1a1", "p6630s2", "p6630s2a1", "p6630s3",
                     "p6630s3a1", "p6630s4", "p6630s4a1", "p6630s6",
                     "p6630s6a1", "p6640", "p6640s1", "p6765", "p6765s1", 
                     "p6772", "p6772s1", "p6773s1", "p6775", "p6750", "p6760",
                     "p550", "p6780", "p6780s1", "p6790", "p1800", "p1800s1",
                     "p1801s1", "p1801s2", "p1801s3", "p1802", "p1879", "p1805",
                     "p6800", "p6810", "p6810s1", "p6850", "p6830", "p6830s1",
                     "p6870", "p6880", "p6880s1", "p6915", "p6915s1", "p6920",
                     "p6930", "p6940", "p6960", "p6980", "p6980s1", "p6980s2",
                     "p6980s3", "p6980s4", "p6980s5", "p6980s6", "p6980s7",
                     "p6980s7a1", "p6980s8", "p6990", "p9450", "p7020", "p760",
                     "p7026", "p7028", "p7028s1", "p7040", "p390", "p7045",
                     "p7050", "p7050s1", "p7070", "p7075", "p7077", "p1880",
                     "p1881", "p1882", "p7090", "p7100", "p7110", "p7120",
                     "p7130", "p7140", "p7140s1", "p7140s2", "p7140s3",
                     "p7140s4", "p7140s5", "p7140s6", "p7140s7", "p7140s8",
                     "p7140s9", "p7140s9a1", "p7150", "p7160", "p7170s1",
                     "p7170s5", "p7170s6", "p7180", "p514", "p515", "p7240",
                     "p7240s1", "oficio", "rama2d", "oci", "p6430", "rama4d",
                     "rama4dp8", "rama2dp8", "inglabo", "fex_c_2011", "mes",
                     "dpto", "area"
                     )
      
    } else if (i == 8) {
      
      col_names <- c("directorio", "secuencia_p", "p5000", "p5010", "p5020",
                     "p5030", "p5040", "p5050", "p5070", "p5080", "p5090",
                     "p5090s1", "p5100", "p5110", "p5130", "p5140", "p5210s1",
                     "p5210s2", "p5210s3", "p5210s4", "p5210s5", "p5210s6",
                     "p5210s7", "p5210s8", "p5210s9", "p5210s10", "p5210s11",
                     "p5210s14", "p5210s15", "p5210s16", "p5210s17", "p5210s18",
                     "p5210s19", "p5210s20", "p5210s21", "p5210s22", "p5210s24",
                     "p5220", "p5220s1", "p6008", "p6007", "p6007s1", "hogar",
                     "p4000", "p4010", "p4020", "p4030s1", "p4030s1a1",
                     "p4030s2", "p4030s3", "p4030s4", "p4030s4a1", "p4030s5",
                     "p4040", "regis", "clase", "fex_c_2011", "mes", "dpto",
                     "area"
                     )
      
    } else { 
    
    }
    
    # El vector de valores logicos dice cuales columnas tomar y cuales no
    logi_c <- names(data_c) %in% col_names
    logi_r <- names(data_r) %in% col_names
    
    # Seleccionar unicamente las columnas del vector iguales a TRUE
    data_c <- data_c[logi_c]
    data_r <- data_r[logi_r]
    
    # Cuando las bases estan listas se le asignan columnas de fecha y geo
    data_c %>%
      mutate(fecha = ymd(paste(year, 
                               grep(file_month, month_names),
                               1,
                               sep = "-"
                          )
                    ),
             geo = geo_c
      ) -> data_c
    
    data_r %>%
      mutate(fecha = ymd(paste(year, 
                               grep(file_month, month_names),
                               1,
                               sep = "-"
                        )
                    ),
             geo = geo_r
      ) -> data_r
    
    # Conversion a factor
    data_c %>% 
      data.frame() %>%
      mutate_all(as.factor) -> data_c
    
    data_r %>% 
      data.frame() %>%
      mutate_all(as.factor) -> data_r
    
    # Se unen las bases de cabecera y resto
    data <- bind_rows(data_c, data_r)
    
    # Asignar las bases de datos a variables en el ambiente global
    assign(paste(file_month, "_",
                 file_names[i], "_",
                 year,
                 sep = ""
          ),
          data,
          envir = .GlobalEnv
    )
    
  }
  
}
    
# Archivos de salida
map(folder_list, import_geih, year = year)

# Listas de archivos de salida
db_list <- objects(pattern = year)

# Nombre de las listas
db_names <- c("Pers", "Desoc", "Fuerza", "Inact", "Ocup", "Hogares")

# Funcion para crear las listas de contencion
pattern_list <- function(x, pattern) {
  
  # Lista vacia para rellenar
  group_list <- list()
  
  # Ubicacion de las bases
  location <- grep(pattern, x)
  
  # Creacion de la lista
  for (i in seq_along(location)) {
    
    group_list[[i]] <- eval(parse(text = x[location[i]]))
    
  }
  assign(paste("dblist", pattern, 
               sep = "_"),
         group_list,
         envir = .GlobalEnv)
}

# Listas de los grupos de las bases
map(db_names, pattern_list, x = db_list)

# Lista global
global_list <- list(dblist_Pers,
                    dblist_Desoc, 
                    dblist_Fuerza, 
                    dblist_Inact,
                    dblist_Ocup,
                    dblist_Hogares)

# Nombres de la lista
names(global_list) <- db_names

# Eliminar sobrepeso
rm(list = ls(pattern = year))
rm(list = ls(pattern = "dblist"))

# Se unen las bases de cada lista en una sola para facilitar la transformacion
union_geih <- function(list, name, year) {
  
  data <- rbindlist(list, fill = TRUE)
  
  assign(paste(name, 
               year, 
               sep = "_"),
         data,
         envir = .GlobalEnv)
}

# Obtener los 8 archivos listos
for (i in seq_along(db_names)) {
  
  union_geih(global_list[[i]], name = db_names[i], year = year)
  
}

# Nueva lista global
final <- objects(pattern = year)

# Lista vacia para llenar
final_list <- list()

# Se meten todas las listas en una unica 
for (i in seq_along(final)) {
  
  final_list[[i]] <- eval(parse(text = final[i]))
  
}

# Nombres de la lista
names(final_list) <- final

# Eliminar sobrepeso
rm(list = ls(pattern = year))
rm(list = ls(pattern = "global"))

# Refinar los archivos finales
refin_geih <- function(x, db, year) {
  
  # Cada una de las bases posee una condicion especial de acuerdo al nombre
  if (db == paste("Hogares", year, sep = "_")) {
    
    x[, id_hogar := as.numeric(paste(directorio, secuencia_p,
                                     sep = ""))
      ] [is.na(p4030s1a1), p4030s1a1 := 7
         ] [, c("directorio", "secuencia_p", "mes", "dpto", 
                "area", "geo") := NULL
            ]
    
  } else if (db == paste("Fuerza", year, sep = "_")) {
    
    x[, id_persona := as.numeric(paste(directorio, secuencia_p, orden,
                                       sep = ""))
      ] [p6280 == 1, p6280 := 8
         ] [p6280 == 2, p6280 := 9
            ] [, c("directorio", "secuencia_p", "orden", 
                   "mes", "dpto", "area", "geo") := NULL
               ]
    
  } else if (db == paste("Desoc", year, sep = "_")) {
    
    x[, id_persona := as.numeric(paste(directorio, secuencia_p, orden,
                                       sep = ""))
      ] [, c("directorio", "secuencia_p", "orden", 
             "mes", "dpto", "area", "geo") := NULL
         ]
    
  } else if (db == paste("Ocup", year, sep = "_")) {
    
    x[, id_persona := as.numeric(paste(directorio, secuencia_p, orden,
                                       sep = ""))
      ] [, tipo1 := ifelse(p6850 < 48 & p7090 == 1 & p7110 == 2,
                           1,
                           ifelse(p6850 < 48 & p7090 == 1 & p7110 == 1
                                  & p7120 == 2,
                                  1,
                                  ifelse(p6850 < 48 & p7090 == 1 & p7110 == 1
                                         & p7120 == 1,
                                         4,
                                         7)))
         ]  [, tipo2 := ifelse(p7130 == 1 & p7140s1 == 1 & p7150 == 2,
                               2,
                               ifelse(p7130 == 1 & p7140s1 == 1 & 
                                        p7150 == 1 & p7160 %in% c(2, 9),
                                      2,
                                      ifelse(p7130 == 1 & p7140s1 == 1 & 
                                               p7150 == 1 & p7160 == 1,
                                             5,
                                             7)))
             ] [, tipo3 := ifelse(p7130 == 1 & p7140s2 == 1 & p7150 == 2,
                                  3,
                                  ifelse(p7130 == 1 & p7140s2 == 1 & p7150 == 1
                                         & p7160 %in% c(2, 9),
                                         3,
                                         ifelse(p7130 == 1 & p7140s2 == 1 
                                                & p7150 == 1 & p7160 == 1,
                                                6,
                                                7)))
                ] [, tipo_ocup := as.numeric(paste(tipo1, tipo2, tipo3,
                                                   sep = ""))
                   ] [, c("tipo1", "tipo2", "tipo3") := NULL
                      ] [, c("directorio", "secuencia_p", "orden", "mes", 
                             "dpto", "area", "geo") := NULL
                         ]
    
  } else if (db == paste("Inact", year, sep = "_")) {
    
    x[, id_persona := as.numeric(paste(directorio, secuencia_p, orden,
                                       sep = ""))
      ] [, tipo_inact := 10
         ] [, c("directorio", "secuencia_p", "orden", "mes", 
                "dpto", "area", "geo") := NULL
            ]
    
  } else if (db == paste("Pers", year, sep = "_")) {
    
    x[, id_persona := as.numeric(paste(directorio, secuencia_p, orden,
                                       sep = ""))
      ] [, id_hogar := as.numeric(paste(directorio, secuencia_p,
                                        sep = ""))
         ] [is.na(area), area := 0
            ] [, id_zona := as.numeric(paste(geo, dpto, area,
                                             sep = ""))
               ] [, c("directorio", "secuencia_p", "orden", "mes", 
                      "dpto", "area", "geo") := NULL
                  ]
    
  }
  
  assign(db, x, envir = .GlobalEnv)
  
}

# Archivos finales
for (i in seq_along(final)) {
  
  final_list[[i]] <- refin_geih(x = final_list[[i]],
                                db = final[i],
                                year = year
  )  
  
}

# Funcion para limpiar las bases
rm(list = setdiff(ls(), "final_list"))

# Limpieza final de la base antes de subirla a SQL
clean_geih <- function(file) {
  
  # Encontrar las columnas que son texto y que podrian tener caracteres malos
  logic <- which(map_lgl(file, is_character))
  
  # Lista de los caracteres malos
  patt <- c("^$|^ $", "^$|^\\.$", "^$|^,$", "^$|^;$", "^$|^-$", "^$|^_$")
  
  # Limpieza a cada uno de las columnas 
  for (j in logic) {
    for (k in patt) {
      
      set(file, i = grep(k, file[[j]]), j = j, value = NA)
      
    }
  }
  
}

# Limpieza
map(final_list, clean_geih)

# Nombres para usar 
data_names <- names(final_list)

# Escribir lo archivos en csv
csv_geih <- function(file, name) {
  
  write.csv(file, 
            paste("\\Users\\PC\\Desktop\\",
                  name,
                  ".csv",
                  sep = ""
            ),
            na = "",
            row.names = FALSE,
            fileEncoding = "latin-9"
  )
  
}

# Archivos leidos como csv
for (i in seq_along(data_names)) {
  
  csv_geih(final_list[[i]], data_names[i])
  
}

# Fin de la transformacion /////////////////////////////////////////////////

# Conexion a SQL Server
conex <- dbConnect(odbc(),
                   Driver = "SQL Server",
                   Server = "40.87.53.140",
                   Database = "PyxisFinale",
                   UID = "alejandro.peralta",
                   PWD = "Pyxy54dm1n2018",
                   Encoding = "latin-9"
)

# Lista de las tablas de SQL
sql_tbl <- c("GEIH_Desocupados_N", "GEIH_Fuerza_N", "GEIH_Hogares_N",
             "GEIH_Inactivos_N", "GEIH_Ocupados_N", "GEIH_Personas_N")

# Cargar las tablas a SQL Server
for (i in seq_along(sql_tbl)) {
  
  dbWriteTable(conex,
               sql_tbl[i],
               final_list[[data_names[i]]],
               append = TRUE
  )
  
  print(paste(data_names[i],
              "Terminado",
              sep = " "))
  
}




