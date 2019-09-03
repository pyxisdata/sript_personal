# Script para la transformacion de Empleo Mensual
# Librerias
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(readxl)
library(purrr)
library(odbc)


# Condiciones de descargar
today <- Sys.Date()
month_t <- month(today) - 1
year_t <- year(today)

# Nombres de los meses para la descarga
month_names <- c("ene", "feb", "mar", "abr", "may", "jun",
                 "jul", "ago", "sep", "oct", "nov", "dic")

# Partes del archivo para descargar
fix_url <- paste("http://www.dane.gov.co/files/investigaciones/",
                 "boletines/ech/ech/anexo_empleo_", 
                 sep = "")

month_url <- month_names[month_t]
year_url <- substr(year_t, 3, 4)
ext_url <- ".xlsx"

url <- paste(fix_url, month_url, "_", year_url, ext_url,
             sep = "")

# Carpeta de descarga
folder <- paste("\\Users\\PC\\Desktop\\Archivos Brutos\\Empleo Mensual\\",
                year_t, "\\", 
                sep = "")

emp_file <- paste(folder, "empleo", "_", month_url, "_", year_url, ext_url, 
                  sep = "")

# Descarga del archivo bruto
download.file(url,
              emp_file, 
              mode = "wb")

# Lectura de las pesta?as en el archivo
read_emp <- function(file) {
  
  # Lista de pesta?as del archivo bruto
  sheet_list <- excel_sheets(file)
  
  nsheet <- c(2, 4, 5, 8, 9, 10, 11)
  
  sheet_names <- c("tnal", "tnaltrim", "areatrim", "tnalocu", "tnalposc",
                   "areaocu", "areaposc")
  
  files <- list()
  
  for (i in seq_along(nsheet)) {
  
    data <- read_xlsx(emp_file, 
                      sheet = nsheet[i],
                      col_names = FALSE)
    
    files[[i]] <- data
    
  }
  
  names(files) <- sheet_names
  
  assign("files_emp", 
         files,
         envir = .GlobalEnv)
  
}

# Lectura de los archivos
read_emp(emp_file)



# Transformacion de las bases
clean_emp <- function(x, i) {
  
  # Fechas
  dates <- seq.Date(from = as.Date("2001-01-01"),
                    to = as.Date('2020-01-01'),
                    by = "month")
  
  if (i == 1) {
  
    # Total Nacional Mensual  
    
    name <- "tnal"
    
    x[[i]] %>%
      filter(!is.na(.[[2]])) %>%
      slice(17:n()) %>%
      t() %>%
      data.frame(row.names = NULL) -> data

    names(data) <- c(paste("v", 1:16, sep = ""))

    data %>%
      slice(2:n()) %>%
      mutate(fecha = ymd(dates[1:dim(data)[1] -1])) %>%
      gather(nom_medida, valor, -fecha) %>%
      mutate(id_medida = word(nom_medida, 2, sep = "v"),
            nom_medida = NULL,
            id_medida = as.numeric(id_medida),
            valor = as.numeric(valor)) -> data
  
  } else if (i == 2) {
  
    # Total Nacional Trimestre Movil
    
    name <- "tnaltrim"
    
    x[[i]] %>%
      filter(!is.na(.[[2]])) %>%
      slice(17:32) %>%
      t() %>%
      data.frame(row.names = NULL) %>%
      slice(2:n()) -> data

    names(data) <- c(paste("t", 1:16, sep = "")) 
  
  } else if (i == 3) {
  
    # Areas Trimestre Movil
    
    name <- "areatrim"
    
    nom_mun <- c("bogotá", "medellín", "cali", "barranquilla", "bucaramanga",
                 "manizales", "pasto", "pereira", "cúcuta", "ibagué",
                 "montería", "cartagena", "villavicencio", "tunja",
                 "florencia", "popayán", "valledupar", "quibdó", "neiva",
                 "riohacha", "santa marta", "armenia", "sincelejo")

    nom_mun <- paste(nom_mun, collapse = "|")

    id_mun <- c("11001", "5001", "76001", "8001", "68001",
                "17001", "52001", "66001", "54001", "73001",
                "23001", "13001", "50001", "15001",
                "18001", "19001", "20001", "27001", "41001",
                "44001", "47001", "63001", "70001")

    id_mun <- rep(id_mun, times = 1, each = 16)

    id_medida <- rep(1:16, 23)

    x[[i]] %>%
      filter(!grepl("nota", tolower(.[[1]]))) %>%
      mutate(nom_mun = ifelse(grepl(nom_mun, tolower(.[[1]])),
                          tolower(.[[1]]),
                          NA)
            ) %>%
      fill(nom_mun) %>%
      filter(!is.na(.[[1]]), !is.na(.[[3]])) %>%
      slice(45:720) %>%
      mutate(nom_mun = NULL) %>%
      t() %>%
      data.frame(row.names = NULL) %>%
      slice(2:n()) %>%
      select(1:16, 31:46, 61:76, 91:106, 121:136, 151:166, 181:196, 211:226, 
            241:256, 271:286, 301:316, 331:346, 361:376, 391:406, 421:436,
            451:466, 481:496, 511:526, 541:556, 571:586, 601:616, 631:646,
            661:676) %>%
      t() %>%
      data.frame(row.names = NULL) %>%
      mutate(id_mun = id_mun,
            id_medida = id_medida,
            id_var = paste("v", id_mun, "_", id_medida, sep = ""),
            id_mun = NULL,
            id_medida = NULL
            ) %>%
      t() %>%
      data.frame(row.names = NULL) ->  data

    names(data) <- unlist(tail(data, 1))

    data %>%
      slice(1:(n() - 1)) %>%
      mutate(fecha = dates[1:(nrow(data) - 1)]) -> data
    
  } else if (i == 4) {
    
    # Total Nacional Ocupados
    
    name <- "tnalocu"
    
    x[[i]] %>%
      filter(!is.na(.[[2]])) %>%
      slice(3:14) %>%
      t() %>%
      data.frame(row.names = NULL) %>%
      slice(2:n()) -> data
    
    names(data) <- c(paste("o", 1:12, sep = "")) 
    
  } else if (i == 5) {
    
    # Total Nacional Ocupados
    
    name <- "tnalposc"
    
    x[[i]] %>%
      filter(!is.na(.[[2]])) %>%
      slice(3:12) %>%
      t() %>%
      data.frame(row.names = NULL) %>%
      slice(2:n()) -> data
    
    names(data) <- c(paste("p", 1:10, sep = ""))
    
  } else if (i == 6) {
    
    # Areas Ocupados
    
    name <- "areaocu"
    
    nom_mun <- c("medellín", "barranquilla", "bogotá", "cartagena", 
                 "manizales", "monter?a", "villavicencio", "pasto", 
                 "cúcuta", "pereira", "bucaramanga", "ibagué", "cali",  
                 "tunja", "florencia", "popayán", "valledupar", "quibdó",
                 "neiva", "riohacha", "santa marta", "armenia", "sincelejo")
    
    nom_mun <- paste(nom_mun, collapse = "|")
    
    id_mun <- c("5001", "8001", "11001", "13001",
                "17001", "23001", "50001", "52001",
                "54001", "66001", "68001", "73001", "76001",
                "15001", "18001", "19001", "20001", "27001",
                "41001", "44001", "47001", "63001", "70001")
    
    id_mun <- rep(id_mun, times = 1, each = 12)
    
    id_medida <- rep(1:12, 23)
    
    x[[i]] %>%
      filter(!grepl("nota", tolower(.[[1]]))) %>%
      mutate(nom_mun = ifelse(grepl(nom_mun, tolower(.[[1]])),
                              tolower(.[[1]]),
                              NA),
             nom_mun = ifelse(grepl("ocupados", nom_mun),
                              NA,
                              nom_mun)
      ) %>%
      fill(nom_mun) %>%
      filter(!is.na(.[60]), !is.na(.[[1]])) %>%
      slice(13:n()) %>%
      mutate(nom_mun = NULL) %>%
      t() %>%
      data.frame(row.names = NULL) %>%
      slice(2:n()) %>%
      t() %>%
      data.frame(row.names = NULL) %>% 
      mutate(id_mun = id_mun,
             id_medida = id_medida,
             id_var = paste("v", id_mun, "_", id_medida, sep = ""),
             id_mun = NULL,
             id_medida = NULL
      ) %>%
      t() %>%
      data.frame(row.names = NULL) -> data
    
    names(data) <- unlist(tail(data, 1))
    
    data %>%
      slice(1:(n() - 1)) %>%
      mutate(fecha = dates[7:(nrow(data) + 5)]) -> data
    
  } else if (i == 7) {
    
    # Areas Posicion
    
    name <- "areaposc"
    
    nom_mun <- c("medellín", "barranquilla", "bogotá", "cartagena", 
                 "manizales", "montería", "villavicencio", "pasto", 
                 "cúcuta", "pereira", "bucaramanga", "ibagué", "cali",  
                 "tunja", "florencia", "popayán", "valledupar", "quibdó",
                 "neiva", "riohacha", "santa marta", "armenia", "sincelejo")
    
    nom_mun <- paste(nom_mun, collapse = "|")
    
    id_mun <- c("5001", "8001", "11001", "13001",
                "17001", "23001", "50001", "52001",
                "54001", "66001", "68001", "73001", "76001",
                "15001", "18001", "19001", "20001", "27001",
                "41001", "44001", "47001", "63001", "70001")
    
    id_mun <- rep(id_mun, times = 1, each = 10)
    
    id_medida <- rep(1:10, 23)
    
    x[[i]] %>%
      filter(!grepl("nota", tolower(.[[1]]))) %>%
      mutate(nom_mun = ifelse(grepl(nom_mun, tolower(.[[1]])),
                              tolower(.[[1]]),
                              NA),
             nom_mun = ifelse(grepl("ocupados", nom_mun),
                              NA,
                              nom_mun)
      ) %>%
      fill(nom_mun) %>%
      filter(!is.na(.[[3]]), !is.na(.[[1]])) %>%
      slice(11:n()) %>%
      mutate(nom_mun = NULL) %>%
      t() %>%
      data.frame(row.names = NULL) %>%
      slice(2:n()) %>%
      t() %>%
      data.frame(row.names = NULL) %>% 
      mutate(id_mun = id_mun,
             id_medida = id_medida,
             id_var = paste("v", id_mun, "_", id_medida, sep = ""),
             id_mun = NULL,
             id_medida = NULL
      ) %>%
      t() %>%
      data.frame(row.names = NULL) -> data
    
    names(data) <- unlist(tail(data, 1))
    
    data %>%
      slice(1:(n() - 1)) %>%
      mutate(fecha = dates[1:(nrow(data) -1)]) -> data
    
  }
  
  assign(name, data, envir = .GlobalEnv)
  
}

# Lectura de las transformaciones
clean_emp(files_emp, 1)
clean_emp(files_emp, 2)
clean_emp(files_emp, 3)
clean_emp(files_emp, 4)
clean_emp(files_emp, 5)
clean_emp(files_emp, 6)
clean_emp(files_emp, 7)

# Union de los totales y las areas
union_geih <- function(a, b, i) {
  
  data <- bind_cols(a, b)

  data %>%
    mutate_at(vars(-fecha), as.character) %>%
    mutate_at(vars(-fecha), as.numeric) -> data
  
  data[is.na(data)] <- 0
  
  if (i == 1) {
    
    # Nacional y Areas Trimestral
    
    for (i in seq_along(1:16)) {
      
      n1 <- i 
      n2 <- i + 16 
      n3 <- i + 32
      n4 <- i + 48
      n5 <- i + 64
      n6 <- i + 80
      n7 <- i + 96
      n8 <- i + 112
      n9 <- i + 128
      n10 <- i + 144
      n11 <- i + 160
      n12 <- i + 176
      n13 <- i + 192
      n14 <- i + 208
      n15 <- i + 224
      n16 <- i + 240
      n17 <- i + 256
      n18 <- i + 272
      n19 <- i + 288
      n20 <- i + 304
      n21 <- i + 320
      n22 <- i + 336
      n23 <- i + 352
      n24 <- i + 368
      
      col <- as.numeric(as.character(data[[n1]])) - (
             as.numeric(as.character(data[[n2]])) +
             as.numeric(as.character(data[[n3]])) +
             as.numeric(as.character(data[[n4]])) +
             as.numeric(as.character(data[[n5]])) +
             as.numeric(as.character(data[[n6]])) +
             as.numeric(as.character(data[[n7]])) +
             as.numeric(as.character(data[[n8]])) +
             as.numeric(as.character(data[[n9]])) +
             as.numeric(as.character(data[[n10]])) +
             as.numeric(as.character(data[[n11]])) +
             as.numeric(as.character(data[[n12]])) +
             as.numeric(as.character(data[[n13]])) +
             as.numeric(as.character(data[[n14]])) +
             as.numeric(as.character(data[[n15]])) +
             as.numeric(as.character(data[[n16]])) +
             as.numeric(as.character(data[[n17]])) +
             as.numeric(as.character(data[[n18]])) +
             as.numeric(as.character(data[[n19]])) +
             as.numeric(as.character(data[[n20]])) +
             as.numeric(as.character(data[[n21]])) +
             as.numeric(as.character(data[[n22]])) +
             as.numeric(as.character(data[[n23]])) +
             as.numeric(as.character(data[[n24]])))
      
      data[paste("v100000", i, sep = "_")] <- col
      
    }
    
    name <- "area_fin"
    
    data %>%
      mutate(fecha = ymd(fecha)) %>%
      select(fecha, 17:(ncol(data))) -> data
    
  } else if (i == 2) {
    
    # Nacional y Areas Ocupados
    
    for (i in seq_along(1:12)) {
      
      n1 <- i 
      n2 <- i + 12 
      n3 <- i + 24
      n4 <- i + 36
      n5 <- i + 48
      n6 <- i + 60
      n7 <- i + 72
      n8 <- i + 84
      n9 <- i + 96
      n10 <- i + 108
      n11 <- i + 120
      n12 <- i + 132
      n13 <- i + 144
      n14 <- i + 156
      n15 <- i + 168
      n16 <- i + 180
      n17 <- i + 192
      n18 <- i + 204
      n19 <- i + 216
      n20 <- i + 228
      n21 <- i + 240
      n22 <- i + 252
      n23 <- i + 264
      n24 <- i + 276
      
      col <- as.numeric(as.character(data[[n1]])) - (
        as.numeric(as.character(data[[n2]])) +
          as.numeric(as.character(data[[n3]])) +
          as.numeric(as.character(data[[n4]])) +
          as.numeric(as.character(data[[n5]])) +
          as.numeric(as.character(data[[n6]])) +
          as.numeric(as.character(data[[n7]])) +
          as.numeric(as.character(data[[n8]])) +
          as.numeric(as.character(data[[n9]])) +
          as.numeric(as.character(data[[n10]])) +
          as.numeric(as.character(data[[n11]])) +
          as.numeric(as.character(data[[n12]])) +
          as.numeric(as.character(data[[n13]])) +
          as.numeric(as.character(data[[n14]])) +
          as.numeric(as.character(data[[n15]])) +
          as.numeric(as.character(data[[n16]])) +
          as.numeric(as.character(data[[n17]])) +
          as.numeric(as.character(data[[n18]])) +
          as.numeric(as.character(data[[n19]])) +
          as.numeric(as.character(data[[n20]])) +
          as.numeric(as.character(data[[n21]])) +
          as.numeric(as.character(data[[n22]])) +
          as.numeric(as.character(data[[n23]])) +
          as.numeric(as.character(data[[n24]]))
      )
      
      data[paste("v100000", i, sep = "_")] <- col
      
    }
    
    name <- "ocu_fin"
    
    data %>%
      mutate(fecha = ymd(fecha)) %>%
      select(fecha, 13:(ncol(data))) -> data
    
  } else if (i == 3) {
    
    # Nacional y Areas Ocupados
    
    for (i in seq_along(1:10)) {
      
      n1 <- i 
      n2 <- i + 10 
      n3 <- i + 20
      n4 <- i + 30
      n5 <- i + 40
      n6 <- i + 50
      n7 <- i + 60
      n8 <- i + 70
      n9 <- i + 80
      n10 <- i + 90
      n11 <- i + 100
      n12 <- i + 110
      n13 <- i + 120
      n14 <- i + 130
      n15 <- i + 140
      n16 <- i + 150
      n17 <- i + 160
      n18 <- i + 170
      n19 <- i + 180
      n20 <- i + 190
      n21 <- i + 200
      n22 <- i + 210
      n23 <- i + 220
      n24 <- i + 230
      
      col <- as.numeric(as.character(data[[n1]])) - (
        as.numeric(as.character(data[[n2]])) +
          as.numeric(as.character(data[[n3]])) +
          as.numeric(as.character(data[[n4]])) +
          as.numeric(as.character(data[[n5]])) +
          as.numeric(as.character(data[[n6]])) +
          as.numeric(as.character(data[[n7]])) +
          as.numeric(as.character(data[[n8]])) +
          as.numeric(as.character(data[[n9]])) +
          as.numeric(as.character(data[[n10]])) +
          as.numeric(as.character(data[[n11]])) +
          as.numeric(as.character(data[[n12]])) +
          as.numeric(as.character(data[[n13]])) +
          as.numeric(as.character(data[[n14]])) +
          as.numeric(as.character(data[[n15]])) +
          as.numeric(as.character(data[[n16]])) +
          as.numeric(as.character(data[[n17]])) +
          as.numeric(as.character(data[[n18]])) +
          as.numeric(as.character(data[[n19]])) +
          as.numeric(as.character(data[[n20]])) +
          as.numeric(as.character(data[[n21]])) +
          as.numeric(as.character(data[[n22]])) +
          as.numeric(as.character(data[[n23]])) +
          as.numeric(as.character(data[[n24]]))
      )
      
      data[paste("v100000", i, sep = "_")] <- col
      
    }
    
    name <- "posc_fin"
    
    data %>%
      mutate(fecha = ymd(fecha)) %>%
      select(fecha, 11:(ncol(data))) -> data
    
  }
  
  
  data %>%
    gather(id_medida, valor, -fecha) %>%
    separate(id_medida, c("id_munpio", "id_medida"), sep = "_") %>%
    mutate(id_munpio = sub("v", "", id_munpio),
           id_munpio = as.numeric(as.character(id_munpio)),
           id_medida = as.numeric(as.character(id_medida)),
           valor = as.numeric(as.character(valor))
           ) -> data

  assign(name, data, envir = .GlobalEnv)
  
}

# Archivos Finales
union_geih(tnaltrim, areatrim, 1)
union_geih(tnalocu, areaocu, 2)
union_geih(tnalposc, areaposc, 3)

# Hacemos la conexion a SQL
conex <- dbConnect(odbc(),
                   Driver = "SQL Server",
                   Server = "40.87.53.140",
                   Database = "PyxisFinale",
                   UID = "alejandro.peralta",
                   PWD = "Pyxy54dm1n2018",
                   Encoding = "latin-9")

# Escribimos las tablas
dbWriteTable(conex, 
             "Empleo_Tnal", 
             tnal, 
             overwrite = TRUE)

dbWriteTable(conex, 
             "Empleo_Areas", 
             area_fin, 
             overwrite = TRUE)

dbWriteTable(conex, 
             "Empleo_Ramas", 
             ocu_fin, 
             overwrite = TRUE)

dbWriteTable(conex, 
             "Empleo_Posicion", 
             posc_fin, 
             overwrite = TRUE)
  





