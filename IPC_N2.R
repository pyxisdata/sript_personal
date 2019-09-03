# Script de union de la inflacion

# Librerias
library(data.table)
library(readxl)
library(lubridate)
library(stringr)

# Carpeta
folder <- "\\Users\\PC\\Desktop\\Archivos Brutos\\IPC\\IPC_NAC"
folder_l <- list.files(folder, full.names = TRUE)

# Funcion de lectura
read_ipc <- function(x) {
  
  files <- list()
  nomen <- c()
  header <- c("anno", "mes", "ingreso", "nivel", "coicop", "ciudad", "indice")
  for (i in seq_along(x)) {
    data <- read_xls(x[i], sheet = 1, col_names = TRUE)
    data <- data.table(data)
    names(data) <- header
    nom <- word(x[i], 1, sep = ".xls") # Se toma solo el texto antes de xls
    nom <- word(nom, 2, sep = "/") # Se toma solo el texto depues de /
    nomen[i] <- nom
    files[[i]] <- data
  }
  
  names(files) <- nomen
  assign("ipc_list", files, envir = .GlobalEnv)
}

# Ejecucion de la lectura de los archivos
read_ipc(folder_l)

# Separacion de indices
sep_ipc <- function(x) {
  
  # Encontrar los indices respectivos a cada tipo de indice
  nt <- grep("total", names(x))
  nd <- grep("division", names(x))
  ng <- grep("grupo", names(x))
  nc <- grep("clase", names(x))
  ns <- grep("subclase", names(x))
  
  # Separar en sublistas y unificar en un solo datatable
  total <- rbindlist(x[nt])
  division <- rbindlist(x[nd])
  grupo <- rbindlist(x[ng])
  clase <- rbindlist(x[nc])
  subclase <- rbindlist(x[ns])
  
  # Se meten todos los datatable en una sola lista
  tables <- list(total, division, grupo, clase, subclase)
  
  final <- list()
  # Limpieza de los datatable
  for (i in seq_along(tables)) {
    
    meses <- setNames(c(1:12), 
                      c("Ene", "Feb", "Mar", "Abr", "May", "Jun",
                        "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"))
    ingresos <- setNames(c(1:5),
                         c("Pobres", "Vulnerables",
                           "Clase Media", "Ingresos Altos",
                           "Total Nivel de Ingresos"))
    geografia <- setNames(c(169, 5001, 8001, 11001, 13001, 15001, 17001, 18001,
                            19001, 20001, 23001, 41001, 44001, 47001, 50001,
                            52001, 54001, 63001, 66001, 68001, 70001, 73001,
                            76001, 100000),
                          c("NACIONAL", "MEDELLÍN", "BARRANQUILLA",
                            "BOGOTÁ, D.C.", "CARTAGENA DE INDIAS", "TUNJA",
                            "MANIZALES", "FLORENCIA", "POPAYÁN", "VALLEDUPAR",
                            "MONTERÍA", "NEIVA", "RIOHACHA", "SANTA MARTA",
                            "VILLAVICENCIO", "PASTO", "CÚCUTA", "ARMENIA",
                            "PEREIRA", "BUCARAMANGA", "SINCELEJO", "IBAGUÉ",
                            "CALI", "OTRAS AREAS URBANAS"))
    
    # Separar columna de coicop en nombre y codigo
    data <- tables[[i]]
    data[, c("codigo", "nombre") := tstrsplit(coicop, " - ", fixed = TRUE)]
    data[, ciudad := toupper(ciudad)] # Mayuscula
    
    # Reemplazar valores estilo BUSCARV
    data$mes[] <- meses[unlist(data$mes)]
    data$ingreso[] <- ingresos[unlist(data$ingreso)]
    data$ciudad[] <- geografia[unlist(data$ciudad)]
    data[, codigo := str_sub(codigo, 1, 7)]
    
    # Realizar la fecha
    data[, fecha := ymd(paste(anno, mes, 1, sep = "-"))]
    
    # Eliminar columnas innecesarias
    data[, c("nombre", "coicop", "nivel", "anno", "mes") := NULL]
    final[[i]] <- data
    
    # Conversiones de data finales
    data[, c("ingreso", "ciudad", "indice", "codigo") := 
           .(as.numeric(ingreso), as.numeric(ciudad),
             as.numeric(indice), as.character(codigo))]
  }
  
  names(final) <- c("Total", "Division", "Grupo", "Clase", "Subclase")
  assign("final_ipc", final, envir = .GlobalEnv)
}
    
# Eejecutar limpieza
sep_ipc(ipc_list)
    
    
    
  
  
  
  
  
  
  
  
  
  
  
}

  
  
