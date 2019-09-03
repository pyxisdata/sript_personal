# Limpieza datos de RIPS - Diabetes Mellitus

library(data.table)
library(readxl)
library(odbc)
library(lubridate)
library(stringr)
library(tidyr)

# Importar la data
folder <- "\\Users\\PC\\Desktop\\Armenia\\"
file <- paste(folder, "DM.xlsx", sep = "")

# Lista de pestañas
sheets <- excel_sheets(file)[1:6]

# Importar la data
import_data <- function(file, sheets) {
  
  list_data <- list()
  names_data <- c()
  
  for (i in seq_along(sheets)) {
    
    data <- read_xlsx(file, sheet = sheets[i], col_names = FALSE)
    
    data <- data.table(data)
    data <- data[-1:-4, ]
    
    header <- c("diagnostico", "procedimiento", "municipio", "entidad", "edad",
                "genero", "regimen", "personas", "atenciones", "costo_proc",
                "valor_consult")
    
    names(data) <- header
    name <- paste("m", substr(sheets[i], 5, 6), sep = "_")
    
    data[, date := ymd(paste("2018", substr(sheets[i], 5, 6), "01",
                             sep = "-"))
         ]
    
    list_data[[i]] <- data
    names_data[i] <- name
    
  }
  
  names(list_data) <- names_data
  assign("data_list", list_data, envir = .GlobalEnv) 
  
}

# Importar
import_data(file, sheets)

# Unir toda la data
diabetes <- rbindlist(data_list)

# Limpieza de los datos
# Rellenar columnas
diabetes[, diagnostico := diagnostico[1], by = cumsum(!is.na(diagnostico))
] [, procedimiento := procedimiento[1], by = cumsum(!is.na(procedimiento))
] [, municipio := municipio[1], by = cumsum(!is.na(municipio))
] [, entidad := entidad[1], by = cumsum(!is.na(entidad))
] [, genero := genero[1], by = cumsum(!is.na(genero))
] [, edad := edad[1], by = cumsum(!is.na(edad))
] [, c("diagnosID", "diagnos") := tstrsplit(diagnostico, "-", fixed = TRUE)
] [, c("procedID", "proced") := tstrsplit(procedimiento, "-", fixed = TRUE)
] [, c("munpioID", "munpio") := tstrsplit(municipio, "-", fixed = TRUE)
] [, c("entidadID", "entidad") := tstrsplit(entidad, "-", fixed = TRUE)
] [, c("regimenID", "regimen") := tstrsplit(regimen, "-", fixed = TRUE)
] [, genero := ifelse(genero == "FEMENINO", 1, 
                      ifelse(genero == "MASCULINO",
                             2,
                             0))
]

# Seleccionar las columnas indicadas
diabetes <- diabetes[, c(13, 15, 17, 19, 20, 5:6, 8:12)]

# Totales
totals <- diabetes[diagnosID == "Grand Total"]

# Eliminar totales
diabetes <- diabetes[diagnosID != "Grand Total"]

# Transformaciones de tipo de dato
diabetes[, c("diagnosID", "procedID", "entidadID", "munpioID", "rgimenID")
         := .(trimws(diagnosID, "both"), trimws(procedID, "both"), 
              trimws(entidadID, "both"), trimws(munpioID, "both"),
              trimws(regimenID, "both"))
] [, c("edad", "genero", "personas", "atenciones", "costo_proc",
       "valor_consult", "regimenID", "munpioID") 
   := .(as.numeric(edad), as.numeric(genero), as.numeric(personas),
        as.numeric(atenciones), as.numeric(costo_proc),
        as.numeric(valor_consult), as.numeric(regimenID), as.numeric(munpioID))
]

# Escribir como csv
dest_file <- paste(folder, "DM_T.csv", sep = "")
write.csv(diabetes, dest_file, row.names = FALSE, na = "")





