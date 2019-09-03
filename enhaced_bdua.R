# Base mejorada BDUA

library(data.table)
library(readxl)

# Archivo
file <- "\\Users\\PC\\Desktop\\Archivos Brutos\\Afiliados\\Enhanced1.xlsx"

# Lectura
data1 <- read_xlsx(file, sheet = 1, col_names = FALSE)
data2 <- read_xlsx(file, sheet = 2, col_names = FALSE)

# Conversion a data.table
data1 <- data.table(data1)
data2 <- data.table(data2)

# Refinacion data1
data1f <- data1[c(-1:-3, -5:-7)]
names(data1f) <- unlist(data1f[1, ])
data1f <- data1f[-1, ]
data1f <- melt(data1f, 
               variable.name = "variable",
               value.name = "valor",
               id.vars = c(1:5)
               )
data1f <- data1f[!is.na(valor)]

# Refinacion data2
data2f <- data2[c(-1:-3, -5:-7)]
names(data2f) <- unlist(data2f[1, ])
data2f <- data2f[-1, ]
data2f <- melt(data2f, 
               variable.name = "variable",
               value.name = "valor",
               id.vars = c(1:5)
)
data2f <- data2f[!is.na(valor)]

# Union de las bases
data <- rbindlist(list(data1f, data2f))

# Transformacion total
data[, c("edad", "munpioID", "valor") := 
       .(as.numeric(edad), as.numeric(munpioID), as.numeric(valor))
    ] [, c("regimenID", "generoID") := tstrsplit(variable, "", fixed = TRUE)
    ] [, c("regimenID", "generoID") := 
         .(as.numeric(regimenID), as.numeric(generoID))
    ] [, variable := NULL
    ]

# Escritura
write.csv(data, "\\Users\\PC\\Desktop\\bdua_febrero19.csv", row.names = FALSE)

