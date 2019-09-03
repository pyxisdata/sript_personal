# Condiciones mejoradas

# Librerias
library(data.table)
library(readxl)
library(optiRum)
library(dplyr)
library(lubridate)
library(tidyr)

# Archivo
file <- "\\Users\\PC\\Desktop\\Armenia\\Condicion_Ruta.xlsx"

# Lectura
edades <- read_xlsx(file, sheet = 1, col_names = TRUE)
condiciones <- read_xlsx(file, sheet = 2, col_names = TRUE)
bdua <- read_xlsx(file, sheet = 3, col_names = TRUE)

# Conversion
edades <- data.table(edades)
condiciones <- data.table(condiciones)
bdua <- data.table(bdua)

# Ampliacion de las edades hasta los periodos deseados EDADES
actual <- ymd("2019-01-01")
final <- ymd("2023-12-01")
fechas <- seq(actual, final, by = "month")
edad0 <- edades$annodec
pronostico <- data.frame(index = 1:length(edad0))

# Ampliacion de las edades hasta los periodos deseados BDUA
actual <- ymd("2019-04-01")
final <- ymd("2024-04-01")
fechas <- seq(actual, final, by = "month")
edad0 <- bdua$annodec
pronostico <- data.frame(index = 1:length(edad0))

# Para edades
for (i in seq_along(fechas)) {
    anno <- edades$anno * 12
    mes <- edades$mes
    edad <- anno + mes
    edadn = as.integer(round((edad + i) / 12, 2))
    edad2 = round(round((round((edad + i) / 12, 2) - edadn) * 12, 0) / 100, 2)
    edad3 = edadn + edad2
    pronostico[i] <- edad3
}

# Para bdua
for (i in seq_along(fechas)) {
    anno <- bdua$annodec * 12
    edad <- anno
    edadn = as.integer(round((edad + i) / 12, 2))
    edad2 = round(round((round((edad + i) / 12, 2) - edadn) * 12, 0) / 100, 2)
    edad3 = edadn + edad2
    pronostico[i] <- edad3
}


finaln <- ymd("2024-01-01")
nombres <- seq(actual, finaln, by = "month")
pronostico <- cbind(edad0, pronostico)
names(pronostico) <- nombres
edades <- cbind(edades, pronostico) # Edades
edades <- cbind(bdua, pronostico) # Bdua

# Tabla UPC EDADES
tablaupc <- edades[, c(-1:-3, -65)]
tablaupc %>%
    gather("fecha", "edad", -annodec) %>%
    mutate(r_1 = 1, 
           r_2 = 2,
           g_1 = 1,
           g_2 = 2) %>%
    gather("genero", "generoID", -1:-5) %>%
    select(-genero) %>%
    gather("regimen", "regimenID", -1:-3, -6) %>%
    select(-regimen) -> tablaupc
names(tablaupc) <- c("edad", "fecha", "edad_pronostico", "generoID", "regimenID")

# Tabla UPC BDUA
tablaupc <- edades[, c(1, 2, 14, 26, 38, 50, 62)]
tablaupc %>%
    gather("fecha", "edad", -1) -> tablaupc
names(tablaupc) <- c("edad", "fecha", "actual")
tablaupc$f <- 1
tablaupc$m <- 2
tablaupc %>%
    gather("genero", "generoID", -1:-3) -> tablaupc
tablaupc <- tablaupc[, -4]

write.csv(tablaupc,
          "\\Users\\PC\\Desktop\\tablaupc_bdua.csv",
          row.names = FALSE)

# Fusion (Producto Cartesiano)
dato <- CJ.dt(edades, condiciones)
datu <- CJ.dt(bdua, condiciones)

# Cruce Definivo
cruce_completo <- dato
cruce_completo <- datu
cruce_completo[, gfuturo := ifelse(annodec < maximo,
                                   1,
                                   0)
              ] [, gruta := ifelse(annodec >= minimo &
                                   annodec < maximo,
                                   1,
                                   0)
              ] [, ifuturo := ifelse(final > annodec,
                                     1,
                                     0)
              ] [, ginme := ifelse(gruta == 1 & ifuturo == 1,
                                   1,
                                   0)
              ] [, inme := ifelse(annodec >= inicial &
                                  annodec < final,
                                  1,
                                  0)
              ]
              
 
# Filtro por el mayor
cruce_completo <- cruce_completo[generoID == IDgenero, ]
cruce_completo <- cruce_completo[gfuturo == 1, ]
cruce_completo <- cruce_completo[ifuturo == 1, ]
cruce_completo <- cruce_completo[inme == 1, ]

# Parcial
cruce_final <- cruce_completo[, c(1, 2, 4, 7, 8, 9, 10, 13, 14, 15)]
cruce_final %>%
    mutate(periodos_a = (anno * 12) + mes,
           anno_inicial = as.integer(inicial),
           anno_final = as.integer(final),
           mes_inicial = round((inicial - anno_inicial) * 100, 0),
           mes_final = round((final - anno_final) * 100, 0),
           periodos_i = (anno_inicial * 12) + mes_inicial,
           periodos_f = (anno_final * 12) + mes_final,
           valor_causado = valoru * ((periodos_a - periodos_i) + 1),
           valor_disponible = valoru * ((periodos_f - periodos_a) - 1),
           costo_causado = costou * ((periodos_a - periodos_i) + 1),
           costo_disponible = costou * ((periodos_f - periodos_a) - 1),
           proporcion_causada = porcentaje * ((periodos_a - periodos_i) + 1),
           proporcion_disponible = porcentaje * ((periodos_f - periodos_a) - 1)
           ) %>%
    select(3, 6, 7, 18:23) -> cruce_final

write.csv(cruce_final,
          "\\Users\\PC\\Desktop\\cruce_estandar.csv",
          row.names = FALSE)


# Cruce fechas
cruce_fechas <- cruce_completo[, c(1, 4, 5)]
cruce_fechas <- cruce_completo[, c(4, 7, 8)]
cruce_fechas %>%
    mutate(act = today(),
           anact = year(today()),
           mact = month(today()),
           an = as.integer(annodec),
           m = round((annodec - an) * 100, 0),
           anin = as.integer(inicial),
           anfi = as.integer(final),
           min = round((inicial - anin) * 100, 0),
           mfi = round((final - anfi) * 100, 0),
           mi1 = min - m,
           mi2 = ifelse(mi1 < 0, (12 + mi1) + mact,
                        ifelse(mi1 > 0, mi1 + mact, mact)),
           mi3 = ifelse(mi2 > 12, mi2 - 12, mi2),
           ai1 = anin - an,
           ai2 = anact + ai1,
           ai3 = ifelse(mi1 < 0, ai2 - 1, ai2),
           ai4 = ifelse(mi2 > 12, ai3 + 1, ai3),
           fecha_inicial = ymd(paste(ai4, mi3, 1, sep = "-")), # Fecha Inicial
           mf1 = mfi - m,
           mf2 = ifelse(mf1 < 0, (12 + mf1) + mact,
                        ifelse(mf1 > 0, mf1 + mact, mact)),
           mf3 = ifelse(mf2 > 12, mf2 - 12, mf2),
           af1 = anfi - an,
           af2 = anact + af1,
           af3 = ifelse(mf1 < 0, af2 - 1, af2),
           af4 = ifelse(mf2 > 12, af3 + 1, af3),
           fecha_final = ymd(paste(af4, mf3, 1, sep = "-")), # Fecha Final
           periodos_t = (((anfi * 12) + mfi) - ((anin * 12) + min)),
           periodos_p = ((an * 12) + m) - ((anin * 12) + min),
           periodos_f = ((anfi * 12) + mfi) - ((an * 12) + m),
           periodos_p = ifelse(periodos_p < 0, 0, periodos_p),
           periodos_f = periodos_t - periodos_p
           ) -> cruce_fechas

cruce_fechas <- cruce_fechas[, c("fecha_inicial", "fecha_final",
                                 "periodos_t", "periodos_p", "periodos_f")]

cruce_final <- cbind(cruce_completo, cruce_fechas)
cruce_final <- cruce_final[, c("annodec", "generoID",
                               "jerarquiaID", "fecha_inicial", "fecha_final",
                               "periodos_t", "periodos_p", "periodos_f",
                               "ifuturo", "ginme", "inme",
                               "costo", "valor")]


# Valores upc

# -------------------------------------------------------------------------
write.csv(cruce_final,
          "\\Users\\PC\\Desktop\\cruce_pais.csv",
          row.names = FALSE)

# Clasificacion
cruce_seguimiento[, inmediato := ifelse(annodec >= inicial &
                                        annodec < final,
                                        1, 
                                        0)
                 ] [, general := ifelse(annodec >= minimo &
                                      annodec < maximo,
                                      1,
                                      0)
                ] [, genim := ifelse(final > annodec &
                                     general == 1,
                                     1, 
                                     0)
                ]

cruce_seguimiento <- cruce_seguimiento[general == 1, ]

# Generacion de filtros
data[, inmediato := ifelse(annodec >= inicial & 
                           annodec < final,
                           1,
                           0)
    ] [, general := ifelse(annodec >= minimo &
                           annodec < maximo,
                           1, 
                           0)
    ] [, genim := ifelse(final > annodec &
                         general == 1,
                         1, 
                         0)
    ] -> data

# Generacion de filtros BDUA
datu[, inmediato := ifelse(anno >= inicial & 
                           anno < final,
                           1,
                           0)
     ] [, general := ifelse(anno >= minimo &
                            anno < maximo,
                            1, 
                            0)
        ] [, genim := ifelse(final >= anno &
                             general == 1,
                             1, 
                             0)
           ] -> datu

# Columnas necesarias
data <- data[,c("annodec", "anno", "id_genero", "jerarquiaID","seccionID",
                "inmediato", "general", "genim", "inicial", "final")]

# Columnas necesarias BDUA
datu <- datu[,c("anno", "id_genero", "jerarquiaID","seccionID",
                "inmediato", "general", "genim", "inicial", "final")]

# Bases separadas
general <- data[general == 1, ]

# Bases separadas BDUA
general <- datu[general == 1, -c("general")]

# Actiidades
actividades <- general[, c("jerarquiaID", "id_genero", "inicial", "final")]

# Test Lorena
issac <- general[annodec == 1 & id_genero == 2]

test <- issac[annodec == 1 & inmediato == 1]
test <- issac[annodec == 1 & genim== 1]


# Escritura
write.csv(cruce_completo, "\\Users\\PC\\Desktop\\cruce_general.csv", row.names = FALSE)
write.csv(cruce_completo, "\\Users\\PC\\Desktop\\cruce_seguimiento.csv", row.names = FALSE)
write.csv(cruce_completo, "\\Users\\PC\\Desktop\\cruce_BDUA.csv", row.names = FALSE)
write.csv(edades, "\\Users\\PC\\Desktop\\edades.csv", row.names = FALSE)
