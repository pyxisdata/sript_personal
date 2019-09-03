# Script para generar las tablas
# Resolucion 3280

# Librerias
library(readxl)
library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(stringi)
library(lubridate)
library(odbc)

# Desactivar la notacion cientifica
options(scipen = 999)

# Archivo de las condiciones de 3380
condiciones <- paste0(getwd(), "/condiciones.csv") %>%
  read_csv(col_names = TRUE,
           col_types = cols(.default = "c"),
           locale = locale(encoding = "ISO-8859-1"))

# Tabla de upc
upc <- paste0(getwd(), "/upc.csv") %>%
  read_csv(col_names = TRUE,
           col_types = cols(.default = "c")) %>%
  mutate(valor = as.numeric(valor)) %>%
  mutate_at(vars(edad, generoID, regimenID), as.integer)

# Archivos de texto de los afiliados
archivos <- paste0(getwd(), "/Afiliados") %>%
  list.files(".TXT", full.names = TRUE)

# Importar los datos
importados <- archivos %>%
  map(read_csv, 
      col_names = FALSE,
      locale = locale(encoding = "ISO-8859-1"),
      col_types = cols(.default = "c"))

names(importados) <- c("contributivo", "subsidiado")

# Encabezados 
# Base de afiliados contributivo
names_contr <- c("idident", "idpersona", "apellido1", "apellido2",
                 "nombre1", "nombre2", "fechanac", "iddepto", "idmunpio",
                 "idafiliado", "idgenero", "idestado", "aa1", "aa2", 
                 "aa3", "aa4", "identidad", "vacio", "fechaaf", "fechaar")

# Base de afiliaados subsidiado
names_subs <- c("aa1", "identidad", "ident", "aa2", "idident",
                "idpersona", "apellido1", "apellido2", "nombre1", "nombre2",
                "fechanac", "idgenero", "aa4", "aa5", "aa6", "aa7", "aa8",
                "aa9", "iddepto", "idmunpio", "idzona", "fechasgss",
                "fechaaf", "aa10", "aa11", "aa12", "aa13", "idmodalidad",
                "idafiliado", "fecha", "fechaar")

# Asignar encabezados a las bases individuales
names(importados[["contributivo"]]) <- names_contr
names(importados[["subsidiado"]]) <- names_subs

# Limpiar memoria
rm(names_contr, names_subs)

# Añadir las columnas de clasificacion
importados[["contributivo"]][["idregimen"]] <- 1
importados[["subsidiado"]][["idregimen"]] <- 2

# Preparacion de las bases
# Union de las bases
afiliados <- importados %>%
  bind_rows() %>%
  select(idpersona, idident, apellido1, apellido2, nombre1, nombre2,
         fechanac, identidad, idgenero, idregimen, iddepto, idmunpio,
         idafiliado) %>%
  mutate(fechanac = dmy(fechanac),
         idmunpio = paste0(iddepto, idmunpio),
         idmunpio = case_when(idmunpio == "631" ~ "63001",
                              idmunpio == "630" ~ "63000",
                              TRUE ~ idmunpio),
         idgenero = case_when(idgenero == "F" ~ 1,
                              idgenero == "M" ~ 2),
         idafiliado = case_when(idafiliado == "DE" ~ 1,
                                idafiliado == "AC" ~ 2,
                                idafiliado == "RE" ~ 3,
                                idafiliado == "AF" ~ 4,
                                idafiliado == "SM" ~ 5,
                                idafiliado == "PL" ~ 6,
                                idafiliado == "SU" ~ 7,
                                idafiliado == "SD" ~ 8),
         idident = case_when(idident == "TI" ~ 1,
                             idident == "CC" ~ 2,
                             idident == "PA" ~ 3,
                             idident == "CE" ~ 4,
                             idident == "RC" ~ 5,
                             idident == "CN" ~ 6,
                             idident == "PE" ~ 7,
                             idident == "SC" ~ 8,
                             idident == "MS" ~ 9,
                             idident == "AS" ~ 10)) %>%
  mutate_at(vars(idmunpio, idident, idgenero, idregimen, idafiliado), 
            as.integer) %>%
  mutate_at(vars(apellido1, apellido2, nombre1, nombre2), 
            function(x) {
              x <- x %>%
                str_to_lower() %>%
                replace_na("") %>%
                str_trim() %>%
                stri_trans_general("Latin-ASCII")
            }) %>%
  mutate(nombre = paste(apellido1, apellido2, nombre1, nombre2),
         nombre = str_trim(nombre),
         nombre = str_squish(nombre)) %>%
  select(-iddepto, -apellido1, -apellido2, -nombre1, -nombre2) 

# Anonimizacion
afiliados <- afiliados %>%
  mutate(idpersona = str_replace_all(idpersona, "1", "A"),
         idpersona = str_replace_all(idpersona, "5", "C"),
         nombre = word(nombre, 1, sep = " "))

# Afiliados con la edad en la base
afiliados <- afiliados %>%
  filter(idafiliado == 2) %>%
  mutate(fechanac = ymd(fechanac),
         anno = 2019 - year(fechanac),
         mes = 3 - month(fechanac),
         edad = (anno * 12) + mes,
         edad = round(edad / 12, 2),
         anno = as.integer(edad),
         mes = round(edad - anno, 2),
         mes = (mes * 12) / 100,
         edad = round(anno + mes, 2)) %>%
  select(-anno, -mes)

# Tabla de afiliados agrupada
afiliados_grupo <- afiliados %>%
  group_by(edad, idgenero, idregimen, idmunpio, identidad) %>%
  summarise(personas = n())

# Escrbir a csv
write_csv(afiliados,
          paste0(getwd(), "/afiliados_0319.csv"),
          na = "")

# Limpiar memoria
rm(importados)

# Vector con los años
annos <- rep(c(0:120), each = 12)

# Vector con los meses
meses <- rep(c(0:11), 121)

# Tabla de edades con años y meses
edades <- tibble(annos, meses) %>%
  mutate(mesesdec = meses / 100,
         edad = annos + mesesdec)

# Aladir a cadad edad las atenciones que debe hacer
atenciones <- condiciones %>%
  select(jerarquiaID, inicial, final, generoID) %>%
  mutate(union = 1,
         inicial = as.numeric(inicial),
         final = as.numeric(final)) %>%
  full_join(mutate(edades, union = 1), by = "union") %>%
  filter(edad >= inicial & edad < final) %>%
  select(jerarquiaID, generoID, edad, inicial, final) %>%
  rename(idjerarquia = jerarquiaID,
         idgenero = generoID) %>%
  mutate_at(vars(idjerarquia, idgenero), as.integer)

# Añadir fechas a atenciones
atenciones <- atenciones %>%
  mutate(ai = as.integer(inicial),
         af = as.integer(final),
         mi = round(inicial - ai, 2),
         mf = round(final - af, 2),
         mi = mi * 100,
         mi = as.integer(mi),
         mf = mf * 100,
         mf = as.integer(mf),
         ai = ai * 12,
         ai = as.integer(ai),
         af = af * 12,
         af = as.integer(af),
         pi = ai + mi,
         pf = af + mf,
         ae = as.integer(edad),
         me = round(edad - ae, 2),
         ae = ae * 12,
         ae = as.integer(ae),
         me = me * 100,
         me = as.integer(me),
         pe = ae + me) %>%
  mutate(fecha = ymd("2019-03-01"),
         di = pe - pi,
         df = pf - pe,
         di = as.integer(di),
         df = as.integer(df),
         fecha_in = fecha %m-% months(di),
         fecha_fi = fecha %m+% months(df),
         fecha_fi = fecha_fi %m-% months(1)) %>%
  select(idjerarquia, idgenero, edad, fecha_in, fecha_fi)

# Aladir periodos y proporciones
atenciones <- atenciones %>%
  group_by(idjerarquia) %>%
  mutate(periodo = row_number(),
         total = max(periodo),
         periodo2 = total - periodo,
         periodo2 = periodo2 + 1,
         porcentaje = periodo / total,
         proporcion = 1 / periodo2) %>%
  ungroup() %>%
  select(-periodo, -total, -periodo2)
  
# Pronostico de fechas
# Funcion para realizar pronostico de edades
pronostico <- function(datos, hoy, inicio, fin) {
  
  # Partes de la fecha actual
  hoy <- ymd(hoy)
  inicio <- ymd(inicio)
  fin <- ymd(fin)
  
  # Periodos a pronosticar
  atras <- length(seq(inicio, hoy, by = "month")) - 1
  adelante <- length(seq(hoy, fin, by = "month")) - 1
  
  # Dataframe vacio atras
  lista_atras <- data.frame(anterior = 1:nrow(datos))
  
  # Periodos hacia atras
  for (i in 1:atras) {
    actual <- (datos$annos * 12) + datos$meses
    actual <- actual - i
    actual <- round(actual / 12, 2)
    entero <- as.integer(actual)
    decimal <- (actual - entero) * 12
    actual <- round(entero + (decimal / 100), 2)
    lista_atras[[i + 1]] <- actual
  }
  
  # Reversa de columnas
  lista_atras <- rev(lista_atras)
  
  # Dataframe vacio adelante
  lista_adelante <- data.frame(posterior = 1:nrow(datos))
  
  # Periodos hacia adelante
  for (i in 1:adelante) {
    actual <- (datos$annos * 12) + datos$meses
    actual <- actual + i
    actual <- round(actual / 12, 2)
    entero <- as.integer(actual)
    decimal <- (actual - entero) * 12
    actual <- round(entero + (decimal / 100), 2)
    lista_adelante[[i + 1]] <- actual
  }
  
  # Limpieza
  datos <- datos %>% select(edad)
  lista_atras <- lista_atras %>% select(-anterior)
  lista_adelante <- lista_adelante %>% select(-posterior)
  
  # Unir con la base actual
  tabla <- cbind(datos, lista_atras, datos, lista_adelante)
  # Lista de fechas para encabezados
  fechas <- seq(inicio, fin, by = "month")
  names(tabla) <- c("actual", as.character(fechas))
  # Negativos como NA
  tabla[tabla < 0] <- NA
  # Unpivot
  tabla <- tabla %>%
    gather("fecha", "edad", -actual) %>%
    mutate(fecha = ymd(fecha),
           edad = as.numeric(edad),
           actual = as.numeric(actual)) %>%
    as_tibble() %>%
    rename(base = actual) %>%
    mutate(edad = replace_na(edad, 0))
  
}

# Ejecutar
periodos <- pronostico(edades, 
                       hoy = "2019-03-01",
                       inicio = "2019-01-01",
                       fin = "2023-12-01")

# Corregir posibles errores de decimales
periodos <- periodos %>%
  mutate(edad = round(edad, 2))

# Corregir posibles errores de decimales
atenciones <- atenciones %>%
  mutate(edad = round(edad, 2))

# Limpiar memoria
rm(pronostico, meses, annos)

# Funcion para dividir el tamaño de la tabla resultado
division <- function(inicio, fin) {
  
  corte <- periodos %>%
    filter(fecha >= ymd(inicio) & fecha <= ymd(fin))
    
  
  tabla <- atenciones %>%
    left_join(corte, by = "edad") %>%
    filter(!is.na(idjerarquia))
}

# Ejecutar por periodos
union1 <- division("2019-01-01", "2019-12-01")
union2 <- division("2020-01-01", "2020-12-01")
union3 <- division("2021-01-01", "2021-12-01")
union4 <- division("2022-01-01", "2022-12-01")
union5 <- division("2023-01-01", "2023-12-01")

# Unificar todas las tablas en una sola
union <- list(union1, union2, union3, union4, union5) %>%
  bind_rows()

# Union
prueba <- union %>%
  group_by(base, idjerarquia) %>%
  mutate(condicion = ifelse(ymd("2019-03-01") %in% fecha,
                            1,0),
         fecha_inicial = ifelse(condicion == 0,
                                first(fecha_in, order_by = fecha),
                                case_when(fecha == ymd("2019-03-01") 
                                          ~ fecha_in)),
         fecha_final = ifelse(condicion == 0,
                              first(fecha_fi, order_by = fecha),
                              case_when(fecha == ymd("2019-03-01") 
                                        ~ fecha_fi))) %>%
  ungroup() %>%
  mutate(idjerarquia = as.integer(idjerarquia),
         idgenero = as.integer(idgenero))

# Prueba agrupado
prueba2 <- prueba %>%
  group_by(base, idjerarquia) %>%
  summarise(fecha_inicial = min(fecha_inicial, na.rm = TRUE),
            fecha_final = min(fecha_final, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(fecha_inicial = as_date(fecha_inicial),
         fecha_final = as_date(fecha_final))

# Añadir las fechas
prueba3 <- prueba %>%
  select(-condicion, -fecha_in, -fecha_fi,
         -fecha_inicial, -fecha_final) %>%
  left_join(prueba2, by = c("base", "idjerarquia"))
  
# Limpiar memoria
rm(union1, union2, union3, union4, union5)

# Escrbir a csv la tabla de union
write_csv(prueba,
          paste0(getwd(), "/atenciones_3280.csv"),
          na = "")  

# Tabla de upc
upc_valores <- periodos %>%
  mutate(anno = as.integer(edad)) %>%
  left_join(upc, by = c("anno" = "edad")) %>%
  rename(idgenero = generoID,
         idregimen = regimenID)

# Escrbir a csv la tabla de upc_valor
write_csv(upc_valores,
          paste0(getwd(), "/upc_valor.csv"),
          na = "")  



