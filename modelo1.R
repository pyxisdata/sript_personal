# Script para el modelo de evaluacion de EMA
# Librerias
# Conexion a google drive
library(googledrive)
library(googlesheets)
library(readr)
# Procesamiento de los datos
library(lubridate)
library(dplyr)
library(tidyr)
library(stringr)
library(stringi)
library(purrr)

# Desactivar la notacion cientifica
options(scipen = 999)

# Conectarse a la hoja de calculo de google
hoja_calculo <- gs_key("1iKJj0l3Gix8Zcmg6-2M0CYS4m_ZEJclbxd4F38QU7cg")

# Importar los datos de la hoja de calculo de registros
encuesta <- hoja_calculo %>%
  gs_read(ws = "Hoja 1", col_types = cols(.default = "c")) %>%
  mutate(NIT = c("888888888", "890110964", "860529657",
                 "999999999", "860032115"))

# Importar encabezados de la tabla de la encuesta
param_encuesta <- paste0(getwd(), "/nombres_encuesta.csv") %>%
  read_csv(col_names = TRUE,
           col_types = cols(.default = "c"),
           locale = locale(encoding = "ISO-8859-1"))

# Nombres de la encuesta
nombres_encuesta <- param_encuesta %>%
  select(campof) %>%
  unlist()

names(encuesta) <- nombres_encuesta

# Limpiar memoria y espacio de variables
rm(nombres_encuesta)

# Directorio de empresas 
# Codigo CIIU
directorio <- paste0(getwd(), "/Limpio") %>%
  list.files(pattern = "dir", full.names = TRUE) %>%
  map(read_csv, col_types = cols(.default = "c")) %>%
  # Asignar nombres
  map2(list(c("nombre", "nit", "munpio", "codclase"),
            c("nit", "nombre", "codclase", "depto", "munpio"),
            c("nit", "nombre", "codclase", "depto", "munpio"),
            c("nombre", "nit", "munpio", "codclase")),
       function(x, y) {
         names(x) <- y 
         x
       }) %>%
  bind_rows()

# Importar informacion de la empresa (codigo CIIU)
informacion <- hoja_calculo %>%
  gs_read(ws = "registros", col_types = cols(.default = "c")) %>%
  filter(Tipo == 4) %>%
  select(NIT, Empresa, CIIU, Municipio, Departamento, Tamano) %>%
  left_join(distinct(select(encuesta, nit, reporta)),
            by = c("NIT" = "nit")) %>%
  left_join(directorio, by = c("NIT" = "nit")) %>%
  mutate(directorio = ifelse(is.na(nombre), "NO", "SI")) %>%
  select(-munpio, -codclase, -depto, -nombre)

names(informacion) <- c("nit", "empresa", "codclase", "municipio",
                        "departamento", "tamano", "reporta", "directorio")

# Importar clasificacion CIIU 4
ciiu4 <- paste0(getwd(), "/clasificacion_ciiu4.csv") %>%
  read_csv(col_names = TRUE, col_types = cols(.default = "c"),
           locale = locale(encoding = "ISO-8859-1"))

# Resultados cualitativos -----------------------------------------------------
# Datos cualitativos a utilizar para el modelo
cualitativos <- encuesta %>%
  # Proceso
  select(nit, ends_with("1")) %>%
  select(-direccion1, -contains("AN")) %>%
  mutate_at(vars(-nit), function(x) {
    x <- str_match(x, "\\d+")[, 1]
    }) %>%
  gather("parte", "puntaje_emp", -nit) %>%
  mutate(puntaje_emp = as.integer(puntaje_emp)) %>%
  # Asignar el codigo CIIU
  left_join(select(informacion, nit, tamano, codclase), by = "nit") %>%
  left_join(ciiu4, by = c("codclase")) %>%
  select(-codea, -nomea, -codgea, -nomgea, -coddea, -nomdea, 
         -codsea, -nomsea) %>%
  select(nit, tamano, parte, puntaje_emp, codclase, everything())

# Puntaje promedio de las preguntas cualitativas a nivel sector
# Funcion para realizar los calculos a los cuatro niveles
calificar_cualitativo <- function(data, columna, ciiu) {
  
  # Identificador de las columnas de nivel ciiu
  codigo <- paste0("cod", columna)
  nombre <- paste0("nom", columna)
  
  # Medidas del sector
  # Agrupar por el nivel ciiu
  nivel <- data %>%
    group_by(parte, !!as.name(codigo)) %>%
    summarise(puntaje_sec = mean(puntaje_emp),
              num_empresas = n()) %>%
    mutate(puntaje_sec = round(puntaje_sec, 2)) %>%
    ungroup()
  
  # Puntaje
  puntaje <- data %>%
    select(nit, parte, puntaje_emp,
           !!as.name(codigo), !!as.name(nombre)) %>%
    left_join(nivel, by = c(codigo, "parte")) %>%
    mutate(diferencia = puntaje_emp - puntaje_sec,
           diferencia = round(diferencia, 2)) %>%
    group_by(parte) %>%
    mutate(posicion = dense_rank(-diferencia)) %>%
    arrange(posicion) %>%
    ungroup()
  
}

# Ejecutar funcion
posiciones_cualitativo <- list(cualitativos, cualitativos,
                             cualitativos, cualitativos) %>%
  map2(c("clase", "grupo", "division", "seccion"),
       calificar_cualitativo, ciiu = ciiu4)

names(posiciones_cualitativo) <- c("clase", "grupo", "division", "seccion")

# Remover tablas intermedias
rm(cualitativos, calificar_cualitativo)

# Resultados cuantitativos ----------------------------------------------------
# Datos cuantitativos procedentes de la encuesta
cuantitativos <- encuesta %>%
  # Proceso
  select(nit, reporta, contains("201")) %>%
  mutate_at(vars(-nit, -reporta), function(x) {
    x <- str_replace(x, "[^\\d+]", "") %>%
      str_replace_all("\\.", "")
  }) %>%
  gather("parte", "valor", -nit, -reporta) %>%
  separate(parte, c("parte", "fecha"), -4) %>%
  mutate(fecha = paste(fecha, "01", "01", sep = "-"),
         fecha = ymd(fecha)) %>%
  spread(parte, valor) %>%
  left_join(select(informacion, nit, codclase, tamano, directorio),
            by = "nit") %>%
  select(fecha, nit, reporta, directorio, tamano, codclase, everything())

# Estados financieros sector
# Base empresarial
estados_financieros <- paste0(getwd(), "/Limpio") %>%
  list.files(pattern = "ef", full.names = TRUE) %>%
  map(read_csv, col_types = cols(.default = "c")) %>%
  bind_rows() %>%
  mutate(fecha = ymd(fecha),
         tamano = as.integer(tamano)) %>%
  mutate_at(vars(-fecha, -nit, -tamano), as.numeric) %>%
  group_by(nit) %>%
  # Aplicar el ultimo tamano de la empresa
  mutate(tamano = last(tamano, order_by = fecha)) %>%
  rename(activo = `1`,
         patrimonio = `3`,
         ingresos = `41`,
         utilidad = `33`,
         impuesto = `54`) %>%
  select(fecha, nit, tamano, everything()) %>%
  left_join(select(directorio, nit, codclase), by = "nit") %>%
  select(fecha, nit, tamano, codclase, everything()) %>%
  ungroup()

# Estados financieros de las empresas de la encuesta
# Quienes reportan utilizan la base empresarial y quienes no la encuesta
estados_empresa <- cuantitativos %>%
  select(fecha, nit, codclase, tamano, reporta, directorio,
         activo, patrimonio, ingresos, utilidad, impuesto) %>%
  mutate_at(vars(activo, patrimonio, ingresos, utilidad, impuesto), 
            as.numeric) %>%
  mutate(tamano = as.integer(tamano)) %>%
  # Anexion de la base empresarial
  left_join(estados_financieros, by = c("nit", "fecha")) %>%
  mutate(tamano = tamano.x,
         # Reemplazos
         activo = ifelse(reporta == "SI", activo.y, activo.x),
         patrimonio = ifelse(reporta == "SI", patrimonio.y, activo.x),
         ingresos = ifelse(reporta == "SI", ingresos.y, activo.x),
         utilidad = ifelse(reporta == "SI", utilidad.y, activo.x),
         impuesto = ifelse(reporta == "SI", impuesto.y, impuesto.x)) %>%
  select(fecha, nit, codclase.x, tamano.x, activo, patrimonio,
         ingresos, utilidad, impuesto) %>%
  rename(codclase = codclase.x,
         tamano = tamano.x) %>%
  left_join(ciiu4, by = "codclase") %>%
  select(-codea, -nomea, -codgea, -nomgea, -coddea, -nomdea, 
         -codsea, -nomsea)

# Añadir estados financieros a la base de datos empresarial
# Reemplazar por la encuesta
estados_financieros <- estados_financieros %>%
  left_join(ciiu4, by = "codclase") %>%
  select(-codea, -nomea, -codgea, -nomgea, -coddea, -nomdea, 
         -codsea, -nomsea) %>%
  filter(!nit %in% informacion[["nit"]]) %>%
  bind_rows(estados_empresa)
  
# Funcion para calcular el puntaje
# Estados financieros agrupados por sector y tamaño
# Funcion para realizar los calculos a los cuatro niveles
calificar_estados <- function(base, columna, data, ciiu) {
  
  # Identificador de loas columnas de nivel ciiu
  codigo <- paste0("cod", columna)
  nombre <- paste0("nom", columna)
  
  # Calcular las medidas de las empresas
  empresas <- data %>%
    select(-impuesto) %>%
    rename(activo_emp = activo,
           patrimonio_emp = patrimonio,
           ingresos_emp = ingresos,
           utilidad_emp = utilidad) %>%
    # Calculo de las medidas
    mutate(roa_emp = round(utilidad_emp / activo_emp, 2),
           roe_emp = round(utilidad_emp / patrimonio_emp, 2),
           margen_emp = round(utilidad_emp / ingresos_emp, 2)) %>%
    group_by(nit) %>%
    mutate(ingresos_emp_1 = lag(ingresos_emp, n = 1, order_by = fecha)) %>%
    ungroup() %>%
    mutate(varing_emp = ((ingresos_emp / ingresos_emp_1) - 1),
           varing_emp = round(varing_emp * 100, 2)) %>%
    group_by(nit, !!as.name(codigo), !!as.name(nombre), tamano) %>%
    # Calculo de los promedios entre periodos
    summarise(roa_emp_p = round(mean(roa_emp), 2),
              roe_emp_p = round(mean(roe_emp), 2),
              margen_emp_p = round(mean(margen_emp), 2),
              varing_emp_p = round(mean(varing_emp, na.rm = TRUE),
                                       2)) %>%
    ungroup() %>%
    gather("parte", "valor_emp", -nit, -!!as.name(codigo), -!!as.name(nombre),
           -tamano) %>%
    mutate(parte = word(parte, 1, sep = "_"))
  
  # Calcular las medidas sectoriales
  # Agrupar segun el nivel ciiu
  nivel <-  base %>%
    select(-impuesto) %>%
    group_by(fecha, !!as.name(codigo), tamano) %>%
    summarise(activo_sec = sum(activo, na.rm = TRUE),
              patrimonio_sec = sum(patrimonio, na.rm = TRUE),
              ingresos_sec = sum(ingresos, na.rm = TRUE),
              utilidad_sec = sum(utilidad, na.rm = TRUE),
              num_empresas = n_distinct(nit)) %>%
    ungroup() %>%
    mutate(roa_sec = round(utilidad_sec / activo_sec, 2),
           roe_sec = round(utilidad_sec / patrimonio_sec, 2),
           margen_sec = round(utilidad_sec / ingresos_sec, 2)) %>%
    group_by(!!as.name(codigo), tamano) %>%
    mutate(ingresos_sec_1 = lag(ingresos_sec, n = 1, order_by = fecha)) %>%
    ungroup() %>%
    mutate(varing_sec = ((ingresos_sec / ingresos_sec_1) - 1),
           varing_sec = round(varing_sec * 100, 2)) %>%
    group_by(!!as.name(codigo), tamano, num_empresas) %>%
    summarise(roa_sec_p = round(mean(roa_sec), 2),
              roe_sec_p = round(mean(roe_sec), 2),
              margen_sec_p = round(mean(margen_sec), 2),
              varing_sec_p = round(mean(varing_sec, na.rm = TRUE), 2)) %>%
    ungroup() %>%
    gather("parte", "valor_sec", -!!as.name(codigo),
           -tamano, -num_empresas) %>%
    mutate(parte = word(parte, 1, sep = "_"))
  
  # Puntaje
  puntaje <- empresas %>%
    left_join(nivel, by = c(codigo, "tamano", "parte")) %>%
    mutate(valor_emp = round(valor_emp, 2),
           valor_sec = round(valor_sec, 2),
           diferencia = round(valor_emp - valor_sec, 2)) %>%
    group_by(parte) %>%
    mutate(posicion = dense_rank(-diferencia)) %>%
    arrange(posicion) %>%
    select(nit, tamano, parte, !!as.name(codigo), !!as.name(nombre),
           num_empresas, valor_emp, valor_sec, diferencia, posicion)
  
}

# Ejecutar funcion estados financieros
posiciones_estados <- list(estados_financieros, estados_financieros,
                           estados_financieros, estados_financieros) %>%
  map2(c("clase", "grupo", "division", "seccion"),
       calificar_estados, data = estados_empresa, ciiu = ciiu4)

names(posiciones_estados) <- c("clase", "grupo", "division", "seccion")

# Productividad
# Datos de productividad de la encuesta
productividad <- cuantitativos %>%
  select(fecha, nit, codclase, 
         perper, pertemp, presper, suelper, gastemp) %>%
  mutate_at(vars(perper, pertemp, presper, suelper, gastemp), 
            as.numeric) %>%
  left_join(select(ciiu4, codclase, codea), by = "codclase") %>%
  filter(fecha != ymd("2018-01-01")) %>%
  left_join(select(estados_empresa, fecha, nit, ingresos), 
            by = c("fecha", "nit")) %>%
  rename(valor = ingresos)

# Filtrar empresas que no tienen codigos de encuesta anual
productividad_adjunto <- productividad %>%
  filter(is.na(codea)) %>%
  mutate(codea = codclase) %>%
  select(-codclase) %>%
  group_by(fecha, codea) %>%
  summarise_at(vars(-nit), sum, na.rm = TRUE) %>%
  left_join(select(ciiu4, codclase, nomclase, codgrupo, nomgrupo,
                   coddivision, nomdivision, codseccion, nomseccion),
            by = c("codea" = "codclase")) %>%
  rename(nomea = nomclase,
         codgea = codgrupo,
         nomgea = nomgrupo,
         coddea = coddivision,
         nomdea = nomdivision,
         codsea = codseccion,
         nomsea = nomseccion)
  
# Importar datos de las encuestas anuales
encuestas_anuales <- paste0(getwd(), "/Encuestas Anuales") %>%
  list.files(full.names = TRUE) %>%
  map(read_csv, col_types = cols(.default = "c"), col_names = TRUE) %>%
  map2(list(c("fecha", "codea", "perper", "pertemp", "gastemp",
              "suelper", "presper", "valor", "venta"),
            c("fecha", "codea", "valor", "suelper", "presper", 
              "perper", "pertemp", "gastemp"),
            c("fecha", "codea", "valor", "suelper", "sueltem", 
              "presper", "prestem", "perper", "pertemp")),
       function (x, y) {
         names(x) <- y
         x
        })

names(encuestas_anuales) <- c("eac", "eam", "eas")

# Proceso de union de columnas
encuestas_anuales <- encuestas_anuales %>%
  map(mutate_at, vars(-fecha, -codea), as.numeric) %>%
  map_at(vars(1), select, -venta) %>%
  map_at(vars(3), mutate, gastemp = sueltem + prestem) %>%
  map_at(vars(3), select, -sueltem, -prestem) %>%
  bind_rows() %>%
  mutate(fecha = ymd(fecha)) %>%
  # Añador la jerarquia de las encuestas anuales bajo CIIU
  left_join(distinct(select(ciiu4, codea, nomea)), by = "codea") %>%
  left_join(distinct(select(ciiu4, codea, codgea, nomgea)),
            by = "codea") %>%
  left_join(distinct(select(ciiu4, codea, coddea, nomdea)),
            by = "codea") %>%
  left_join(distinct(select(ciiu4, codea, codsea, nomsea)),
            by = "codea") %>%
  # Añadir los sectores ajenos a las encuestas
  bind_rows(productividad_adjunto)

# Remover limpiar memoria
rm(productividad_adjunto)

# Funcion para calcular el puntaje
# Medidas de productividad
# Agrupar por distintos niveles de CIIU 
calificar_productividad <- function(base, columna, data, ciiu) {
  
  # Identificador de las columnas de nivel ciiu
  codigo <- paste0("cod", columna)
  nombre <- paste0("nom", columna)
  
  # Medidas de las empresas
  empresas <- data %>%
    mutate(codea = ifelse(is.na(codea), codclase, codea)) %>%
    select(-codclase) %>%
    select(fecha, nit, codea, everything()) %>%
    rename(perper_emp = perper,
           pertemp_emp = pertemp,
           presper_emp = presper,
           suelper_emp = suelper,
           gastemp_emp = gastemp,
           valor_emp = valor) %>%
    # Calcular las medidas
    mutate(salprom_emp = suelper_emp / perper_emp,
           propor_emp = pertemp_emp / (perper_emp + pertemp_emp),
           product_emp = valor_emp / (perper_emp + pertemp_emp)) %>%
    # Formato de las medidas
    mutate(salprom_emp = round(salprom_emp, 2),
           propor_emp = round(propor_emp * 100, 2),
           product_emp = round(product_emp, 2)) %>%
    # Calculo de los promedios entre periodos
    group_by(nit, codea) %>%
    summarise(salprom_emp_p = mean(salprom_emp, na.rm = TRUE),
              propor_emp_p = mean(propor_emp, na.rm = TRUE),
              product_emp_p = mean(product_emp, na.rm = TRUE)) %>%
    gather("parte", "valor_emp", -nit, -codea) %>%
    mutate(parte = word(parte, 1, sep = "_")) %>%
    # Añadir el sector de las encuestas anuales
    left_join(ciiu4, by = c("codea")) %>%
    select(nit, codea, parte, valor_emp, nomea, codgea, nomgea,
           coddea, nomdea, codsea, nomsea) %>%
    left_join(ciiu4, by = c("codea" = "codclase")) %>%
    mutate(nomea = ifelse(is.na(nomea.x), nomclase, nomea.x),
           codgea = ifelse(is.na(codgea.x), codgrupo, codgea.x),
           nomgea = ifelse(is.na(nomgea.x), nomgrupo, nomgea.x),
           coddea = ifelse(is.na(coddea.x), coddivision, coddea.x),
           nomdea = ifelse(is.na(nomdea.x), nomdivision, nomdea.x),
           codsea = ifelse(is.na(codsea.x), codseccion, codsea.x),
           nomsea = ifelse(is.na(nomsea.x), nomseccion, nomsea.x)) %>%
    select(nit, parte, valor_emp, codea, nomea, codgea, nomgea,
           coddea, nomdea, codsea, nomsea)
  
  # Medidas de los sectores
  nivel <- encuestas_anuales %>%
    # Agrupar por el nivel ciiu
    group_by(fecha, !!as.name(codigo)) %>%
    summarise(perper_sec = sum(perper),
              pertemp_sec = sum(pertemp),
              gastemp_sec = sum(gastemp),
              suelper_sec = sum(suelper),
              presper_sec = sum(presper),
              valor_sec = sum(valor)) %>%
    ungroup() %>%
    # Calculo de las medidas
    mutate(salprom_sec = suelper_sec / perper_sec,
           propor_sec = pertemp_sec / (perper_sec + pertemp_sec),
           product_sec = valor_sec / (perper_sec + pertemp_sec)) %>%
    # Formato de las medidas
    mutate(salprom_sec = round(salprom_sec, 2),
           propor_sec = round(propor_sec * 100, 2),
           product_sec = round(product_sec, 2)) %>%
    group_by(!!as.name(codigo)) %>%
    summarise(salprom_sec_p = mean(salprom_sec),
              propor_sec_p = mean(propor_sec),
              product_sec_p = mean(product_sec)) %>%
    ungroup() %>%
    # Formato largo
    gather("parte", "valor_sec", -!!as.name(codigo)) %>%
    mutate(parte = word(parte, 1, sep = "_"))
  
  # Puntaje
  puntaje <- empresas %>%
    select(nit, parte, !!as.name(codigo), !!as.name(nombre), valor_emp) %>%
    left_join(nivel, by = c(codigo, "parte")) %>%
    ungroup() %>%
    mutate(valor_emp = round(valor_emp, 2),
           valor_sec = round(valor_sec, 2),
           diferencia = round(valor_emp - valor_sec, 2)) %>%
    group_by(parte) %>%
    mutate(posicion = dense_rank(-diferencia)) %>%
    ungroup() %>%
    arrange(posicion)
}

# Ejecutar funcion estados financieros
posiciones_productividad <- list(encuestas_anuales, encuestas_anuales,
                                 encuestas_anuales, encuestas_anuales) %>%
  map2(c("ea", "gea", "dea", "sea"),
       calificar_productividad,
       data = productividad,
       ciiu = ciiu4)

names(posiciones_productividad) <- c("clase", "grupo", "division", "seccion")

# Relacion Sector Publico
# Parte de Impuestos
impuestos <- cuantitativos %>%
  select(fecha, nit, codclase, tamano, iva, exenciones,
         licitaciones, subsidios, subvenciones, credito) %>%
  mutate_at(vars(-fecha, -nit, -codclase, -tamano), 
            as.numeric) %>%
  mutate(tamano = as.integer(tamano)) %>%
  left_join(select(estados_empresa, -activo, -patrimonio),
            by = c("nit", "fecha", "codclase", "tamano"))

# Estados financieros de impuestos
estados_impuesto <- estados_financieros %>%
  select(-activo, -patrimonio, -ingresos)


# Funcion para calcular el puntaje
# Medidas de impuestos
# Agrupar por distintos niveles de CIIU 
calificar_impuestos <- function(base, columna, data, ciiu) {

  # Identificador de las columnas de nivel ciiu
  codigo <- paste0("cod", columna)
  nombre <- paste0("nom", columna)
  
  empresas <- data %>%
    select(fecha, nit, !!as.name(codigo), !!as.name(nombre),
           tamano, ingresos, utilidad, impuesto,
           iva, exenciones, licitaciones, subsidios, 
           subvenciones, credito) %>%
    # Calcular las medidas
    mutate(margenimp = impuesto / utilidad,
           margeniva = iva / ingresos,
           margenexe = exenciones / ingresos,
           margenlic = licitaciones / ingresos,
           margensub = subsidios / ingresos,
           margensuv = subvenciones / ingresos,
           margencre = credito / ingresos) %>%
    # Multiuplicar porcentajes por 100
    mutate_at(vars(margenimp, margeniva, margenexe, margenlic,
                   margensub, margensuv, margencre),
              function(x) {x <- round(x * 100, 2)}) %>%
    group_by(nit, !!as.name(codigo), !!as.name(nombre), tamano) %>%
    # Calcular los promedios de los periodos
    summarise_at(vars(margenimp, margeniva, margenexe, margenlic,
                      margensub, margensuv, margencre),
                 function(x) {x <- round(mean(x, na.rm = TRUE), 2)}) %>%
    ungroup() %>%
    gather("parte", "valor_emp", -nit, -!!as.name(codigo),
           -!!as.name(nombre), -tamano)
  
  
  # Medidas de los sectores con los datos de la encuesta
  nivel1 <- data %>%
    select(-utilidad, -impuesto) %>%
    group_by(fecha, !!as.name(codigo), tamano) %>%
    summarise_at(vars(ingresos, iva, exenciones, licitaciones,
                      subsidios, subvenciones, credito),
                 function(x) {x <- sum(x, na.rm = TRUE)}) %>%
    ungroup() %>%
    # Calculo de las medidas 
    mutate(margeniva = iva / ingresos,
           margenexe = exenciones / ingresos,
           margenlic = licitaciones / ingresos,
           margensub = subsidios / ingresos,
           margensuv = subvenciones / ingresos,
           margencre = credito / ingresos) %>%
    # Multiuplicar porcentajes por 100
    mutate_at(vars(margeniva, margenexe, margenlic, margensub,
                   margensuv, margencre),
              function(x) {x <- round(x * 100, 2)}) %>%
    group_by(!!as.name(codigo), tamano) %>%
    # Calcular los promedios de los periodos
    summarise_at(vars(margeniva, margenexe, margenlic,
                      margensub, margensuv, margencre),
                 function(x) {x <- round(mean(x, na.rm = TRUE), 2)}) %>%
    ungroup() %>%
    gather("parte", "valor_sec", -!!as.name(codigo), -tamano)
  
  # Medicas calculadas del sector base empresarial
  nivel <- base %>%
    group_by(fecha, !!as.name(codigo), tamano) %>%
    summarise_at(vars(utilidad, impuesto),
                 function(x) {x <- sum(x, na.ram = TRUE)}) %>%
    ungroup() %>%
    # Calcular medida y multiplicar por 100
    mutate(margenimp = round((impuesto / utilidad) * 100, 2)) %>% 
    group_by(!!as.name(codigo), tamano) %>%
    # Calcular los promedios de los periodos
    summarise(margenimp = round(mean(margenimp, na.rm = TRUE), 2)) %>%
    ungroup() %>%
    gather("parte", "valor_sec", -!!as.name(codigo), -tamano) %>%
    bind_rows(nivel1)
  
  # Puntaje
  puntaje <- empresas %>%
    left_join(nivel, by = c(codigo, "parte", "tamano")) %>%
    mutate(valor_emp = round(valor_emp, 2),
           valor_sec = round(valor_sec, 2),
           diferencia = round(valor_emp - valor_sec, 2)) %>%
    group_by(parte) %>%
    mutate(posicion = dense_rank(-diferencia))
  
}

# Ejecutar funcion impuestos
posiciones_impuestos <- list(estados_impuesto, estados_impuesto,
                             estados_impuesto, estados_impuesto) %>%
  map2(c("clase", "grupo", "division", "seccion"),
       calificar_impuestos,
       data = impuestos,
       ciiu = ciiu4)

names(posiciones_impuestos) <- c("clase", "grupo", "division", "seccion")













