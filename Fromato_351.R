# Formato 351 Caxdac

# Librerias
library(readxl)
library(dplyr)
library(stringr)
library(lubridate)
library(odbc)

# Notacion Estandar
options(scipen = 999)

# Lectura
folder <- "\\Users\\PC\\Desktop\\Archivos Brutos\\Fondos\\Formato 351\\Caxdac\\"
files <- list.files(folder, full.names = TRUE)

# Leer el archivo
data <- read_xlsx(files[3], col_names = TRUE)

names_data <- c("titulo", "puc", "aval", "avalID", "no_avalID", "razon_aval",
                "admonID", "razon_admon", "clase_inversion", "nemo",
                "cupon_total", "fecha_emision", "fecha_ven_titulo",
                "fecha_ven_cup", "fecha_compra", "monedaID", "valor_nom",
                "amortizacion", "valor_nom_residual", "num_acciones",
                "cant_acciones", "valor_compra_und", "valor_compra_pesos",
                "tasa_facial", "tasa_spread", "base_dias", "periodo",
                "modalidad", "ind_ref", "valor_raz_pesos", "valor_pres_pesos",
                "valor_raz_dif_pesos", "tasa_negoc", "dias_venc", "tasa_ref",
                "valor_tasa_ref", "tasa_var_1flujo", "margen", "tasa_desc",
                "precio", "metodo_valor", "fecha_ult_bursa", "valor_pres_ref",
                "ind_bursat", "int_ctal_dto", "c_puc", "base_deterioro",
                "valor_deterioro", "califica_titulo", "entidad_califica",
                "califca_riesgo", "califica_avalis", "deuda", "entida_deuda",
                "deposito_valores", "isisn", "fungibleID", "monto_emision",
                "participacion", "ramo", "relacion_subs", "concentracion",
                "rel_vinculacion", "pucID", "causacion_valor", 
                "pucID_causacion_ori", "causacion_valora_ori", "restricciones",
                "valora", "desvaora", "fecha_c", "num_operacion",
                "valor_raz_inv_vto", "paisID_emisor", "nom_emisor",
                "nit_emisor", "dv_emisor", "tipo_emisor", "empresaID",
                "proveedorID", "fondoID_param")

names(data) <- names_data

# Limpieza
data %>%
  slice(3:n()) %>%
  mutate(fecha_emision = str_c(str_sub(fecha_emision, -4, -1),
                               str_sub(fecha_emision, -6, -5),
                               ifelse(str_length(fecha_emision) == 8,
                                      str_sub(fecha_emision, 1, 2),
                                      str_sub(fecha_emision, 1, 1)),
                               sep = "-"),
         fecha_ven_titulo = str_c(str_sub(fecha_ven_titulo, -4, -1),
                                  str_sub(fecha_ven_titulo, -6, -5),
                                  ifelse(str_length(fecha_ven_titulo) == 8,
                                         str_sub(fecha_ven_titulo, 1, 2),
                                         str_sub(fecha_ven_titulo, 1, 1)),
                                  sep = "-"),
         fecha_compra = str_c(str_sub(fecha_compra, -4, -1),
                              str_sub(fecha_compra, -6, -5),
                              ifelse(str_length(fecha_compra) == 8,
                                     str_sub(fecha_compra, 1, 2),
                                     str_sub(fecha_compra, 1, 1)),
                              sep = "-")
         ) %>%
  mutate_at(vars(1:7, 11, 17:23, 25:55, 57:74, 76:80), as.numeric) %>%
  mutate_at(vars(12:15), ymd) -> dataf

# Data final
glimpse(dataf)

# Hacemos la conexion a SQL
conex <- dbConnect(odbc(),
                   Driver = "SQL Server",
                   Server = "40.87.53.140",
                   Database = "PyxisFinale",
                   UID = "alejandro.peralta",
                   PWD = "Pyxy54dm1n2018",
                   Encoding = "latin-9"
)

# Cargar data
dbWriteTable(conex,
             "caxdac_351",
             dataf,
             append = TRUE)



