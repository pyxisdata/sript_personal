# Script para obtener avance de los formularios de las empresas
# Librerias
# Conexion a google drive
library(googledrive)
library(googlesheets)
library(readr)
library(dplyr)
library(purrr)
library(stringi)
library(stringr)
library(tibble)
# Web scraping
library(RSelenium)

# Conectarse a la hoja de calculo de google
hoja_calculo <- gs_key("1iKJj0l3Gix8Zcmg6-2M0CYS4m_ZEJclbxd4F38QU7cg")

# Importar los datos de la hoja de calculo de registros
formularios <- hoja_calculo %>%
  gs_read(ws = "registros",
          col_types = cols(.default = "c")) %>%
  filter(Tipo == "1") %>%
  select(Empresa, NIT, Formulario)

# Ingresar el servidor de RSelenium
# Ingresar la ip de la maquina virtual y el puerto disponible
remDr <- remoteDriver(remoteServerAddr = "192.168.99.100",
                      port = 4465L)

# Inicializar el servidor docker
remDr$open()

# Funcion para extrar los porcentajes de avance de la lista de enlaces
acceso <- function(enlace, driver) {
    
  driver$navigate(enlace)
    
  elemento <- driver$findElement("id", "progressPercentage")
  porcentaje <- elemento$getElementText()
  
  print("terminado")
  porcentaje
    
}

# Ejecutar prueba
avance <- formularios[[3]] %>% 
  map(acceso, driver = remDr) %>%
  unlist()

avance_2 <- avance %>%
  enframe(name = NULL) %>%
  bind_cols(as_tibble(formularios[[1]])) %>%
  select(2, 1) %>%
  rename(empresa = value1,
         avance = value) %>%
  mutate(empresa = stri_trans_general(empresa, "Latin-ASCII"),
         empresa = str_to_upper(empresa))

write_csv(avance_2,
          "\\Users\\PC\\Desktop\\avance_semana_6.csv")


