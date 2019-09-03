# Script para el modelo
# Librerias



# Importar base de afiliados
# Calcular la edad basado en la fecha de nacimiento
afiliados <- paste0(getwd(), "/afiliados_0319.csv") %>%
  read_csv(col_names = TRUE, col_types = cols(.default = "c")) %>%
  mutate(anno = 2019 - year(fechanac),
         mes = 4 - month(fechanac),
         edad = (anno * 12) + mes,
         edad = round(edad / 12, 2),
         anno = as.integer(edad),
         mes = round(edad - anno, 2),
         mes = (mes * 12) / 100,
         edad = round(anno + mes, 2)) %>%
  select(-anno, -mes)

# Importar base de resolucion para ruta de intervenciones

