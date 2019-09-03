# Modelo Salud 1
# Porcesamiento de las tablas de salud para su inclusion con armenia_44

# Libreria
library(readxl)
library(readr)
library(dplyr)
library(stringr)
library(stringi)
library(tidyr)
library(lubridate)

# Importar los datos a utilizar en el modelo

# Personas RIPS
rips_pers_aten <- list.files(pattern = "rips_1") %>%
  .[str_detect(., "^[~]") == FALSE] %>%
  read_excel(col_types = "text", col_names = FALSE, sheet = "rips_p1")

# Personas por tipo RIPS
rips_pers_tipo <- list.files(pattern = "rips_1") %>%
  .[str_detect(., "^[~]") == FALSE] %>%
  read_excel(col_types = "text", col_names = FALSE, sheet = "rips_pa1")

# Personas por diagnostico RIPS
rips_pers_diag <- list.files(pattern = "rips_1") %>%
  .[str_detect(., "^[~]") == FALSE] %>%
  read_excel(col_types = "text", col_names = FALSE, sheet = "rips_pd1")

# Personas por procedimiento RIPS
rips_pers_proc <- list.files(pattern = "rips_1") %>%
  .[str_detect(., "^[~]") == FALSE] %>%
  read_excel(col_types = "text", col_names = FALSE, sheet = "rips_pp1")

# Personas por procedimiento y diagnostico RIPS
rips_pers_dp <- list.files(pattern = "rips_1") %>%
  .[str_detect(., "^[~]") == FALSE] %>%
  read_excel(col_types = "text", col_names = FALSE, sheet = "rips_pdp1")

# Atenciones RIPS
rips_aten <- list.files(pattern = "rips_1") %>%
  .[str_detect(., "^[~]") == FALSE] %>%
  read_excel(col_types = "text", col_names = FALSE, sheet = "rips1")

# Afiliados BDUA
bdua_pers <- list.files(pattern = "rips_1") %>%
  .[str_detect(., "^[~]") == FALSE] %>%
  read_excel(col_types = "text", col_names = FALSE, sheet = "bdua1")

# Procesamiento de los datos

# Personas BDUA
afiliados <- bdua_pers %>%
  filter(!is.na(.[[3]])) %>%
  filter(!is.na(.[[2]])) %>%
  slice(-1) %>%
  mutate(idgenero = case_when(.[[2]] == "FEMENINO" ~ 1,
                              .[[2]] == "MASCULINO" ~ 2)) %>%
  rename("edad" = "...1",
         "afiliados" = "...3") %>%
  select(-2) %>%
  select(idgenero, edad, afiliados) %>%
  mutate_at(vars(idgenero, edad, afiliados),
            as.integer)

# Personas atendidas RIPS
personas_aten <- rips_pers_aten %>%
  filter(!is.na(.[[3]])) %>%
  filter(!is.na(.[[2]])) %>%
  slice(-1) %>%
  mutate(idgenero = case_when(.[[2]] == "FEMENINO" ~ 1,
                              .[[2]] == "MASCULINO" ~ 2)) %>%
  rename("edad" = "...1",
         "personas" = "...3") %>%
  select(-2) %>%
  select(idgenero, edad, personas) %>%
  mutate_at(vars(idgenero, edad, personas), 
            as.integer)

# Personas por tipo de atnecion RIPS
personas_tipo <- rips_pers_tipo %>%
  filter(!is.na(.[[3]])) %>%
  slice(-1) %>%
  mutate(idgenero = case_when(.[[3]] == "FEMENINO" ~ 1,
                              .[[3]] == "MASCULINO" ~ 2),
         idatencion = word(.[[1]], 1, sep = " - ")) %>%
  rename("edad" = "...2",
         "personas" = "...4") %>%
  select(-1, -3) %>%
  select(idgenero, edad, idatencion, personas) %>%
  mutate_at(vars(idgenero, edad, personas, idatencion),
            as.integer)

# Personas por diagnostico RIPS
personas_diag <- rips_pers_diag %>%
  filter(!is.na(.[[3]])) %>%
  slice(-1) %>%
  mutate(idgenero = case_when(.[[2]] == "FEMENINO" ~ 1,
                              .[[2]] == "MASCULINO" ~ 2),
         iddiagnost = word(.[[3]], 1, sep = " - ")) %>%
  rename("edad" = "...1",
         "personas" = "...4") %>%
  select(-2, -3) %>%
  select(idgenero, edad, iddiagnost, personas) %>%
  mutate_at(vars(idgenero, edad, personas),
            as.integer)

# Personas por procedimiento RIPS
personas_proc <- rips_pers_proc %>%
  filter(!is.na(.[[3]])) %>%
  slice(-1) %>%
  mutate(idgenero = case_when(.[[2]] == "FEMENINO" ~ 1,
                              .[[2]] == "MASCULINO" ~ 2),
         idproced = word(.[[3]], 1, sep = " - ")) %>%
  rename("edad" = "...1",
         "personas" = "...4") %>%
  select(-2, -3) %>%
  select(idgenero, edad, idproced, personas) %>%
  mutate_at(vars(idgenero, edad, personas),
            as.integer)

# Personas por diagnostico y procedimiento
personas_dp <- rips_pers_dp %>%
  filter(!is.na(.[[3]])) %>%
  slice(-1) %>%
  mutate(idgenero = case_when(.[[2]] == "FEMENINO" ~ 1,
                              .[[2]] == "MASCULINO" ~ 2),
         iddiagnost = word(.[[3]], 1, sep = " - "),
         idproced = word(.[[4]], 1, sep = " - ")) %>%
  rename("edad" = "...1",
         "personas" = "...5") %>%
  select(-2, -3, -4) %>%
  select(idgenero, edad, iddiagnost, idproced, personas) %>%
  mutate_at(vars(idgenero, edad, personas),
            as.integer)

# Personas por diuagnostico, procedimiento y tipo RIPS
atenciones <- rips_aten %>%
  filter(!is.na(.[[3]])) %>%
  slice(-1) %>%
  mutate(idgenero = case_when(.[[2]] == "FEMENINO" ~ 1,
                              .[[2]] == "MASCULINO" ~ 2),
         iddiagnost = word(.[[3]], 1, sep = " - "),
         idproced = word(.[[4]], 1, sep = " - "),
         idatencion = word(.[[5]], 1, sep = " - ")) %>%
  rename("edad" = "...1",
         "atenciones" = "...6",
         "costo_proc" = "...7",
         "valor_cons" = "...8",
         "personas" = "...9") %>%
  select(-2, -3, -4, -5) %>%
  select(idgenero, edad, iddiagnost, idproced, idatencion, personas,
         atenciones, costo_proc, valor_cons) %>%
  mutate_at(vars(idgenero, edad, personas, idatencion),
            as.integer) %>%
  mutate_at(vars(costo_proc, valor_cons), as.numeric) %>%
  mutate(costo = costo_proc + valor_cons) %>%
  select(-costo_proc, -valor_cons)

# Calculo de las probabilidades 
union_probabilidad <- atenciones %>% 
  left_join(personas_dp, by = c("idgenero", "edad", "iddiagnost",
                                "idproced")) %>%
  rename(pers_diaproate = personas.x,
         pers_diapro = personas.y) %>%
  left_join(personas_proc, by = c("idgenero", "edad", "idproced")) %>%
  rename(pers_pro = personas) %>%
  left_join(personas_diag, by = c("idgenero", "edad", "iddiagnost")) %>%
  rename(pers_dia = personas) %>%
  left_join(personas_tipo, by = c("idgenero", "edad", "idatencion")) %>%
  rename(pers_tip = personas) %>%
  left_join(personas_aten, by = c("idgenero", "edad")) %>%
  rename(pers_ate = personas) %>%
  left_join(afiliados, by = c("idgenero", "edad")) %>%
  rename(pers_afi = afiliados) %>%
  filter(!is.na(pers_afi)) %>%
  mutate(idregimen = 2)


# Tabla de diagnosticos
diagnosticos <- rips_aten %>%
  filter(!is.na(.[[3]])) %>%
  slice(-1) %>%
  select(5) %>%
  distinct() %>%
  separate(1, c("codigo", "nombre"), sep = " - ") %>%
  mutate(nombre = stri_trans_general(nombre, "Latin-ASCII"))

# Tabla de procedimientos
procedimientos <- rips_aten %>%
  filter(!is.na(.[[3]])) %>%
  slice(-1) %>%
  select(3) %>%
  distinct() %>%
  separate(1, c("codigo", "nombre"), sep = " - ") %>%
  mutate(nombre = stri_trans_general(nombre, "Latin-ASCII"))


  
# Exportar datos
write_csv(union_probabilidad, 
          paste0(getwd(), "/union_probabilidad_1.csv"),
          na = "")

write_csv(diagnosticos, 
          paste0(getwd(), "/diagnosticos_circasia.csv"),
          na = "")
         
write_csv(procedimientos, 
          paste0(getwd(), "/procedimientos_circasia.csv"),
          na = "")
