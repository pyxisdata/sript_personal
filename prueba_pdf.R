# Prueba de pdf
library(pdftools)
library(tesseract)
library(magick)
library(dplyr)
library(stringi)
library(purrr)
library(stringr)
library(tidyr)

# PDF de la empresa
carpeta <- "\\Users\\PC\\Desktop\\EQMAP\\Superfinanciera\\RNVE\\"
archivos <- list.files(carpeta, full.names = TRUE, pattern = ".pdf")

# Funcion lectura de los pdf
lectura <- function(nombre, paginas) {
  # Lista vacia
  lista <- list()
  # Convertir paginas en imagenes
  img_bg <- pdf_convert(nombre, pages = paginas[1], dpi = 375)
  img_er <- pdf_convert(nombre, pages = paginas[2], dpi = 375)
  # Añadir a la lista
  lista[c(1, 2)] <- c(img_bg, img_er)
  # Lectura de las imagenes
  for (i in seq_along(lista)) {
    data <- lista[[i]] %>%
      image_read() %>%
      image_contrast(sharpen = 5) %>%
      image_modulate(brightness = 100) %>%
      ocr() %>%
      stri_split_lines() %>%
      unlist()
    lista[[i]] <- data
  }
  # Asignar nombres
  names(lista) <- c("BG", "ER")
  assign("textos", lista, envir = .GlobalEnv)
}
# Leer las paginas de los pdf
lectura(archivos[1], c(6, 7))

# Lectura de las imagenes

# Limpieza del texto
palabras <- textos[[1]] %>%
  str_replace_all(",\\s", ",") %>%
  str_replace_all("\\.", ",") %>% 
  str_split(pattern = " ")
letras <- palabras %>%
  map(str_extract_all, pattern = "[:alpha:]+") %>%
  map(unlist) %>%
  map(paste, collapse = " ") %>%
  unlist()
numeros <- palabras %>%
  map(str_extract_all, pattern = "\\d+,.+") %>%
  map(unlist) %>%
  map(paste, collapse = " ") %>%
  unlist()
tabla <- tibble(letras = letras, numeros = numeros) %>%
  separate(numeros, c("2016", "2015"), sep = " ")


         
         
         
         
         
         nom_cuenta = map(nom_cuenta, paste, collapse = " "),
         nom_cuenta = as.character(nom_cuenta),
         valor = str_extract_all(texto, "\\d+,\\d+,\\d+"),
         valor = map(valor, paste, collapse = " "),
         valor = str_replace_all(valor, ",", "")) %>%
  separate(valor, c("2018", "2017"), sep = " ") %>%
  filter(!(.[3] == "" & is.na(.[4]))) %>%
  select(-1) %>%
  gather("anno", "valor", -1) %>%
  mutate(valor = as.numeric(valor))
# Exportar prueba
write.csv(tabla, "\\Users\\PC\\Desktop\\prueba_pdf.csv", na = "")



