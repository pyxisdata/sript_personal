# Librerias
library(pdftools)
library(stringr)
library(dplyr)
library(tidyr)
library(tesseract)


# PDF prueba 
pdf <- "\\Users\\PC\\Desktop\\EQMAP\\Superfinanciera\\RNVE\\ecopetrol.pdf"

prueba <- pdf_text(pdf)
spa <- tesseract("spa")
prueba <- ocr(pdf, engine = "spa")

# Pasar de pdf a imagen
imagen <- pdf_convert(pdf, dpi = 600, pages = 6)
texto <- ocr(imagen, engine = "spa")
texto_cat <- cat(texto)