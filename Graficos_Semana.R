# Prueba de grafico de barras
library(tidyverse)
library(stringi)
library(ggmap)

# Datos de prueba
data <- read_csv("\\Users\\PC\\Desktop\\cualitativa.csv")
# Refinacion
data %>%
  mutate_if(is.character, as.factor) %>%
  arrange(Dimension, Valore) -> data

# Barras vacias
vacios <- 1
incluir <- data.frame(matrix(NA, vacios * nlevels(data$Dimension), ncol(data)))
colnames(incluir) <- colnames(data)
incluir$Dimension <- rep(levels(data$Dimension), each = vacios)
data <- rbind(data, incluir)
data <- data %>% arrange(Dimension)
data$id <- seq(1, nrow(data))

# Etiquetas
data_etiqueta <- data # Copia de los datos originales
barras <- nrow(data_etiqueta) # Numero de niveles en la jerarquia
angulo <- 90 - 360 * (data_etiqueta$id - 0.5) / barras # Angulos en las barras
# Alineaciones
data_etiqueta$hajuste <- ifelse(angulo < -90, 1, 0)
data_etiqueta$angulo <- ifelse(angulo < -90, angulo + 180, angulo)

# Datos del grafico
# Base en blanco para el grafico de barras estandar
base <- ggplot(data, 
               aes(x = as.factor(id),
                   y = Valore,
                   fill = Dimension))

# Primera parte: añadir las barras
base + 
  # Añadir las barras a la base
  geom_bar(stat = "identity") +
  # Negativo es el radio del circulo interno, el positivo la altura maxima
  ylim(-4, 5.5) + 
  # Tema simple
  theme_minimal() +
  # Eliminar titulos de ejes y cuadricula
  theme(axis.text = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        plot.margin = unit(rep(-1, 4), "cm")) + 
  # Convertir la base en un circulo
  coord_polar(start = 0) +
  geom_text(data = data_etiqueta,
            aes(x = id,
                y = Valore + 0.25,
                label = str_wrap(Pilar, 17),
                hjust = hajuste),
            color = "black",
            inherit.aes = FALSE,
            size = 3,
            angle = data_etiqueta$angulo) +
  geom_text(data = data_etiqueta,
            aes(x = id,
                y = Valore - 0.8,
                label = Valore,
                hjust = hajuste),
            color = "black",
            inherit.aes = FALSE,
            size = 5,
            angle = data_etiqueta$angulo) +
  annotate("text",
           x = 0,
           y = -4,
           label = str_wrap("Confecciones Guayaba", 10),
           size = 8)

