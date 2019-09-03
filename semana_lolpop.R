# Libreria
library(readr)
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)

# Datos de prueba cuantitativos
data <- read_csv("\\Users\\PC\\Desktop\\cuantitativa.csv")
data <- data %>% 
  mutate(Anno = mdy(Anno),
         Anno = year(Anno),
         Anno = as.factor(Anno))
                    
# Grafico data2
data %>%
  ggplot(aes(x = Anno, y = Valor)) +
  geom_point(aes(color = Origen),
             size = 15) + 
  geom_line(aes(group = Anno),
            linetype = "dotted",
            size = 1) +
  scale_color_manual(values = c("blue", "darkorange2")) +
  geom_text(aes(label = Valor),
            color = "white",
            size = 4) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = paste0("Variación Ingresos Operacionales: ",
                      "\nArroz Flor No Huila vs Sector")) 

             