# Pruebas de ggplot2

library(MASS)
library(ggplot2)
library(dplyr)

data <- diamonds

# Graficas
data %>%
  ggplot(aes(x = carat,
             y = price,
             color = clarity)
         ) + 
  geom_jitter(alpha = 0.4) +
  facet_grid(. ~ cut)


