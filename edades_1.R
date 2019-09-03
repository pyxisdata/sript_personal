library(dplyr)

# Serie
meses <- c(0:11)
annos <- c(0:120)

# Union
vect_1 <- rep(meses, 121)
vect_2 <- rep(annos, each = 12)

# tabla edades
edades <- data.frame(anno = vect_2, mes = vect_1)
edades %>%
  mutate(mesdec = round(mes / 100, 2),
         annodec = round(anno + mesdec, 2)) -> edades

# Exportar
write.csv(edades, "\\Users\\PC\\Desktop\\edades.csv", row.names = FALSE)


