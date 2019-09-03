# Librerias
library(treemap)
library(d3treeR)
library(readr)


# Datos
data <- read_csv("PruebaFinca.csv",
                 col_names = TRUE
                )

data$Tamano <- 4

# basic treemap
Basico = treemap(data,
                 index = c("Lote","Surco", "Planta"),
                 vSize = "Tamano",
                 type = "index",
                 title = "Finca La Hermosa Pereza",
                 title.legend = "Lote",
                 border.col = "white",
                 border.lwds = 2
                 )            

# make it interactive ("rootname" becomes the title of the plot):
Inter = d3tree2(Basico,
                rootname = "Total"
                )
