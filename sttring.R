library(readxl)
library(stringdist)
library(dplyr)
library(purrr)

options(scipen = 999)

folder <- "\\Users\\PC\\Desktop\\Archivos Brutos\\Comex\\Directorios Exportadores"

folder_list <- list.files(folder, full.names = TRUE)
folder_list <- grep(".xlsx", folder_list, value = TRUE)

data <- read_excel(folder_list[11],
                   sheet = 1,
                   col_names = FALSE)

data %>%
  filter(!is.na(X__2)) %>%
  select(X__2, X__3) %>%
  slice(2:n()) %>%
  mutate(X__3 = tolower(X__3)) -> dataf

dataf %>%
  mutate(nom_osa = stringdist(X__3, "innovandes s.a.s", method = "osa"),
         nom_lv = stringdist(X__3, "innovandes s.a.s", method = "lv"),
         nit_osa = stringdist(X__2, "900748451", method = "osa"),
         nit_lv = stringdist(X__2, "900748451", method = "lv")
         ) -> dataf

