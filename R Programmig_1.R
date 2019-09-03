# Quiz de R Programming
library(readr)
library(dplyr)
library(tidyr)
library(readxl)
library(lubridate)

# Lectura de los datos de csv
datos <- 
  read_csv("\\Users\\PC\\Desktop\\R Programming\\daily_SPEC_2014.csv.bz2")

# Media
datos %>%
  filter(`State Name` == "Wisconsin",
         `Parameter Name` == "Bromine PM2.5 LC") %>%
  summarise(mean = mean(`Arithmetic Mean`, na.rm = TRUE))

# Media global
datos %>%
  group_by(`Parameter Name`) %>%
  summarise(mean = mean(`Arithmetic Mean`, na.rm = TRUE)) %>%
  arrange(desc(mean))

# Media por monitor 
datos %>%
  filter(`Parameter Name` == "Sulfate PM2.5 LC") %>%
  group_by(`State Code`, `County Code`, `Site Num`) %>%
  summarise(mean = mean(`Arithmetic Mean`, na.rm = TRUE)) %>%
  arrange(desc(mean))

# Diferencia entre niveles 
datos %>%
  filter(`Parameter Name` == "EC PM2.5 LC TOR",
         `State Name` %in% c("Arizona", "California")) %>%
  group_by(`State Name`) %>%
  summarise(mean= mean(`Arithmetic Mean`, na.rm = TRUE)) %>%
  spread(`State Name`, mean) %>%
  mutate(diff = California - Arizona)

# Mediana
datos %>%
  filter(`Parameter Name` == "OC PM2.5 LC TOR",
         Longitude < -100) %>%
  summarise(median = median(`Arithmetic Mean`, na.rm = TRUE))

# Lectura de datos de excel
datos_2 <- 
  read_xlsx("\\Users\\PC\\Desktop\\R Programming\\aqs_sites.xlsx",
            sheet = 1)

# Conteo
datos_2 %>%
  filter(`Land Use` == "RESIDENTIAL",
         `Location Setting` == "SUBURBAN") %>%
  summarise(number = n())

# Mediana de tablas cruzadas
datos_2 %>%
  filter(`Land Use` == "RESIDENTIAL",
         `Location Setting` == "SUBURBAN") %>%
  left_join(mutate(datos,
                   `State Code` = as.numeric(`State Code`),
                   `County Code` = as.numeric(`County Code`),
                   `Site Num` = as.numeric(`Site Num`)),
            by = c("State Code", "County Code",
                   "Site Number" = "Site Num",
                   "Latitude", "Longitude")) %>%
  filter(`Parameter Name` == "EC PM2.5 LC TOR",
         Longitude >= -100) %>%
  summarise(median = median(`Arithmetic Mean`, na.rm = TRUE))

# Mes
datos_2 %>%
  filter(`Land Use` == "COMMERCIAL") %>%
  left_join(mutate(datos,
                   `State Code` = as.numeric(`State Code`),
                   `County Code` = as.numeric(`County Code`),
                   `Site Num` = as.numeric(`Site Num`)),
            by = c("State Code", "County Code",
                   "Site Number" = "Site Num",
                   "Latitude", "Longitude")) %>%
  filter(`Parameter Name` == "Sulfate PM2.5 LC") %>%
  mutate(month = month(`Date Local`)) %>%
  group_by(month) %>%
  summarise(mean = mean(`Arithmetic Mean`, na.rm = TRUE)) %>%
  arrange(desc(mean))

# Dias
datos %>%
  filter(`State Code` == "06",
         `County Code` == "065",
         `Site Num` == "8001",
         `Parameter Name` %in% c("Sulfate PM2.5 LC",
                                 "Total Nitrate PM2.5 LC")) %>%
  group_by(`Parameter Name`, `Date Local`) %>%
  summarise(mean = mean(`Arithmetic Mean`, na.rm = TRUE)) %>%
  spread(`Parameter Name`, mean) %>%
  mutate(sum = `Sulfate PM2.5 LC` + `Total Nitrate PM2.5 LC`) %>%
  filter(sum > 10)

# Correlaciones
datos %>%
  filter(`Parameter Name` %in% c("Sulfate PM2.5 LC",
                                 "Total Nitrate PM2.5 LC")) %>%
  group_by(`Parameter Name`, `State Code`, `County Code`,
           `Site Num`, `Date Local`) %>%
  summarise(mean = mean(`Arithmetic Mean`, na.rm = TRUE)) %>%
  spread(`Parameter Name`, mean) %>%
  group_by(`State Code`, `County Code`, `Site Num`) %>%
  summarise(corr = cor(`Sulfate PM2.5 LC`, `Total Nitrate PM2.5 LC`)) %>%
  arrange(desc(corr))
  

