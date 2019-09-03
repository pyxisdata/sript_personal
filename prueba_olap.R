library(olapR)

optional <- "Update Isolation Level=2"

conex_str <- paste("Provider=MSOLAP",
                   "Persist Security Info=TRUE",
                   "User ID=sispro.local\\pyxis",
                   "Initial Catalog=SGD_CUBOS",
                   "Data Source=cubos.sispro.gov.co",
                   "MDX Compatibility=1",
                   "Safety Options=2",
                   "MDX Missing Member Mode=Error",
                   sep = ";")

olap_conex <- OlapConnection(conex_str)





