# Prueba para el Dashboard

# Librerias
library(odbc)
library(RevoScaleR)
library(shiny)
library(shinydashboard)

# Conexion a SQL
conex <- dbConnect(odbc(),
                   Driver = "SQL Server",
                   Server = "40.87.53.140",
                   Database = "PyxisFinale",
                   UID = "alejandro.peralta",
                   PWD = "Pyxy54dm1n2018",
                   Encoding = "latin-9"
                   )

conex_r <- paste("Driver={SQL Server}", 
                 "Server=40.87.53.140", 
                 "Database=PyxisFinale",
                 "UID=alejandro.peralta",
                 "PWD=Pyxy54dm1n2018;",
                 sep = "; "
                 )

# Datos del dashboard de prueba 
ipc_conn <- RxSqlServerData(connectionString = conex_r,
                            table = "IPC",
                            )

ipc_name <- file.path(tempdir(), "ipc.xdf")

ipc <- rxImport(ipc_conn, 
                outFile = ipc_name, 
                rowsPerRead = 500000,
                reportProgress = 1,
                overwrite = TRUE)

# Lista Municipios
ipc_data <- rxDataStep(inData = ipc,
                      maxRowsByCols = 50000000
                      )

# Crear la base del dashboard

# Header
header <- dashboardHeader(
  dropdownMenu(
    type = "messages",
    messageItem(
      from = "Paquito",
      message = "Salude pues a paquito"
      )
    ),
  dropdownMenu(
    type = "notifications",
    notificationItem(
      text = "ya llego Paquito"
      )
    ),
  dropdownMenu(
    type = "tasks",
    taskItem(
      text = "Reunirse con Paquito",
      value = 50
    )
  )
  )


# Sidebar
sidebar <- dashboardSidebar(
  selectInput(
    inputId = "munpio",
    label = "Municipio",
    choices = ipc_data$id_munpio
  ),
  sidebarMenu(
    menuItem(
      "Dashboard",
      tabName = "dashboard"
      ),
    menuItem(
      "Entradas",
      tabName = "entradas"
    )
  )
  )

# Body
body <- dashboardBody(
  tabItems(
    tabItem(
      tabName = "dashboard",
      tabBox(
        title = "La caja de Paquito",
        tabPanel("Paquito esta vivo"),
        tabPanel("Mentira, se murio")
      )
      ),
    tabItem(tabName = "entradas")
    )
  )

# Ui
ui <- dashboardPage(header, sidebar, body)

# Server
server <- function(input, output) {}

# App
shinyApp(ui, server)
