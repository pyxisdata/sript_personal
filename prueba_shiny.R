# Libreria de dashboard
library(shinydashboard)
library(shiny)

# datos de prueba
testfile <- "\\Users\\PC\\Desktop\\data.csv"

# Partes de la pagina 
header <-dashboardHeader(
  dropdownMenu(
    type = "messages",
    messageItem(
      from = "Diego Peralta",
      message = "Bienvenido a R Dashboard"
    ),
    messageItem(
      from = "Diego Peralta",
      message = "Confio en que le parezca sensacional"
    )
  ),
  dropdownMenu(
    type = "notifications",
    notificationItem(
      text = "El dashboard esta disponible"
    )
  ),
  dropdownMenu(
    type = "tasks",
    taskItem(
      text = "Iniciando datos",
      value = 10
    )
  )
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Prueba datos",
             tabName = "prueba datos"
    ),
    menuItem("Datos esperando",
             tabName = "datos esperando"
    )
  ),
  sliderInput(
    inputId = "anno",
    label = "Año", 
    min = 2013,
    max = 2018,
    value = 2018
  ),
  selectInput(
    inputId = "nombre",
    label = "Nombre",
    choices = c("El beto", "Narizon", "Galleta con crema", "Sin mano")
  )
)

body <- dashboardBody(
    tabItem(
      tabName = "prueba datos",
      tabBox(
        title = "Caja de felicidad",
        tabPanel("Feliz 1", "Esto esta buenisimo"),
        tabPanel("Feliz 2", "Esto es la gozadera")
      )
    ),
    tabItem(tabName = "datos esperando"),
    tableOutput("tabla")
)


# Pagina
ui <- dashboardPage(header, sidebar, body)

# Datos
reactive_datos <- reactiveFileReader(
  intervalMillis = 1000,
  session = session,
  filePath = testfile,
  readFunc = read.csv
)


# Salida del servidor
server <- function(input, output, session) {
  output
}


# Cargar la app
shinyApp(ui, server)
