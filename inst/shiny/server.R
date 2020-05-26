
server <- function(input, output, session) {
  options(shiny.maxRequestSize = 30*1024^2)

  esquisse_data <- reactiveValues(data = iris, name = 'iris')

  observeEvent(input$esquisse_data, {
    esquisse_data$data <- bind_rows(map(input$esquisse_data$datapath, read_csv))
    esquisse_data$name <- 'User Data'
  })


  callModule(esquisserServer, id = 'esquisse_ui', data = esquisse_data)

  callModule(imagej_server, id = 'imagej_ui')

  callModule(curves_server, id = 'curves_ui')
}




