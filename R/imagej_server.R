#' Imagej Server
#'
#' @param input
#' @param output
#' @param session
#'
#' @return
#' @export
#'
#' @examples
imagej_server <- function(input, output, session){

  imagej_data <- eventReactive(input$run,{

    filenames <- input$file$name
    data_upload <- map(input$file$datapath, read.delim)
    for (i in seq_along(data_upload)){
      data_upload[[i]]<-cbind(data_upload[[i]], filenames[i])}

    combined <- bind_rows(data_upload) %>%
      separate('filenames[i]', c("Condition", "Rest"), sep = "_00") %>%
      separate("Rest", c("Video", "Extenstion"), sep = "_")

  })


      data_video <- reactive({
        imagej_data() %>%
        group_by(Condition, Video)%>%
        analyze_motility(cutoff = input$cutoff) %>%
        rename("Velocity" = average_velocity,
               "Percent Moving" =  percent_moving,
               "Moving Filaments" = moving_filaments,
               "Total Filaments" = total_filaments)
      })

  output$by_video <- renderTable({
   validate(need(data_video(), 'Please Upload Data'))
    data_video()
  })



  data_summary <- reactive({

    summary_analysis <- data_video() %>%
      group_by(Condition) %>%
      summarize("Average Velocity" = mean(Velocity),
                "Average Percent Moving" = mean(`Percent Moving`))

  })

  output$summary <- renderTable({
   validate(need(data_summary(), 'Please Upload Data'))
    data_summary()
  })


  output$download <- downloadHandler(
    filename = function() {
      paste0("motility_analyzed.xlsx")
    },
    content = function(file) {
      export(list('By Video' = data_video(), 'Summary'  = data_summary()), file = file)
    }
    )






}
