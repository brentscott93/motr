#' Curves Server
#'
#' @param input
#' @param output
#' @param session
#'
#' @return
#' @export
#'
#' @examples
curves_server <- function(input, output, session) {

  #get user uploaded data
  user_data <- eventReactive(input$file_input,{
    read.csv(input$file_input$datapath)
  })

  #get selected built-in dataset
  drc_data <- eventReactive(input$drc_dataset_picker,{
    chosen_data <- input$drc_dataset_picker
    get(chosen_data)
  })


observe({
  if(input$drc_data == 'user'){
    shinyjs::show('file_input')
    shinyjs::hide('drc_dataset_picker')
  } else {
    shinyjs::hide('file_input')
    shinyjs::show('drc_dataset_picker')

  }
})

  #select user data or built-in data
  data <- reactive({
    if(input$drc_data == 'drc_data'){
      drc_data()
    } else {
      req(input$file_input)
      user_data()
    }
  })

  #make the selectInputs for 'dose' based on loaded dataset
  output$dose = renderUI({
    selectInput(session$ns('dose'),
                "Dose (independent variable)",
                c(Choose='', colnames(data())),
                selectize = TRUE,
                width ="100%")
  })



  #make the selectInputs for 'response' based on loaded dataset
  output$response <- renderUI({
    selectInput(session$ns('response'),
                "Response (dependent variable)",
                c(Choose='', colnames(data())),
                selectize = TRUE,
                width ="100%")
  })

  #make the selectInputs for 'curveid' based on loaded dataset
  output$curveid <- renderUI({
    if(req(input$group_switch) == TRUE){
      selectInput(session$ns('curveid'),
                  "Groups (curveid)",
                  c(Choose='', colnames(data())),
                  selectize = TRUE,
                  width ="100%")
    }
  })

  plot_data_no_fit <- reactive({
    if(input$group_switch == FALSE){
      req(input$dose, input$response)
      data() %>%
        dplyr::select(input$dose, input$response)
    } else {
      req(input$curveid)
      data() %>%
        dplyr::select(input$dose, input$response, input$curveid)
    }
  })


  drm_data <- reactive({
    req(input$dose, input$response)
    resp <- input$response
    dos <-  input$dose
    data.frame(resp = data()[,input$response],
               dos = data()[,input$dose])
  })



  plot1_log_scale <- reactive({
    ifelse(input$log_dose_plot1 == "off", "", input$log_dose_plot1)
  })

  output$plot1 <- renderPlot({
    req(input$dose, input$response)
    if(input$group_switch == FALSE){

      plot(plot_data_no_fit(),
           log = plot1_log_scale(),
           cex = as.numeric(input$plot1_cex),
           cex.axis = as.numeric(input$plot1_cex),
           cex.lab = as.numeric(input$plot1_cex))

    } else {

      plot(x = plot_data_no_fit()[,1],
           y = plot_data_no_fit()[,2],
           col = plot_data_no_fit()[,3],
           xlab = input$dose,
           ylab = input$response,
           log = plot1_log_scale(),
           cex = as.numeric(input$plot1_cex),
           cex.axis = as.numeric(input$plot1_cex),
           cex.lab = as.numeric(input$plot1_cex))

    }
  })

  log_dose_number <- reactive({
    if(input$log_dose == "NULL"){
      "NULL"
    } else if(input$log_dose == "ln"){
      "exp(1)"
    } else {
      "10"
    }
  })

  log_dose_drm <- reactive({
    if(input$log_dose == "NULL"){
      NULL
    } else if(input$log_dose == "ln"){
      exp(1)
    } else {
      10
    }
  })

  output$mean_fun_desc <- renderText({
    req(data())
    if(input$drc_data == 'drc_data'){
      if(input$group_switch == FALSE){
        paste0("drm(",
               input$response,
               " ~ ",
               input$dose,
               ", data = ",
               input$drc_dataset_picker,
               ",", " fct = ",
               input$equation, "(), ",
               "logDose = ",
               log_dose_number(),
               ")")
      } else {
        paste0("drm(",
               input$response,
               " ~ ",
               input$dose,
               ", curveid = ",
               input$curveid,
               ", data = ",
               input$drc_dataset_picker,
               ",", " fct = ",
               input$equation, "(), ",
               "logDose = ",
               log_dose_number(),
               ")")
      }} else {
        if(input$group_switch == FALSE){
          paste0("drm(",
                 input$response,
                 " ~ ",
                 input$dose,
                 ", data = ",
                 str_sub(input$file_input$name, 1, -5),
                 ",", " fct = ",
                 input$equation, "(), ",
                 "logDose = ",
                 log_dose_number(),
                 ")")
        } else {
          paste0("drm(",
                 input$response,
                 " ~ ",
                 input$dose,
                 ", curveid = ",
                 input$curveid,
                 ", data = ",
                 str_sub(input$file_input$name, 1, -5),
                 ",", " fct = ",
                 input$equation, "(), ",
                 "logDose = ",
                 log_dose_number(),
                 ")")

        }


      }
  })


  drm_curve <- reactive({
    c <- sym(input$curveid)
    pull(data(), c)
  })


  #fit curves
  fit <- eventReactive(input$fit_button, {

    fitting <- if(input$group_switch == FALSE){

      drm(paste(input$response, "~", input$dose),
          fct = eval(call(input$equation)),
          data = data(),
          logDose = log_dose_drm())

    } else if(input$group_switch == TRUE){

      c <- input$curveid
      df <- data() %>%
        mutate(curveid = data()[,c] )

      drm(paste(input$response, "~", input$dose),
          curveid = curveid,
          data = df,
          fct = eval(call(input$equation)),
          logDose = log_dose_drm())
    }

  })

  observeEvent(fit(),{
    showNotification("Dose-Response curve fit. See 'Summary' or 'Plot.drm' tab for output",
                     type = "message")
  })

  output$summary <- renderPrint({
    req(fit())
    summary(fit())
  })

  plot2_log_scale <- reactive({
    ifelse(input$log_dose_plot2 == "off", "", input$log_dose_plot2)
  })


  ##############plot inputs################

  num_groups <- reactive({
    seq_along(unique(data()[[input$curveid]]))
  })

  color_id <- reactive({
    generate_id('color', data(), input$curveid)
  })

  group_names_label <- reactive({
    unique(data()[[input$curveid]])
  })

  output$plot_colors <- renderUI({
    map2(session$ns(color_id()), group_names_label(), ~ colourInput(.x, label = .y, value = "black"))
  })

  user_colors <- reactive({
    if(input$use_custom_colors == 'Custom'){
      req(input$color1)
      col <- map_chr(color_id(), ~input[[.x]])
      alpha(col, input$alpha)
    } else if(input$use_custom_colors == 'Default') {
      TRUE
    } else {
      FALSE
    }
  })

  observe({
    if(input$use_custom_colors == 'Custom'){
      req(input$color1)
      map(color_id(), shinyjs::show)
    } else {
      req(input$color1)
      map(color_id(), shinyjs::hide)
    }
  })


  lty_id <- reactive({
    generate_id('lty', data(), input$curveid)

  })

  default_lty <- c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash", "blank")

  output$plot_lty <- renderUI({
    pmap(list(lty_id(),
              paste0('Line type (', group_names_label(), ')'),
              num_groups()),
         function(first, second, third){

           sliderTextInput(
             inputId = session$ns(first),
             label = second,
             grid = FALSE,
             force_edges = TRUE,
             choices = c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash", "blank"),
             selected = default_lty[[third]])
         }
    )

  })

  user_lty <- reactive({
    if(is.null(input$lty1)){
      rep(1:6, 2)
    } else {
      line <-  map_chr(lty_id(), ~input[[.x]])

      line_num <- match(line, default_lty) %>%
        ifelse(. == 7, 0, .)
    }
  })

  #pointshape

  pch_id <- reactive({
    generate_id('pch', data(), input$curveid)
  })

  output$plot_pch <- renderUI({
    pmap(list(pch_id(),
              paste0('Point shape (', group_names_label(), ')'),
              num_groups()),
         function(first, second, third){

           sliderInput(
             inputId = session$ns(first),
             label = second,
             min = 0,
             max = 25,
             value = third)
         }
    )

  })

  user_pch <- reactive({
    if(is.null(input$pch1)){
      1:25
    } else {
      map_int(pch_id(), ~input[[.x]])
    }
  })

  #xlim sliderInput

  xlim_max <- reactive({
    max(data()[[input$dose]])
  })

  output$xlim <- renderUI({
    sliderInput(session$ns('xlim'),
                'X-Axis Upper Limit',
                min = -(xlim_max()/4),
                max = xlim_max()*1.5,
                value = c(0, xlim_max()))
  })

  #ylim sliderInput

  ylim_max <- reactive({
    max(data()[[input$response]])
  })

  output$ylim <- renderUI({
    sliderInput(session$ns('ylim'),
                'Y-Axis Upper Limit',
                min = -(ylim_max()/4),
                max = ylim_max()*1.5,
                value = c(0, ylim_max()))
  })

  xlab <- reactive({
    if(is.null(input$xlab)){
      input$dose
    } else if(nchar(input$xlab) == 0){
      input$dose
    } else {
      input$xlab
    }
  })

  xlim <- reactive({
    if(is.null(input$xlim)){
      c(0, xlim_max())
    } else {
      input$xlim
    }
  })

  ylim <- reactive({
    if(is.null(input$ylim)){
      c(0,  ylim_max())
    } else {
      input$ylim
    }

  })


  ylab <- reactive({
    if(is.null(input$ylab)){
      input$dose
    } else if(nchar(input$ylab) == 0){
      input$dose
    } else {
      input$ylab
    }
  })
  #legend textboxes

  # legend_text_id <- reactive({
  #   generate_id('legend_text', data(), input$curveid)
  # })
  #
  # output$legend_text <- renderUI({
  #   pmap(list(legend_text_id(),
  #        paste0('Legend Label (', group_names_label(), ')'),
  #        group_names_label()),
  #        function(first, second, third){
  #        textInput(first, second, value = third, placeholder = 'Variable Name')
  #          }
  #        )
  # })
  #
  # user_legendText <- reactive({
  #   if(is.null(legend_text1)){
  #
  #   } else {
  #   map_chr(legend_text_id(), ~input[[.x]])
  #   }
  # })

  output$legend_position <- renderUI({
    tagList(
      sliderInput(session$ns('legend_position_x'),
                  'Legend Position (X)',
                  min = -(xlim_max()/4),
                  max = xlim_max()*1.5,
                  value = xlim_max()),

      sliderInput(session$ns('legend_position_y'),
                  'Legend Position (Y)',
                  min = -(ylim_max()/4),
                  max = ylim_max()*1.5,
                  value = ylim_max())

    )
  })

  legend_position_x <- reactive({
    if(is.null(input$legend_position_x)){
      xlim_max() * 0.9
    } else {
      input$legend_position_x
    }
  })

  legend_position_y <- reactive({
    if(is.null(input$legend_position_y)){
      ylim_max() * 0.9
    } else {
      input$legend_position_y
    }
  })

  observe({
    output$drm_plot <- renderPlot(height = as.numeric(input$plot_height), width = as.numeric(input$plot_width),{
      req(fit())
      par(mar = c(5, 6, 4, 2) + 0.1)
      plot(fit(),
           type = "all",
           #colors
           col = user_colors(),#user
           #& aesthetics
           lty = user_lty(), #user - linetype
           lwd = as.numeric(input$lwd), #line thickness
           pch = user_pch(), #user - points style
           cex = as.numeric(input$cex), #user
           gridsize = as.numeric(input$gridsize),#, #user - smoothnest of fit
           # #axes & labels
           log = plot2_log_scale(),#user
           cex.lab = as.numeric(input$cex.lab), #user
           cex.axis = as.numeric(input$cex.axis), #user
           xlab = xlab(), #user
           xlim = xlim(), #user
           ylab = ylab(), #user
           ylim = ylim(),#user
           #legend
           legend = input$legend, #user
           #legendText = user_legendText(), #user
           legendPos = c(legend_position_x(), legend_position_y()), #user
           cex.legend = as.numeric(input$cex.legend), #user
           bty = "l") #user
      graphics::box(lwd = input$box_lwd, bty = "l") #user

    })
  })


  # downloadHandler contains 2 arguments as functions, namely filename, content
  output$plot_down <- downloadHandler(
    filename =  function() {
      paste("drc.png")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {

      png(file, height = as.numeric(input$plot_height), width = as.numeric(input$plot_width), units = "px") # open the png device

      plot(fit(),
           type = "all",
           #colors
           col = user_colors(),#user
           #& aesthetics
           lty = user_lty(), #user - linetype
           lwd = as.numeric(input$lwd), #line thickness
           pch = user_pch(), #user - points style
           cex = as.numeric(input$cex), #user
           gridsize = as.numeric(input$gridsize),#, #user - smoothnest of fit
           # #axes & labels
           log = plot2_log_scale(),#user
           cex.lab = as.numeric(input$cex.lab), #user
           cex.axis = as.numeric(input$cex.axis), #user
           xlab = xlab(), #user
           xlim = xlim(), #user
           ylab = ylab(), #user
           ylim = ylim(),#user
           #legend
           legend = input$legend, #user
           #legendText = user_legendText(), #user
           legendPos = c(legend_position_x(), legend_position_y()), #user
           cex.legend = as.numeric(input$cex.legend), #user
           bty = "l") #user
      graphics::box(lwd = input$box_lwd, bty = "l") #user





      dev.off()  # turn the device off

    }
  )




  #Example from https://github.com/rstudio/shiny-examples/blob/master/112-generate-report/app.R
  output$report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "report.html",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)

      # Set up parameters to pass to Rmd document
      params <- list(drm_obj = fit(),
                     col = user_colors(),#user
                     #& aesthetics
                     lty = user_lty(), #user - linetype
                     lwd = as.numeric(input$lwd), #line thickness
                     pch = user_pch(), #user - points style
                     cex = as.numeric(input$cex), #user
                     gridsize = as.numeric(input$gridsize),#, #user - smoothnest of fit
                     # #axes & labels
                     log = plot2_log_scale(),#user
                     cex.lab = as.numeric(input$cex.lab), #user
                     cex.axis = as.numeric(input$cex.axis), #user
                     xlab = xlab(), #user
                     xlim = xlim(), #user
                     ylab = ylab(), #user
                     ylim = ylim(),#user
                     #legend
                     legend = input$legend, #user
                     #legendText = user_legendText(), #user
                     legendPos = c(legend_position_x(), legend_position_y()), #user
                     cex.legend = as.numeric(input$cex.legend),
                     box_lwd = input$box_lwd)

      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )


  output$compParm_diff <- renderPrint({
    req(fit())
    n <- unique(fit()$parNames[[2]])
    walk(n, ~compParm(fit(), strVal = .x, operator = "-"))
  })

  output$compParm_ratio <- renderPrint({
    req(fit())
    n <- unique(fit()$parNames[[2]])
    walk(n, ~compParm(fit(), strVal = .x, operator = "/"))
  })

}
