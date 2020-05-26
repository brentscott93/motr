#' Curves Ui
#'
#' @param id
#'
#' @return
#' @export
#'
#' @examples
curves_ui <- function(id){

ns <- NS(id)

#start of the vanilla shiny app UI

  fluidRow(
    column(3,
         box(title = 'Input',width = NULL,
                 radioGroupButtons(
                   inputId = ns("drc_data"),
                   label = "Choose Data:",
                   choices = c("Upload" = "user",
                               "Built-in" = "drc_data"),
                   justified = TRUE,
                   checkIcon = list(
                     yes = tags$i(class = "fa fa-check-square",
                                  style = "color: green"),
                     no = tags$i(class = "",
                                 style = ""))
                 ),
             fileInput(inputId = ns("file_input"),
                       label = "Upload File (.csv)",
                       multiple = FALSE,
                       accept = c(".csv")),
             pickerInput(inputId = ns("drc_dataset_picker"),
                         label = "Choose a 'drc' dataset",
                         choices = sort(datasets$Item),
                         selected = NULL,
                         options = list(
                           `live-search` = TRUE,
                           size = 10)),
                 uiOutput(ns("dose")),
                 uiOutput(ns("response")),
                 prettySwitch(
                   inputId = ns("group_switch"),
                   label = "Multiple groups?",
                   status = "success",
                   fill = TRUE
                 ),
                 uiOutput(ns("curveid")),
                 pickerInput(inputId = ns("equation"),
                             label = "Equation (fct)",
                             choices = all_drc_starters,
                             options = list(
                               `live-search` = TRUE,
                               size = 10)
                 ),

                 awesomeRadio(
                   inputId = ns("log_dose"),
                   label = "Log Dose",
                   choices = c("NULL", "ln", "10-log"),
                   selected = "NULL",
                   inline = TRUE,
                   checkbox = TRUE
                 ),



                 div(style="display:inline-block; width: 175px",
                     actionBttn(
                       inputId = ns("fit_button"),
                       label = "Fit",
                       style = "unite",
                       color = "danger",
                       block = T)
                 ),
                 div(style="display:inline-block; width: 175px",

                     downloadBttn(ns("report"),
                                  "Save",
                                  style = "unite",
                                  color = "default",
                                  block = T)
                 )
    )),

    column(9,
    box(title = 'Output', width = NULL,
      h4("Your current drm function:"),
      verbatimTextOutput(ns("mean_fun_desc")),

      tabsetPanel(type = "tabs",
                  tabPanel("Plot",
                           plotOutput(ns("plot1")),
                           div(style="display: inline-block;vertical-align:top; width: 250px;",
                               conditionalPanel("input.response != null &&
                                             input.response.length > 0 ",
                                                awesomeRadio(
                                                  inputId = ns("log_dose_plot1"),
                                                  label = "Plot axis log scaling",
                                                  choices = c("off", "x", "y", "xy"),
                                                  selected = "off",
                                                  inline = TRUE,
                                                  checkbox = TRUE
                                                ))),
                           div(style="display: inline-block;vertical-align:top; width: 250px;",
                               conditionalPanel("input.response != null &&
                                             input.response.length > 0 ",
                                                sliderInput(
                                                  inputId = ns("plot1_cex"),
                                                  label = "Magnify (cex)",
                                                  min = 0.5,
                                                  max = 2,
                                                  value = 1,
                                                  step = 0.25
                                                ))),
                  ),

                  tabPanel("Summary",
                           verbatimTextOutput(ns("summary"))
                  ),

                  tabPanel("Plot.drm",

                           div(style="display: inline-block;vertical-align:top; width: 250px;",
                               dropdownButton(

                                 tags$h4("Select Colors"),
                                 radioGroupButtons(
                                   inputId = ns("use_custom_colors"),
                                   label = "Color Scales",
                                   choices = c("B&W", "Default", "Custom"),
                                   justified = TRUE,
                                   checkIcon = list(
                                     yes = icon("ok",
                                                lib = "glyphicon"))
                                 ),




                                                  uiOutput(ns("plot_colors")),
                                                  sliderInput(ns('alpha'),
                                                              'Transparency',
                                                              min = 0,
                                                              max = 1,
                                                              value = 1,
                                                              step = 0.01),

                                 circle = TRUE, status = "primary",
                                 icon = icon("palette"), width = "250px",

                                 tooltip = tooltipOptions(title = "Click to choose plot colors")
                               )
                           ), #divclose

                           div(style="display: inline-block;vertical-align:top; width: 250px;",
                               dropdownButton(

                                 tags$h4("Select Aesthetics"),

                                 #linetype
                                 uiOutput(ns('plot_lty')),

                                 #line thickness
                                 sliderInput(
                                   inputId = ns("lwd"),
                                   label = "Line Thickness",
                                   min = 0.25,
                                   max = 6,
                                   value = 1,
                                   step = 0.25
                                 ),

                                 #Point shape
                                 uiOutput(ns('plot_pch')),


                                 #Point size
                                 sliderInput(
                                   inputId = ns("cex"),
                                   label = "Point Size",
                                   min = .25,
                                   max = 6,
                                   value = 1,
                                   step = 0.25
                                 ),

                                 #smooth line
                                 sliderInput(
                                   inputId = ns("gridsize"),
                                   label = "Smooth Line",
                                   min = 50,
                                   max = 5000,
                                   value = 100,
                                   step = 50
                                 ),
                                 circle = TRUE, status = "info",
                                 icon = icon("shapes"), width = "250px",

                                 tooltip = tooltipOptions(title = "Click to choose aesthetics")
                               )), #divclose

                           div(style="display: inline-block;vertical-align:top; width: 250px;",
                               dropdownButton(

                                 h4("Axes & Labels"),

                                 textInput(
                                   inputId = ns("xlab"),
                                   label = "X-Axis Label",
                                   placeholder = "x-axis label here"
                                 ),

                                 textInput(
                                   inputId = ns("ylab"),
                                   label = "Y-Axis Label",
                                   placeholder = "x-axis label here"
                                 ),

                                 sliderInput(
                                   inputId = ns("cex.lab"),
                                   label = "Axis Label Size",
                                   min = 0.25,
                                   max = 6,
                                   value = 1.2,
                                   step = 0.25
                                 ),

                                 sliderInput(
                                   inputId = ns("cex.axis"),
                                   label = "Axis tick label size",
                                   min = 0.25,
                                   max = 6,
                                   value = 1.25,
                                   step = 0.25
                                 ),

                                 uiOutput(ns("xlim")),
                                 uiOutput(ns("ylim")),


                                 sliderInput(
                                   inputId = ns("box_lwd"),
                                   label = "Axis Thickness",
                                   min = 0.25,
                                   max = 6,
                                   value = 2,
                                   step = 0.25
                                 ),

                                 radioGroupButtons(
                                   inputId = ns("log_dose_plot2"),
                                   label = "Log Scaling",
                                   choices = c("off", "x", "y", "xy"),
                                   justified = TRUE,
                                   checkIcon = list(
                                     yes = icon("ok",
                                                lib = "glyphicon"))
                                 ),

                                 circle = TRUE, status = "danger",
                                 icon = icon("chart-bar"), width = "250px",

                                 tooltip = tooltipOptions(title = "Click for axis options")

                               )), #divclose


                           plotOutput(ns("drm_plot"), width = "100%", height = "100%"),

                           div(style="display: inline-block;vertical-align:top; width: 250px;",
                               dropdownButton(

                                 tags$h4("Legend Options"),

                                 switchInput(
                                   inputId = ns("legend"),
                                   label = "Show Legend",
                                   labelWidth = "260px",
                                   value = TRUE,
                                   onLabel = "Show",
                                   offLabel = "Hide",
                                   onStatus = "success",
                                   offStatus = "danger"
                                 ),

                                 uiOutput(ns("legend_text")),

                                 sliderInput(
                                   inputId = ns("cex.legend"),
                                   label = "Text Size",
                                   min = 0.25,
                                   max = 5,
                                   value = 1,
                                   step = 0.25
                                 ),


                                 uiOutput(ns('legend_position')),

                                 circle = TRUE, status = "warning",
                                 icon = icon("info-circle"), width = "250px",

                                 tooltip = tooltipOptions(title = "Click to choose legend options")
                               ) #dropdown button close
                           ), #divclose

                           div(style="display: inline-block;vertical-align:top; width: 250px;",
                               dropdownButton(

                                 tags$h4("Plot Dimensions"),

                                 sliderInput(inputId = ns("plot_width"),
                                             label = "Plot Width",
                                             min = 200,
                                             max = 2000,
                                             value = 1000,
                                             step = 25),

                                 sliderInput( inputId = ns("plot_height"),
                                              label = "Plot Height",
                                              min = 200,
                                              max = 2000,
                                              value = 500,
                                              step = 25),

                                 downloadBttn(ns("plot_down"),
                                              "Download Plot",
                                              style = "unite",
                                              color = "default",
                                              block = T),

                                 circle = TRUE, status = "success",
                                 icon = icon("ruler-combined"), width = "250px",

                                 tooltip = tooltipOptions(title = "Click to download plot")
                               )
                           ) #divClose




                  ), #tabPanel close

                  tabPanel("Stats",
                           h5("Parameter differences"),
                           verbatimTextOutput(ns("compParm_diff")),
                           h5("Parameter ratios"),
                           verbatimTextOutput(ns("compParm_ratio"))

                  )

      ) #tabsetPanel close
    )#box close
  )#col close
) #ui close

}
