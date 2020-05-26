#'ImageJ analysis UI
#'
#' @param id
#'
#' @return
#' @export
#'
#' @examples
imagej_ui <- function(id){
  ns <- NS(id)

  fluidRow(
  column(4,
    box(title = 'Input', width = NULL,
      fileInput(inputId = ns("file"),
                label = "Upload Motility Files (.xls)",
                multiple = TRUE,
                accept = ".xls",
                buttonLabel = "Browse...",
                placeholder = "No files selected"),

      sliderInput(inputId = ns('cutoff'),
                   label = 'Select cutoff distance (µm)',
                   min = 0,
                   max = 10,
                   value = 4),
div(style='display:inline-block; width: 225px',
      actionBttn(
        inputId = ns('run'),
        label = "Run",
        style = "bordered",
        color = "success",
        icon = icon("running"),
        block = T)),

div(style='display:inline-block; width: 225px',
     downloadBttn(ns('download'),
                  label = 'Download',
                  style = 'bordered',
                  color = 'primary',
                  block = T)
)
    )), #box/col



    column(8,
           box(title = 'Output', width = NULL,

      tabsetPanel(id = ns('tabs'), type = "tabs",
                  tabPanel("By Video", tableOutput(ns("by_video"))),
                  tabPanel("Summary", tableOutput(ns("summary")))

      )
    ) ##box
  ) #col close
) #fluidRow close
}
