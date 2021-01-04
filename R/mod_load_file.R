#' load_file UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList fileInput
mod_load_file_ui <- function(id){
  ns <- NS(id)
  tagList(
    fileInput(ns("file1"), "Choose XLSX File", accept = ".xlsx"),
    br(),
    verbatimTextOutput(ns("fileName"))
  )
}

#' load_file Server Function
#'
#' @noRd
mod_load_file_server <- function(input, output, session, r){
  ns <- session$ns
  output$fileName <- renderText({input$file1$datapath})
  observeEvent(input$file1,{
    r$data <- openxlsx::loadWorkbook(input$file1$datapath)
  })

}

## To be copied in the UI
# mod_load_file_ui("load_file_ui_1")

## To be copied in the server
# callModule(mod_load_file_server, "load_file_ui_1")

