#' load_file UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList fileInput
#' @importFrom shinycssloaders withSpinner
#'
mod_load_file_ui <- function(id){
  ns <- NS(id)
  tagList(
    fileInput(ns("file1"), "Choose XLSX File",
              accept = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"),
    br(),
    verbatimTextOutput(ns("fileName")) %>%
      withSpinner()
  )
}

#' load_file Server Function
#'
#' @noRd
mod_load_file_server <- function(input, output, session, r){
  ns <- session$ns
  r$import_steps <- reactiveValues()
  output$fileName <- renderText(r$log)

## Import and treat files and update log message for user
  observeEvent(input$file1,{
    r$processed <- NULL
    r$process_error <- FALSE
    r$filename <- input$file1$name
    ## Reading File
    r$log <- paste("[1] Reading file", input$file1$name, "...")
    tryCatch(
      {
        r$data <- openxlsx::loadWorkbook(input$file1$datapath)
        r$log <- paste(r$log, "\n[1] File Opened!")
      },
      error = function(e){
        r$log <- paste(r$log, "\n[1] ERROR Cannot open file:\n", e)
        r$process_error <- TRUE
      },
      warning = function(w){
        r$log <- paste(r$log, "\n[1] Warnings during file opening:\n", w)
      }
    )
    ## Correcting category
    if(r$process_error == FALSE){
      r$log <- paste(r$log, "\n\n[2] Correcting Category...")
      tryCatch(
        {
          r$processed <- process_evavelo(r$data)
          r$log <- paste(r$log, "\n[2] Category corrected!")
        },
        error = function(e){
          r$log <- paste(r$log, "\n[2] ERROR during category check:\n", e)
          r$process_error <- TRUE
        },
        warning = function(w){
          r$log <- paste(r$log, "\n[2] Warnings during category check:\n", w)
        }
      )
    }

    ## End message
    if (r$process_error)
      r$log <- paste(r$log, "\n\n ERROR. File cannot be processed")
    else
      r$log <- paste(r$log, "\n\n OK! You can now download the result")


  })

}

## To be copied in the UI
# mod_load_file_ui("load_file_ui_1")

## To be copied in the server
# callModule(mod_load_file_server, "load_file_ui_1")

