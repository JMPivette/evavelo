#' load_file UI Function
#'
#' This module is used in the user interface to load and process files with information displayed to the user in a modal and in a log window.
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList fileInput
#' @importFrom shinycssloaders withSpinner
#' @importFrom shinyWidgets progressSweetAlert useSweetAlert updateProgressBar closeSweetAlert sendSweetAlert
#'
mod_load_file_ui <- function(id){
  ns <- NS(id)
  tagList(
    useSweetAlert(), # /!\ needed with 'progressSweetAlert'
    fileInput(ns("file1"), "Choose XLSX File",
              accept = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"),
    br(),
    verbatimTextOutput(ns("file_name")) %>%
      withSpinner()
  )
}

#' load_file Server Function
#'
#' @noRd
mod_load_file_server <- function(input, output, session, r){
  ns <- session$ns

  ## Display logs on screen
  output$file_name <- renderText(r$log)

  ## Import and treat files, update log message and display a modal information for user
  observeEvent(input$file1,{
    ## Initialize values
    r$processed <- NULL
    r$process_error <- FALSE
    r$filename <- input$file1$name
    ## Reading File-------------------
    progressSweetAlert(
      session = session, id = "load_progress",
      title = "Reading file",
      total = 3, value = 1, status = "success"
    )

    r$log <- paste("[1] Reading file", input$file1$name, "...")
    tryCatch(
      {
        r$data <- openxlsx::loadWorkbook(input$file1$datapath)
        r$log <- paste(r$log, "\n[1] File Opened!")
      },
      error = function(e){
        r$log <- paste(r$log, "\n[1] ERROR Cannot open file:\n", e)
        r$process_error <- TRUE
        updateProgressBar(session = session, id = "load_progress",
                          status = "danger", value = 2, total = 3)
      },
      warning = function(w){
        r$log <- paste(r$log, "\n[1] Warnings during file opening:\n", w)
        updateProgressBar(session = session, id = "load_progress",
                          status = "warning", value = 2, total = 3)
      }
    )
    ## Checking file correctness -------------------
    updateProgressBar(session = session, id = "load_progress",
                      title = "Checking file", value = 2, total = 3)
    r$log <- paste(r$log, "\n\n[2] Checking file...")
    tryCatch(
      {
        comptage <- read_comptage(r$data)
        enquete <- read_enquete(r$data)
        check_result <- check_evavelo(comptage, enquete)
        if (check_result$error) r$process_error <- TRUE
        r$log <- paste(r$log, check_result$log)
      },
      error = function(e){
        r$log <- paste(r$log, "\n[2] ERROR during file check:\n", e)
        r$process_error <- TRUE
        updateProgressBar(session = session, id = "load_progress",
                          status = "danger", value = 2, total = 3)
      },
      warning = function(w){
        r$log <- paste(r$log, "\n[2] Warnings during file check:\n", w)
        updateProgressBar(session = session, id = "load_progress",
                          status = "warning", value = 2, total = 3)
      }

    )



    ## Correcting category-------------------
    updateProgressBar(session = session, id = "load_progress",
                      title = "Correcting category", value = 3, total = 3)

    if(r$process_error == FALSE){
      r$log <- paste(r$log, "\n\n[3] Correcting Category...")
      tryCatch(
        {
          r$processed <- process_evavelo(r$data)
          r$log <- paste(r$log, "\n[3] Category corrected!")
          updateProgressBar(session = session, id = "load_progress",
                            title = "Category corrected", value = 3, total = 3)
        },
        error = function(e){
          r$log <- paste(r$log, "\n[3] ERROR during category correction:\n", e)
          r$process_error <- TRUE
          updateProgressBar(session = session, id = "load_progress",
                            status = "danger", value = 3, total = 3)
        },
        warning = function(w){
          r$log <- paste(r$log, "\n[3] Warnings during category correction:\n", w)
          updateProgressBar(session = session, id = "load_progress",
                            status = "warning", value = 3, total = 3)
        }
      )
    }
    closeSweetAlert(session = session)


    ## End message------------------------------
    if (r$process_error){
      r$log <- paste(r$log, "\n\n ERROR. File cannot be processed")
      sendSweetAlert(
        session = session,
        title =" Error during process",
        type = "error"
      )
    }else{
      r$log <- paste(r$log, "\n\n OK! You can now download the result")
      sendSweetAlert(
        session = session,
        title =" Calculation completed !",
        type = "success"
      )}


  })

}

## To be copied in the UI
# mod_load_file_ui("load_file_ui_1")

## To be copied in the server
# callModule(mod_load_file_server, "load_file_ui_1")

