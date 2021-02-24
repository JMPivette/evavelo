#' load_file UI Function
#'
#' This module is used in the user interface to load and process files with information stored in r$log
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
    fileInput(ns("file1"), "Choisissez un fichier XLSX",
              buttonLabel = "Parcourir...",
              placeholder = "Aucun fichier sélectionné",
              accept = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"),
  )
}

#' load_file Server Function
#'
#' @noRd
mod_load_file_server <- function(input, output, session, r){
  ns <- session$ns

  ## Import and treat files, update log message and display a modal information for user
  observeEvent(input$file1,{
    ## Initialize values
    r$eva_data <- NULL
    r$processed <- NULL
    r$process_error <- FALSE
    r$filename <- paste0(stringr::str_remove(input$file1$name, ".xlsx$"),
                         "_scan.xlsx")
    ## Reading File-------------------
    progressSweetAlert(
      session = session, id = "load_progress",
      title = "Reading file",
      total = 4, value = 1, status = "success"
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
                          status = "danger", value = 2, total = 4)
      },
      warning = function(w){
        r$log <- paste(r$log, "\n[1] Warnings during file opening:\n", w)
        updateProgressBar(session = session, id = "load_progress",
                          status = "warning", value = 2, total = 4)
      }
    )

    ## Checking file correctness -------------------
    updateProgressBar(session = session, id = "load_progress",
                      title = "Checking cities", value = 2, total = 4)
    r$log <- paste(r$log, "\n\n[2] Checking file...\n")
    ## loading and checking  cities.....
    tryCatch(
      withCallingHandlers(
        r$eva_data <- read_evavelo(r$data),
        message = function(m) r$log<- paste(r$log, m$message)),
      error = function(e){
        r$log <- paste(r$log, "\n[2] ERROR during file loading:\n", e)
        r$process_error <- TRUE
        updateProgressBar(session = session, id = "load_progress",
                          status = "danger", value = 2, total = 4)
      }
    )
    updateProgressBar(session = session, id = "load_progress",
                      title = "Checking file", value = 3, total = 4)
    ## Checking relationships between worksheets.....
    tryCatch(
      {
        check_result <- check_evavelo(r$eva_data)
        if (check_result$error) r$process_error <- TRUE
        r$log <- paste(r$log, check_result$log)
      },
      error = function(e){
        r$log <- paste(r$log, "\n[2] ERROR during file check:\n", e)
        r$process_error <- TRUE
        updateProgressBar(session = session, id = "load_progress",
                          status = "danger", value = 3, total = 4)
      },
      warning = function(w){
        r$log <- paste(r$log, "\n[2] Warnings during file check:\n", w)
        updateProgressBar(session = session, id = "load_progress",
                          status = "warning", value = 3, total = 4)
      }

    )

    ## Correcting category-------------------
    updateProgressBar(session = session, id = "load_progress",
                      title = "Correcting category", value = 4, total = 4)

    if(r$process_error == FALSE){
      r$log <- paste(r$log, "\n\n[3] Correcting Category...\n")
      tryCatch(
        {
          withCallingHandlers({
            r$processed <- process_evavelo(r$eva_data)},
            message = function(m) r$log<- paste(r$log, m$message))
          updateProgressBar(session = session, id = "load_progress",
                            title = "Category corrected", value = 4, total = 4)
        },
        error = function(e){
          r$log <- paste(r$log, "[3] ERROR during category correction:\n", e)
          r$process_error <- TRUE
          updateProgressBar(session = session, id = "load_progress",
                            status = "danger", value = 4, total = 4)
        },
        warning = function(w){
          r$log <- paste(r$log, "[3] Warnings during category correction:\n", w$message)
          updateProgressBar(session = session, id = "load_progress",
                            status = "warning", value = 4, total = 4)
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
      r$log <- paste(r$log, "\n[3] Category corrected!\n\n OK! You can now download the result")
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

