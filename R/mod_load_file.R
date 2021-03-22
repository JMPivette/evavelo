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
              placeholder = "Aucun fichier s\u00e9lectionn\u00e9",
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
    r$file_checked <- NULL
    r$mult_quest <- NULL
    r$num_outliers <- NULL
    r$process_error <- FALSE
    r$filename <- stringr::str_remove(input$file1$name, ".xlsx$")


    ## Reading File-------------------
    nb_step = 2
    progressSweetAlert(
      session = session, id = "load_progress",
      title = "Chargement du fichier",
      total = nb_step, value = 1, status = "success"
    )

    r$log <- paste("[1] Lecture du fichier", input$file1$name, "...")
    tryCatch(
      {
        r$data <- openxlsx::loadWorkbook(input$file1$datapath)
        r$log <- paste(r$log, "\n[1] Fichier charg\u00e9!")
      },
      error = function(e){
        r$log <- paste(r$log, "\n[1] ERREUR Impossible d\'ouvrir le fichier:\n", e)
        r$process_error <- TRUE
        updateProgressBar(session = session, id = "load_progress",
                          status = "danger", value = 2, total = nb_step)
      },
      warning = function(w){
        r$log <- paste(r$log, "\n[1] Warnings pendant l\'ouverture du fichier:\n", w)
        updateProgressBar(session = session, id = "load_progress",
                          status = "warning", value = 2, total = nb_step)
      }
    )
    updateProgressBar(session = session, id = "load_progress",
                      title = "Ouverture du fichier", value = 2, total = nb_step)

    ## loading file
    tryCatch(
      withCallingHandlers(
        {r$eva_data <- read_evavelo(r$data)
        r$log <- paste(r$log, "\n[1] Fichier ouvert!")
        },
        message = function(m) r$log<- paste(r$log, m$message)),
      error = function(e){
        r$log <- paste(r$log, "\n[2] ERREUR pendant l\'e chargement\'ouverture du fichier:\n", e)
        r$process_error <- TRUE
        updateProgressBar(session = session, id = "load_progress",
                          status = "danger", value = 2, total = nb_step)
      }
    )


    closeSweetAlert(session = session)

    ## End message------------------------------
    if (r$process_error){
      r$log <- paste(r$log, "\n\n ERREUR. Le fichier ne peut pas \u00eatre trait\u00e9")
      sendSweetAlert(
        session = session,
        title =" Erreur pendant le traitement",
        type = "error"
      )
    }


  })

}

## To be copied in the UI
# mod_load_file_ui("load_file_ui_1")

## To be copied in the server
# callModule(mod_load_file_server, "load_file_ui_1")

