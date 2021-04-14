#' process_file UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#'
mod_process_file_ui <- function(id){
  ns <- NS(id)
  tagList(
    actionButton(ns("process_file"), "Traiter fichier")

  )
}

#' process_file Server Function
#'
#' @noRd
mod_process_file_server <- function(input, output, session, r){
  ns <- session$ns

  observeEvent(input$process_file, {

    if(!is.null(r$file_checked) && r$file_checked == FALSE){
      sendSweetAlert(
        session = session,
        title = "Fichier incorrect",
        text = "Le fichier a rencontr\u00e9 des erreurs lors de l\'\u00e9tape de v\u00e9rification",
        type = "error"
      )
    } else {
      r$log <- paste0(r$log,
                      "\n\n Traitement du fichier...\n",
                      "------------------------------\n")
      check_to_do <- is.null(r$file_checked) ## In case File check hasn't been done previously
      tryCatch(
        {
          withCallingHandlers({
            r$processed <- process_evavelo(r$eva_data, check = check_to_do)},
            message = function(m) r$log<- paste(r$log, m$message),
            warning = function(w){
              r$log <- paste(r$log, " Warnings pendant le traitement du fichier:\n", w$message)
            })
        },
        error = function(e){
          r$log <- paste(r$log, " ERREUR pendant le traitement du fichier:\n", e)
          r$process_error <- TRUE
        }
      )
    }

  })

}

## To be copied in the UI
# mod_process_file_ui("process_file_ui_1")

## To be copied in the server
# callModule(mod_process_file_server, "process_file_ui_1")

