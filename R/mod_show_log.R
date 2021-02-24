#' show_log UI Function
#'
#' This module is used to show logs stored in r$log
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
mod_show_log_ui <- function(id){
  ns <- NS(id)
  tagList(
    verbatimTextOutput(ns("file_name")) %>%
      withSpinner()
  )
}

#' show_log Server Function
#'
#' @noRd
mod_show_log_server <- function(input, output, session, r){
  ns <- session$ns

  ## Display logs on screen
  output$file_name <- renderText(r$log)

}

## To be copied in the UI
# mod_show_log_ui("show_log_ui_1")

## To be copied in the server
# callModule(mod_show_log_server, "show_log_ui_1")

