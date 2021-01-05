#' download_result UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#'
mod_download_result_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    shinyjs::disabled(
      downloadButton(ns("download"), "Download Result")
    )
  )
}

#' load_file Server Function
#'
#' @noRd
mod_download_result_server <- function(input, output, session, r){
  ns <- session$ns

  output$download <- downloadHandler(
    filename = function() r$filename,
    content = function(file) {
      openxlsx::write.xlsx(r$processed, file)
    }
  )

  observeEvent(r$processed,{
    if(is.null(r$processed))
      shinyjs::disable("download")
    else
      shinyjs::enable("download")
  },
  ignoreNULL = FALSE)
}

## To be copied in the UI
# mod_download_result_ui("download_result_ui_1")

## To be copied in the server
# callModule(mod_download_result_server, "download_result_ui_1")

