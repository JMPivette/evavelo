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
      downloadButton(ns("download_light"), "T\u00e9l\u00e9charger les colonnes modifi\u00e9es")
    ),
    shinyjs::disabled(
      downloadButton(ns("download_full"), "T\u00e9l\u00e9charger le fichier entier (experimental)")
    ),
    br(),br(),
    downloadButton(ns("download_logs"), "T\u00e9l\u00e9charger les logs")
  )
}

#' load_file Server Function
#'
#' @noRd
mod_download_result_server <- function(input, output, session, r){
  ns <- session$ns

  output$download_light <- downloadHandler(
    filename = function() r$filename,
    content = function(file) {
      openxlsx::write.xlsx(r$processed, file)
    }
  )

  output$download_full <- downloadHandler(
    filename = function() r$filename,
    content = function(file) {
      sendSweetAlert(
        session = session,
        title = "Creating output file.",
        text = "Please wait...",
        btn_labels = NA,
        closeOnClickOutside = FALSE
      )
      evavelo::update_wb(r$data, r$processed)
      openxlsx::saveWorkbook(r$data, file)
      closeSweetAlert()
    }
  )

  output$download_logs <- downloadHandler(
    filename = "log.txt",
    content = function(file) {
      cat(r$log, file = file)
    }
  )


  observeEvent(r$processed,{
    if(is.null(r$processed)){
      shinyjs::disable("download_light")
      shinyjs::disable("download_full")
    }else {
      shinyjs::enable("download_light")
      shinyjs::enable("download_full")
    }
  },
  ignoreNULL = FALSE)
}

## To be copied in the UI
# mod_download_result_ui("download_result_ui_1")

## To be copied in the server
# callModule(mod_download_result_server, "download_result_ui_1")

