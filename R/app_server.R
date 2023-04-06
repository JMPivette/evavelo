#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  options(shiny.maxRequestSize=500*1024^2) ## Allow file upload up to 500MB To be adjusted if needed

  ## Small r strategy
  r <- reactiveValues()
  # List the first level callModules here
  callModule(mod_load_file_server, "load_file_ui_1", r = r)
  callModule(mod_download_result_server, "download_result_ui_1", r = r)
  callModule(mod_show_log_server, "show_log_ui_1", r = r)
  callModule(mod_check_file_server, "check_file_ui_1", r = r)
  # callModule(mod_process_file_server, "process_file_ui_1", r = r)
  callModule(mod_visualize_classification_server, "visualize_classification_ui_1", r = r)


  observeEvent(input$browser, {
    browser()
  })

  observeEvent(r$clust$data, {
    updateTabsetPanel(inputId = "tabs",
                      selected = "cluster")

  })

}
