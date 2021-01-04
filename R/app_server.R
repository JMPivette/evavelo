#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # List the first level callModules here
  options(shiny.maxRequestSize=30*1024^2) ## Allow file upload up to 30MB To be adjusted if needed
  callModule(mod_load_file_server, "load_file_ui_1", r)

  observeEvent(input$browser, {
    browser()
  })

  ## Small r strategy
  r <- reactiveValues()

}
