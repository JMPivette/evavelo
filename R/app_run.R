#' Run the Shiny Application
#'
#'
#' @export
app_run <- function() {
  shiny::shinyApp(
    ui = app_ui,
    server = app_server
  )
}