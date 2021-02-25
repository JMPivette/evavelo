#' Run the Shiny Application
#'
#'
#' @export
app_run <- function() {
  # Supress Shiny's auto-load behaviour
  old <- options(shiny.autoload.r = FALSE)
  on.exit(options(old), add = TRUE)

  addResourcePath('img', system.file('app/img', package = 'evavelo'))

  shiny::shinyApp(
    ui = app_ui,
    server = app_server
  )
}
