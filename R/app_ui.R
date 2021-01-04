#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    # List the first level UI elements here
    fluidPage(
      h1("Eva-Velo"),
      mod_load_file_ui("load_file_ui_1"),
      actionButton("browser", "Browser")
    )
  )
}
