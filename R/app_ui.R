#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    #addResourcePath('img', system.file('app/img', package = 'evavelo')),
    # List the first level UI elements here
    fluidPage(
      #theme = bslib::bs_theme(version = 4, bg = "black", fg = "white"),
      theme = bslib::bs_theme(bootswatch = "yeti"),
      shinyjs::useShinyjs(),
      fluidPage(fluidRow(
        column(2,
               img(src="img/aob.jpg", width = "100%"),
               style = "margin :  auto;"),
        column(2, img(src = "img/vnt.png", width = "100%"),
               style = "margin :  auto;"),
        column(8)
      )),
      titlePanel("EVA-Scan : outil de traitement des données collectées selon la méthode ÉVA-VÉLO"),
      sidebarLayout(
        sidebarPanel(mod_load_file_ui("load_file_ui_1"),
                     mod_download_result_ui("download_result_ui_1")),
        mainPanel(mod_show_log_ui("show_log_ui_1"))
      ),

      br(),br(),

      ##Debuging button. To show it, open your web browser JavaScript console
      ##And run $('#browser').show();
      actionButton("browser", "Browser"),
      tags$script("$('#browser').hide();")
    )
  )
}
