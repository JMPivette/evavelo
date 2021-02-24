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
      titlePanel("EVA-Scan : outil de traitement des donn\u00e9es collect\u00e9es selon la m\u00e9thode \u00c9VA-V\u00c9LO"),
      em("Application d\u00e9velopp\u00e9e dans le cadre du projet Interreg AtlanticOnBike et cofinanc\u00e9 par l\u2019Union Europ\u00e9enne"),
      sidebarLayout(
        sidebarPanel(
          mod_load_file_ui("load_file_ui_1"),
          mod_download_result_ui("download_result_ui_1"),
          br(),br(),
          p("D\u00e9veloppements r\u00e9alis\u00e9s sur la base de la ",
            tags$a( "m\u00e9thode v1.0",
                    href="https://www.velo-territoires.org/resource/eva-velo/",
                    target="_blank"),
            br(),
            "Version EVA-Scan: ", packageDescription("evavelo")$Version,
            tags$a(" (code) ",
                   href = packageDescription("evavelo")$URL,
                   target = "_blank")),
        ),
        mainPanel(mod_show_log_ui("show_log_ui_1"))
      ),

      br(),br(),

      ##Debuging button. To show it, open your web browser JavaScript console
      ##And run $('#browser').show();
      actionButton("browser", "Browser"),
      tags$script("$(\'#browser\').hide();")
    )
  )
}
