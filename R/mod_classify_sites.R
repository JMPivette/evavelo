#' classify_sites UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#'
mod_classify_sites_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinyjs::hidden(
      actionButton(ns("classify_sites"), "Grouper sites similaires")
    )

  )
}

#' classify_sites Server Function
#'
#' @noRd
mod_classify_sites_server <- function(input, output, session, r){
  ns <- session$ns
  r$clust <- reactiveValues()
  observeEvent(input$classify_sites, {
    r$log <- paste0(r$log,
                    "\n\n Classification des sites...\n",
                    "------------------------------\n")
    r$clust$data <- r$eva_data$comptages_automatiques

  })

  observeEvent(r$eva_data$comptages_automatiques,{
    if(is.null(r$eva_data$comptages_automatiques)){
      shinyjs::hide("classify_sites")
    }else {
      shinyjs::show("classify_sites")
    }
  },
  ignoreNULL = FALSE)

}

## To be copied in the UI
# mod_classify_sites_ui("classify_sites_ui_1")

## To be copied in the server
# callModule(mod_classify_sites_server, "classify_sites_ui_1", r = r)

