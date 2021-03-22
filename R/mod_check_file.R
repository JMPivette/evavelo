#' check_file UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#'
mod_check_file_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    actionButton(ns("check_file"), "V\u00e9rifier Fichier"),br(),
    br(),
    actionButton(ns("check_quest_multiple"), "Chercher questionnaires multiples"),br(),
    shinyjs::hidden(downloadLink(ns("download_mult_quest"), "T\u00e9l\u00e9charger questionnaires multiples")),
    br(),
    actionButton(ns("check_geocode"), "G\u00e9ocodage des villes"),
    br(),br(),
    actionButton(ns("check_outliers"), "Chercher valeurs num\u00e9riques anormales"),br(),
    shinyjs::hidden(downloadLink(ns("download_outliers"), "T\u00e9l\u00e9charger valeurs anormales")),
  )
}

#' check_file Server Function
#'
#' @noRd
mod_check_file_server <- function(input, output, session, r){
  ns <- session$ns

# Observer on action buttons ------------------------------------------------------------------


  observeEvent(input$check_file, {
    r$log <- paste0(r$log, "\n\n")
    tryCatch(
      {
        check_result <- check_evavelo(r$eva_data)
        ##if (check_result$error) r$process_error <- TRUE
        r$file_checked <- !check_result$error
        r$log <- paste0(r$log, check_result$log)
      },
      error = function(e){
        r$log <- paste0(r$log, " ERREUR pendant la v\u00e9rification du fichier:\n", e)
        r$process_error <- TRUE
      },
      warning = function(w){
        r$log <- paste0(r$log, " Warnings pendant la v\u00e9rification du fichier:\n", w)
      })
  })

  observeEvent(input$check_quest_multiple, {
    r$log <- paste0(r$log, "\n\n")
    tryCatch(
      {
        check_enq_mult <- check_similar_enquete(r$eva_data$enquete)
        r$log <- paste0(r$log, check_enq_mult$log)
        r$mult_quest <- check_enq_mult$simil_enq
        #shinyjs::show("download_mult_quest")
      },
      error = function(e){
        r$log <- paste0(r$log, " ERREUR pendant la recherche de questionnaires multiples:\n", e)
      },
      warning = function(w){
        r$log <- paste0(r$log, " Warnings pendant la recherche de questionnaires multiples:\n", w)
      })
  })

  observeEvent(input$check_outliers, {
    r$log <- paste0(r$log,
                    "\n\nRecherche de valeurs num\u00e9riques anormales...\n",
                    "--------------------------------------------\n")
    if(is.null(r$processed)){
      r$log <- paste0(r$log,
                      "Utilisation de 'categorie_corrige' du fichier xlsx\n",
                      "Si vous voulez calculer 'categorie_corrige', cliquez sur 'Traiter Fichier' avant de faire ce traitement\n")
    } else {
      r$log <- paste0(r$log,
                      "Utilisation de 'categorie_corrige' recalculé lors du traitement du fichier\n",
                      "Si vous voulez utiliser 'categorie_corrige' du fichier xlsx, rechargez le dans l'application.\n")
    }

    tryCatch(
      {browser()
        r$num_outliers <- check_num_outliers(r$eva_data,
                                             categorie_corrige = r$processed$enquetes_post_traitement$categorie_corrige)
        r$log <- paste0(r$log, "Valeurs num\u00e9riques anormales disponible au t\u00e9l\u00e9chargement!\n")
        #shinyjs::show("download_outliers")
      },
      error = function(e){
        r$log <- paste0(r$log, " ERREUR pendant la recherche de valeurs num\u00e9riques anormales:\n", e)
      },
      warning = function(w){
        r$log <- paste0(r$log, " Warnings pendant la recherche de valeurs num\u00e9riques anormales:\n", w)
      })

  })

  observeEvent(input$check_geocode, {
    r$log <- paste0(r$log,
                    "\n\n")
    sendSweetAlert(
      session = session,
      title = "G\u00e9ocodage en cours...",
      text = "Le g\u00e9ocodage des villes peut prendre du temps.\nMerci d\'\u00eatre patient.",
      type = "info",
      html = TRUE,
      btn_labels = NA,
      closeOnClickOutside = FALSE
    )
    tryCatch(
      {
        withCallingHandlers({
          r$eva_data <- geocode_evavelo(r$eva_data)},
          message = function(m) r$log<- paste(r$log, m$message))
      },
      error = function(e){
        r$log <- paste0(r$log, " ERREUR pendant le g\u00e9ocodage:\n", e)
      },
      warning = function(w){
        r$log <- paste0(r$log, " Warnings pendant le g\u00e9ocodage", w)
      })
    closeSweetAlert()

  })


# Hide/Show downloads link --------------------------------------------------------------------

  observeEvent(r$mult_quest,{
    if(is.null(r$mult_quest))
      shinyjs::hide("download_mult_quest")
    else
      shinyjs::show("download_mult_quest")
  },
  ignoreNULL = FALSE)

  observeEvent(r$num_outliers,{
    if(is.null(r$num_outliers))
      shinyjs::hide("download_outliers")
    else
      shinyjs::show("download_outliers")
  },
  ignoreNULL = FALSE)


# Download Handlers ---------------------------------------------------------------------------

output$download_mult_quest <- downloadHandler(
  filename = function() paste0(r$filename, "_mult_quest.xlsx"),
  content = function(file){
    openxlsx::write.xlsx(r$mult_quest, file)
  }
)

output$download_outliers <- downloadHandler(
  filename = function() paste0(r$filename, "_anomaly.xlsx"),
  content = function(file){
    openxlsx::write.xlsx(r$num_outliers, file)
  }
)

}

## To be copied in the UI
# mod_check_file_ui("check_file_ui_1")

## To be copied in the server
# callModule(mod_check_file_server, "check_file_ui_1")

