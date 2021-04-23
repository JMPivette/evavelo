#' visualize_classification UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#'
mod_visualize_classification_ui <- function(id){
  ns <- NS(id)
  tagList(
    br(),
    numericInput(ns("k_clust"), "Nombre de groupes", value = 4, step = 1, min = 1),
    downloadButton(ns("downl_data"), "T\u00e9l\u00e9charger les donn\u00e9es"),
    br(),
    plotOutput(ns("dendogram"))
  )
}

#' visualize_classification() Server Function
#'
#' @noRd
mod_visualize_classification_server <- function(input, output, session, r){
  ns <- session$ns

  data_to_analyse <- reactive({ ## Remove NAs from predicate
    req(r$clust$data)
    data_to_analyse <- r$clust$data %>%
      tidyr::drop_na(dplyr::starts_with("pred"))

    diff_na <- nrow(r$clust$data) - nrow(data_to_analyse)
    if(diff_na != 0){
      shinyWidgets::show_alert(
        title = paste0("Impossible d\'analyser ", diff_na, " site(s)."),
        type = "warning"
      )
    }
    data_to_analyse
  })


  clust <- reactive({
    pred <- data_to_analyse() %>%
      dplyr::select(dplyr::starts_with("pred")) %>%
      as.data.frame()
    rownames(pred) <- paste0(data_to_analyse()$name,"(",data_to_analyse()$id_site, ")")

    pred %>%
      dist() %>%
      hclust(method = "ward.D2")
  })

  output$dendogram <- renderPlot({
    req(r$clust$data)
    validate(
      need(input$k_clust %% 1 == 0, "Utilisez une valeur enti\u00e8re")
      # need(data_to_analyse(), "Pas de donn\u00e9es \u00e0 analyser")
    )
    clust() %>%
      factoextra::fviz_dend(cex = 0.8,
                            k = input$k_clust,
                            rect = TRUE,
                            horiz = TRUE) +
      ggplot2::labs(title = NULL) +
      ggplot2::theme(axis.title.x=ggplot2::element_blank(),
                     axis.text.x=ggplot2::element_blank(),
                     axis.ticks.x=ggplot2::element_blank())
  },
  height = function(){ifelse(nrow(r$clust$data) < 30,
                             600,
                             nrow(r$clust$data)*20)})

  final_result <- reactive({ ## Reinclude sites that haven't been analyzed
    group_affectation <- dplyr::bind_cols(
      id_channel = data_to_analyse()$id_channel,
      group = cutree(clust(), k = input$k_clust))

    r$clust$data %>%
      dplyr::select(-dplyr::any_of(c("n_missing_days"))) %>%
      dplyr::left_join(group_affectation,by = "id_channel")
  })


  output$downl_data <- downloadHandler(
    filename = function() paste0(r$filename,"_cluster.xlsx"),
    content = function(file){
      openxlsx::write.xlsx(final_result(), file)
    }
  )
}

## To be copied in the UI
# mod_visualize_classification_ui("visualize_classification_ui_1")

## To be copied in the server
# callModule(mod_visualize_classification_server, "visualize_classification_ui_1")

