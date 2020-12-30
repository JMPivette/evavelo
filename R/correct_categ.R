
#' Analyze and correct categorie
#'
#' This function takes `comptage` and `enquete` data to compute categorie_corrigee field.
#'
#' @param comptage a data.frame that must contain the following columns: .....
#' @param enquete a data.frame that must contain the following columns: .....
#'
#' @import magrittr
#' @importFrom dplyr mutate filter select left_join starts_with
#' @importFrom rlang .data
#'
#' @return a vector the same length as the number of rows in comptage with the new `categorie` value
#' @export

correct_categ <- function(comptage,
                          enquete) {

# Test input data.frames ----------------------------------------------------------------------

  df_has_cols(comptage,
              c("id_quest", "categorie_visuelle_cycliste", "categorie_breve"))

  df_has_cols(enquete,
              c("id_quest", "categorie", "categorie_corrige",
                "type_sortie", "dms", "km_sortie", "type_trajet",
                "nb_vae", "nb_total_velo", "activites", "activite_motiv")) ## don't forget iti_....

  ## TODO: add test for id_quest check in both df




# Test input for unexpected values ------------------------------------------------------------
 ## TODO






# Combine result to the ouput -----------------------------------------------------------------

  ## Add comptage information to enquete
  enquete <- enquete %>%
    select(.data$id_quest, .data$categorie, .data$categorie_corrige,
           .data$type_sortie, .data$dms, starts_with("iti_"),
           .data$km_sortie, .data$type_trajet, # used for Case 5
           .data$nb_vae, .data$nb_total_velo, .data$activites, ## Used for Case 6 11
           .data$activite_motiv # USed for Case 9 12
    ) %>%
    left_join(select(comptage,
                     .data$id_quest, .data$categorie_visuelle_cycliste),
              by = "id_quest") %>%
    filter(!is.na(.data$categorie_visuelle_cycliste)) ##Delete enquete on non-cyclists (https://github.com/JMPivette/evavelo/discussions/3)

  ## Deal with differences in categorie and categorie_visuelle

  enquete %>%
    filter(.data$categorie != .data$categorie_visuelle_cycliste) %>%
    ## Apply case 1 2 3 4 7 10 algorithm
    correct_itinerant() %>%
    ## Apply case 6 11 algorithm
    correct_spor_lois() %>%
    ## Apply case 9 12 algorithm
    correct_util_lois() %>%
    ## Apply case 5 8
    correct_util_sport()
}


# Individual cases functions ------------------------------------------------------------------



#' Apply categorie_corrige methodology when Itinerant is present in the answer
#'
#' In Chapter 3.1.11 , this corresponds to cases 1 2 3 4 7 10
#'
#' this function can be used inside pipe operator and is compatible with dplyr
#'
#' @param data a data.frame
#'
#' @importFrom rlang .data
#'
#' @return a data.frame the same size of data with updated categorie_corrige values.
#' @export
correct_itinerant <- function(data){

  ## Apply algorithm
  rows_to_update <- data %>%
    dplyr::filter(.data$categorie != .data$categorie_visuelle_cycliste) %>%
    dplyr::filter(.data$categorie == "Itin\u00e9rant" |
                    .data$categorie_visuelle_cycliste == "Itin\u00e9rant") %>%
    dplyr::mutate(iti_any = rowSums(dplyr::across(dplyr::starts_with("iti"),
                                                  ~ !is.na(.x))) != 0,## check if any response from Q25 to Q27. Might change in future methodology version
                  ## Other category than Itinerant
                  other_cat = dplyr::coalesce(
                    dplyr::na_if(.data$categorie, "Itin\u00e9rant"),
                    dplyr::na_if(.data$categorie_visuelle_cycliste, "Itin\u00e9rant"))
    ) %>%
    dplyr::mutate(
      categorie_corrige =
        dplyr::case_when(
          iti_any ~ "Itin\u00e9rant",
          type_sortie == "Plusieurs jours" & dms > 1 ~ "Itin\u00e9rant",
          TRUE ~ other_cat
        )
    )
  ## Update rows
  data %>%
    dplyr::rows_update(select(rows_to_update,
                              .data$id_quest, .data$categorie_corrige),
                       by = "id_quest")

}


#' Apply categorie_corrigee Methodology to decide between Sportif and Loisir
#'
#' In Chapter 3.1.11, this corresponds to cases 6 11
#'
#' this function can be used inside pipe operator and is compatible with dplyr
#'
#' @param data a data.frame
#'
#' @importFrom rlang .data
#'
#' @return a data.frame the same size of data with updated categorie_corrige values.
#' @export

correct_spor_lois <- function(data){
  rows_to_update <- data %>%
    dplyr::filter(.data$categorie == "Sportif" & .data$categorie_visuelle_cycliste =="Loisir" |
                    .data$categorie == "Loisir" & .data$categorie_visuelle_cycliste =="Sportif"
    ) %>%
    dplyr::mutate(
      autre_activite = !is.na(.data$activites) & .data$activites != "Aucune",
      ## VAE definition might change in future version of Methodology
      vae = !is.na(.data$nb_vae) & .data$nb_vae == .data$nb_total_velo
    ) %>%
    dplyr::mutate(
      categorie_corrige = dplyr::case_when(
        is.na(km_sortie) ~ "Loisir",
        km_sortie > 50 & autre_activite == FALSE & vae == FALSE ~ "Sportif",
        TRUE ~ "Loisir")
    )

  data %>%
    dplyr::rows_update(select(rows_to_update,
                              .data$id_quest, .data$categorie_corrige),
                       by = "id_quest")

}

#' Apply categorie_corrigee Methodology to decide between Utilitaire and Loisir
#'
#' In Chapter 3.1.11, this corresponds to cases 9 12
#'
#' this function can be used inside pipe operator and is compatible with dplyr
#'
#' @param data a data.frame
#'
#' @importFrom rlang .data
#'
#' @return a data.frame the same size of data with updated categorie_corrige values.
#' @export
correct_util_lois <- function(data){

  rows_to_update <- data %>%
    dplyr::filter(.data$categorie == "Utilitaire" & .data$categorie_visuelle_cycliste == "Loisir" |
                    .data$categorie == "Loisir" & .data$categorie_visuelle_cycliste == "Utilitaire"
    ) %>%
    dplyr::mutate(
      categorie_corrige = dplyr::case_when(
        is.na(activite_motiv) ~ categorie_visuelle_cycliste,
        stringr::str_detect(activite_motiv, "but") ~ "Utilitaire",
        TRUE ~"Loisir"
      ))
  ## Update rows
  data %>%
    dplyr::rows_update(select(rows_to_update,
                              .data$id_quest, .data$categorie_corrige),
                       by = "id_quest")
}


#' Apply categorie_corrigee Methodology to decide between Utilitaire and Sportif
#'
#' In Chapter 3.1.11, this corresponds to cases 5 8
#'
#' this function can be used inside pipe operator and is compatible with dplyr
#'
#' @param data a data.frame
#'
#' @importFrom rlang .data
#'
#' @return a data.frame the same size of data with updated categorie_corrige values.
#' @export
correct_util_sport <- function(data){
  # Cas 5
  cas_5 <- data %>%
    dplyr::filter(.data$categorie == "Utilitaire" & .data$categorie_visuelle_cycliste =="Sportif") %>%
    dplyr::mutate(
      categorie_corrige =
        dplyr::case_when(
          km_sortie <= 30 & type_trajet == "Aller-retour" ~ "Utilitaire",
          TRUE ~ "Sportif"
        )
    ) %>%
    dplyr::select(.data$id_quest, .data$categorie_corrige)

  # Cas 8
  cas_8 <- data %>%
    dplyr::filter(.data$categorie == "Sportif" & .data$categorie_visuelle_cycliste =="Utilitaire") %>%
    dplyr::mutate(vae = !is.na(.data$nb_vae) & .data$nb_vae == .data$nb_total_velo) %>%
    dplyr::mutate(
      categorie_corrige = ifelse(
        .data$km_sortie > 50 & .data$vae == FALSE,
        "Sportif",
        "Utilitaire"
      )
    ) %>%
    dplyr::select(.data$id_quest, .data$categorie_corrige)

  ## Update rows
  data %>%
    dplyr::rows_update(rbind(cas_5, cas_8),
                       by = "id_quest")


}
