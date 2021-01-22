
#' Analyze and correct categorie
#'
#' This function takes `comptage` and `enquete` data to compute categorie_corrigee field.
#'
#' @param comptage a data.frame that must contain the following columns: .....
#' @param enquete a data.frame that must contain the following columns: .....
#'
#' @import magrittr
#' @importFrom dplyr mutate filter select left_join starts_with coalesce rows_update rename
#' @importFrom rlang .data
#'
#' @return a vector the same length as the number of rows in comptage with the new `categorie` value
#' @export

correct_categ <- function(comptage,
                          enquete) {

# Test input data.frames ----------------------------------------------------------------------

  df_has_cols(comptage,
              comptage_colnames)

  df_has_cols(enquete,
              enquete_colnames)

  ## TODO: add test for id_quest check in both df
  ## TODO: test for duplicated id_quest values.

# Test input for unexpected values ------------------------------------------------------------
 ## TODO

# Combine result to the output -----------------------------------------------------------------

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
              by = "id_quest")

  ## Deal with differences in categorie and categorie_visuelle

  cat_to_correct <- enquete %>%
    ##Delete enquete on non-cyclists (https://github.com/JMPivette/evavelo/discussions/3)
    filter(!is.na(.data$categorie_visuelle_cycliste)) %>%
    filter(.data$categorie != .data$categorie_visuelle_cycliste) %>%
    ## Apply case 1 2 3 4 7 10 algorithm
    correct_itinerant() %>%
    ## Apply case 6 11 algorithm
    correct_spor_lois() %>%
    ## Apply case 9 12 algorithm
    correct_util_lois() %>%
    ## Apply case 5 8
    correct_util_sport() %>%
    select(.data$id_quest,
           .data$categorie_corrige)

  ## Update comptage values (categorie_visuelle_cycliste_corrige)
  comptage <- comptage %>%
    select(.data$id_quest, .data$categorie_visuelle_cycliste, .data$categorie_breve) %>%
    left_join(cat_to_correct, ## TODO add left join since we have NAs in id_quest
              by = "id_quest"
    ) %>%
    dplyr::transmute(.data$id_quest,
                     categorie_visuelle_cycliste_corrige = coalesce(
                       .data$categorie_corrige,
                       .data$categorie_breve, ## chapter 3.1.12.2 categorie_breve override categorie_visuelle_cycliste
                       .data$categorie_visuelle_cycliste)
    )


  ## Update enquete values (categorie_corrige)
  enquete <- enquete %>%
    select(.data$id_quest, .data$categorie_corrige) %>%
    rows_update(cat_to_correct, by = "id_quest")

  ## Return a list with all information.
  list(comptage = comptage,
       enquete = enquete)
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
    ## check coherence of itinerant answers ('coherent' column)
    add_coherence() %>%
    dplyr::mutate(
      ## Other category than Itinerant
      other_cat = dplyr::coalesce(
        dplyr::na_if(.data$categorie, "Itin\u00e9rant"),
        dplyr::na_if(.data$categorie_visuelle_cycliste, "Itin\u00e9rant"))
    ) %>%
    dplyr::mutate(
      categorie_corrige =
        dplyr::case_when(
          coherent ~ "Itin\u00e9rant",
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



#' Add a column with 'coherence' information to a data.frame
#'
#' Internal function that calls is_iti_coherent
#'
#' data should have the following columns: dms, iti_km_voyage, iti_experience,
#' iti_depart_itineraire, iti_arrivee_itineraire,
#' iti_depart_initial, iti_arrivee_final
#'
#' @param data a data.frame. Some columns are mandatory. See details for more information
#' @param col_name name of the new column created
#'
#' @importFrom rlang .data
#'
#' @return 'data' data.frame with a new logical column name after 'col_name'

add_coherence <- function(data,
                          col_name = "coherent") {
  data %>%
    dplyr::mutate(
      ## check coherence of itinerant answers
      !!col_name := is_iti_coherent(.data$dms, .data$iti_km_voyage, .data$iti_experience,
                                    .data$iti_depart_itineraire, .data$iti_arrivee_itineraire,
                                    .data$iti_depart_initial, .data$iti_arrivee_final)
    )
}

#' Helper function to control "coherence" of specific "itinerance" answers:
#'
#'  https://github.com/JMPivette/evavelo/discussions/7
#'
#' @param dms
#' @param iti_km_voyage
#' @param iti_experience
#' @param iti_depart_itineraire
#' @param iti_arrivee_itineraire
#' @param ... other answers to iti_* questions
#'
#' @return a boolean vector indicating if answer is coherent
is_iti_coherent <- function (dms,
                             iti_km_voyage,
                             iti_experience,
                             iti_depart_itineraire,
                             iti_arrivee_itineraire,...){
  ## Check distance
  coher_dist <-  iti_km_voyage/dms
  coher_dist[is.na(coher_dist)] <- 0 # Remove NA
  coher_dist <- coher_dist > 40

  ## Check iti_depart_itineraire & iti_arrivee_itineraire
  coher_commune <- !is.na(iti_depart_itineraire) & !is.na(iti_arrivee_itineraire)
  # TODO Create a function that contains names of communes

  ## Check iti_other has all answers (no NAs)
  #(We don't need iti_depart_itineraire and iti_arrivee_itineraire that are already tested)
  coher_iti_other <- lapply(list(...), is.na)
  coher_iti_other <- Reduce(`+`, coher_iti_other) == 0
  ## Check that we have at least 2 answers out of three
  coher_2_3 <- ((!is.na(iti_km_voyage)) + coher_iti_other + (!is.na(iti_experience))) >= 2

  return(coher_dist | (coher_commune & coher_2_3))
}
