
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


# Test input for unexpected values ------------------------------------------------------------
 ## TODO

# Combine result to the output -----------------------------------------------------------------
  ## Add `comptage` information to enquete
  enquete <- enquete %>%
    select(.data$id_quest, .data$categorie, .data$categorie_corrige,
           .data$type_sortie, .data$dms, starts_with("iti_"),
           .data$km_sortie, .data$type_trajet, # used for Case 5
           .data$nb_vae, .data$nb_total_velo, .data$activites, ## Used for Case 6 11
           .data$activite_motiv, .data$activites, .data$activites_aucune # USed for Case 9 12
    ) %>%
    mutate(main_id_quest = radical_quest(.data$id_quest)) %>% # Deal with multiple quest by group
    left_join(select(comptage,
                     .data$id_quest, .data$categorie_visuelle_cycliste),
              by = c("main_id_quest" = "id_quest"))

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
    select(.data$main_id_quest,
           .data$id_quest,
           .data$categorie_corrige,
           .data$categorie)
  message("Correction de categories pour ", nrow(cat_to_correct), " questionnaires ....")

## Check where decision couldn't be made and replace with answer from enquete.

  no_decision <- cat_to_correct %>%
    dplyr::filter(is.na(.data$categorie_corrige)) %>%
    dplyr::pull(.data$id_quest)

  nb_no_decision <- length(no_decision)
  if(nb_no_decision != 0){
    warning(
      "Il n'a pas ete possible de corriger les categories de ",
      nb_no_decision,
      " questionnaire(s).\nLa categorie du declarant sera utilisee:\n\t",
      paste(no_decision, collapse = ", "),
      call. = FALSE, immediate. = TRUE
    )
  }

  cat_to_correct <- cat_to_correct %>%
    mutate(categorie_corrige = coalesce(.data$categorie_corrige,
                                        .data$categorie)) %>%
    select(.data$main_id_quest,
           .data$id_quest,
           .data$categorie_corrige)


  ## Check for multiple "categorie_corrige" in the same group
  quest_multiple_cat <- cat_to_correct %>%
    dplyr::distinct(.data$main_id_quest, .data$categorie_corrige) %>%
    dplyr::count(.data$main_id_quest) %>%
    dplyr::filter(.data$n>1) %>%
    dplyr::pull(.data$main_id_quest)
  if(length(quest_multiple_cat) != 0){
    warning(
      "Les questionnaires multiples suivants ont plusieurs valeurs de categorie corrigees:\n\t",
      quest_multiple_cat,
      call. = FALSE
            )
  }


  ## Update comptage values (categorie_visuelle_cycliste_corrige)
  comptage <- comptage %>%
    select(.data$id_quest, .data$categorie_visuelle_cycliste, .data$categorie_breve) %>%
    left_join(## use left join since we have NAs in id_quest
      cat_to_correct %>%
        dplyr::select(.data$main_id_quest, .data$categorie_corrige) %>%
        dplyr::group_by(.data$main_id_quest) %>%
        dplyr::slice_head(), ## Remove multiple cat per questionary to avoid adding rows
      by = c("id_quest" = "main_id_quest")
    ) %>%
    dplyr::transmute(
      .data$id_quest,
      categorie_visuelle_cycliste_corrige = dplyr::coalesce(
        .data$categorie_corrige,
        .data$categorie_breve, ## chapter 3.1.12.2 categorie_breve override categorie_visuelle_cycliste
        .data$categorie_visuelle_cycliste)
    ) %>%
    dplyr::mutate( ## Remove answers in case of divergent answers in multiple questionary
      categorie_visuelle_cycliste_corrige = dplyr::if_else(
        .data$id_quest %in% quest_multiple_cat,
        NA_character_,
        .data$categorie_visuelle_cycliste_corrige)
    )

  ## Update enquete values (categorie_corrige)

  enquete <- enquete %>%
    dplyr::transmute(.data$id_quest,
                     categorie_corrige = .data$categorie) %>% ## initialize with response from cyclist
    dplyr::rows_update(select(cat_to_correct, -.data$main_id_quest),
                       by = "id_quest")

  ## Return a list with all information.
  list(comptages_man_post_traitements = comptage,
       enquetes_post_traitement = enquete)
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
          is.na(type_sortie) & is.na(dms) ~ NA_character_,
          is.na(dms) & (type_sortie != "Plusieurs jours" | !coherent) ~ NA_character_,
          is.na(type_sortie) & dms > 1 & !coherent ~ NA_character_,
          type_sortie == "Plusieurs jours" & (dms > 1 | coherent) ~ "Itin\u00e9rant",
          dms <= 1 ~ other_cat,
          coherent ~ "Itin\u00e9rant",
          iti_km_voyage/dms > 40 ~ "Itin\u00e9rant",
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
      autre_activite = .data$activites_aucune == 0,
      ## VAE definition might change in future version of Methodology
      vae = .data$nb_vae == .data$nb_total_velo
    ) %>%
    dplyr::mutate(
      categorie_corrige = dplyr::case_when(
        vae ~ "Loisir",
        km_sortie > 50 & !autre_activite  ~ "Sportif",
        km_sortie > 50 & autre_activite ~ "Loisir",
        km_sortie <= 50 ~ "Loisir",
        TRUE ~ NA_character_)
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
      no_activity = is.na(.data$activites) | .data$activites_aucune == 1
    ) %>%
    dplyr::mutate(
      categorie_corrige = dplyr::case_when(
        no_activity ~ categorie,
        stringr::str_detect(activite_motiv, "but") ~ "Utilitaire",
        stringr::str_detect(activite_motiv, "occasion") ~ "Loisir",
        TRUE ~ NA_character_
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
  # https://github.com/JMPivette/evavelo/issues/48
  cas_5 <- data %>%
    dplyr::filter(.data$categorie == "Utilitaire" & .data$categorie_visuelle_cycliste =="Sportif") %>%
    dplyr::mutate(
      categorie_corrige =
        dplyr::case_when(
          is.na(km_sortie) ~ NA_character_,
          km_sortie <= 30 & (is.na(type_trajet) | type_trajet == "Aller-retour") ~ "Utilitaire",
          TRUE ~ "Sportif"
        )
    ) %>%
    dplyr::select(.data$id_quest, .data$categorie_corrige)

  # Cas 8
  # https://github.com/JMPivette/evavelo/issues/50
  cas_8 <- data %>%
    dplyr::filter(.data$categorie == "Sportif" & .data$categorie_visuelle_cycliste =="Utilitaire") %>%
    dplyr::mutate(vae = !is.na(.data$nb_vae) & .data$nb_vae == .data$nb_total_velo) %>%
    dplyr::mutate(
      categorie_corrige = dplyr::case_when(
        .data$km_sortie > 50 & .data$vae == FALSE ~ "Sportif",
        .data$km_sortie <= 50 | .data$vae == TRUE ~ "Utilitaire",
        TRUE ~ NA_character_
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
#' data should have the following columns: iti_km_voyage, iti_experience,
#' iti_depart_itineraire, iti_arrivee_itineraire,
#' iti_depart_initial, iti_arrivee_final
#'
#' @param data a data.frame. Some columns are mandatory. See details for more information
#' @param col_name name of the new column created
#'
#' @importFrom rlang .data :=
#'
#' @return 'data' data.frame with a new logical column name after 'col_name'

add_coherence <- function(data,
                          col_name = "coherent") {
  data %>%
    dplyr::mutate(
      ## check coherence of itinerant answers
      !!col_name := is_iti_coherent(iti_km_voyage = .data$iti_km_voyage,
                                    iti_experience = .data$iti_experience,
                                    iti_dep_iti_valide = .data$iti_dep_iti_valide,
                                    iti_arr_iti_valide = .data$iti_arr_iti_valide,
                                    iti_depart_initial = .data$iti_depart_initial,
                                    iti_arrivee_final = .data$iti_arrivee_final)
    )
}

#' Helper function to control "coherence" of specific "itinerance" answers:
#'
#'  https://github.com/JMPivette/evavelo/discussions/7
#'  https://github.com/JMPivette/evavelo/issues/39
#'
#' @param iti_km_voyage numeric vector
#' @param iti_experience character vector
#' @param iti_dep_iti_valide character vector
#' @param iti_arr_iti_valide character vector
#' @param iti_depart_initial character vector
#' @param iti_arrivee_final character vector
#'
#' @return a boolean vector indicating if answer is coherent
is_iti_coherent <- function (iti_km_voyage,
                             iti_experience,
                             iti_dep_iti_valide,
                             iti_arr_iti_valide,
                             iti_depart_initial,
                             iti_arrivee_final){

  ## Check iti depart and arrivee
  coher_commune <- (!is.na(iti_dep_iti_valide) | !is.na(iti_depart_initial)) &
    (!is.na(iti_arr_iti_valide) | !is.na(iti_arrivee_final))

  ## Check that we have at least 2 answers out of three
  coher_2_3 <- ((!is.na(iti_km_voyage)) + coher_commune + (!is.na(iti_experience))) >= 2

  return(coher_2_3)
}
