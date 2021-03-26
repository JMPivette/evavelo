#' Detects similar response in enquete
#'
#' Try to detect multiple id_quest that were not tagged as multiple
#'
#' Will group identical answers to a series of text answers
#'
#' And then will confirm with the similarity of numeric responses (km_sortie, heure_enq)
#'
#' @param enquete enquete data.frame obtained which is part of an eva_data object.
#'
#' @return a data.frame with one columns enq_list containing potential similar answers
#' @export

check_similar_enquete <- function(enquete){
  ## Mandatory Identical answers within a group.
  mand_variables_to_check <- c("id_site_enq", "date_enq", "mode_heb", "ville_heb",
                               "type_groupe", "type_trajet", "type_sortie",
                               "iti_depart_initial", "iti_depart_itineraire", "iti_arrivee_itineraire", "iti_arrivee_final")
  ## Not necessary identical within a group
  opt_variables_to_check <- c("heure_enq", "age", "km_sortie", "ville_res")


  ## Inner algorithm function to define similar answers from numeric answers
  keep_potential_duplicated <- function(nb_na, heure_enq, age, km_sortie, ville_res){
    # Case where a lot of answers are not missing
    if(unique(nb_na) <=4) return(TRUE)

    # Allow an additional missing answer if ville_res is the same
    ville_res <- unique(ville_res)
    if(unique(nb_na) == 5 && !is.na(ville_res) && length(ville_res) == 1)
      return(TRUE)

    # For the other cases we use the km_sortie and heure_diff information.
    age_diff <- max(age) - min(age)
    km_diff <- (max(km_sortie) - min(km_sortie)) / max(km_sortie)
    heure_diff <- max(heure_enq) - min(heure_enq)
    ## Case where in addition to not having enough answers we don't know km_sortie.
    if(is.na(km_diff) | is.na(heure_diff))
      return(FALSE)
    ## Passing within 1 hour with less than 20% distance difference with maximum
    if(heure_diff <= 1 & km_diff  <= 0.2)
      return(TRUE)
    ## Other cases
    return(FALSE)
  }

 log <- "Recherche de questionnaires multiples...\n------------------------------------------\n"

  simil_enq <- enquete %>%
    dplyr::select(
      .data$id_quest, .data$date_enq,
      dplyr::all_of(mand_variables_to_check),
      dplyr::all_of(opt_variables_to_check)
    ) %>%
    dplyr::filter(.data$type_groupe != "Seul") %>% ## remove group defined as alone
    dplyr::mutate(## Count number of non-answers
      nb_na = rowSums(is.na(dplyr::across(dplyr::all_of(mand_variables_to_check))))
    ) %>%
    dplyr::group_by(
      dplyr::across(dplyr::all_of(mand_variables_to_check))
    ) %>%
    dplyr::filter(dplyr::n()>1) %>% ## keep only identical answers in multiple lines
    dplyr::summarise(enq_list = paste0(.data$id_quest, collapse = ", "),
                     res = keep_potential_duplicated(.data$nb_na,.data$heure_enq,
                                                     .data$age, .data$km_sortie, .data$ville_res),
                     ## Spot groups that were already identified
                     n_radical = length(unique(radical_quest(.data$id_quest)))) %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data$res == TRUE & .data$n_radical > 1) %>%
    dplyr::select(.data$enq_list)

  if(nrow(simil_enq) !=0){
    log <- paste0(log, "Les questionnaires suivants sont peut-\u00eatre multiples:\n\t",
                  paste0(simil_enq$enq_list, collapse = "\n\t"))
  } else {
    log <- paste0(log, "Aucun questionnaire mutiple d\u00e9tect\u00e9 \n")
  }


  return(list(simil_enq = simil_enq,
              log = log))


}



