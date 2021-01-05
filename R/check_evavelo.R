#' Check input file correcteness (Eva-velo)
#'
#' @param comptage a data.frame
#' @param enquete a data.frame
#'
#' @return a list of two values: error (a boolean) and log (a string)
#'
#' @importFrom rlang .data
#' @export
#'
check_evavelo <- function(comptage, enquete){
  ## fonction that returns a status and log information. not really clear for the moment but will be
  log <- ""
  err <- FALSE

  log <- add_message_log(log, "Checking comptage...")
  ## Check integrity of comptage----------------
  #Check variable names
  comptage_names <- c("id_quest", "categorie_visuelle_cycliste", "categorie_breve")
  if (!all(comptage_names %in% names(comptage))) {
    not_present <- setdiff(comptage_names, names(comptage))
    err <- TRUE
    log <- add_message_log(log,
                           "Error", paste(not_present, collapse = ", "),
                           " missing from comptage")
  }
  # Check uniqueness of id_quest
  dupli_id <- comptage %>%
    select(.data$id_quest) %>%
    filter(!is.na(.data$id_quest)) %>%
    filter(duplicated(.data$id_quest))
  if (nrow(dupli_id) != 0) {
    err <- TRUE
    log <- add_message_log(log, "Error: duplicated id_quest in 'comptage':",
                           paste(dupli_id$id_quest, collapse = ", ")
    )
  }


  ## Check integrity of enquete
  #Check variable names
  log <- add_message_log(log, "Checking enquete...")
  enquete_names <- c("id_quest", "categorie", "categorie_corrige",
                     "type_sortie", "dms", "km_sortie", "type_trajet",
                     "nb_vae", "nb_total_velo", "activites", "activite_motiv")
  if (!all(enquete_names %in% names(enquete))) {
    not_present <- setdiff(enquete_names, names(enquete))
    err <- TRUE
    log <- add_message_log(log,
                           "Error", paste(not_present, collapse = ", "),
                           " missing from enquete")
  }

  # Check uniqueness of id_quest
  dupli_id <- enquete %>%
    select(.data$id_quest) %>%
    filter(!is.na(.data$id_quest)) %>%
    filter(duplicated(.data$id_quest))
  if (nrow(dupli_id) != 0) {
    err <- TRUE
    log <- add_message_log(log, "Error: duplicated id_quest in 'enquete':\n",
                           paste(dupli_id$id_quest, collapse = ", ")
    )
  }

  ## Check relationship between comptage and enquete.
  log <- add_message_log(log, "Checking relationship between comptage and enquete...")
  # Find id_quest with no relationship
  id_notin_enq <- setdiff(comptage$id_quest,
                          c(enquete$id_quest, NA))
  id_notin_compt <- setdiff(enquete$id_quest,
                            c(comptage$id_quest, NA))
  if(length(id_notin_compt) != 0){
    err <- TRUE
    log <- add_message_log(log,
                           "Error: The following id_quest are not present in 'comptage':\n",
                           paste(id_notin_compt, collapse = ", "))
  }
  if(length(id_notin_enq) != 0){
    err <- TRUE
    log <- add_message_log(log,
                           "Error: The following id_quest are not present in 'enquete':\n",
                           paste(id_notin_enq, collapse = ", "))
  }

  ## Final message
  if(err == FALSE){
    log <- add_message_log(log, "No error found when checking input data.")
  }


return(list(error = err,
            log = log))
}



add_message_log <- function(log, ...){
  message <- paste(list(...), collapse = " ")
  paste(log, message, sep = "\n")
}
