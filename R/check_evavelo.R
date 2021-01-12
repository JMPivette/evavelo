#' Check input file correctness (Eva-velo)
#'
#' @param calendrier a data.frame
#' @param comptage a data.frame
#' @param enquete a data.frame
#'
#' @return a list of two values: error (a boolean) and log (a string)
#'
#' @importFrom rlang .data
#' @export
#'
check_evavelo <- function(calendrier, comptage, enquete){
  ## fonction that returns a status and log information. not really clear for the moment but will be later

  # initialize outputs
  log <- "" ## string to store log information
  err <- FALSE ## boolean output to tell if an error occured

  log <- add_message_log(log, "Checking comptage...")
  ## Check integrity of comptage-------------------------
  #Check variable names
  comptage_names <- c("id_quest", "categorie_visuelle_cycliste", "categorie_breve",
                      "id_site_enq", "date_enq")
  if (!all(comptage_names %in% names(comptage))) {
    not_present <- setdiff(comptage_names, names(comptage))
    err <- TRUE
    log <- add_message_log(log,
                           "Error", paste(not_present, collapse = ", "),
                           " missing from comptage")
  }
  # Check uniqueness of id_quest
  dupli_id <- comptage %>%
    dplyr::select(.data$id_quest) %>%
    dplyr::count(.data$id_quest) %>%
    dplyr::filter(!is.na(.data$id_quest) & .data$n > 1)
  if (nrow(dupli_id) != 0) {
    err <- TRUE
    log <- add_message_log(log, "Error: duplicated id_quest in 'comptage':",
                           paste(dupli_id$id_quest, "(", dupli_id$n, ")\n", collapse = " ")
    )
  }


  ## Check integrity of enquete--------------------------
  #Check variable names
  log <- add_message_log(log, "Checking enquete...")
  enquete_names <- c("id_quest", "categorie", "categorie_corrige",
                     "type_sortie", "dms", "km_sortie", "type_trajet",
                     "nb_vae", "nb_total_velo", "activites", "activite_motiv",
                     "id_site_enq", "date_enq")
  if (!all(enquete_names %in% names(enquete))) {
    not_present <- setdiff(enquete_names, names(enquete))
    err <- TRUE
    log <- add_message_log(log,
                           "Error", paste(not_present, collapse = ", "),
                           " missing from enquete")
  }

  # Check uniqueness of id_quest
  dupli_id <- enquete %>%
    dplyr::select(.data$id_quest) %>%
    dplyr::count(.data$id_quest) %>%
    dplyr::filter(!is.na(.data$id_quest) & .data$n > 1)
  if (nrow(dupli_id) != 0) {
    err <- TRUE
    log <- add_message_log(log, "Error: duplicated id_quest in 'enquete':\n",
                           paste(dupli_id$id_quest, collapse = ", ")
    )
  }

  ## Check integrity of calendrier
  #Check variable names
  log <- add_message_log(log, "Checking calendrier")
  calendrier_names <- c("id_site_enq", "date_enq")
  if (!all(calendrier_names %in% names(calendrier))) {
    not_present <- setdiff(calendrier_names, names(calendrier))
    err <- TRUE
    log <- add_message_log(log,
                           "Error", paste(not_present, collapse = ", "),
                           " missing from calendrier")
  }



  ## Check relationship between comptage and enquete-------------------------------
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

  ## Check relationship with calendrier_sites-------------------------
  log <- add_message_log(log, "Checking id_site_enq and date from calendrier...")

  cal_enq_difflog <- check_diff(a = calendrier, b = enquete,
                                name_a = "calendrier", name_b = "enquete")
  cal_compt_difflog <- check_diff(a = calendrier, b = comptage,
                                  name_a = "calendrier", name_b = "comptage")

  if(length(cal_enq_difflog) != 0 | length(cal_compt_difflog) != 0) { ## A difference has been spoted
    err <- TRUE
    log <- add_message_log(log, cal_compt_difflog, cal_enq_difflog)
  }


  ## Final message-----------------------
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




#' Check correctness of id_site_enq and date_enq between 2 data.frames
#'
#' @param a a data.frame containing id_site_enq and date_enq variables
#' @param b a data.frame containing id_site_enq and date_enq variables
#' @param name_a name of the df to be used in the log message. If missing, we will use the name of data.frame a
#' @param name_b name of the df to be used in the log message. If missing, we will use the name of data.frame b
#'
#' @importFrom rlang .data
#'
#' @return a log message with the differences between data.frames

check_diff <- function(a, b, name_a = NULL, name_b = NULL) {
  # Deal with missing names
  if(missing(name_a))
    name_a <- deparse(substitute(a))
  if(missing(name_b))
    name_b <- deparse(substitute(b))
  # Get unique id_site_enq / date_enq combinations
  a <- dplyr::distinct(a,
                       .data$id_site_enq, .data$date_enq)
  b <- dplyr::distinct(b,
                       .data$id_site_enq, .data$date_enq)
  # Check errors
  log <- paste0(
    log_in_x_not_in_y(a, b, name_a, name_b),
    log_in_x_not_in_y(b, a, name_b, name_a)
  )

}


#' Utils function to log date site combination that are in x and not in y
#'
#' @param x a data.frame containing id_site_enq and date_enq variables
#' @param y a data.frame containing id_site_enq and date_enq variables
#' @param name_x name of the df to be used in the log message.
#' @param name_y name of the df to be used in the log message.
#'
#' @return a string which logs the differences

log_in_x_not_in_y <- function(x, y, name_x, name_y) {
  log <- character(0)
  in_x_notin_y <- dplyr::anti_join(x, y,
                                   by = c("id_site_enq", "date_enq"))
  if(nrow(in_x_notin_y) != 0){
    log <- paste(
      "Error:", nrow(in_x_notin_y), "days are in", name_x, "but missing from", name_y,":\n",
      paste("id_site_enq:", in_x_notin_y$id_site_enq, "\tdate:" ,in_x_notin_y$date_enq, "\n",
            collapse = " ")
    )
  }
  log
}
