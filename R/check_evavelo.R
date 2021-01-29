#' Check input file correctness (Eva-velo)
#'
#' @param calendrier a data.frame corresponding to sheet "calendrier_sites"
#' @param comptage a data.frame corresponding to sheet "comptages_man_post_traitements"
#' @param enquete a data.frame corresponding to sheet "enquetes_post_traitement"
#' @param comptage_init a data.frame corresponding to sheet "comptages_manuels"
#' @param enquete_init a data.frame corresponding to sheet "enquetes_saisies"
#'
#' @return a list of two values: error (a boolean) and log (a string)
#'
#' @importFrom rlang .data
#' @export
#'
check_evavelo <- function(calendrier,
                          comptage,
                          enquete,
                          comptage_init,
                          enquete_init){
  ## function that returns a status and log information. not really clear for the moment but will be later

  # initialize outputs
  log <- "" ## string to store log information
  err <- FALSE ## boolean output to tell if an error occurred


  ## Check sheets with their associated post_traitement----------------------

  log <- add_message_log(log, "Comparing enquetes_saisies and enquetes_post_traitement...")
  withCallingHandlers(enq_diff <- compare_init_post(init = enquete_init,
                                             post_trait = enquete),
                      warning = function(w) {
                        log <<- add_message_log(log, w$message)
                        err <<- TRUE
                      }) %>%
    suppressWarnings()

  log <- add_message_log(log, "Comparing comptages_manuels and comptages_man_post_traitements...")
  withCallingHandlers(compt_diff <- compare_init_post(init = comptage_init,
                                             post_trait = comptage),
                      warning = function(w) {
                        log <<- add_message_log(log, w$message)
                        err <<- TRUE
                      }) %>%
    suppressWarnings()

  ## Check integrity of comptage-------------------------
  log <- add_message_log(log, "Checking comptage...")
  #Check variable names
  if (!all(comptage_colnames %in% names(comptage))) {
    not_present <- setdiff(comptage_colnames, names(comptage))
    err <- TRUE
    log <- add_message_log(log,
                           " ERROR:", paste(not_present, collapse = ", "),
                           " missing from comptage")
  }
  # Check uniqueness of id_quest
  dupli_id <- comptage %>%
    dplyr::select(.data$id_quest) %>%
    dplyr::count(.data$id_quest) %>%
    dplyr::filter(!is.na(.data$id_quest) & .data$n > 1)
  if (nrow(dupli_id) != 0) {
    err <- TRUE
    log <- add_message_log(log, " ERROR: duplicated id_quest in 'comptage':\n",
                           paste(dupli_id$id_quest, "(", dupli_id$n, "), ", collapse = "")
    )
  }

  # Check values of categorie_visuelle_cycliste
  wrong_cat <- setdiff(unique(comptage$categorie_visuelle_cycliste),
          c("Loisir","Sportif","Utilitaire","Itin\u00e9rant", NA))
  if (length(wrong_cat) != 0 ) {
    err <- TRUE
    log <- add_message_log(
      log,
      " ERROR: the following non-standard values are in 'categorie_visuelle_cycliste': ",
      paste(wrong_cat, collapse = ", ")
    )
  }

  # Check relationship between categorie_visuelle_cycliste and categorie_visuelle (Not implemented yet)
  ## Extra check but will need to take in account children
  # mismatch_categorie_vis <- comptage %>%
  #   dplyr::transmute(.data$categorie_visuelle,
  #                    .data$categorie_visuelle_cycliste,
  #                    index= 1:dplyr::n() + 1) %>%
  #   filter(!is.na(.data$categorie_visuelle_cycliste) &
  #            .data$categorie_visuelle != .data$categorie_visuelle_cycliste)
  # if (length(mismatch_categorie_vis) != 0){
  #   log <- add_message_log(
  #     log,
  #     " Warning: Mismatch between 'categorie_visuelle' and 'categorie_visuelle_cycliste' at line(s): ",
  #     paste(mismatch_categorie_vis$index, collapse = ", ")
  #   )
  # }


  ## Check integrity of enquete--------------------------
  #Check variable names
  log <- add_message_log(log, "Checking enquete...")

  if (!all(enquete_colnames %in% names(enquete))) {
    not_present <- setdiff(enquete_colnames, names(enquete))
    err <- TRUE
    log <- add_message_log(log,
                           " ERROR", paste(not_present, collapse = ", "),
                           " missing from enquete")
  }

  # Check uniqueness of id_quest
  dupli_id <- enquete %>%
    dplyr::select(.data$id_quest) %>%
    dplyr::count(.data$id_quest) %>%
    dplyr::filter(!is.na(.data$id_quest) & .data$n > 1)
  if (nrow(dupli_id) != 0) {
    err <- TRUE
    log <- add_message_log(log, " ERROR: duplicated id_quest in 'enquete':\n",
                           paste(dupli_id$id_quest, collapse = ", ")
    )
  }

  ## Check integrity of calendrier
  #Check variable names
  log <- add_message_log(log, "Checking calendrier...")
  if (!all(calendrier_colnames %in% names(calendrier))) {
    not_present <- setdiff(calendrier_colnames, names(calendrier))
    err <- TRUE
    log <- add_message_log(log,
                           " ERROR", paste(not_present, collapse = ", "),
                           " missing from calendrier")
  }



  ## Check relationship between comptage and enquete-------------------------------
  log <- add_message_log(log, "Checking relationship between comptage and enquete...")
  # Find id_quest with no relationship
  enquete_id_quest <- radical_quest(comptage$id_quest)%>% ## remove id_quest suffixes that can appear in 'enquete' when using multiple 'enquete'
    unique()
  id_notin_enq <- setdiff(comptage$id_quest,
                          c(enquete_id_quest, NA))
  id_notin_compt <- setdiff(enquete_id_quest,
                            c(comptage$id_quest, NA))
  if(length(id_notin_compt) != 0){
    err <- TRUE
    log <- add_message_log(log,
                           " ERROR: The following id_quest are not present in 'comptage':\n",
                           paste(id_notin_compt, collapse = ", "))
  }
  if(length(id_notin_enq) != 0){
    err <- TRUE
    log <- add_message_log(log,
                           " ERROR: The following id_quest are not present in 'enquete':\n",
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
      " ERROR:", nrow(in_x_notin_y), "days are in", name_x, "but missing from", name_y,":\n",
      paste("id_site_enq:", in_x_notin_y$id_site_enq, "\tdate:" ,in_x_notin_y$date_enq, "\n",
            collapse = " ")
    )
  }
  log
}
