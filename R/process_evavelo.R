#' Read and process a eva-velo file
#'
#'
#'
#' @param file an xlsx file, Workbook object or URL to xlsx file
#' @param check  a boolean. Is data checked before being processed.
#' This value can be set to FALSE if you already ised check_evavelo() and don't want to reun it twice
#'
#' @return a list of corrected categories
#' @export
#'
process_evavelo <- function(file,
                            check = TRUE){
  ## Sheet: Comptage Post Traitements-------------
  comptage <- read_comptage(file)
  ## Sheet: Enquetes Post Traitements-----------
  enquete <- read_enquete(file)
  ## Sheet: Calendrier--------------------------
  calendrier <- read_calendrier(file)
  ## Sheet before "post_traitement"
  comptage_init <- read_comptage(file, init = TRUE)
  enquete_init <- read_enquete(file, init = TRUE)

  ## Check values
  if (check == TRUE){
    check_result <- check_evavelo(calendrier = calendrier,
                                  comptage = comptage,
                                  enquete = enquete,
                                  comptage_init = comptage_init,
                                  enquete_init = enquete_init)
    if (check_result$error == TRUE){
      warning("Impossible de traiter le fichier qui comporte des erreurs")
      message(check_result$log)
      return(NULL)
    }
  }


    correct_categ(comptage, enquete)


}


#' Read and clean "comptage" information
#'
#' Read a specific sheet of an xlsx object and perform some basic cleaning.
#'
#' @param file xlsx file, Workbook object or URL to xlsx file
#' @param init boolean to import the initial sheet instead of the _post_traitement
#'
#' @importFrom rlang .data
#'
#' @return a data.frame
#' @export
read_comptage <- function(file, init = FALSE){
  sheet <- ifelse(init, "comptages_manuels", "comptages_man_post_traitements")
  comptage <- openxlsx::read.xlsx(file, sheet)

  comptage %>%
    dplyr::select(starts_with("[")) %>%  ## Don't take in account "old" col names that could create duplicated entries
    janitor::clean_names() %>%
    dplyr::mutate(
      dplyr::across(starts_with("categorie"), as.character),
      dplyr::across(starts_with("categorie_visuelle"),
             ~ dplyr::if_else(.x == "Loisirs", "Loisir", .x)) ## Fix error in test input file
    ) %>%
    dplyr::mutate(
      date_enq = openxlsx::convertToDate(.data$date_enq)
    )

}

#' Read and clean "enquete" information
#'
#' Read a specific sheet of an xlsx object and perform some basic cleaning.
#'
#' @param file xlsx file, Workbook object or URL to xlsx file
#' @param init boolean to import the initial sheet instead of the _post_traitement
#'
#' @importFrom rlang .data
#'
#' @return a data.frame
#' @export
read_enquete <- function(file, init = FALSE) {
  sheet <- ifelse(init, "enquetes_saisies", "enquetes_post_traitement")
  enquete <- openxlsx::read.xlsx(file, sheet)

  enquete %>%
    janitor::clean_names() %>%
    dplyr::mutate(
      type_sortie = dplyr::case_when(
        type_sortie == "Demi journ\u00e9e" ~ "Demi-journ\u00e9e",
        type_sortie == "La journ\u00e9e" ~ "Journ\u00e9e",
        TRUE ~ type_sortie)
    ) %>%
    dplyr::mutate(
      type_trajet = dplyr::case_when(
        stringr::str_detect(type_trajet, "simple") ~ "Trajet simple",
        stringr::str_detect(type_trajet, "retour") ~ "Aller-retour",
        TRUE ~ type_trajet)
    ) %>%
    dplyr::mutate(
      date_enq = openxlsx::convertToDate(.data$date_enq)
    )
}

#' Read and clean "calendrier" information
#'
#' Read a specific sheet of an xlsx object and perform some basic cleaning.
#'
#' @param file xlsx file, Workbook object or URL to xlsx file
#' @param sheet Name of the worksheet caontaining "calendrier" information.
#'
#' @return a data.frame
#' @export
read_calendrier <- function(file, sheet = "calendrier_sites"){

  calendrier <- openxlsx::read.xlsx(file,
                                    sheet,
                                    startRow = 2) %>%  # We skip the first row that contains global information and not data.
    dplyr::mutate(
      date_enq = openxlsx::convertToDate(.data$date_enq)
    ) %>%
    janitor::clean_names()

}
