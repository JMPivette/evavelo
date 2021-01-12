#' Read and process a eva-velo file
#'
#'
#'
#' @param file An xlsx file, Workbook object or URL to xlsx file
#'
#' @return a list of corrected categories
#' @export
#'
process_evavelo <- function(file){
  ## Sheet: Comptage Post Traitements-------------
  comptage <- read_comptage(file)
  ## Sheet: Enquetes Post Traitements-----------
  enquete <- read_enquete(file)
  ## Check values


  correct_categ(comptage, enquete)

}


#' Read and clean "comptage" information
#'
#' Read a specific sheet of an xlsx object and perform some basic cleaning.
#'
#' @param file xlsx file, Workbook object or URL to xlsx file
#' @param sheet Name of the worksheet caontaining "comptage" information.
#'
#' @importFrom rlang .data
#'
#' @return a data.frame
#' @export
read_comptage <- function(file, sheet = "comptages_man_post_traitements"){

  comptage <- openxlsx::read.xlsx(file, sheet)

  comptage %>%
    dplyr::select(starts_with("[")) %>%  ## Don't take in account "old" col names that could create duplicated entries
    janitor::clean_names() %>%
    dplyr::mutate(
      categorie_breve = as.character(.data$categorie_breve),
      categorie_visuelle_cycliste = stringr::str_remove(.data$categorie_visuelle_cycliste,
                                                        "s$")
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
#' @param sheet Name of the worksheet caontaining "enquete" information.
#'
#' @importFrom rlang .data
#'
#' @return a data.frame
#' @export
read_enquete <- function(file, sheet = "enquetes_post_traitement") {

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
                                    startRow = 2, # We skip the first row that contains global information and not data.
                                    detectDates = TRUE) %>%
    janitor::clean_names()

}
