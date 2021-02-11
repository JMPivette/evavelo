#' Read and clean eva-velo xlsx file
#'
#' Read specific sheets from an xlsx object and perform some basic cleaning.
#'
#' @param file xlsx file, Workbook object or URL to xlsx file
#'
#'
#' @return a list of data.frames
#' @export

read_evavelo <- function(file){
  message("V\u00e9rification des noms de communes")
  message("---------------------------------")
  out <- list(
    calendrier = read_calendrier(file),
    table_communes = read_table_communes(file),
    comptage_init = read_comptage(file, init = TRUE),
    comptage = read_comptage(file),
    enquete_init = read_enquete(file, init = TRUE),
    enquete = read_enquete(file)
  )

  class(out) <- c("evadata", class(out))
  out

}

#' Read and clean "table_communes" information
#'
#' Read a specific sheet of an xlsx object and perform some basic cleaning.
#'
#' @param file xlsx file, Workbook object or URL to xlsx file
#' @param sheet Name of the worksheet containing "table_communes" information.
#'
#' @return a data.frame
#' @export
read_table_communes <- function(file, sheet = "table_communes"){

  openxlsx::read.xlsx(file,
                      sheet,
                      startRow = 2) %>%  # We skip the first row that contains global information and not data.
    janitor::clean_names() %>%
    geocode_table_communes()

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

  enquete <- enquete %>%
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
    ) %>%
    dplyr::mutate( ## Avoid leading or trailing whitespaces.
      dplyr::across(
        dplyr::all_of(c("nom_site_enq", "iti_arrivee_itineraire", "iti_depart_itineraire",
                        "ville_heb", "ville_res")),
        stringr::str_trim
      )
    )


  if(init == FALSE){ ## check cities and add columns
    enquete <- enquete %>%
      geocode_cities(ville_heb) %>%
      geocode_cities(iti_depart_itineraire) %>%
      geocode_cities(iti_arrivee_itineraire) %>%
      geocode_cities(nom_site_enq) %>%
      geocode_cities_cp(ville_res,
                        cp_col = cp_res,
                        country_col = pays_res)
  }

  enquete

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

