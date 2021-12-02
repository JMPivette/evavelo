#' Read and clean eva-velo xlsx file
#'
#' Read specific sheets from an xlsx object and perform some basic cleaning.
#'
#' @param file xlsx file, Workbook object or URL to xlsx file
#'
#'
#' @return an evadata object which is in fact a list of data.frames
#' @export

read_evavelo <- function(file){
  out <- list(
    calendrier = read_calendrier(file),
    table_communes = read_table_communes(file),
    comptage_init = read_comptage(file, init = TRUE),
    comptage = read_comptage(file),
    enquete_init = read_enquete(file, init = TRUE),
    enquete = read_enquete(file),
    comptages_automatiques = read_compt_auto(file)
  )

  class(out) <- c("evadata", class(out))
  attr(out, "geocoded") <- FALSE
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
#' @keywords internal
read_table_communes <- function(file, sheet = "table_communes"){

  openxlsx::read.xlsx(file,
                      sheet,
                      startRow = 2) %>%  # We skip the first row that contains global information and not data.
    janitor::clean_names()
  # geocode_table_communes()

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
#' @keywords internal
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
      date_enq = openxlsx::convertToDate(.data$date_enq),
      id_quest = as.character(.data$id_quest)
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
#' @keywords internal
read_enquete <- function(file, init = FALSE) {
  sheet <- ifelse(init, "enquetes_saisies", "enquetes_post_traitement")
  enquete <- openxlsx::read.xlsx(file, sheet)
  enquete <- enquete %>%
    janitor::clean_names() %>%
    dplyr::mutate( ## Deal with empty case that are considered as logical after import
      dplyr::across(dplyr::starts_with("type_"), as.character),
      dms = as.numeric(.data$dms)
    ) %>%
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
      date_enq = openxlsx::convertToDate(.data$date_enq),
      heure_enq = as.integer(.data$heure_enq),
      revenu = as.numeric(revenu),
      id_quest = as.character(.data$id_quest),
      pays_res = as.character(.data$pays_res),
      dms = dplyr::if_else(.data$dms != 0,.data$dms, NA_real_) ## Fix issue #44 when dms = 0
    ) %>%
    dplyr::mutate( ## Avoid leading or trailing whitespaces.
      dplyr::across(
        dplyr::all_of(c("nom_site_enq", "iti_arrivee_itineraire", "iti_depart_itineraire",
                        "ville_heb", "ville_res")),
        stringr::str_trim
      )
    )

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
#' @keywords internal
read_calendrier <- function(file, sheet = "calendrier_sites"){

  calendrier <- openxlsx::read.xlsx(file,
                                    sheet,
                                    startRow = 2) %>%  # We skip the first row that contains global information and not data.
    dplyr::mutate(
      date_enq = openxlsx::convertToDate(.data$date_enq)
    ) %>%
    janitor::clean_names()

}

#' Read and clean "comptages_automatiques" information
#'
#' Read a specific sheet of an xlsx object and perform some basic cleaning.
#' Compute predictors that are later user for clustering classification
#'
#' @param file xlsx file, Workbook object or URL to xlsx file
#' @param sheet Name of the worksheet containing "comptages_automatiques" information.
#'
#' @return a data.frame
#' @export
read_compt_auto <- function(file, sheet = "comptages_automatiques"){
  ## header
  header_data <- openxlsx::read.xlsx(file,
                                     sheet,
                                     sep.names = " ",
                                     rows = 1:4)
  if(is.null(header_data)){
    warning("Aucune donn\u00e9e pr\u00e9sente dans l\'onglet comptages_automatiques")
    return(NULL)
  }


  # Load header data ----------------------------------------------------------------------------


  header_data <- header_data %>%
    dplyr::select(-(1:9)) %>% ## start col is 10
    t() %>%
    dplyr::as_tibble(rownames = "site_name") %>%
    dplyr::rename_with(~ c("site_name", "id_site","id_channel", "name")) %>%
    dplyr::mutate(dplyr::across(where(is.character),
                                stringr::str_squish)) ## Remove repeated white spaces

  ##TODO add a test for uniqueness of id_channel.
  if(anyDuplicated(header_data$id_channel) != 0){
    warning("id_channels dupliqu\u00e9s dans l\'onglet comptages_automatiques")
    return(NULL)
  }


  # Load data -----------------------------------------------------------------------------------
  load_data <- openxlsx::read.xlsx(file,
                                   sheet,
                                   sep.names = " ",
                                   startRow = 4) %>%
    dplyr::select(-dplyr::any_of(c("date", "annee", "mois", "jour", "type_jour"))) %>%
    dplyr::rename(date = 1) %>%
    ## Remove non-existing date (like winter> summer time) before conversion
    dplyr::filter(dplyr::if_any(c(where(is.numeric), -date),
                                ~ !is.na(.x))) %>%
    ## Transform "x" to TRUE or FALSE
    dplyr::mutate(dplyr::across(where(is.character),
                                ~ !is.na(.x))) %>%
    ## Create useful variables
    dplyr::mutate(
      date= openxlsx::convertToDateTime(.data$date),
      week_end = lubridate::wday(.data$date) %in% c(1,7),
      july_august = lubridate::month(.data$date) %in% 7:8
    ) %>%
    tidyr::pivot_longer(where(is.numeric),
                        names_to = "name",
                        values_to = "count"
    ) %>%
    dplyr::left_join(header_data,
                     by = "name")

  # Check data produced to avoid inconsistency later on
  if(all(!c("date", "jour_ferie", "pont", "vacances", "week_end", "july_august",
            "name", "count", "site_name", "id_site", "id_channel") == names(load_data))){
    warning("Probl\u00e8me de format dans l\'onglet comptages_automatiques. Impossible d\'importer")
    return(NULL)
  }

  # Compute predictors --------------------------------------------------------------------------


  pred <- load_data %>%
    dplyr::group_by(site_name, id_site, id_channel, name) %>%
    dplyr::summarize(
      ## Weekday proportion excluding holiday (working period)
      pred_wd_wp = sum_prod(!vacances, !week_end, !pont, !jour_ferie, count) /
        sum_prod(!vacances, !jour_ferie, count),
      ## Weekday proportion during holiday
      pred_wd_ho = sum_prod(vacances, !week_end, count) / sum_prod(vacances, count),
      ## July August over total
      pred_jul_aug = sum_prod(july_august, count) / sum_prod(count),
      ## Pont 14 juillet et 15 aout dans fréquentation mois aout juillet
      pred_pont_jul_aug = sum_prod(july_august, pont, count) / sum_prod(july_august, count),
      ## Proportion of count after 17:00 and before 9:00 (on weekday / working period)
      pred_wp_17_9 = sum_prod(!dplyr::between(lubridate::hour(date),9,17), !vacances, !week_end, !pont, !jour_ferie, count) /
        sum_prod(!vacances, !week_end, !pont, !jour_ferie, count),
      ## Proportion of counts between 9h and 11h in week-end
      pred_we_09_11 = sum_prod(dplyr::between(lubridate::hour(date),9,11), week_end, count) /
        sum_prod(week_end, count),
      ## Proportion of missing data
      missing_perc = sum(is.na(count)) / dplyr::n(),
      n_missing_days = dplyr::n_distinct(as.Date(date)) - dplyr::n_distinct(as.Date(date[!is.na(count)])),
      .groups = "drop"
    )

  return(pred)
}


