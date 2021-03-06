#' Search for outliers in numeric values
#'
#' @param evadata an evadata object obtained with read_eva_velo()
#' @param categorie_corrige optional vector of categorie_corrige. Must be the same size as the number of rows in evadata$enquete.
#' To be used if we want to use different categorie_corrige than the one stored in evadata
#'
#' @return a list of data.frames with different outliers detected
#' @export

check_num_outliers <- function(evadata,
                               categorie_corrige = NULL){

  enquete <- evadata$enquete
  ## Checking categorie_corrige vector.
  if(!is.null(categorie_corrige)){
    if (length(categorie_corrige) != nrow(enquete))
      warning("categorie_corrige should be a vector with the same length as number of rows in enquete. \n
              Ignoring categorie_corrige")
    else
      enquete$categorie_corrige <- categorie_corrige
  }


# km_sortie (3.1.14) --------------------------------------------------------------------------

  km_sortie <- enquete %>%
    dplyr::select(.data$id_quest, .data$categorie_corrige, .data$km_sortie) %>%
    dplyr::filter(!is.na(.data$km_sortie)) %>%
    dplyr::group_by(.data$categorie_corrige) %>%
    outliers_std_detect(.data$km_sortie)

# dms (3.1.17) --------------------------------------------------------------------------------

  ## Grouping is done by mode_heb_regroupe and subdivised by categorie_corrige in case of big groups (>200)
  dms <- enquete %>%
    dplyr::select(.data$id_quest, .data$mode_heb_regroupe, .data$categorie_corrige, .data$dms) %>%
    dplyr::filter(!is.na(.data$dms)) %>%
    dplyr::group_by(.data$mode_heb_regroupe, .data$categorie_corrige) %>%
    dplyr::mutate(
      categorie_lump = dplyr::case_when(dplyr::n() < 50 ~ "Other",
                                 TRUE ~ categorie_corrige)
    ) %>%
    dplyr::group_by(.data$mode_heb_regroupe) %>%
    dplyr::mutate(
      categorie_lump = dplyr::case_when(dplyr::n() < 200 ~ "All",
                                 TRUE ~ categorie_lump)
    ) %>%
    dplyr::group_by(.data$mode_heb_regroupe, .data$categorie_lump) %>%
    outliers_std_detect(.data$dms, filter = FALSE) %>%
    dplyr::mutate(
      raison = dplyr::case_when(
        .data$dms < 1 ~ "< 1",
        .data$dms == 365 ~ "= 365",
        TRUE ~ raison)
    ) %>%
    dplyr::filter(!is.na(.data$raison)) %>%
    dplyr::relocate(.data$categorie_lump, .before = .data$dms)


# revenu (3.1.33) -----------------------------------------------------------------------------

  revenu <- enquete %>%
    dplyr::select(.data$id_quest, .data$revenu) %>%
    dplyr::filter(!is.na(revenu)) %>%
    outliers_std_detect(.data$revenu)



# tour_dep_xxx (3.1.26) -----------------------------------------------------------------------

  dep_by_cat <- c("tour_dep_alim",
                  "tour_dep_activites",
                  "tour_dep_souvenirs",
                  "tour_dep_location",
                  "tour_dep_autres")

  dep_by_heb <- c("tour_dep_to_jour",
                  "tour_dep_heb")

  ## Depense by categorie
  tour_dep_cat <- enquete %>%
    dplyr::select(.data$id_quest, .data$categorie_corrige,
           dplyr::all_of(dep_by_cat)) %>%
    tidyr::pivot_longer(starts_with("tour_dep"),
                        names_to = "depense",
                        values_to = "valeur") %>%
    dplyr::filter(!is.na(.data$valeur)) %>%
    dplyr::group_by(.data$depense, .data$categorie_corrige) %>%
    outliers_std_detect(.data$valeur)

  ## Depense by mode_heb
  tour_dep_heb <- enquete %>%
    dplyr::select(.data$id_quest, .data$mode_heb_regroupe,
           dplyr::all_of(dep_by_heb)) %>%
    tidyr::pivot_longer(starts_with("tour_dep"),
                        names_to = "depense",
                        values_to = "valeur") %>%
    dplyr::filter(!is.na(.data$valeur)) %>%
    dplyr::group_by(.data$depense, .data$mode_heb_regroupe) %>%
    outliers_std_detect(.data$valeur)

  ## Depense tour_dep_transp (3.1.26.4)
  ## group by mode_transp_jour that have more than 100 answers
  ## https://github.com/JMPivette/evavelo/discussions/59
  tour_dep_transp <- enquete %>%
    dplyr::select(.data$id_quest,
                  .data$mode_transp_jour,
                  .data$tour_dep_transp) %>%
    dplyr::filter(!is.na(.data$tour_dep_transp) & .data$mode_transp_jour != "Aucun") %>%
    dplyr::group_by(.data$mode_transp_jour) %>%
    dplyr::mutate(
      mode_transp_lump = dplyr::case_when(
        dplyr::n() > 100 ~ mode_transp_jour,
        TRUE ~ "Other")
    ) %>%
    dplyr::group_by(.data$mode_transp_lump) %>%
    outliers_std_detect(.data$tour_dep_transp)

  ## Deal with case where 'Aucun' is selected but there is a tour_dep_transp value
  tour_dep_transp_aucun <- enquete %>%
    dplyr::select(.data$id_quest,
                  .data$mode_transp_jour,
                  .data$tour_dep_transp) %>%
    dplyr::filter(.data$mode_transp_jour == "Aucun") %>%
    dplyr::filter(!is.na(tour_dep_transp) & tour_dep_transp != 0)

  tour_dep_transp <- dplyr::bind_rows(tour_dep_transp,
                                      tour_dep_transp_aucun)



  list(km_sortie = km_sortie,
       dms = dms,
       revenu = revenu,
       tour_dep_cat = tour_dep_cat,
       tour_dep_heb = tour_dep_heb,
       tour_dep_transp = tour_dep_transp)

}

#' Standard detection of outliers
#'
#' Internal function that detects first centile, last centile and IQR per group
#'
#' @param .data a tibble. if grouped, the analysis will be done per group
#' @param variable name of the variable we want to analyse
#' @param col_name name of the new column that will be created.
#' @param filter a boolean. If TRUE a filter will be applied to keep only outliers.
#' If FALSE, the number of rows in the output will be the same as .data.
#' @param k coefficient applied to the IQR outlier detection
#'
#' @return .data tibble with an additional column names after col_name that conains a string with the type of outlier detection. For non outlier, NA is used
#' @keywords internal

outliers_std_detect <- function(.data,
                                variable,
                                col_name = raison,
                                filter = TRUE,
                                k = 1.5){
  v <- rlang::enquo(variable)
  col_name <- rlang::enquo(col_name)
  if(nrow(.data) == 0) return(.data)

  .data %>%
    dplyr::mutate({{col_name}} := dplyr::case_when(
      !!v < quantile(!!v, 0.01) ~ "premier centile",
      !!v > quantile(!!v, 0.99) ~ 'dernier centile',
      !!v < quantile(!!v, 0.25) - k * IQR(!!v) |
        !!v > quantile(!!v, 0.75) + k * IQR(!!v) ~ paste(k,"x IQR")
    )) %>%
    {if(filter) dplyr::filter(., !is.na(!!col_name)) else .}
}
