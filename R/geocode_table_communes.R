
#' geocode table_communes cities
#'
#' This function check that the municipalities defined in tables_communes are correct
#' and geocodes when possible
#'
#' @param table_communes a data.frame containing columns nom_commune and cog
#'
#' @return a data.frame similar to table_communes with latitude and longitude added.
#' @importFrom rlang .data
#'
#' @export
#'

geocode_table_communes <- function(table_communes){
  ## Geocode table_communes
  result <- table_communes %>%
    dplyr::select(.data$cog,
                  .data$nom_commune) %>%
    banR::geocode_tbl(tbl = .,
                      adresse = nom_commune,
                      code_insee = cog) %>%
    suppressMessages()

  ## Identify wrong answers and create informative warnings
  wrong_result <- result %>%
    dplyr::filter(.data$result_type != "municipality" |
                    .data$result_score < 0.9 |
                    .data$cog != .data$result_citycode) %>%
    dplyr::select(.data$nom_commune, .data$cog,
                  .data$result_city, .data$result_citycode,
                  .data$result_oldcitycode)


  ## Specific case of "communes nouvelles"
  old_cities <- wrong_result %>%
    dplyr::filter(.data$cog == .data$result_oldcitycode)

  message("...Verification de table_communes.............")
  if(nrow(old_cities) != 0){
    message("Les communes suivantes n'existent plus et doivent etre remplacees par les communes nouvelles:",
            paste0("\n\t",old_cities$nom_commune,"(", old_cities$cog,") -> ",
                   old_cities$result_city, "(",old_cities$result_citycode, ")"))
  }

  ## Failed to identify
  other_cities <- wrong_result %>%
    dplyr::anti_join(old_cities,
                     by = names(wrong_result))

  if(nrow(other_cities) != 0){
    message("Impossible de reconnaitre les communes suivantes:\n\t",
            paste0("\n\t",other_cities$nom_commune,"(", other_cities$cog, ")"))
  }

  ## Remove wrong results
  result <- result %>%
    dplyr::select(.data$cog, .data$latitude, .data$longitude) %>%
    dplyr::anti_join(wrong_result,
                     by = "cog")

  ## add lon. and lat. to original table
  table_communes %>%
    dplyr::left_join(result,
                     by = "cog")

}
