
#' geocode table_communes cities
#'
#' This function check that the municipalities defined in tables_communes are correct
#' and geocodes when possible
#'
#' @param table_communes a data.frame containing columns nom_commune and cog
#'
#' @return a data.frame similar to table_communes with latitude and longitude added.
#' And also proposition_nom_commune and proposition_cog
#' @importFrom rlang .data
#'
#' @keywords internal
#'

geocode_table_communes <- function(table_communes){
  ## Geocode with local data base (exact match)
  local_result <- table_communes %>%
    dplyr::transmute(.data$cog,
                     city = rename_french_cities(.data$nom_commune)
    ) %>%
    dplyr::left_join(
      france_cities,
      by = c("city", "cog")
    ) %>%
    tidyr::drop_na() %>%
    dplyr::select(.data$cog,
                  longitude = .data$lon,
                  latitude = .data$lat) %>%
    dplyr::group_by(.data$cog) %>%
    dplyr::slice_head() %>%
    dplyr::ungroup()


  ## Geocode ont founds with banR
  result <- table_communes %>%
    dplyr::anti_join(local_result, by = "cog") %>%
    dplyr::select(.data$cog,
                  .data$nom_commune) %>%
    dplyr::mutate(
      city_renamed = stringi::stri_trans_general(.data$nom_commune,id = "Latin-ASCII") # to avoid strange result from geocode_tbl
    ) %>%
    banR::geocode_tbl(tbl = .,
                      adresse = city_renamed,
                      code_insee = cog) %>%
    dplyr::select(-.data$city_renamed) %>%
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

  message("...V\u00e9rification de table_communes.............")
  if(nrow(old_cities) != 0){
    message("Les communes suivantes n\'existent plus et doivent etre remplac\u00e9es par les communes nouvelles:",
            paste0("\n\t",old_cities$nom_commune,"(", old_cities$cog,") -> ",
                   old_cities$result_city, "(",old_cities$result_citycode, ")"))
  }

  ## Failed to identify
  other_cities <- wrong_result %>%
    dplyr::anti_join(old_cities,
                     by = names(wrong_result))

  if(nrow(other_cities) != 0){
    message("Impossible de reconna\u00eetre les communes suivantes:",
            paste0("\n\t",other_cities$nom_commune,"(", other_cities$cog, ")"))
  }

  ## Remove wrong results and binding local results
  result <- result %>%
    dplyr::select(.data$cog, .data$latitude, .data$longitude) %>%
    dplyr::anti_join(wrong_result,
                     by = "cog") %>%
    dplyr::bind_rows(local_result)

  ## add lon. and lat. to original table
  result <- table_communes %>%
    dplyr::left_join(
      result,
      by = "cog"
    )

  ## Add proposal correction inside the data:
  old_cities <- old_cities %>%
    dplyr::transmute(
      .data$cog,
      proposition_nom_commune = .data$result_city,
      proposition_cog = .data$result_citycode
    )

  dplyr::left_join( ## store information of proposal correction
    result,
    old_cities,
    by = "cog")

}
