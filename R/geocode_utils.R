
#' geocode df for cities
#'
#' Similar to banR::geocode_tbl() specifically for cities but with improvement:
#'
#' Size of the request is reduced by avoiding asking several times the same city.
#' NA returns NA ansd not cities starting with "na"
#' Messages are not displayed when there is no error.
#' input.data is not altered even if contains special characters. Column order is also preserved
#'
#' @param .data a data.frame
#' @param city_col name of the column that contains the city names
#'
#' @importFrom rlang .data
#'
#' @return .data with additional columns named :
#' result_name, result_cog, result_lat, result_lon, result_score
#' @keywords internal
#'
geocode_df_cities <- function(.data,
                              city_col){
  city <- rlang::enquo(city_col)
  city_col_name <- rlang::as_name(city)
  ## Create a correspondance table----------------
  unique_cities <- .data %>%
    dplyr::mutate(# In case of logical values (all NAs for example)
      !!city_col_name := dplyr::if_else(
        is.na(!!city),
        "",
        as.character(!!city))) %>%
    dplyr::distinct(!!city)


  # First round----
  cor_table <- unique_cities %>%
    banR::geocode_tbl(!!city) %>%
    suppressMessages() %>%
    dplyr::transmute(
      result_name = .data$result_city,
      result_cog = .data$result_citycode,
      result_lat = .data$latitude,
      result_lon = .data$longitude,
      .data$result_score,
      geocode_ok = !is.na(.data$result_cog) &
        .data$result_type == "municipality" ) %>%
    dplyr::bind_cols(unique_cities,.)

  # Second round-----
  cor_table_second <- cor_table %>%
    dplyr::filter(!.data$geocode_ok) %>%
    dplyr::select(!!city) %>%
    geocode_row_by_row(!!city) %>%
    dplyr::select(!!city, result_name = .data$result_city, result_cog = .data$result_citycode,
                  result_lat = .data$result_latitude, result_lon = .data$result_longitude,
                  .data$result_score)

  # Adding rounds together---------------------
  cor_table_first <- cor_table %>%
    dplyr::filter(.data$geocode_ok) %>%
    dplyr::select(-dplyr::any_of("geocode_ok"))

  cor_table <- dplyr::bind_rows(
    cor_table_first,
    cor_table_second
  )
  # %>% dplyr::rename_with(
  #   .fn = ~ paste0(city_col_name, "_",
  #                  stringr::str_remove(.x, "^result_")),
  #   .cols = dplyr::starts_with("result_"))

  ## Adding information to original data.frame---------------------
  .data %>%
    dplyr::left_join(cor_table,
              by = city_col_name)

}




#' Geocode a data.frame of cities row by row
#'
#' Equivalent to banR::geocode_tbl() but much slower (one API request per row)
#' This function is used when we only want "municipality" as result type which is impossible with geocode_tbl().
#'
#' @param .data a data.frame containing a columns with city names to geocode
#' @param city_col column that contains city names
#' @param score_limit results below this score will be ignored and replaced by NAs
#'
#' @return the input data.frame with geocoding infomation.
#' @keywords internal

geocode_row_by_row <- function(.data,
                               city_col,
                               score_limit = 0.4){
  ## specific case with empty .data
  if(nrow(.data) == 0)
    return(dplyr::bind_cols(
      .data,
      data.frame(
        result_label = character(),
        result_score = numeric(),
        result_id = character(),
        result_type = character(),
        result_name = character(),
        result_postcode = character(),
        result_citycode = character(),
        result_x = numeric(),
        result_y = numeric(),
        result_population = numeric(),
        result_city = character(),
        result_context = character(),
        result_importance = numeric(),
        result_type_geo = character(),
        result_longitude = numeric(),
        result_latitude = numeric())
    ))

  ## Normal process
  city_col <- rlang::enquo(city_col)
  result <- .data %>%
    dplyr::pull(!!city_col) %>%
    purrr::map_df(~ geocode_city(.x,score_limit = score_limit)) %>%
    dplyr::bind_cols(.data, .)

  return(result)
}



#' Geocode one city name
#'
#' wrapper around banR::geocode() but that limits to one result of type "municipality"
#'
#' Contrary to geocode(), if there is no result or if the request is empty we still get a one row data.frame
#' (that contains only Nas)
#'
#' @param city_name a string containing the city name wanted
#' @param score_limit results below this score will be ignored and replaced by NAs
#'
#' @return a one line data.frame
#' @keywords internal

geocode_city <- function(city_name, score_limit = 0.4){

  if (!is.na(city_name) & city_name != ""){
    res <- banR::geocode(city_name, limit = 1, type = "municipality") %>%
      suppressMessages()
  }else{
    res <- data.frame()
  }

  if(nrow(res) == 0 || res$score < score_limit)
    return(data.frame(result_label = NA_character_,
                      result_score = NA_real_,
                      result_id = NA_character_,
                      result_type = NA_character_,
                      result_name = NA_character_,
                      result_postcode = NA_character_,
                      result_citycode = NA_character_,
                      result_x = NA_real_,
                      result_y = NA_real_,
                      result_population = NA_real_,
                      result_city = NA_character_,
                      result_context = NA_character_,
                      result_importance = NA_real_,
                      result_type_geo = NA_character_,
                      result_longitude = NA_real_,
                      result_latitude = NA_real_)
           )

  names(res) <- stringr::str_c("result_", names(res))
  res

}
