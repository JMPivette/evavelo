
#' Geocode a data.frame of cities
#'
#' Equivalent to banR::geocode_tbl() but much slower (one API request per row)
#' This function is used when we only want "municipality" as result type which is impossible with geocode_tbl().
#'
#' @param .data a data.frame containing a columns with city names to geocode
#' @param city_col column that contains city names
#' @param score_limit results below this score will be ignored and replaced by NAs
#'
#' @return the input data.frame with geocoding infomation.

geocode_row_by_row <- function(.data,
                               city_col,
                               score_limit = 0.4){
  ## specific case with empty .data
  if(nrow(.data) == 0)
    return(dplyr::bind_cols(
      .data,
      data.frame(
        result_label = character(),
        result_postcode = character(),
        result_score = numeric(),
        result_latitude = numeric(),
        result_longitude = numeric(),
        result_citycode = character())
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
