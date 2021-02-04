#' geocode a tibble with french cities and postcode
#'
#' @param .data a dataframe that needs to be updated
#' @param city_col column name containing the city name in .data
#' This value will also be used to name the new columns.
#' @param cp_col  postcode columns
#' @param country_col optional contry_col (to avoid geocoding non-french cities)
#'
#' @return a the input data.frame with 3 new columns with a name based on city_col (_lat, _long, _cog)
#' @export
#'

geocode_cities_cp <- function(.data, city_col, cp_col, country_col = NULL){
  ## enquo col_names
  city_col <- rlang::enquo(city_col)
  cp_col <- rlang::enquo(cp_col)

  ## Identify "non-french" addresses if country_col is provided--------------------
  if(!missing(country_col)){
    country_col <- rlang::enquo(country_col)
    data <- .data %>%
      dplyr::transmute(!!city_col, !!cp_col,
                       french = is.na(!!country_col) | !!country_col == "France")
  } else {
    ## Assume that all addresses are french
    data <- .data %>%
      dplyr::transmute(!!city_col, !!cp_col,
                       french = TRUE)
  }

  ## Detecting cities with postcode ---------------
  city_list_cp <- data %>%
    banR::geocode_tbl(tbl = .,
                      adresse = !!city_col,
                      code_postal = !!cp_col) %>%
    suppressMessages() %>%
    dplyr::mutate(
      geocode_ok = (.data$result_type == "municipality" &
                      .data$result_score >= 0.8 &
                      .data$french == TRUE)
    )

  ## Define name for the new columns.
  city_col_name <- rlang::as_name(city_col)

  result <- city_list_cp %>%
    dplyr::transmute(
      dplyr::across(c(.data$latitude, .data$longitude),
                    ~dplyr::if_else(geocode_ok,
                                    .x,
                                    NA_real_)),
      cog = dplyr::if_else(.data$geocode_ok,
                           .data$result_citycode,
                           NA_character_)
    ) %>%
    dplyr::rename_with(~ paste0(city_col_name, "_",
                                stringr::str_sub(.x, 1, 3))
    ) %>%
    dplyr::bind_cols(.data, .)

  ## Checking wrong results and propose an alternative in warnings-------------------

  city_list_cp %>%
    filter(!is.na(!!city_col)) %>%
    filter(.data$french == TRUE) %>%
    filter(is.na(.data$geocode_ok) | .data$geocode_ok == FALSE) %>%
    dplyr::select(
      city = !!city_col, postcode = !!cp_col,
      .data$result_type, .data$result_score, .data$result_label, .data$result_postcode) %>%
    dplyr::distinct() %>%
    check_warn_cities_cp()

  ## Return result ---------------------
  result

}


#' Check a list of cities and try to find replacement values
#'
#' Information is printed in warnings. This function doesn't return any values.
#'
#' @param data a data.frame with the following columns:
#' city (string), postcode(string), result_type, result_score, result_label, result_postcode.
#' This data.frame is produced using banR::geocode_tbl()
#'
#' @return invisible(0)

check_warn_cities_cp <- function(data){

  # Checking for "name" mispelling (wrong name with correct postcode)
  ## . Mispelling wih correct postcode -------------------------
  mispelling <- data %>%
    dplyr::filter(.data$result_type == "municipality" & .data$result_score < 0.8) %>%
    dplyr::select(
      .data$city, .data$postcode,
      .data$result_label, .data$result_postcode, .data$result_score
    ) %>%
    dplyr::distinct() %>%
    ## Improve score to be consistent with score given without postcode (postcode is good)
    ## Score is now between .5 and 1
    dplyr::mutate(result_score = 1-((1-.data$result_score)/2))


  ## . Wrong post code --------------------
  bad_city_nocp <- data %>%
    filter(is.na(.data$result_type) | .data$result_type != "municipality") %>%
    dplyr::distinct(.data$city, .data$postcode) %>%
    banR::geocode_tbl(tbl = .,
                      adresse = city) %>%
    suppressMessages() %>%
    select(
      .data$city, .data$postcode,
      .data$result_label, .data$result_postcode,
      .data$result_score, .data$result_type)

  wrong_cp <- bad_city_nocp %>%
    filter(.data$result_type == "municipality" & .data$result_score > .8) %>%
    select(-dplyr::any_of(c("result_type")))

  ## . Remaining mismatches  ---------------
  last_search <- bad_city_nocp %>%
    dplyr::anti_join(wrong_cp, by = c("city", "postcode")) %>%
    filter(!is.na(.data$city)) %>%
    select(.data$city, .data$postcode) %>%
    geocode_row_by_row(city_col = .data$city) %>%
    select(
      .data$city, .data$postcode ,.data$result_label, .data$result_postcode, .data$result_score
    )

  ## . Send warning---------------------
  wrong_with_proposal <- last_search %>%
    filter(!is.na(.data$result_label)) %>%
    dplyr::bind_rows(mispelling, wrong_cp) %>%
    dplyr::arrange(dplyr::desc(.data$result_score))

  wrong_no_proposal <- last_search %>%
    filter(is.na(.data$result_label))

  if(nrow(wrong_no_proposal)!=0 & nrow(wrong_with_proposal))
    warning("Des villes n'ont pas pu etre reconnue et seront ignorees: \n",
            call. = FALSE)

  if(nrow(wrong_no_proposal)!=0)
    warning("Villes inconnues:\n",
            paste0("\t", wrong_no_proposal$city, "(", wrong_no_proposal$postcode, ")\n"),
            call. = FALSE)


  if(nrow(wrong_with_proposal) != 0){
    warning("\nLes villes suivantes ont peut-etre besoin d'etre corrigees:\n\t",
            paste0(wrong_with_proposal$city," (", wrong_with_proposal$postcode,") -> \t",
                   wrong_with_proposal$result_label, " (",wrong_with_proposal$result_postcode, ")\n\t"),
            call. = FALSE)
  }
  invisible(0)
}


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
  city_col <- rlang::enquo(city_col)
  .data %>%
    dplyr::pull(!!city_col) %>%
    purrr::map_df(~ geocode_city(.x,score_limit = score_limit)) %>%
    dplyr::bind_cols(.data, .)

}



#' Geocode one city name
#'
#' wrapper around banR::geocode() but that limits to one result of type "municipality"
#'
#' @param city_name a vector of city names
#' @param score_limit results below this score will be ignored and replaced by NAs
#'
#' @return a one line data.frame

geocode_city <- function(city_name, score_limit = 0.4){
  res <- banR::geocode(city_name, limit = 1, type = "municipality") %>%
    suppressMessages()
  if(nrow(res) == 0 || res$score < score_limit)
    return(data.frame(result_label = NA))

  names(res) <- stringr::str_c("result_", names(res))
  res

}
