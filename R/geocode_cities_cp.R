#' geocode a data.frame with french cities and postcode
#'
#' Since there is a postcode information, this function is stricter than geo_code_cities()
#' mispleeing or wrong postcode are not taken in account but an informative warning is created.
#'
#' If country_col is provided, the
#'
#' @param .data a dataframe that needs to be updated
#' @param city_col column name containing the city name in .data
#' This value will also be used to name the new columns.
#' @param cp_col  postcode columns
#' @param country_col contry_col (used to define the type of geocoding)
#'
#' @return the input data.frame with 3 new columns with a name based on city_col (_lat, _long, _cog)
#' @keywords internal
#'

geocode_cities_cp <- function(.data, city_col, cp_col, country_col){
  ## enquo col_names
  country_col <- rlang::enquo(country_col)
  city_col <- rlang::enquo(city_col)
  cp_col <- rlang::enquo(cp_col)
  city_col_name <- rlang::as_name(city_col)

  ## Define names of new columns
  new_cols <- list(NA_real_, NA_real_, NA_character_)
  names(new_cols) <- paste0(city_col_name, c("_lat", "_lon", "_cog"))

  # Geocode Cities -----------------------------------------------------------------------
  message("\n...V\u00e9rification de ",
          city_col_name, ".............")

  result <- .data %>%
    ## Geocode french cities with postalcode
    geocode_df_cities_cp(!!city_col,
                         !!cp_col,
                         !!country_col) %>%
    ## Geocode Foreign cities with country name
    geocode_df_foreign_cities(!!city_col,
                              !!country_col)

  ## add empty cols if they have not been added during the geocoding process
  result %>%
    tibble::add_column(!!!new_cols[setdiff(names(new_cols), names(result))])

}


#' geocode df for foreign cities
#'
#' A variation of geocode_df_cities for foreign cities.
#' Instead of using banR, we use tidygeocoder with osm (nomatim) API
#'
#' Messages are not displayed when there is no error.
#' input.data is not altered even if contains special characters.
#' Column order is also preserved.
#' Countries that not "France" or NA will not be altered by this function
#'
#' @param .data a data.frame
#' @param city_col name of the column that contains the city names
#' @param country_col name of the column that contains the country names
#'
#' @return .data with additional (or updated) columns named after city_col with the extension:
#' _lat _lon and _cog
#' @keywords internal

geocode_df_foreign_cities <- function(.data,
                                      city_col,
                                      country_col){
  city_col <- rlang::enquo(city_col)
  city_col_name <- rlang::as_name(city_col)
  country_col <- rlang::enquo(country_col)
  input_data <- .data
  ## Geocode ----------------------
  foreign_cities <- input_data %>%
    dplyr::transmute(
      id_rows = dplyr::row_number(),
      city = !!city_col,
      country = !!country_col) %>%
    dplyr::filter(!is.na(.data$country) &
                    !is.na(.data$city) &
                    .data$country != "France") %>%
    dplyr::mutate(
      country = dplyr::case_when(
        country == "Angleterre" ~ "Royaume-Uni",
        TRUE ~ country)
    )

  if(nrow(foreign_cities) == 0)
    return(.data)

  result <- foreign_cities %>%
    tidygeocoder::geocode(city = city,
                          country = country,
                          method = "osm",
                          full_results = TRUE)

  ## Warns on geocoding that failed -------------------
  wrong <- result %>%
    dplyr::filter(is.na(.data$type) |
                    !.data$type %in% c("city", "administrative")) %>%
    dplyr::select(.data$city, .data$country)

  if(nrow(wrong) != 0)
    message("Villes inconnues:",
            paste0("\n\t", wrong$city, " (", wrong$country, ")"))


  ## Format data for output ----------------------------
  index <- input_data %>%
    dplyr::transmute(id_rows = dplyr::row_number())

  ## Compute vectors to add to the data.frame.
  col_to_add <- result %>%
    dplyr::filter(.data$type %in% c("city", "administrative")) %>%
    dplyr::select(.data$id_rows,
                  .data$lat,
                  .data$long) %>%
    dplyr::right_join(index, by = "id_rows") %>%
    dplyr::arrange(.data$id_rows) %>%
    dplyr::select(-.data$id_rows) %>%
    dplyr::mutate(cog = NA_character_) %>% ## For compatibility with other functions (French)
    dplyr::rename_with(~ paste0(city_col_name, "_",
                                stringr::str_sub(.x, 1, 3)))
  ## "Coalesce" new values with potential existing values.
  col_to_add <- col_to_add %>%
    dplyr::coalesce(input_data %>%
                      dplyr::select(dplyr::any_of(names(col_to_add))))
  ## update input data
  input_data %>%
    dplyr::select(-dplyr::any_of(names(col_to_add))) %>%
    dplyr::bind_cols(col_to_add)
}

#' geocode df with postal code
#'
#' A variation of geocode_df_cities for data with postcode provided
#' Use of banR. Countries that are not "France" or NA will not be altered by this function
#'
#' Messages are not displayed when there is no error.
#' input.data is not altered even if contains special characters.
#' Column order is also preserved
#'
#' @param .data a data.frame
#' @param city_col name of the column that contains the city names
#' @param cp_col name of the column that contains postal code (french)
#' @param country_col name of the column that contains the country names
#'
#' @return .data with additional (or updated) columns named after city_col with the extension:
#' _lat _lon and _cog
#'
#' @keywords internal

geocode_df_cities_cp <- function(.data,
                                 city_col,
                                 cp_col,
                                 country_col){
  city_col <- rlang::enquo(city_col)
  city_col_name <- rlang::as_name(city_col)
  cp_col <- rlang::enquo(cp_col)
  cp_col_name <- rlang::as_name(cp_col)
  country_col <- rlang::enquo(country_col)
  input_data <- .data

  # Geocode -------------------------------------------------------------------------------------

  french_cities <- input_data %>%
    dplyr::mutate(id_rows = dplyr::row_number()) %>%
    dplyr::filter(!is.na(!!city_col)) %>%
    dplyr::filter(is.na(!!country_col) | !!country_col == "France") %>%
    dplyr::select(.data$id_rows,
                  !!city_col,
                  !!cp_col) %>%
    dplyr::mutate(
      !!cp_col_name := stringr::str_pad(!!cp_col,### converting postcode that have less than 5 digits.
                                        5L,
                                        side = "left",
                                        pad ="0")
    )

  if(nrow(french_cities) == 0) ## Nothing to geocode
    return(.data)

  result <- french_cities%>%
    banR::geocode_tbl(tbl = .,
                      adresse = !!city_col,
                      code_postal = !!cp_col) %>%
    suppressMessages() %>%
    dplyr::mutate(
      geocode_ok = (.data$result_type == "municipality" &
                      .data$result_score >= 0.8)
    )

  ## Checking wrong results and propose an alternative in warnings-------------------
  anomaly_to_check <- result %>%
    dplyr::filter(is.na(.data$geocode_ok) | .data$geocode_ok == FALSE) %>%
    dplyr::select(
      city = !!city_col, postcode = !!cp_col,
      .data$result_type, .data$result_score, .data$result_label, .data$result_postcode) %>%
    dplyr::distinct()

  if(nrow(anomaly_to_check) !=0)
    check_warn_cities_cp(anomaly_to_check)


  ## Format data for output ----------------------------
  index <- input_data %>%
    dplyr::transmute(id_rows = dplyr::row_number())

  col_to_add <- result %>%
    dplyr::filter(.data$geocode_ok) %>%
    dplyr::select(.data$id_rows,
                  .data$latitude,
                  .data$longitude,
                  cog = .data$result_citycode) %>%
    dplyr::right_join(index, by = "id_rows") %>%
    dplyr::arrange(.data$id_rows) %>%
    dplyr::select(-.data$id_rows) %>%
    dplyr::rename_with(~ paste0(city_col_name, "_",
                                stringr::str_sub(.x, 1, 3)))

  ## "Coalesce" new values with potential existing values.
  col_to_add <- col_to_add %>%
    dplyr::coalesce(input_data %>%
                      dplyr::select(dplyr::any_of(names(col_to_add))))
  ## update input data
  input_data %>%
    dplyr::select(-dplyr::any_of(names(col_to_add))) %>%
    dplyr::bind_cols(col_to_add)

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
#' @keywords internal
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
    dplyr::filter(is.na(.data$result_type) | .data$result_type != "municipality") %>%
    dplyr::distinct(.data$city, .data$postcode)

  if(nrow(bad_city_nocp)!=0){
    bad_city_nocp <- bad_city_nocp %>%
      banR::geocode_tbl(tbl = .,
                        adresse = city) %>%
      suppressMessages() %>%
      dplyr::select(
        .data$city, .data$postcode,
        .data$result_label, .data$result_postcode,
        .data$result_score, .data$result_type)

    wrong_cp <- bad_city_nocp %>%
      dplyr::filter(.data$result_type == "municipality" & .data$result_score > .8) %>%
      dplyr::select(-dplyr::any_of(c("result_type")))

    ## . Remaining mismatches  ---------------
    last_search <- bad_city_nocp %>%
      dplyr::anti_join(wrong_cp, by = c("city", "postcode")) %>%
      dplyr::filter(!is.na(.data$city)) %>%
      dplyr::select(.data$city, .data$postcode) %>%
      geocode_row_by_row(city_col = .data$city) %>%
      dplyr::select(
        dplyr::any_of(c("city", "postcode", "result_label", "result_postcode", "result_score"))
        #.data$city, .data$postcode ,.data$result_label, .data$result_postcode, .data$result_score
      )
  } else{
    wrong_cp <- data.frame()
    last_search <- data.frame()
  }


  if(nrow(last_search) != 0){
    last_search_proposal <- last_search %>%
      dplyr::filter(!is.na(.data$result_label))
    wrong_no_proposal <- last_search %>%
      dplyr::filter(is.na(.data$result_label)) %>%
      dplyr::arrange(.data$city)
  } else {# empty data.frames to avoid error subsetting non-existing cols
    last_search_proposal <- data.frame()
    wrong_no_proposal <- data.frame()
  }

  ## . Send warning---------------------

  wrong_with_proposal <- dplyr::bind_rows(last_search_proposal,
                                          mispelling,
                                          wrong_cp) %>%
    dplyr::arrange(.data$city)

  if(nrow(wrong_no_proposal)!=0)
    message("Villes inconnues:",
            paste0("\n\t", wrong_no_proposal$city, "(", wrong_no_proposal$postcode, ")"))


  if(nrow(wrong_with_proposal) != 0){
    message("Les villes suivantes ont ete ignor\u00e9es. Propositions de corrections:",
            paste0("\n\t",wrong_with_proposal$city," (", wrong_with_proposal$postcode,") -> \t",
                   wrong_with_proposal$result_label, " (",wrong_with_proposal$result_postcode, ")"))
  }
  invisible(0)
}
