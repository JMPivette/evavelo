#' geocode a data.frame based on city names
#'
#' This function will try to find the best match in case of mispelling.
#' A warning message will be generated indicating the city that has been chosen in case of doubt.
#'
#' @param .data a dataframe that needs to be updated
#' @param city_col column name containing the city name in .data
#' This value will also be used to name the new columns.
#'
#' @return the input data.frame with 3 new columns with a name based on city_col (_lat, _long, _cog)
#' @export
#'

geocode_cities <- function(.data, city_col){
  city_col <- rlang::enquo(city_col)
  city_col_name <- rlang::as_name(city_col)

  .data <- .data %>%
    mutate(row_n = dplyr::row_number()) ## Necessary for result update later

  ## First try geocode_tbl()------------------
  city_list <- .data %>%
    select(!!city_col, .data$row_n) %>%
    filter(!is.na(!!city_col)) %>%
    banR::geocode_tbl(tbl = .,
                      adresse = !!city_col) %>%
    suppressMessages() %>%
    mutate(
      geocode_ok = (!is.na(.data$result_label) &
                      .data$result_type == "municipality" &
                      .data$result_score >= 0.9)
    ) %>%
    select(.data$row_n, !!city_col,
           result_latitude = .data$latitude, result_longitude = .data$longitude,
           result_cog = .data$result_citycode, .data$geocode_ok)

  # Create result
  result <- city_list %>%
    filter(.data$geocode_ok == TRUE) %>%
    select(.data$row_n, .data$result_latitude,
           .data$result_longitude, .data$result_cog) %>%
    left_join(.data,., by = "row_n")

  ## Second Try geocode_row_by_row() on non detected cities---------------
  corres_table_second_try <- city_list %>%
    dplyr::filter(.data$geocode_ok == FALSE) %>%
    dplyr::distinct(!!city_col) %>%
    geocode_row_by_row(city_col = !!city_col, score_limit = 0.5) %>%
    select(
      !!city_col, .data$result_latitude, .data$result_longitude,
      result_cog = .data$result_citycode, .data$result_score,
      .data$result_label, .data$result_postcode)

  # Update results
  result <- city_list %>%
    dplyr::select(!!city_col, .data$row_n) %>%
    dplyr::inner_join(select(corres_table_second_try,
                      !!city_col, .data$result_latitude, .data$result_longitude, .data$result_cog),
               by = city_col_name) %>%
    dplyr::rows_update(result, ., by = "row_n") %>%
    dplyr::rename_with(
      .fn = ~ paste0(city_col_name, "_",
                     stringr::str_sub(stringr::str_remove(.x, "^result_"),
                                      1,3)),
      .cols = dplyr::starts_with("result_")) %>%
    dplyr::select(-dplyr::any_of("row_n"))

 ## Warning  -----------------------------------------------
  errors <- corres_table_second_try %>%
    dplyr::filter(is.na(.data$result_latitude)) %>%
    dplyr::pull(!!city_col)

  replaced_to_check <- corres_table_second_try %>%
    dplyr::filter(.data$result_score < 0.9) %>%
    dplyr::rename(city = !!city_col)

  warning("\nVerification de ",
          city_col_name, "......\n")
  if(length(errors) != 0)
    warning("\nImpossible de trouver les communes suivantes:\n\t",
            paste(errors, "\n\t"))

  if(nrow(replaced_to_check) != 0)
    warning("\nInterpretation de communes mal nommees:\n\t",
            paste0(replaced_to_check$city, " -> ",
                   replaced_to_check$result_label, " (",
                   replaced_to_check$result_postcode, ") ",
                   "\n\t"))

  result
}
