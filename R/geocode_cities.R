#' geocode a data.frame based on city names
#'
#' This function will try to find the best match in case of mispelling.
#' A warning message will be generated indicating the city that has been chosen in case of doubt.
#' This function is much more permissive to mispelling errors than geocode_cities_cp()
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

  ## Geocode-------------------

  result <- geocode_df_cities(.data, !!city_col)

  ## Warning  and errors -----------------------------------------------
  errors <- result %>%
    dplyr::filter(is.na(.data$result_lat) & !is.na(!!city_col)) %>%
    dplyr::pull(!!city_col) %>%
    unique()

  replaced_to_check <- result %>%
    dplyr::filter(.data$result_score < 0.9) %>%
    dplyr::rename(city = !!city_col) %>%
    dplyr::distinct(.data$city,
                    .data$result_name,
                    .data$result_cog)

  message("\nVerification de ",
          city_col_name, ".............")
  if(length(errors) != 0)
    message("Impossible de trouver les communes suivantes:",
            paste("\n\t",errors))

  if(nrow(replaced_to_check) != 0)
    message("Interpretation de communes mal nommees:",
            paste0("\n\t",replaced_to_check$city, " -> ",
                   replaced_to_check$result_name, " (",
                   replaced_to_check$result_cog, ") ")
            )
  ## reformat output------------------------------------------
  result %>%
    select(-.data$result_name, -.data$result_score) %>%
    dplyr::rename_with(
      .fn = ~ paste0(city_col_name, "_",
                     stringr::str_sub(stringr::str_remove(.x, "^result_"),
                                      1,3)),
      .cols = dplyr::starts_with("result_"))

}
