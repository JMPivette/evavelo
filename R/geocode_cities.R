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
#' In case of correction proposition, a new colmuns starting with proposition_ is also added
#' @keywords internal
#'

geocode_cities <- function(.data, city_col){
  city_col <- rlang::enquo(city_col)
  city_col_name <- rlang::as_name(city_col)

  ## Geocode-------------------

  result <- geocode_df_cities(.data, !!city_col)

  ## Warning  and errors -----------------------------------------------
  message("\n...V\u00e9rification de ",
          city_col_name, ".............")
  errors <- result %>%
    dplyr::filter(is.na(.data$result_lat) & !is.na(!!city_col)) %>%
    dplyr::pull(!!city_col) %>%
    unique() %>%
    sort()

  replaced_to_check <- result %>%
    dplyr::mutate(row_id = dplyr::row_number()) %>%
    dplyr::filter(.data$result_score < 0.9) %>%
    dplyr::rename(city = !!city_col) %>%
    dplyr::select(
      .data$row_id,
      .data$city,
      .data$result_name,
      .data$result_cog)


  if(length(errors) != 0)
    message("Impossible de trouver les communes suivantes:",
            paste("\n\t",errors))

  ## Warn on interpretation and store the information
  if(nrow(replaced_to_check) != 0){
    ## Warns
    replaced_recap <- replaced_to_check %>%
      dplyr::distinct(.data$city,
                      .data$result_name,
                      .data$result_cog) %>%
      dplyr::arrange(.data$city)
    message("Interpretation de communes mal nomm\u00e9es:",
            paste0("\n\t",replaced_recap$city, " -> ",
                   replaced_recap$result_name, " (",
                   replaced_recap$result_cog, ") ")
    )

    ## Store interpretation
    proposition_name <- paste0("proposition_", city_col_name )
    col_to_add <- replaced_to_check %>%
      dplyr::select(
        .data$row_id,
        !!proposition_name := .data$result_name
      ) %>%
      dplyr::left_join(
        x = data.frame(row_id = seq_len(nrow(result))),
        y = .,
        by = "row_id"
      ) %>%
      dplyr::select(-.data$row_id)

    result <- result %>%
      dplyr::bind_cols(col_to_add)

  }
  ## reformat output------------------------------------------
  result %>%
    dplyr::select(-.data$result_name, -.data$result_score) %>%
    dplyr::rename_with(
      .fn = ~ stringr::str_replace(
        .x,
        "^result_",
        paste0(city_col_name, "_")
      )
    )

}
