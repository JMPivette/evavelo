#' Check that columns are present in a data.frame
#'
#' THrows an error if some columns `cols` are not present in the data.frame `x`
#'
#' @param x a data.frame
#' @param cols a columns names vector
#' @keywords internal
#'

df_has_cols <- function(x, cols) {
  parent_frame <- sys.parent()
  calling_function <- sys.call(parent_frame)[[1]]
  stopifnot(is.data.frame(x))
  if(!all(cols %in% colnames(x))) {
    not_present <- setdiff(cols, colnames(x))
    stop(paste(not_present, collapse = ", "), " missing from the data frame in function ", calling_function)
  }
}


#' Remove the multiple information from id_quest
#'
#' In a case of multiple id_quest for the same groups, they wil be named "ABC-01", "ABC-02", etc..
#' This function removes the suffix and keep only "ABC" in this example.
#'
#' @param id_quest a character vector of id_quest to modify
#'
#' @return a character vector the same size of `id_quest` without suffix
#' @keywords internal

radical_quest <- function(id_quest){
  stringr::str_remove(id_quest,
                      "-\\d+$")
}

## Helper function for "not in"
`%ni%` = Negate(`%in%`)


#' Test equality between vectors including NA values
#'
#' This function is a variant of == that takes in account NAs.
#' By default NA == NA returns FALSE in R
#' With this fonction, NAs can be compared
#'
#' @param a a vector
#' @param b a vector
#'
#' @return a boolean vector with element wise comparison
#' @keywords internal
#'
equal_with_na <- function(a,b){
  result <- a == b
  nas <- is.na(a) & is.na(b)
  dplyr::coalesce(result, nas)
}



#' Compare 2 data.frames
#'
#' This function compares 2 data.frames element_wise and returns a logical data.frame
#' NA values are also compared (as opposed to base R `==`)
#' Comparison is done columns by columns so both date.frames should be ordered.
#'
#' @param x a data.frame
#' @param y a data.frame
#' @param verbose boolean to add warning when there is a mismatch
#'
#' @return a logical data.frame
#' @keywords internal
#'
df_compare <- function(x, y, verbose = TRUE) {
  compare_lgl <- purrr::map2_df(x, y, equal_with_na)

  if (verbose){
    perc_equal <- purrr::map_df(compare_lgl,
                                ~ sum(.x)/length(.x)) %>%
      tidyr::pivot_longer(tidyr::everything(), values_to = "perc_equal") %>%
      dplyr::filter(perc_equal != 1)
    if(nrow(perc_equal) != 0){
      perc_equal <- perc_equal %>%
        dplyr::mutate(
          n_mismatch = round((1 - perc_equal) * nrow(compare_lgl)),
          perc_equal = scales::percent(.data$perc_equal, accuracy = 0.01)
        )
      warning("Les colonnes suivantes ne sont pas identiques \u00e0 100%:\n",
              paste("\t",perc_equal$name, " : \t",
                    perc_equal$perc_equal,
                    "(", perc_equal$n_mismatch, "diff\u00e9rences)","\n"),
              call. = FALSE)
    }
  }

  return(compare_lgl)
}


compare_init_post <- function(init, post_trait){
  post_trait <- post_trait %>%
    dplyr::select(names(init))

  mismatch_col <- janitor::compare_df_cols(
    post = post_trait,
    init = init
  ) %>%
    dplyr::filter(.data$init != .data$post) %>%
    dplyr::pull(.data$column_name)

  if(length(mismatch_col != 0))
    warning("Les colonnes suivantes sont diff\u00e9rentes: \n\t", paste(mismatch_col, collapse = ", "),
            call. = FALSE)
  df_compare(dplyr::select(init, -dplyr::all_of(mismatch_col)),
             dplyr::select(post_trait, -dplyr::all_of(mismatch_col)))


}

is.evadata <- function(x) inherits(x, "evadata")

#' An improved version of base R which.min that returns NA is all are Nas (instead of empty integer)
#'
#' @param x numeric vector whose min is searched for
#'
#' @return an integer vector with position of minimum value
#' @keywords internal
which_min <- function(x){
  if(all(is.na(x)))
    NA_integer_
  else
    which.min(x)
}


#' Column binds list of data.frames element by element
#'
#' This function will look at each element of the list y name (not by position) and will cbind the one with the same names
#' It returns one list with names of all the input lists
#'
#' @param ... several named lists of data.frames
#'
#' @return a list of data.frames
#' @keywords internal
#'
bind_list_df <- function(...){
  lst_df <- list(...)

  df_names <- purrr::map(lst_df, names) %>%
    unlist() %>%
    unique()

  purrr::map(df_names,
             .f = ~ dplyr::bind_cols(
               purrr::map(lst_df,
                          function(x) x[[.x]]))
  ) %>%
    stats::setNames(df_names)
}

#' Rename french cities names compatible to local data.base
#'
#' Convert a string vector and change it to capital without accent. Removes `-` or `'` and transform SAINT to ST.
#'
#' @param cities a string vector
#'
#' @return a string vector the same length as cities.
#' @keywords internal
rename_french_cities <- function(cities){
  cities %>%
    stringr::str_to_upper() %>%
    stringr::str_replace_all("[-\']", " ") %>%
    stringi::stri_trans_general(id = "Latin-ASCII") %>%
    stringr::str_replace_all("SAINT ", "ST ")%>%
    stringr::str_replace_all("SAINTE ", "STE ")
}
