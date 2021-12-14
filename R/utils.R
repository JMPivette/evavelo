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


#' Compare two data.frames (initial and  post treatment)
#'
#' In this comparison, all elements from init data.frame should appear inside post data.frame
#' post data.frame can only have new columns but no new rows.
#'
#' This function doesn't return anything but creates warnings if there are mismatches.
#'
#' @param init a data.frame (initial data.frame)
#' @param post a data.frame (post treatment data.frame)
#' @param key by default (NULL) the row order of both data.frame has to be the same.
#' by using a key column name, both data.frames are ordered before being compared.
#'
#' @importFrom utils head
#' @return invisible(0)
#' @keywords internal
#'
compare_init_post <- function(init,
                              post,
                              key = NULL){
  ## Select only columns from init data.frame
  post <- post %>%
    dplyr::select(
      dplyr::any_of(names(init))
    )

  ## Test number of rows
  if(nrow(post) != nrow(init)){
    warning(
      " Le nombre de lignes est diff\u00e9rent (",
      nrow(init), " / ", nrow(post), ")",
      call. = FALSE
    )
    if(is.null(key)){ ## Truncate and keep same number of rows if no key is provided.
      min_row <- min(nrow(init), nrow(post))
      init <- head(init, n = min_row)
      post <- head(post, n = min_row)
    }

  }
  ## Test for missing columns or different data type
  mismatch_col <- janitor::compare_df_cols(
    post = post,
    init = init
  ) %>%
    dplyr::filter(.data$init != .data$post | is.na(.data$post)) %>%
    dplyr::pull(.data$column_name)

  if(length(mismatch_col != 0))
    warning(
      " Les colonnes suivantes sont diff\u00e9rentes: \n\t",
      paste(mismatch_col, collapse = ", "),
      call. = FALSE
    )

  ## Reorder post compared to init if key is provided. Keep the same number of rows otherwise
  if(!is.null(key)){
    ## remove duplicated key if any (no warnings since the check is done later.)
    if(anyDuplicated(init[[key]]) | anyDuplicated(post[[key]])){
      warning(" Il y a des doublons dans '", key,
              "', la comparaison des valeurs ne peut pas \u00eatre effectu\u00e9e",
              call. = FALSE)
      return(invisible(0))
    } else {
      ## order post as init. (and keep the same number of lines)
      post <- post[match(init[[key]], post[[key]]),]
    }
  }

  ## On other columns, check for exactly the same values
  df_compare(
    dplyr::select(init, -dplyr::all_of(mismatch_col)),
    dplyr::select(post, -dplyr::any_of(mismatch_col))
  )

  invisible(0)

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


#' Multiply a list of vectors element-wise and and sum the result
#'
#' Useful function inside a summarize() to multiply columns together especially if some are booleans.
#' This way we can summarize in a single command with different "pseudo-filters"
#'
#' @param ... a list of vectors
#' @param na.rm Remove NA during the summation. Default to TRUE
#'
#' @return a single value
#' @keywords internal
#'

sum_prod <- function(..., na.rm = TRUE){
  sum(purrr::reduce(list(...), `*`),
      na.rm = na.rm)

}

# not_in function
"%ni%" <- Negate("%in%")


#' Check if  data.frame has 0 row
#'
#' @param df
#'
#' @return a boolean
#' @keywords internal

is_empty_df <- function(df){
  nrow(df) == 0
}
