#' Process an eva-velo file or evadata object
#'
#' @param file an evadata object obtained with read_evavelo() or an xlsx file, Workbook object or URL to xlsx file
#' @param check  a boolean. Is data checked before being processed.
#' This value can be set to FALSE if you already used check_evavelo() and don't want to run it twice
#'
#' @details
#' This function works with a lot of different `file` input. If an excel file is passed as input, this function will internally call `read_evavelo()` and `geocode_evavelo()`.
#' An evadata object or even an already geocoded object can be used as an input.
#'
#' This function will do the followings
#' \itemize{
#'   \item Compute distances between cities
#'   \item Compute categorie_corrige field
#' }
#' @return a list of data.frames with variables that have been changed
#' @export
#'
process_evavelo <- function(file,
                            check = TRUE){
  ## Import data
  if(is.evadata(file) == FALSE)
    file <- read_evavelo(file)


  ## Check values
  if (check == TRUE){
    check_result <- check_evavelo(file)
    if (check_result$error == TRUE){
      warning("Impossible de traiter le fichier qui comporte des erreurs")
      message(check_result$log)
      return(NULL)
    }
  }

  if(attr(file, "geocoded") == FALSE)
    file <- geocode_evavelo(file)

  ## process file
  distances_calculated <- calc_distance(file)

  ## Update computed variables that are going to be used by correct_categ()
  file$enquete$iti_dep_iti_valide <- distances_calculated$enquetes_post_traitement$iti_dep_iti_valide
  file$enquete$iti_arr_iti_valide <- distances_calculated$enquetes_post_traitement$iti_arr_iti_valide

  bind_list_df(
    correct_categ(file$comptage,
                  file$enquete),
    distances_calculated
  )


}

