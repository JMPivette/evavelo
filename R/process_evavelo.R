#' Read and process a eva-velo file
#'
#'
#'
#' @param file an evadata object obtained with read_evavelo() or an xlsx file, Workbook object or URL to xlsx file
#' @param check  a boolean. Is data checked before being processed.
#' This value can be set to FALSE if you already used check_evavelo() and don't want to run it twice
#'
#' @return a list of corrected categories
#' @export
#'
process_evavelo <- function(file,
                            check = TRUE){
  ## Import data
  if(!is.evadata(file))
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

  ## process

  bind_list_df(
    correct_categ(file$comptage,
                  file$enquete),
    calc_distance(file)
  )


}

