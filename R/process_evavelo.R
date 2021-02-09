#' Read and process a eva-velo file
#'
#'
#'
#' @param file an xlsx file, Workbook object or URL to xlsx file
#' @param check  a boolean. Is data checked before being processed.
#' This value can be set to FALSE if you already ised check_evavelo() and don't want to reun it twice
#'
#' @return a list of corrected categories
#' @export
#'
process_evavelo <- function(file,
                            check = TRUE){
  ## Sheet: Comptage Post Traitements-------------
  comptage <- read_comptage(file)
  ## Sheet: Enquetes Post Traitements-----------
  enquete <- read_enquete(file)
  ## Sheet: Calendrier--------------------------
  calendrier <- read_calendrier(file)
  ## Sheet before "post_traitement"
  comptage_init <- read_comptage(file, init = TRUE)
  enquete_init <- read_enquete(file, init = TRUE)

  ## Check values
  if (check == TRUE){
    check_result <- check_evavelo(calendrier = calendrier,
                                  comptage = comptage,
                                  enquete = enquete,
                                  comptage_init = comptage_init,
                                  enquete_init = enquete_init)
    if (check_result$error == TRUE){
      warning("Impossible de traiter le fichier qui comporte des erreurs")
      message(check_result$log)
      return(NULL)
    }
  }


    correct_categ(comptage, enquete)


}

