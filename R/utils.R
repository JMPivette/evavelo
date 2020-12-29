#' Check that columns are present in a data.frame
#'
#' THrows an error if some columns `cols` are not present in the data.frame `x`
#'
#' @param x a data.frame
#' @param cols a columns names vector
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
