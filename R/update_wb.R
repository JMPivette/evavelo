
#' Update a workbook with data from R object
#'
#' This function doesn't return an value but modifies object `wb`
#'
#' @param wb a workbook object created with openxlsx::loadWorkbook()
#' @param data a named list of data.frames
#' Each name correspond to a worksheet name to update
#' Each data.frame column is corresponding to a column name in wb
#'
#' @return invisible(0)
#' @export

update_wb <- function(wb, data){
  purrr::walk(.x = names(data),
              .f = update_ws,
              wb = wb,
              data = data)
  invisible(0)
}




#' Update a specific worksheet from a workbook.
#'
#' @param wb a workbook object created with openxlsx::loadWorkbook()
#' @param data a named list of data.frames
#' @param sheet name of the sheet that needs to be updated
#'
#' @return invisible(0)
#' @keywords internal

update_ws <- function(wb, data, sheet){
  col_names <- openxlsx::readWorkbook(wb, sheet = sheet) %>%
    names() %>%
    janitor::make_clean_names()

  data <- data[[sheet]]

  for (n in names(data)){
    col_idx <- which(col_names == n)
    openxlsx::writeData(wb,
                        sheet = sheet,
                        data[n],
                        startRow = 2,
                        startCol = col_idx,
                        colNames = FALSE)
  }
}
