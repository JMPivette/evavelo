xlsx_path <- system.file("example-data/02_simplified.xlsx", package = "evavelo")
wb_obj <- openxlsx::loadWorkbook(xlsx_path)

test_that("read_evavelo works", {
  expect_type(output <- read_evavelo(xlsx_path),
              "list")

  ## Check than reading from path or object is similar
  expect_equal(output,
               read_evavelo(wb_obj))

})

