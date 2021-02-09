xlsx_path <- system.file("example-data/02_simplified.xlsx", package = "evavelo")
wb_obj <- openxlsx::loadWorkbook(xlsx_path)

test_that("process_evavelo works", {

  expect_type(output <- process_evavelo(xlsx_path) %>%
                suppressMessages(),
              "list")
  ## Check than reading from path or object is similar
  expect_equal(output,
               process_evavelo(wb_obj) %>%
                 suppressMessages())
})
