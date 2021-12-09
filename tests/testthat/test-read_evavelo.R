xlsx_path <- system.file("example-data/02_simplified.xlsx", package = "evavelo")
wb_obj <- openxlsx::loadWorkbook(xlsx_path)

test_that("read_evavelo works", {
  expect_type(output <- read_evavelo(xlsx_path) %>%
                suppressMessages(),
              "list")

  ## Check than reading from path or object is similar
  expect_equal(output,
               read_evavelo(wb_obj) %>%
                 suppressMessages())

})


test_that("missing data in comptages_automatiques creates only a warning (#77)",{

  ## Empty woksheet
  xlsx_path <- system.file("example-data/05_missing_comptages.xlsx", package = "evavelo")

  expect_warning(
    read_evavelo(xlsx_path),
    regexp = "onglet comptages_automatiques"
  ) %>%
    suppressWarnings()

  ## Worksheet with header but no data
  xlsx_path <- system.file("example-data/05_missing_comptages_bis.xlsx", package = "evavelo")

  expect_warning(
    read_evavelo(xlsx_path),
    regexp = "onglet comptages_automatiques"
  )
})

test_that("missing data in other sheets creates an error (#77)",{

  ## Empty enquetes woksheets
  xlsx_path <- system.file("example-data/06_missing_enquetes.xlsx", package = "evavelo")
  expect_error(
    read_evavelo(xlsx_path) %>%
      suppressWarnings()
  )


})
