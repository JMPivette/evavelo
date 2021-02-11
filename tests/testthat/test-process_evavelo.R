test_that("process_evavelo works", {

  expect_type(output <- process_evavelo(evavelo_example) %>%
                suppressMessages(),
              "list")

})
