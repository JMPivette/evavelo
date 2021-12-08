test_that("find_wrong_geocoding works", {
  expect_error(find_wrong_geocoding(evavelo_example))

  wrong_geo <- find_wrong_geocoding(evavelo_example_geocoded)
  expect_type(wrong_geo,
              "list")
  expect_equal(length(wrong_geo),
               6)

  purrr::walk(wrong_geo,
              ~ expect_s3_class(., "data.frame"))
})

test_that("geocode_evavelo() works", {


    geocoded_eva <- geocode_evavelo(evavelo_example) %>%
      suppressMessages()

    expect_true(
      attr(geocoded_eva, "geocoded")
      )


})
