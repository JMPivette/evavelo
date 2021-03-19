test_that("calc_distance works", {

  expect_type(calc_distance(evavelo_example_geocoded),
                  "list")

  expect_error(calc_distance(evavelo_example),
               "Cannot calculate distance on evadata that is not geocoded")
})
