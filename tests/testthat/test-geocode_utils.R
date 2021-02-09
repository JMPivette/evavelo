test_that("geocode_city", {
  expect_s3_class(geocode_city("perros"),
                  "data.frame")

  expect_equal(nrow(geocode_city("rennes")),
               1)

  ## NA, empty or no result sends back a 1 row data.frame with Nas
  expect_true(is.na(geocode_city(NA)$result_label))
  expect_true(is.na(geocode_city("")$result_label))
  expect_true(is.na(geocode_city("zzzzz")$result_label))

})
