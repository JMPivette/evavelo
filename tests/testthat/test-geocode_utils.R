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


test_that("geocode_df_cities works", {

  geo_df <- tibble::tribble(
    ~ville, ~a, ~b,
    "Nantes", 1, 1.0,
    "Perros", 2, 2.0,
    "Nantes", 1, 1.0,
    "Perros", 2, 2.0,
    "Nantes", 1, 1.0,
    "Canberra", 2, 2.0,
    NA, 1, 1.0,
    NA, 2, 2.0,
    "St Brieuc",  2, 2.0,
    "Saint-Brieuc",  2, 5.0,
    "par", 3,4,
    "L'argentière", 4,5
  )

  result <- geocode_df_cities(geo_df, city_col = ville)

  expect_equal(geo_df,
               result %>%
                 select(names(geo_df))
  )

  expect_error(geocode_df_cities(geo_df, city_col = city))

  na_df <- tibble::tribble(
    ~city, ~a, ~b, ~country,
    NA, 1, 1.0, "France",
    NA, 2, 2.0, "Australie"
  )

  expect_s3_class(geocode_df_cities(na_df, city),
                  "data.frame")

})
