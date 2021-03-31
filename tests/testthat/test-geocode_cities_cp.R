test_that("geocode_cities_cp works", {
  correct_df <- tibble::tribble(
    ~city, ~postcode, ~a, ~b, ~country,
    "Nantes", "44000", 1, 1.0, "France",
    "Saint-Brieuc", "22000", 2, 2.0, "France"
  )

  wrong_cp_df <- tibble::tribble(
    ~city, ~postcode, ~a, ~b, ~country,
    "Nantes", "44000", 1, 1.0, "France",
    "Saint-Brieuc", "22278", 2, 2.0, "France"
  )

  mispelling_df <- tibble::tribble(
    ~city, ~postcode, ~a, ~b, ~country,
    "Nantes", "44000", 1, 1.0, "France",
    "St Brieuc", "22000", 2, 2.0, "France"
  )

  other_country_df <- tibble::tribble(
    ~city, ~postcode, ~a, ~b, ~country,
    "Nantes", "44000", 1, 1.0, "France",
    "Canberra", "22000", 2, 2.0, "Australie"
  )

  na_df <- tibble::tribble(
    ~city, ~postcode, ~a, ~b, ~country,
    NA, NA, 1, 1.0, "France",
    NA, NA, 2, 2.0, "Australie"
  )

  expected_name <- c(names(correct_df), "city_lat", "city_lon", "city_cog")

  expect_snapshot_output(
    geocode_cities_cp(na_df,
                      city_col = city,
                      cp_col = postcode,
                      country_col = country) %>%
      suppressMessages())

  expect_snapshot_output(
    geocode_cities_cp(correct_df,
                      city_col = city,
                      cp_col = postcode,
                      country_col = country) %>%
      suppressMessages())

  expect_snapshot_output(
    geocode_cities_cp(wrong_cp_df,
                      city_col = city,
                      cp_col = postcode,
                      country_col = country) %>%
      suppressMessages())
  expect_snapshot_output(
    geocode_cities_cp(mispelling_df,
                      city_col = city,
                      cp_col = postcode,
                      country_col = country) %>%
      suppressMessages())
  expect_snapshot_output(
    geocode_cities_cp(other_country_df,
                      city_col = city,
                      cp_col = postcode,
                      country_col = country) %>%
      suppressMessages())

  expect_equal(
    geocode_cities_cp(wrong_cp_df,
                      city_col = city,
                      cp_col = postcode ,
                      country_col = country) %>%
      suppressMessages() %>%
      names(),
    expected_name)

})


