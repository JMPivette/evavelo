test_that("geocode_cities works", {
  correct_df <- tibble::tribble(
    ~city, ~a, ~b, ~country,
    "Nantes",  1, 1.0, "France",
    "Saint-Brieuc",  2, 2.0, "France"
  )

  light_mispelling_df <- tibble::tribble(
    ~city, ~a, ~b, ~country,
    "Nantes",  1, 1.0, "France",
    "St Brieuc",  2, 2.0, "France"
  )

  heavy_mispelling_df <- tibble::tribble(
    ~city, ~a, ~b, ~country,
    "Nantes", 1, 1.0, "France",
    "Perros", 2, 2.0, "France"
  )

  other_country_df <- tibble::tribble(
    ~city,  ~a, ~b, ~country,
    "Nantes", 1, 1.0, "France",
    "Canberra", 2, 2.0, "Australie"
  )


  na_df <- tibble::tribble(
    ~city, ~a, ~b, ~country,
    NA, 1, 1.0, "France",
    NA, 2, 2.0, "Australie"
  )


  expect_snapshot_output(
    geocode_cities(correct_df, city_col = city) %>% suppressMessages())

  expect_snapshot_output(
    geocode_cities(light_mispelling_df, city_col = city)%>% suppressMessages())

  expect_snapshot_output(
    geocode_cities(heavy_mispelling_df, city_col = city)%>% suppressMessages())

  expect_snapshot_output(
    geocode_cities(other_country_df, city_col = city)%>% suppressMessages())

  expect_snapshot_output(
    geocode_cities(na_df, city_col = city)%>% suppressMessages())
})
