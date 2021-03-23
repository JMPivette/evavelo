
test_that("check_num_outliers() works", {

  ## on raw data
  check <- check_num_outliers(evavelo_example)

  ## Check that we have a list of data.frames
  purrr::walk(check,
              ~ expect_s3_class(., "data.frame"))

  ## on data with corrected categories
  check_corrected <- check_num_outliers(
    evavelo_example,
    categorie_corrige = correct_categ(
      evavelo_example$comptage,
      evavelo_example$enquete)$enquetes_post_traitement$categorie_corrige
  ) %>%
    suppressMessages()

  ## Check that we have a list of data.frames
  purrr::walk(check_corrected,
              ~ expect_s3_class(., "data.frame"))


})
